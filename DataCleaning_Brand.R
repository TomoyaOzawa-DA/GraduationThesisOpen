### import Package
library(fastDummies)
library(tidyverse)
library(readxl)
library(qrng)


### import data
df_attri <- read.csv('attribute_codes.csv')
df_panelattri <- read.csv('panelist_attributes.csv')
df_panelist <- read.csv('panelist_list.csv')
df_panelpurchase <- read.csv('purchase_beverage.csv')
df_tvcom <- read.csv('tv_commercial.csv')
df_temp <- read.csv("temp.csv")
df_market <- read.csv("market_beverage.csv", fileEncoding = "utf8")
df_clean <- read_xlsx("cleaning.xlsx")



### 追加で必要なデータ作成
# 日付データ作成
list_week <- seq(as.Date("2016-12-26"), as.Date("2017-12-25"), by = "week")
list_week <- as.character(list_week)

# 商品カテゴリのデータを作成
brand_name <- unique(df_market$brand_name)
category <- c("Sports", "GreenTea", "Cola", "Coffee", "BlackTea", "GreenTea", 
              "Coffee", "GreenTea", "Water", "GreenTea", "Coffee", "Cider",
              "GreenTea", "Vegetable", "Soy")
df_category <- data.frame(brand_name = brand_name, category = category)

# 休日データ
holiday <- c(1,1,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,1,1)
df_holiday <- data.frame(week = list_week, holiday = holiday)

# 日本語を英語に直す
df_market$type <- gsub("販売個数マーケットサイズ", "units", df_market$type)
df_market$type <- gsub("販売金額マーケットサイズ", "money", df_market$type)
df_market$type <- gsub("販売容量マーケットサイズ", "volume", df_market$type)





#####データ加工①：購入者×期間×商品
### 当該商品ごとのt期における購入量と価格のデータ作成
## ブランド×容量でデータを変形
df_panelpurchase_edit <- merge(df_panelpurchase, df_clean, by = c("brand_name", "item_count_sum", "volume_sum"))
# unclearを含む買い物は削除
df_unclear <- df_panelpurchase_edit %>% 
  group_by(panelist_id, week) %>% 
  summarise(unclear = max(UNCLEAR))

df_panelpurchase_edit <- merge(df_panelpurchase_edit, df_unclear, by = c("panelist_id", "week"))

df_panelpurchase_edit_1 <- df_panelpurchase_edit %>% 
  filter(unclear == 0) %>%
  select(-c(4, 6, 7, 8, 10, 19, 20)) %>% 
  mutate(average_price_volume = price_sum / volume_sum)

## 各ブランドの最小の容量に本数と価格をそろえていく
df_panelpurchase_edit_1[is.na(df_panelpurchase_edit_1)] <- 10000
df_panelpurchase_edit_1_min <- df_panelpurchase_edit_1 %>% 
  group_by(brand_name) %>% 
  summarise(min1 = min(vol1, na.rm = TRUE), min2 = min(vol2, na.rm = TRUE), 
            min3 = min(vol3, na.rm = TRUE), min4 = min(vol4, na.rm = TRUE)) %>% 
  select(1, 2) %>% 
  rename(min_vol = min1)
df_panelpurchase_edit_1[df_panelpurchase_edit_1==10000] <- NA

df_panelpurchase_edit_1_sum <- df_panelpurchase_edit_1 %>% 
  group_by(panelist_id, week, brand_name) %>% 
  summarise(volume_sum = sum(volume_sum), price_sum = sum(price_sum)) %>% 
  mutate(average_price_volume = price_sum / volume_sum)

# 本数は切り捨てにして、価格は本数×最小の容量×1mlあたりの価格
df_panelpurchase_edit_2 <- df_panelpurchase_edit_1_sum %>% 
  inner_join(df_panelpurchase_edit_1_min, by = "brand_name") %>% 
  mutate(units_fixed = floor(volume_sum / min_vol), price_fixed = units_fixed*min_vol*average_price_volume,
         price_fixed_per_unit = min_vol*average_price_volume) %>% 
  select(1, 2, 3, 8, 9, 10)

df_panelpurchase_edit_2$count = 1

for ( i in 2:nrow(df_panelpurchase_edit_2)){
  if(df_panelpurchase_edit_2[i, "week"] == df_panelpurchase_edit_2[i-1, "week"]){
    df_panelpurchase_edit_2[i, "count"] = df_panelpurchase_edit_2[i-1, "count"] + 1
  }
}

##　本数（購入量）をパネル化する
df_check <- df_panelpurchase_edit_2 %>% 
  ungroup() %>% 
  mutate(id = paste(panelist_id, week, count)) %>% 
  select(1, 2, 8)

df_panelpurchase_edit_2_panel_unit <- df_panelpurchase_edit_2 %>% 
  ungroup() %>% 
  mutate(id = paste(panelist_id, week, count)) %>% 
  select(8, 3, 4) %>% 
  spread(key = brand_name, value = units_fixed) %>% 
  inner_join(df_check, by = "id") 
  
df_panelpurchase_edit_2_panel_unit[is.na(df_panelpurchase_edit_2_panel_unit)] <- 0

df_panelpurchase_edit_2_panel_unit <- df_panelpurchase_edit_2_panel_unit %>% 
  group_by(panelist_id, week) %>% 
  summarise(`026bf156cc` = sum(`026bf156cc`), `0598217dd8` = sum(`0598217dd8`), 
            `1135ebe8b1` = sum(`1135ebe8b1`), `1bbf8dfa32` = sum(`1bbf8dfa32`),
            `388786a5a0` = sum(`388786a5a0`), `3ed5d148bd` = sum(`3ed5d148bd`),
            `4461ff9701` = sum(`4461ff9701`), `56009934e9` = sum(`56009934e9`),
            `8107c1b704` = sum(`8107c1b704`), `9213c533a3` = sum(`9213c533a3`),
            `98643524ce` = sum(`98643524ce`), `98aa9c7e5b` = sum(`98aa9c7e5b`),
            `d804a51ed6` = sum(`d804a51ed6`), `db4afa1725` = sum(`db4afa1725`),
            `df6c29e580` = sum(`df6c29e580`)) %>% 
  ungroup()

df_panelpurchase_edit_2_panel_unit <- as.data.frame(df_panelpurchase_edit_2_panel_unit)

##　価格をパネル化する
df_panelpurchase_edit_2_panel_price <- df_panelpurchase_edit_2 %>% 
  ungroup() %>% 
  mutate(id = paste(panelist_id, week, count)) %>% 
  select(8, 3, 6) %>% 
  spread(key = brand_name, value = price_fixed_per_unit) %>% 
  inner_join(df_check, by = "id") 

df_panelpurchase_edit_2_panel_price[is.na(df_panelpurchase_edit_2_panel_price)] <- 0

df_panelpurchase_edit_2_panel_price <- df_panelpurchase_edit_2_panel_price %>% 
  group_by(panelist_id, week) %>% 
  summarise(`026bf156cc` = sum(`026bf156cc`), `0598217dd8` = sum(`0598217dd8`), 
            `1135ebe8b1` = sum(`1135ebe8b1`), `1bbf8dfa32` = sum(`1bbf8dfa32`),
            `388786a5a0` = sum(`388786a5a0`), `3ed5d148bd` = sum(`3ed5d148bd`),
            `4461ff9701` = sum(`4461ff9701`), `56009934e9` = sum(`56009934e9`),
            `8107c1b704` = sum(`8107c1b704`), `9213c533a3` = sum(`9213c533a3`),
            `98643524ce` = sum(`98643524ce`), `98aa9c7e5b` = sum(`98aa9c7e5b`),
            `d804a51ed6` = sum(`d804a51ed6`), `db4afa1725` = sum(`db4afa1725`),
            `df6c29e580` = sum(`df6c29e580`)) %>% 
  ungroup()

df_panelpurchase_edit_2_panel_price[df_panelpurchase_edit_2_panel_price == 0] <- NA

df_panelpurchase_edit_2_panel_price <- as.data.frame(df_panelpurchase_edit_2_panel_price)

# 0を埋めるために、週ごとの平均価格データを作成する
df_price_weekly <- df_panelpurchase_edit_2_panel_price %>% 
  group_by(week) %>% 
  summarise(`026bf156cc` = mean(`026bf156cc`, na.rm = TRUE), `0598217dd8` = mean(`0598217dd8`, na.rm = TRUE), 
            `1135ebe8b1` = mean(`1135ebe8b1`, na.rm = TRUE), `1bbf8dfa32` = mean(`1bbf8dfa32`, na.rm = TRUE),
            `388786a5a0` = mean(`388786a5a0`, na.rm = TRUE), `3ed5d148bd` = mean(`3ed5d148bd`, na.rm = TRUE),
            `4461ff9701` = mean(`4461ff9701`, na.rm = TRUE), `56009934e9` = mean(`56009934e9`, na.rm = TRUE),
            `8107c1b704` = mean(`8107c1b704`, na.rm = TRUE), `9213c533a3` = mean(`9213c533a3`, na.rm = TRUE),
            `98643524ce` = mean(`98643524ce`, na.rm = TRUE), `98aa9c7e5b` = mean(`98aa9c7e5b`, na.rm = TRUE),
            `d804a51ed6` = mean(`d804a51ed6`, na.rm = TRUE), `db4afa1725` = mean(`db4afa1725`, na.rm = TRUE),
            `df6c29e580` = mean(`df6c29e580`, na.rm = TRUE)) %>% 
  ungroup()

df_price_weekly <- as.data.frame(df_price_weekly)

# 埋める
brands = colnames(df_panelpurchase_edit_2_panel_price)
for( i in 3:17){
  brand = brands[i]
  for (j in 1:nrow(df_panelpurchase_edit_2_panel_price)){
    if(is.na(df_panelpurchase_edit_2_panel_price[j, brand]) == TRUE){
      weeks = df_panelpurchase_edit_2_panel_price[j, "week"]
      df_panelpurchase_edit_2_panel_price[j, brand] <- df_price_weekly[df_price_weekly$week == weeks, brand]
    }
  }
}



### ブランドロイヤルティ（前回に購入した商品）
df_panelpurchase_edit_2_panel_unit_t1 <- df_panelpurchase_edit_2_panel_unit
df_panelpurchase_edit_2_panel_unit_t1[, 3:17] <- lag(df_panelpurchase_edit_2_panel_unit_t1[, 3:17])
df_panelpurchase_edit_2_panel_unit_t1$panelist_id_lag <- lag(df_panelpurchase_edit_2_panel_unit_t1$panelist_id)
df_panelpurchase_edit_2_panel_unit_t1$check <- ifelse(df_panelpurchase_edit_2_panel_unit_t1$panelist_id_lag ==
                                                        df_panelpurchase_edit_2_panel_unit_t1$panelist_id, 0, 1)
df_panelpurchase_edit_2_panel_unit_t1 <- na.omit(df_panelpurchase_edit_2_panel_unit_t1)
df_panelpurchase_edit_2_panel_unit_t1 <- df_panelpurchase_edit_2_panel_unit_t1 %>% 
  filter(check == 0) %>% 
  select(1:17)
for (i in 1:nrow(df_panelpurchase_edit_2_panel_unit_t1)) {
  for (j in 3:17) {
    if (df_panelpurchase_edit_2_panel_unit_t1[i, j] > 0){
      df_panelpurchase_edit_2_panel_unit_t1[i, j] <- 1
    }
  }
}



### 購入量と価格のデータをt-1期のデータにそろえる
df_panelpurchase_edit_2_panel_unit_new <- merge(df_panelpurchase_edit_2_panel_unit_t1[, c(1, 2)], 
                                            df_panelpurchase_edit_2_panel_unit,
                                            by = c("panelist_id", "week"),
                                            all.x = TRUE)

df_panelpurchase_edit_2_panel_price_new <- merge(df_panelpurchase_edit_2_panel_unit_t1[, c(1, 2)], 
                                             df_panelpurchase_edit_2_panel_price,
                                             by = c("panelist_id", "week"),
                                             all.x = TRUE)



### 広告接触
# パネル化
df_tvcom$check <- 1
for( i in 2:nrow(df_tvcom)){
  if(df_tvcom[i, "week"] == df_tvcom[i-1, "week"]){
    df_tvcom[i, "check"] <- df_tvcom[i-1, "check"] + 1
  }
}

df_check_tv <- df_tvcom %>% 
  mutate(id = paste(panelist_id, week, check)) %>% 
  select(1, 2, 8)

df_tvcom_1 <- df_tvcom %>% 
  mutate(dummy = 1, id = paste(panelist_id, week, check)) %>% 
  select(9, 4, 8) %>% 
  spread(key = brand_name, value = dummy) %>% 
  inner_join(df_check_tv, by = "id")  %>% 
  group_by(panelist_id, week) %>% 
  summarise(`026bf156cc` = sum(`026bf156cc`, na.rm = TRUE), `0598217dd8` = sum(`0598217dd8`, na.rm = TRUE), 
            `1135ebe8b1` = sum(`1135ebe8b1`, na.rm = TRUE), `1bbf8dfa32` = sum(`1bbf8dfa32`, na.rm = TRUE),
            `388786a5a0` = sum(`388786a5a0`, na.rm = TRUE), `3ed5d148bd` = sum(`3ed5d148bd`, na.rm = TRUE),
            `4461ff9701` = sum(`4461ff9701`, na.rm = TRUE), `56009934e9` = sum(`56009934e9`, na.rm = TRUE),
            `8107c1b704` = sum(`8107c1b704`, na.rm = TRUE), `9213c533a3` = sum(`9213c533a3`, na.rm = TRUE),
            `98643524ce` = sum(`98643524ce`, na.rm = TRUE), `98aa9c7e5b` = sum(`98aa9c7e5b`, na.rm = TRUE),
            `d804a51ed6` = sum(`d804a51ed6`, na.rm = TRUE), `db4afa1725` = sum(`db4afa1725`, na.rm = TRUE),
            `df6c29e580` = sum(`df6c29e580`, na.rm = TRUE)) %>% 
  ungroup()

# 購入量・価格のデータと時系列を合わせる
df_tvcom_panel <- merge(df_panelpurchase_edit_2_panel_unit_new[,c(1, 2)], df_tvcom_1, 
                        by = c("panelist_id", "week"), all.x = TRUE)
# NAは見てないとうことなので、0を代入
df_tvcom_panel[is.na(df_tvcom_panel)] <- 0

##### データ加工➁：購入者 期間 商品 
### 購入期間
df_purchase_period <- df_panelpurchase_edit_2_panel_unit[, c(1, 2)]
df_purchase_period$week <- as.Date(df_purchase_period$week) 
df_purchase_period$period <- 0
for(i in 2:nrow(df_purchase_period)){
  df_purchase_period[i, "period"] <-  (as.numeric(df_purchase_period[i-1, "week"] -  df_purchase_period[i, "week"]))*-1/7
}
df_purchase_period$week <- as.factor(df_purchase_period$week) 
df_purchase_period_new <- merge(df_panelpurchase_edit_2_panel_unit_new[,c(1, 2)], df_purchase_period, 
                                by = c("panelist_id", "week"), all.x = TRUE)



### 気温・休日
df_temp_holiday <- df_panelpurchase_edit_2_panel_unit_new %>% 
  select(1, 2) %>% 
  inner_join(df_temp, by = c("week" = "Date")) %>% 
  inner_join(df_holiday, by = "week")



### 属性
df_panelattri_new <-  df_panelpurchase_edit_2_panel_unit_new %>% 
  select(1, 2) %>% 
  inner_join(df_panelattri, by = "panelist_id")

df_panelattri_new$gender <- ifelse(df_panelattri_new$gender > 1, 0, 1)
df_panelattri_new$marital_status <- ifelse(df_panelattri_new$marital_status > 1, 0, 1)



### 商品属性
df_category_new <- dummy_cols(df_category, select_columns = "category")
df_category_new <- dummy_cols(df_category_new, select_columns = "brand_name")
rownames(df_category_new) <- df_category_new$brand_name
df_category_new <- select(df_category_new, -c(1, 2))
product_attri <- as.matrix(df_category_new)



##### Simulated GMM
### 操作変数（属性・ブランドロイヤルティ・広告）
Z_ht <- df_panelattri_new %>% 
  inner_join(df_panelpurchase_edit_2_panel_unit_t1, by = c("panelist_id", "week")) %>% 
  inner_join(df_tvcom_panel, by = c("panelist_id", "week")) %>% 
  inner_join(df_temp_holiday, by = c("panelist_id", "week")) %>% 
  inner_join(df_purchase_period_new, by= c("panelist_id", "week")) %>% 
  select(-c(1, 2, 6))

Z_ht_t <- as.matrix(t(Z_ht))



### Halton Sequence
Halton <- array(0, dim = c(15, 13411, 300))
set.seed(101)
for ( i in 1:300){
  Halton[,,i] <- t(ghalton(13411, d=15))
}



### データフレームをモデルに合わせる
## 購入量
answer <- as.matrix(df_panelpurchase_edit_2_panel_unit_new[, -c(1, 2)])


## 価格
price <- as.matrix(subset(df_panelpurchase_edit_2_panel_price_new, select = -c(1, 2)))
price_new <- array(0, dim = c(15, 13411, 30))
for (i in 1: 30){
  price_new[,,i]<-t(price)
}


## S
# 年齢、結婚、前回の購買からの期間の順番
d_s_attri <- as.matrix(subset(df_panelattri_new, select = c(4, 5)))
d_s_purchase <- as.matrix(subset(df_purchase_period_new, select = period))
d_s <- cbind(d_s_attri, d_s_purchase, rep(1, 13411))


## m
d_m <- as.matrix(subset(df_panelattri_new,select = 7))


## mu
# 年齢、性別、結婚
d_beta <- t(as.matrix(subset(df_panelattri_new, select = 4 )))
# 広告接触
ad <- as.matrix(subset(df_tvcom_panel,select = -c(1, 2)))
ad <- t(ad)
# ブランドロイヤリティ
br_p <- as.matrix(subset(df_panelpurchase_edit_2_panel_unit_t1,select = -c(1, 2)))
br_p <- t(br_p)
# 結合
d_beta <- rbind(d_beta,ad, br_p)


## lambda
# 性別、年齢、結婚
d_lambda_attri <- as.matrix(subset(df_panelattri_new, select = c(4, 5)))
# 前回の購買の有無
d_lambda_purchase <- as.matrix(subset(df_purchase_period_new, select = period))
# 気温
d_lambda_temp_holi <- as.matrix(subset(df_temp_holiday, select = 3))
# 定数項とともに結合
d_lambda <- cbind(d_lambda_attri, d_lambda_purchase,d_lambda_temp_holi, rep(1, 13411))










  

