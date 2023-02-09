##### Title: DataCleaning.R
##### objective: Processing data & Ready for Modeling
##### Update: 2020/12/10


### import Package
library(fastDummies)
library(tidyverse)
library(readxl)
library(naniar)
library(imputeMissings)
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





##############################################データ加工①：購入者×期間×商品###############################################





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

## 近い容量をまとめる
# +-5%nの容量は同一商品と扱う。その商品の容量は一番小さい容量とする。価格は容量*1mlあたりの金額
# 欠損があるとうまく動かないので、NA -> 0にする
df_panelpurchase_edit_1[is.na(df_panelpurchase_edit_1)] <- 0

df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "026bf156cc" & df_panelpurchase_edit_1$vol1 == 255, "vol1"] <- 240
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "026bf156cc" & df_panelpurchase_edit_1$vol2 == 255, "vol2"] <- 240
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "026bf156cc" & df_panelpurchase_edit_1$vol1 == 300, "vol1"] <- 280
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "026bf156cc" & df_panelpurchase_edit_1$vol1 == 500, "vol1"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "026bf156cc" & df_panelpurchase_edit_1$vol2 == 500, "vol2"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "0598217dd8" & df_panelpurchase_edit_1$vol1 %in% c(190, 195, 200), "vol1"] <- 180
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "0598217dd8" & df_panelpurchase_edit_1$vol2 %in% c(190, 195, 200), "vol2"] <- 180
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "0598217dd8" & df_panelpurchase_edit_1$vol3 %in% c(190, 195, 200), "vol3"] <- 180
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "0598217dd8" & df_panelpurchase_edit_1$vol1 == 330, "vol1"] <- 300
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "0598217dd8" & df_panelpurchase_edit_1$vol2 == 330, "vol2"] <- 300
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "0598217dd8" & df_panelpurchase_edit_1$vol3 == 330, "vol3"] <- 300
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1135ebe8b1" & df_panelpurchase_edit_1$vol1 == 460, "vol1"] <- 430
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1135ebe8b1" & df_panelpurchase_edit_1$vol1 == 500, "vol1"] <- 485
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1135ebe8b1" & df_panelpurchase_edit_1$vol2 == 500, "vol2"] <- 485
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1bbf8dfa32" & df_panelpurchase_edit_1$vol1 == 350, "vol1"] <- 345
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1bbf8dfa32" & df_panelpurchase_edit_1$vol1 %in% c(500, 525, 550), "vol1"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1bbf8dfa32" & df_panelpurchase_edit_1$vol2 %in% c(500, 525, 550), "vol2"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "1bbf8dfa32" & df_panelpurchase_edit_1$vol3 %in% c(500, 525, 550), "vol3"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "3ed5d148bd" & df_panelpurchase_edit_1$vol1 %in% c(260, 280), "vol1"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "3ed5d148bd" & df_panelpurchase_edit_1$vol2 %in% c(260, 280), "vol2"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "3ed5d148bd" & df_panelpurchase_edit_1$vol1 == 345, "vol1"] <- 320
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "4461ff9701" & df_panelpurchase_edit_1$vol1 %in% c(275, 250), "vol1"] <- 245
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "4461ff9701" & df_panelpurchase_edit_1$vol1 == 660, "vol1"] <- 600
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "4461ff9701" & df_panelpurchase_edit_1$vol2 == 660, "vol2"] <- 600
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "56009934e9" & df_panelpurchase_edit_1$vol1 %in% c(280, 285), "vol1"] <- 260
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "56009934e9" & df_panelpurchase_edit_1$vol2 %in% c(280, 285), "vol2"] <- 260
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "56009934e9" & df_panelpurchase_edit_1$vol1 == 400, "vol1"] <- 370
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "56009934e9" & df_panelpurchase_edit_1$vol2 == 400, "vol2"] <- 370
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "56009934e9" & df_panelpurchase_edit_1$vol3 == 400, "vol3"] <- 370
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol1 %in% c(180, 185), "vol1"] <- 165
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol3 %in% c(180, 185), "vol3"] <- 165
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol1 %in% c(260, 275, 280, 285), "vol1"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol2 %in% c(260, 275, 280, 285), "vol2"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol3 %in% c(260, 275, 280, 285), "vol3"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol1 == 500, "vol1"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol2 == 500, "vol2"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol3 == 500, "vol3"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "8107c1b704" & df_panelpurchase_edit_1$vol4 == 500, "vol4"] <- 490
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "9213c533a3" & df_panelpurchase_edit_1$vol1 %in% c(275, 280), "vol1"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "9213c533a3" & df_panelpurchase_edit_1$vol2 %in% c(275, 280), "vol2"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "9213c533a3" & df_panelpurchase_edit_1$vol1 %in% c(345, 350), "vol1"] <- 320
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "9213c533a3" & df_panelpurchase_edit_1$vol2 %in% c(345, 350), "vol2"] <- 320
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "9213c533a3" & df_panelpurchase_edit_1$vol1 %in% c(500, 525), "vol1"] <- 485
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "9213c533a3" & df_panelpurchase_edit_1$vol2 %in% c(500, 525), "vol2"] <- 485
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol1 == 185, "vol1"] <- 170
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol2 == 185, "vol2"] <- 170
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol1 %in% c(260, 265, 270, 280, 290), "vol1"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol2 %in% c(260, 265, 270, 280, 290), "vol2"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol3 %in% c(260, 265, 270, 280, 290), "vol3"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol4 %in% c(260, 265, 270, 280, 290), "vol4"] <- 250
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol1 %in% c(350, 370), "vol1"] <- 345
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol2 %in% c(350, 370), "vol2"] <- 345
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol3 %in% c(350, 370), "vol3"] <- 345
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol4 %in% c(350, 370), "vol4"] <- 345
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol1 == 410, "vol1"] <- 400
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol2 == 410, "vol2"] <- 400
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98643524ce" & df_panelpurchase_edit_1$vol1 == 950, "vol1"] <- 925
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98aa9c7e5b" & df_panelpurchase_edit_1$vol1 %in% c(300, 345), "vol1"] <- 280
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98aa9c7e5b" & df_panelpurchase_edit_1$vol2 %in% c(300, 345), "vol2"] <- 280
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98aa9c7e5b" & df_panelpurchase_edit_1$vol1 == 550, "vol1"] <- 525
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98aa9c7e5b" & df_panelpurchase_edit_1$vol2 == 550, "vol2"] <- 525
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "98aa9c7e5b" & df_panelpurchase_edit_1$vol3 == 550, "vol3"] <- 525
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "d804a51ed6" & df_panelpurchase_edit_1$vol1 == 330, "vol1"] <- 320
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "d804a51ed6" & df_panelpurchase_edit_1$vol1 == 515, "vol1"] <- 500
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "d804a51ed6" & df_panelpurchase_edit_1$vol1 == 550, "vol1"] <- 540
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "d804a51ed6" & df_panelpurchase_edit_1$vol2 == 550, "vol2"] <- 540
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "db4afa1725" & df_panelpurchase_edit_1$vol1 == 300, "vol1"] <- 280
df_panelpurchase_edit_1[df_panelpurchase_edit_1$brand_name == "df6c29e580" & df_panelpurchase_edit_1$vol1 == 300, "vol1"] <- 280


## vol1, vol2, vol3, vol4で等しい値を取っているケースを直す
# vol4との被りをただす。
df_panelpurchase_edit_1[9549, "count2"] <- 2
df_panelpurchase_edit_1[9549, "vol3"] <- 0
df_panelpurchase_edit_1[9549, "count3"] <- 0

df_panelpurchase_edit_1[11183, "count1"] <- 3
df_panelpurchase_edit_1[11183, "vol2"] <- 0
df_panelpurchase_edit_1[11183, "count2"] <- 0
df_panelpurchase_edit_1[11183, "count3"] <- 2
df_panelpurchase_edit_1[11183, "vol4"] <- 0
df_panelpurchase_edit_1[11183, "count4"] <- 0

df_panelpurchase_edit_1[13896, "count2"] <- 3
df_panelpurchase_edit_1[13896, "vol3"] <- 0
df_panelpurchase_edit_1[13896, "count3"] <- 0
df_panelpurchase_edit_1[13896, "vol4"] <- 0
df_panelpurchase_edit_1[13896, "count4"] <- 0

df_panelpurchase_edit_1[16687, "count1"] <- 2
df_panelpurchase_edit_1[16687, "vol2"] <- 0
df_panelpurchase_edit_1[16687, "count2"] <- 0

# vol3との被り
df_panelpurchase_edit_1$check3 <- ifelse(df_panelpurchase_edit_1$vol3>0,ifelse(df_panelpurchase_edit_1$vol1 == df_panelpurchase_edit_1$vol2 |
                                           df_panelpurchase_edit_1$vol1 == df_panelpurchase_edit_1$vol3 |
                                           df_panelpurchase_edit_1$vol3 == df_panelpurchase_edit_1$vol2, 1, 0),0)

df_panelpurchase_edit_1[739, "count2"] <- 2
df_panelpurchase_edit_1[739, "vol3"] <- 0
df_panelpurchase_edit_1[739, "count3"] <- 0

df_panelpurchase_edit_1[3525, "count2"] <- 2
df_panelpurchase_edit_1[3525, "vol3"] <- 0
df_panelpurchase_edit_1[3525, "count3"] <- 0

df_panelpurchase_edit_1[3574, "count1"] <- 2
df_panelpurchase_edit_1[3574, "vol2"] <- 0
df_panelpurchase_edit_1[3574, "count2"] <- 0

df_panelpurchase_edit_1[4490, "count1"] <- 2
df_panelpurchase_edit_1[4490, "vol2"] <- 0
df_panelpurchase_edit_1[4490, "count2"] <- 0

df_panelpurchase_edit_1[9547, "count1"] <- 3
df_panelpurchase_edit_1[9547, "vol2"] <- 0
df_panelpurchase_edit_1[9547, "count2"] <- 0

df_panelpurchase_edit_1[10890, "count1"] <- 2
df_panelpurchase_edit_1[10890, "vol2"] <- 0
df_panelpurchase_edit_1[10890, "count2"] <- 0

df_panelpurchase_edit_1[11930, "count2"] <- 24
df_panelpurchase_edit_1[11930, "vol3"] <- 0
df_panelpurchase_edit_1[11930, "count3"] <- 0

df_panelpurchase_edit_1[15339, "count1"] <- 2
df_panelpurchase_edit_1[15339, "vol3"] <- 0
df_panelpurchase_edit_1[15339, "count3"] <- 0

df_panelpurchase_edit_1[15436, "count1"] <- 2
df_panelpurchase_edit_1[15436, "vol3"] <- 0
df_panelpurchase_edit_1[15436, "count3"] <- 0

df_panelpurchase_edit_1[19237, "count1"] <- 4
df_panelpurchase_edit_1[19237, "vol2"] <- 0
df_panelpurchase_edit_1[19237, "count2"] <- 0
df_panelpurchase_edit_1[19237, "vol3"] <- 0
df_panelpurchase_edit_1[19237, "count3"] <- 0

df_panelpurchase_edit_1[20574, "count1"] <- 2
df_panelpurchase_edit_1[20574, "vol2"] <- 0
df_panelpurchase_edit_1[20574, "count2"] <- 0

df_panelpurchase_edit_1[20891, "count1"] <- 2
df_panelpurchase_edit_1[20891, "vol2"] <- 0
df_panelpurchase_edit_1[20891, "count2"] <- 0

df_panelpurchase_edit_1[21171, "count1"] <- 2
df_panelpurchase_edit_1[21171, "vol2"] <- 0
df_panelpurchase_edit_1[21171, "count2"] <- 0

df_panelpurchase_edit_1[21881, "count1"] <- 3
df_panelpurchase_edit_1[21881, "vol2"] <- 0
df_panelpurchase_edit_1[21881, "count2"] <- 0

df_panelpurchase_edit_1[23472, "count1"] <- 4
df_panelpurchase_edit_1[23472, "vol2"] <- 0
df_panelpurchase_edit_1[23472, "count2"] <- 0
df_panelpurchase_edit_1[23472, "vol3"] <- 0
df_panelpurchase_edit_1[23472, "count3"] <- 0

# vol2とのダブり
df_panelpurchase_edit_1$check2 <- ifelse(df_panelpurchase_edit_1$vol1 == df_panelpurchase_edit_1$vol2, 1, 0)
df_panelpurchase_edit_1$count12 <- df_panelpurchase_edit_1$count1 + df_panelpurchase_edit_1$count2

for ( i in 1:nrow(df_panelpurchase_edit_1)){
  if (df_panelpurchase_edit_1[i, "check2"] == 1){
    df_panelpurchase_edit_1[i, "count1"] <- df_panelpurchase_edit_1[i, "count12"] 
    df_panelpurchase_edit_1[i, "count2"] <- 0
    df_panelpurchase_edit_1[i, "vol2"] <- 0 
  }
}



# どの容量までを採用するか決めるために、容量×ブランドの販売個数を集計する
df_panelpurchase_edit_count1 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,6,7) %>%
  mutate(product = paste(brand_name,vol1,sep = "_")) %>%
  select(1,2,5,6) %>% 
  rename(count = count1)

df_panelpurchase_edit_count2 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,8,9) %>%
  filter(count2>0) %>%
  mutate(product = paste(brand_name,vol2,sep = "_")) %>%
  select(1,2,5,6)  %>% 
  rename(count = count2)

df_panelpurchase_edit_count3 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,10,11) %>%
  filter(count3>0) %>%
  mutate(product = paste(brand_name,vol3,sep = "_")) %>%
  select(1,2,5,6) %>% 
  rename(count = count3)

df_panelpurchase_edit_count4 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,12,13) %>%
  filter(count4>0) %>%
  mutate(product = paste(brand_name,vol4,sep = "_")) %>%
  select(1,2,5,6) %>% 
  rename(count = count4)

df_panelpurchase_count_sum <- df_panelpurchase_edit_count1 %>% 
  bind_rows(df_panelpurchase_edit_count2, df_panelpurchase_edit_count3, df_panelpurchase_edit_count4) %>% 
  group_by(product) %>% 
  summarise(count = sum(count)) %>% 
  mutate(brand = str_sub(product,start = 1,end=10)) %>% 
  group_by(brand) %>% 
  mutate(count_sum = sum(count)) %>% 
  mutate(pro = count/count_sum) %>% 
  arrange(brand, desc(count)) %>%
  mutate(cumsum = cumsum(count) , cum = cumsum / count_sum, size = as.numeric(str_sub(product,start = 12)))


## count編
df_panelpurchase_edit_count1 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,6,7) %>%
  mutate(product = paste(brand_name,vol1,sep = "_"), id = rownames(df_panelpurchase_edit_1)) %>%
  select(1,2,5,6,7) %>% 
  spread(product, count1) %>% 
  select(-3)

df_panelpurchase_edit_count2 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,8,9) %>%
  filter(count2>0)
df_panelpurchase_edit_count2 <- df_panelpurchase_edit_count2 %>% 
  mutate(product = paste(brand_name,vol2,sep = "_"),id = rownames(df_panelpurchase_edit_count2)) %>%
  select(1,2,5,6,7) %>% 
  spread(product, count2) %>% 
  select(-3)

df_panelpurchase_edit_count3 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,10,11) %>%
  filter(count3>0)
df_panelpurchase_edit_count3 <- df_panelpurchase_edit_count3 %>% 
  mutate(product = paste(brand_name,vol3,sep = "_"), id = rownames(df_panelpurchase_edit_count3)) %>%
  select(1,2,5,6,7) %>% 
  spread(product, count3) %>% 
  select(-3)

df_panelpurchase_edit_count4 <- df_panelpurchase_edit_1 %>%
  select(1,2,3,12,13) %>%
  filter(count4>0)
df_panelpurchase_edit_count4 <- df_panelpurchase_edit_count4 %>% 
  mutate(product = paste(brand_name,vol4,sep = "_"), id = rownames(df_panelpurchase_edit_count4)) %>%
  select(1,2,5,6,7) %>% 
  spread(product, count4) %>% 
  select(-3)

# 同じく結合
df_panelpurchase_count <- df_panelpurchase_edit_count1 %>% 
  bind_rows(df_panelpurchase_edit_count2, df_panelpurchase_edit_count3, df_panelpurchase_edit_count4) %>% 
  group_by(panelist_id, week) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))


## price編
df_panelpurchase_edit_price1 <- df_panelpurchase_edit_1 %>% 
  select(1, 2, 3, 6, 14) %>% 
  mutate(product  = paste(brand_name, vol1, sep = "_"), price = vol1*average_price_volume, id = rownames(df_panelpurchase_edit_1)) %>% 
  select(1, 2, 6, 7, 8) %>% 
  spread(product, price) %>% 
  select(-3)

df_panelpurchase_edit_price2 <- df_panelpurchase_edit_1 %>% 
  select(1, 2, 3, 8, 14) %>% 
  filter(vol2 > 0)
df_panelpurchase_edit_price2 <- df_panelpurchase_edit_price2 %>% 
  mutate(product  = paste(brand_name, vol2, sep = "_"), price = vol2*average_price_volume, id = rownames(df_panelpurchase_edit_price2)) %>% 
  select(1, 2, 6, 7, 8) %>% 
  spread(product, price) %>% 
  select(-3)

df_panelpurchase_edit_price3 <- df_panelpurchase_edit_1 %>% 
  select(1, 2, 3, 10, 14) %>% 
  filter(vol3 > 0)
df_panelpurchase_edit_price3 <- df_panelpurchase_edit_price3 %>% 
  mutate(product  = paste(brand_name, vol3, sep = "_"), price = vol3*average_price_volume, id = rownames(df_panelpurchase_edit_price3)) %>% 
  select(1, 2, 6, 7, 8) %>% 
  spread(product, price) %>% 
  select(-3)

df_panelpurchase_edit_price4 <- df_panelpurchase_edit_1 %>% 
  select(1, 2, 3, 12, 14) %>% 
  filter(vol4 > 0)
df_panelpurchase_edit_price4 <- df_panelpurchase_edit_price4 %>% 
  mutate(product  = paste(brand_name, vol4, sep = "_"), price = vol4*average_price_volume, id = rownames(df_panelpurchase_edit_price4)) %>% 
  select(1, 2, 6, 7, 8) %>% 
  spread(product, price) %>% 
  select(-3)

# 再び結合
df_panelpurchase_price <- df_panelpurchase_edit_price1 %>% 
  bind_rows(df_panelpurchase_edit_price2, df_panelpurchase_edit_price3, df_panelpurchase_edit_price4) %>% 
  group_by(panelist_id, week) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))


# 2％に満たない商品はバスケットごと削除することにする。
df_panelpurchase_count_under1 <- df_panelpurchase_count_sum %>% 
  filter(pro < 0.02)
# 商品数は66
length(unique(df_panelpurchase_count_sum$product)) - length(unique(df_panelpurchase_count_under1$product))
# 2%に満たない商品リスト
list_byebye <- unique(df_panelpurchase_count_under1$product)

# 1%以下の商品を消す
# 綺麗な書き方を探る！
check <- data.frame(apply(df_panelpurchase_count[,list_byebye ], 1, sum))
colnames(check) <- "check"
# 元データにくっつける
# count
df_panelpurchase_count_edit <- df_panelpurchase_count %>% 
  bind_cols(check) %>% 
  filter(check == 0) %>% 
  select(-check) %>% 
  select(-list_byebye)
# price
df_panelpurchase_price_edit <- df_panelpurchase_price %>% 
  bind_cols(check) %>% 
  filter(check == 0) %>% 
  select(-check) %>% 
  select(-list_byebye)
# 3%くらいしか落ちないね。おけまる。
nrow(subset(check, check == 0))/nrow(check)
# priceに関しては期間ごとの平均価格を算出する必要がある。
df_panelpurchase_price_edit_ave <- df_panelpurchase_price_edit
df_panelpurchase_price_edit_ave[df_panelpurchase_price_edit_ave== 0] <- NA
df_panelpurchase_price_edit_ave <- df_panelpurchase_price_edit_ave %>% 
  ungroup() %>% 
  select(-panelist_id) %>% 
  group_by(week) %>% 
  summarise(across(everything(), mean, na.rm = TRUE)) %>% 
  ungroup()

# かなり欠損があるので、可視化してみる。13.8%. 価格がNAなら、購入もされていない. こだわれる部分
# vis_miss(df_panelpurchase_price_edit_ave)

# 一旦、中央値で代入しておく。（平均値代入のいい方法いずこ、、）
name = colnames(df_panelpurchase_price_edit_ave)
df_panelpurchase_price_edit_ave <- impute(df_panelpurchase_price_edit_ave, method = "median/mode")
colnames(df_panelpurchase_price_edit_ave) = name
df_panelpurchase_price_edit <- as.data.frame(df_panelpurchase_price_edit)

# 購入していない商品の価格を中央値で埋める. 時間があれば書き直したい。(2,3分かかった)
for (i in 1: nrow(df_panelpurchase_price_edit)) {
  for (j in 3: ncol(df_panelpurchase_price_edit)) {
    weeks <- df_panelpurchase_price_edit[i, "week"]
    if (df_panelpurchase_price_edit[i, j] == 0){
      df_panelpurchase_price_edit[i, j] <- df_panelpurchase_price_edit_ave[df_panelpurchase_price_edit_ave$week == weeks,j-1]
    }
    
  }
  
}

# 各ユーザーに対して最初の購買を消す
df_panelpurchase_price_edit$check <- lag(df_panelpurchase_price_edit$panelist_id)
df_panelpurchase_price_edit$check2 <- ifelse(df_panelpurchase_price_edit$check == df_panelpurchase_price_edit$panelist_id, 0,1)
df_panelpurchase_price_edit[is.na(df_panelpurchase_price_edit)]<- 1
df_panelpurchase_price_edit <- filter(df_panelpurchase_price_edit, check2 == 0)
df_panelpurchase_price_edit <- select(df_panelpurchase_price_edit, -c(69, 70))


### 当該商品ごとのt期における購買経験データ作成
df_panelpurchase_count_panel <- complete(df_panelpurchase_count_edit[, c(1,2)], panelist_id, week = list_week)
df_panelpurchase_count_panel <- merge(df_panelpurchase_count_panel, df_panelpurchase_count_edit, by = c("panelist_id", "week"), all.x = TRUE)
df_panelpurchase_count_panel[is.na(df_panelpurchase_count_panel)] <- 0
df_panelpurchase_count_panel[, -c(1, 2)] <- ifelse(df_panelpurchase_count_panel[, -c(1, 2)] > 0, 1, 0)
df_panelpurchase_count_panel[, c(3:68)] <- lag(df_panelpurchase_count_panel[, c(3:68)])


# 該当期間だけを抽出する
df_period <- df_panelpurchase_count_edit %>% 
  select(1, 2) %>% 
  mutate(check = 1)

df_panelpurchase_count_panel <- merge(df_panelpurchase_count_panel, df_period, by = c("panelist_id", "week"))
df_panelpurchase_count_panel <- select(df_panelpurchase_count_panel, -69)

# 各ユーザーに対して最初の購買を消す
df_panelpurchase_count_panel$check <- lag(df_panelpurchase_count_panel$panelist_id)
df_panelpurchase_count_panel$check2 <- ifelse(df_panelpurchase_count_panel$check == df_panelpurchase_count_panel$panelist_id, 0,1)
df_panelpurchase_count_panel[is.na(df_panelpurchase_count_panel)]<- 1
df_panelpurchase_count_panel <- filter(df_panelpurchase_count_panel, check2 == 0)
df_panelpurchase_count_panel <- select(df_panelpurchase_count_panel, -c(69, 70))


# df_panelpurchase_count_editに関しても各ユーザーの最初の購買を消す
df_panelpurchase_count_edit <-merge(df_panelpurchase_count_panel[, c(1, 2)], df_panelpurchase_count_edit,
                                    by = c("panelist_id", "week"), all.x = TRUE)

### 当該商品ごとのt期における広告接触データ作成
## まずはブランドレベルで集計
df_tvcom_panel <- df_tvcom %>%
  mutate(check = ifelse(count > 0, 1, 0)) %>% 
  select(-count) %>% 
  spread(brand_name, check) %>% 
  select(-c(3, 4)) %>% 
  group_by(panelist_id, week) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))
  
df_tvcom_panel[df_tvcom_panel == 2] <- 1
## 商品分増やしていく。いつか書き直す
dat1 <- replicate(3, df_tvcom_panel$`026bf156cc`)
colnames(dat1) <- name[2:4]
dat2 <- replicate(4, df_tvcom_panel$`0598217dd8`)
colnames(dat2) <- name[5:8]
dat3 <- replicate(5, df_tvcom_panel$`1135ebe8b1`)
colnames(dat3) <- name[9:13]
dat4 <- replicate(3, df_tvcom_panel$`1bbf8dfa32`)
colnames(dat4) <- name[14:16]
dat5 <- replicate(3, df_tvcom_panel$`388786a5a0`)
colnames(dat5) <- name[17:19]
dat6 <- replicate(6, df_tvcom_panel$`3ed5d148bd`)
colnames(dat6) <- name[20:25]
dat7 <- replicate(6, df_tvcom_panel$`4461ff9701`)
colnames(dat7) <- name[26:31]
dat8 <- replicate(3, df_tvcom_panel$`56009934e9`)
colnames(dat8) <- name[32:34]
dat9 <- replicate(5, df_tvcom_panel$`8107c1b704`)
colnames(dat9) <- name[35:39]
dat10 <- replicate(5, df_tvcom_panel$`9213c533a3`)
colnames(dat10) <- name[40:44]
dat11 <- replicate(6, df_tvcom_panel$`98643524ce`)
colnames(dat11) <- name[45:50]
dat12 <- replicate(4, df_tvcom_panel$`98aa9c7e5b`)
colnames(dat12) <- name[51:54]
dat13 <- replicate(3, df_tvcom_panel$`d804a51ed6`)
colnames(dat13) <- name[55:57]
dat14 <- replicate(6, df_tvcom_panel$`db4afa1725`)
colnames(dat14) <- name[58:63]
dat15 <- replicate(4, df_tvcom_panel$`df6c29e580`)
colnames(dat15) <- name[64:67]

dat <- cbind(dat1, dat2, dat3, dat4, dat5, dat6,dat7, 
             dat8, dat9, dat10, dat11, dat12, dat13, dat14, dat15)

df_tvcom_panel <- cbind.data.frame(df_tvcom_panel$panelist_id,df_tvcom_panel$week, dat)
colnames(df_tvcom_panel) <- colnames(df_panelpurchase_count_edit)

dat <- complete(df_panelpurchase_count_edit[, c(1,2)], panelist_id, week = list_week)

df_tvcom_panel <- merge(dat, df_tvcom_panel, by = c("panelist_id", "week"), all.x = TRUE )

# 該当期間だけを抽出する

df_tvcom_panel <- merge(df_tvcom_panel, df_period, by = c("panelist_id", "week"), all.y = TRUE)
df_tvcom_panel <- select(df_tvcom_panel, -69)
df_tvcom_panel[is.na(df_tvcom_panel)] <- 0

# ラグ
df_tvcom_panel_lag <- df_tvcom_panel
df_tvcom_panel_lag[, 3:68] <- lag(df_tvcom_panel_lag[, 3:68])

# 初週を消す
df_tvcom_panel$check <- lag(df_tvcom_panel$panelist_id)
df_tvcom_panel$check2 <- ifelse(df_tvcom_panel$check == df_tvcom_panel$panelist_id, 0,1)
df_tvcom_panel[is.na(df_tvcom_panel)]<- 1
df_tvcom_panel <- filter(df_tvcom_panel, check2 == 0)
df_tvcom_panel <- select(df_tvcom_panel, -c(69, 70))

# ラグも初週を消す
df_tvcom_panel_lag <- merge(df_tvcom_panel[, 1:2], df_tvcom_panel_lag, by = c("panelist_id", "week"), all.x = TRUE)


##############################################データ加工②:購入者×期間＆商品×期間#############################################





### t-1期の購買の有無
## 期間内におけるpanelist の購買有無をデータ化する
df_panelattri_purchase <- df_panelpurchase_count_edit %>% 
  group_by(panelist_id, week) %>% 
  summarise(count = n()) %>% 
  mutate(purchase_dummy = ifelse(count > 0, 1, 0)) %>% 
  select(panelist_id, week, purchase_dummy)

df_panelattri_purchase_new <- complete(df_panelattri_purchase[, c(1, 2)], panelist_id, week = unique(df_panelpurchase$week))
df_panelattri_purchase_new <- merge(df_panelattri_purchase_new,df_panelattri_purchase , by = c("panelist_id", "week"), all.x = TRUE)
df_panelattri_purchase_new$purchase_dummy[is.na(df_panelattri_purchase_new$purchase_dummy)] <- 0
# 前回購買したかどうかダミーを作成
df_panelattri_purchase_t1 <- df_panelattri_purchase_new %>% 
  group_by(panelist_id, week) %>% 
  summarise(purchase_dummy = purchase_dummy) %>% 
  mutate(purchase_dummy_lag1 = lag(purchase_dummy))

df_panelattri_purchase_t1 <- na.omit(df_panelattri_purchase_t1) # 購入者ごとの最初の購買を消した。
df_panelattri_purchase_t1 <- filter(df_panelattri_purchase_t1,purchase_dummy == 1 )
df_panelattri_purchase_t1 <- as.data.frame(df_panelattri_purchase_t1)

# df_panelattri_purchase_t1に関しても各ユーザーの最初の購買を消す
df_panelattri_purchase_t1 <-merge(df_panelpurchase_count_panel[, c(1, 2)], df_panelattri_purchase_t1,
                                    by = c("panelist_id", "week"), all.x = TRUE)


### 期間tに関するデータ: df_temp_holi
df_temp_holi <- merge(df_temp, df_holiday, by.x = "Date", by.y = "week")
df_temp_holiday <- df_panelattri_purchase_t1[, c(1, 2)] %>% 
  inner_join(df_temp_holi, by = c("week" = "Date"))

##############################################データ加工③：購入者＆期間＆商品###############################################





## 購入者の属性に関するデータ: df_panelattri
# ダミー変数化 marital status: 2 -> 0
df_panelattri$marital_status <- as.character(df_panelattri$marital_status)
df_panelattri$marital_status <- sub("2", "0", df_panelattri$marital_status)
df_panelattri$marital_status <- as.integer(df_panelattri$marital_status)
# ダミー変数化 gender: 2 -> 0
df_panelattri$gender <- as.character(df_panelattri$gender)
df_panelattri$gender <- sub("2", "0", df_panelattri$gender)
df_panelattri$gender <- as.integer(df_panelattri$gender)
# 今回の分析に対応するユーザーの属性データだけを抽出
userlist_attri <- unique(df_panelpurchase_count_panel$panelist_id)
df_panelattri <-subset(df_panelattri, subset = panelist_id %in% userlist_attri)
# idを昇順に並び替える
df_panelattri <- df_panelattri[order(df_panelattri$panelist_id),]
rownames(df_panelattri) <- 1:nrow(df_panelattri)
# 購買回数ごとに増やす
df_panelattri_new <- merge(df_panelpurchase_count_panel[, c(1, 2)], df_panelattri,
                           by = "panelist_id", all.x = TRUE)
df_panelattri_new <- subset(df_panelattri_new, select = -2)
## 商品の属性に関するデータ: 
# 商品カテゴリをダミー変数化
df_category$category <- as.factor(df_category$category)
df_category <- dummy_cols(df_category, select_columns = "category")
df_category <- select(df_category, -category)

# 今回採用されている商品
df_product <- df_panelpurchase_count_sum %>% 
  filter(pro >= 0.02) %>% 
  select(1, 3) %>% 
  mutate(size = str_sub(product,start = 12)) %>% 
  arrange(product)

# ブランドをダミー変数化
df_product$brand <- as.factor(df_product$brand)
# df_product$product <- as.factor(df_product$product)
df_product_b <- dummy_cols(df_product, select_columns = "brand")
# df_product_p <- dummy_cols(df_product, select_columns = "product")

# sizeを数値に直して単位をlにする
df_product_b$size <- as.numeric(df_product_b$size)
df_product_b$size <- df_product_b$size / 1000

df_product_attri <- df_product_b %>% 
  inner_join(df_category, by = c("brand"="brand_name")) %>% 
  select(-brand) #%>% 
  #inner_join(df_product_p, by = "product") %>% 
  #select(-c(brand, size.y)) %>% 
  #rename(size = size.x)

# 行列に変換する
product_attri <- as.matrix(df_product_attri[, -1])
rownames(product_attri) <- df_product_attri$product



# 答えのデータを行列化させる
answer <- as.matrix(df_panelpurchase_count_edit[, -c(1, 2)])


### 各ユーザーに対応するTV広告データが存在するかどうかを確認
length(unique(df_panelpurchase_count_edit$panelist_id)) == sum(unique(df_panelpurchase_count_edit$panelist_id) %in%unique(df_tvcom_panel$panelist_id))



##############################################データ加工: 手直し編 ##############################################

# ブランドの購買経験の有無にする
df_panelpurchase_count_panel <- df_panelpurchase_count_panel %>% 
  mutate(A = do.call(pmax, c(select(., c(3, 4, 5)))),
         B = do.call(pmax, c(select(., c(6:9)))),
         C = do.call(pmax, c(select(., c(10:14)))),
         D = do.call(pmax, c(select(., c(15:17)))),
         E = do.call(pmax, c(select(., c(18:20)))),
         `F` = do.call(pmax, c(select(., c(21:26)))),
         G = do.call(pmax, c(select(., c(27:32)))),
         H = do.call(pmax, c(select(., c(33:35)))),
         I = do.call(pmax, c(select(., c(36:40)))),
         J = do.call(pmax, c(select(., c(41:45)))),
         K = do.call(pmax, c(select(., c(46:51)))),
         L = do.call(pmax, c(select(., c(52:55)))),
         M = do.call(pmax, c(select(., c(56:58)))),
         N = do.call(pmax, c(select(., c(59:64)))),
         O = do.call(pmax, c(select(., c(65:68)))))

df_panelpurchase_count_panel[, c(3, 4, 5)] <- df_panelpurchase_count_panel$A
df_panelpurchase_count_panel[, c(6:9)] <- df_panelpurchase_count_panel$B
df_panelpurchase_count_panel[, c(10:14)] <- df_panelpurchase_count_panel$C
df_panelpurchase_count_panel[, c(15:17)] <- df_panelpurchase_count_panel$D
df_panelpurchase_count_panel[, c(18:20)] <- df_panelpurchase_count_panel$E
df_panelpurchase_count_panel[, c(21:26)] <- df_panelpurchase_count_panel$`F`
df_panelpurchase_count_panel[, c(27:32)] <- df_panelpurchase_count_panel$G
df_panelpurchase_count_panel[, c(33:35)] <- df_panelpurchase_count_panel$H
df_panelpurchase_count_panel[, c(36:40)] <- df_panelpurchase_count_panel$I
df_panelpurchase_count_panel[, c(41:45)] <- df_panelpurchase_count_panel$J
df_panelpurchase_count_panel[, c(46:51)] <- df_panelpurchase_count_panel$K
df_panelpurchase_count_panel[, c(52:55)] <- df_panelpurchase_count_panel$L
df_panelpurchase_count_panel[, c(56:58)] <- df_panelpurchase_count_panel$M
df_panelpurchase_count_panel[, c(59:64)] <- df_panelpurchase_count_panel$N
df_panelpurchase_count_panel[, c(65:68)] <- df_panelpurchase_count_panel$O

df_panelpurchase_count_panel <- select(df_panelpurchase_count_panel, -c(69:83))


############################################## SMM推定に必要なデータ加工 ##############################################

### 操作変数作成
# 職業
df_panelattri_new$job <- as.factor(df_panelattri_new$job)
df_panelattri_new <- dummy_cols(df_panelattri_new, select_columns = "job")
# 操作変数
Z_ht <- df_panelpurchase_count_panel %>% 
  select(1, 2, 3, 6, 10, 15, 18, 21, 27, 33, 36, 41, 46, 52, 56, 59, 65) %>% #　前回購入したブランド
  inner_join(df_tvcom_panel[, c(1, 2, 3, 6, 10, 15, 18, 21, 27, 33, 36, 41, 46, 52, 56, 59, 65)], by = c("panelist_id", "week")) %>% # 当該週にTVCMみたか？
  cbind(df_panelattri_new) %>% # 属性
  select(-c(33, 37, 46)) %>% 
  inner_join(df_panelattri_purchase_t1, by = c("panelist_id", "week")) %>% # 先週購入したかどうか？
  select(-37) %>%
  inner_join(df_temp_holiday,by = c("panelist_id", "week") ) %>% # 天気系
  inner_join(df_tvcom_panel_lag[, c(1, 2, 3, 6, 10, 15, 18, 21, 27, 33, 36, 41, 46, 52, 56, 59, 65)], by = c("panelist_id", "week")) %>%  # TV広告（先週どのCMみたか？）
  select(-c(1, 2))

Z_ht_t <- as.matrix(t(Z_ht))

### 配列に合わせるやつら。30回Drawに対応するため。速度早めるため。
## Halton Sequence
Halton <- array(0, dim = c(66, 12933, 300))
set.seed(101)
for ( i in 1:300){
  Halton[,,i] <- t(ghalton(12933, d=66))
}

# 価格
price <- as.matrix(subset(df_panelpurchase_price_edit, select = -c(1, 2)))
price_new <- array(0, dim = c(66, 12933, 30))
for (i in 1: 30){
  price_new[,,i]<-t(price)
}

# その他データ
## S
# 年齢、結婚、前回の購買からの期間の順番
d_s_attri <- as.matrix(subset(df_panelattri_new, select = c(3, 4)))
d_s_purchase <- as.matrix(subset(df_panelattri_purchase_t1, select = purchase_dummy_lag1))
d_s <- cbind(d_s_attri, d_s_purchase, rep(1, 12933))

# 収入
d_m <- as.matrix(subset(df_panelattri_new,select = 6))

# 年齢、性別、結婚
d_beta <- t(d_s_attri[, 1])
# 広告接触
ad <- as.matrix(subset(df_tvcom_panel,select = -c(1, 2)))
ad <- t(ad)
# ブランドロイヤリティ
br_p <- as.matrix(subset(df_panelpurchase_count_panel,select = -c(1, 2)))
br_p <- t(br_p)
# 結合
d_beta <- rbind(d_beta,ad, br_p)
# 性別、年齢、結婚
d_lambda_attri <- as.matrix(subset(df_panelattri_new, select = c(3, 4)))
# 前回の購買の有無（先行研究では前回購入からの期間を使用している）
d_lambda_purchase <- as.matrix(subset(df_panelattri_purchase_t1,  select = purchase_dummy_lag1))
# 気温
d_lambda_temp_holi <- as.matrix(subset(df_temp_holiday, select = 3))

# 定数項とともにいざ結合
d_lambda <- cbind(d_lambda_attri, d_lambda_purchase,d_lambda_temp_holi, rep(1, 12933))








