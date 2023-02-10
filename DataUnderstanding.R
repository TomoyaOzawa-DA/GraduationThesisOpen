################################################ 基本統計量 ########################################
library(stargazer)
## hベース
# 属性
df_panelattri_stat <- df_panelattri[,-c(1, 5)] 
stargazer(df_panelattri_stat, out="stargazer.descript.tex", title="Stat panel_attri", align=F, style="qje")
## tベース
stargazer(df_temp_holi[, -1], out="stargazer.descript.tex", title="Stat panel_attri", align=F, style="qje")
## htベース
#ブランドロイヤルティ広告接触t-1期
df_panelpurchase_count_panel_st <- df_panelpurchase_count_panel[, c(3, 6, 10, 15, 18, 21, 27, 33, 36, 41, 46, 52, 56, 59,65)]
cname = colnames(df_panelpurchase_count_panel_st)
cname = substr(cname, 1, 10)
colnames(df_panelpurchase_count_panel_st) = cname
stargazer(df_panelpurchase_count_panel_st, out="stargazer.descript.tex", title="Stat BL", align=F, style="qje")

df_tvad <- df_tvcom %>%
  mutate(check = ifelse(count > 0, 1, 0)) %>% 
  select(-count) %>% 
  spread(brand_name, check) %>% 
  select(-c(3, 4)) %>% 
  group_by(panelist_id, week) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

df_tvad[df_tvad == 2] <- 1
df_tvad <- merge(df_panelpurchase_count_panel[, c(1, 2)], df_tvad, by = c("panelist_id", "week"), all.x = TRUE)
df_tvad[is.na(df_tvad)] <- 0
adname = colnames(df_tvad)
for ( i in 3:17){
  adname[i] <- paste("TVad", adname[i], sep = "_")
}
colnames(df_tvad) = adname

stargazer(df_tvad[, -c(1, 2)], out="stargazer.descript.tex", title="Stat TVad", align=F, style="qje")
## 商品属性
product_attri_1 <- as.data.frame(product_attri)
stargazer(product_attri_1, out="stargazer.descript.tex", title="Stat panel_attri", align=F, style="qje")


## 一度の購買で何種類の商品を購入しているのかを算出->消費機会の上限を決める
max_purchase_time = apply(df_panelpurchase_count_edit[, -c(1, 2)],1, function(x){
  number = sum(x>0)
})
# マックス9回でした。
max(max_purchase_time)






################################################ 容量を判別した時に使ったコード ########################################3

## 容量がどの程度あるのかを把握する
# item_count_sum = 1
volume_variety_count1_volume1 <- df_panelpurchase %>% 
  filter(count == 1, item_count_sum == 1) %>% 
  group_by(brand_name, volume_sum) %>% 
  summarise(count = n() ) 

# 全てを集計
volume_variety_count <- df_panelpurchase %>% 
  group_by(brand_name, item_count_sum, volume_sum) %>% 
  summarise(count = n() ) %>% 
  mutate(average_vol = as.integer(volume_sum / item_count_sum))

# 各ブランドごとに、1単位あたりの容量が単品で買われたことのある容量と等しい場合には1をとるダミー変数checkを作成
# このダミー変数が0であれば、①異なる容量での購入であった、②データの記入ミスだと思われる。

volume_variety_count$check <- 0
brands = as.matrix(volume_variety_count$brand_name)
for (i in 1:nrow(volume_variety_count)){
  bran = brands[i]
  dat = as.matrix(subset(volume_variety_count1_volume1, brand_name == bran, select = 2))
  if (volume_variety_count[i, 5] %in% dat){
    volume_variety_count[i, 6] <- 1
  }
}


# 77％は被ってた。残り23%は気合です。
mean(volume_variety_count$check)
(1- mean(volume_variety_count$check))*nrow(volume_variety_count)

# エクセル作業用に出力
write.csv(volume_variety_count, "vol.csv", row.names = FALSE)







# 累積確率を基準に落とす？？
# 99%
df_panelpurchase_count_sum99 <- df_panelpurchase_count_sum %>% 
  mutate(check = ifelse(cum >= 0.99, 1, 0)) %>% 
  group_by(brand) %>% 
  mutate(check2 = lag(check, n = 1)) %>% 
  mutate(check3 = check + check2) %>% 
  replace_na(list(check2 = 0, check3=0)) %>% 
  filter(check3 < 2) %>% 
  select(1:7)

# 95%だと82商品
df_panelpurchase_count_sum95 <- df_panelpurchase_count_sum %>% 
  mutate(check = ifelse(cum >= 0.95, 1, 0)) %>% 
  group_by(brand) %>% 
  mutate(check2 = lag(check, n = 1)) %>% 
  mutate(check3 = check + check2) %>% 
  replace_na(list(check2 = 0, check3=0)) %>% 
  filter(check3 < 2) %>% 
  select(1:7)

# 90%だと63商品
df_panelpurchase_count_sum90 <- df_panelpurchase_count_sum %>% 
  mutate(check = ifelse(cum >= 0.90, 1, 0)) %>% 
  group_by(brand) %>% 
  mutate(check2 = lag(check, n = 1)) %>% 
  mutate(check3 = check + check2) %>% 
  replace_na(list(check2 = 0, check3=0)) %>% 
  filter(check3 < 2) %>% 
  select(1:7)
