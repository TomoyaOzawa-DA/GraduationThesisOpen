### 最新の進捗状況 容量を考慮しないver.###
## 最終改訂日: 1220 ##

##### 必要なファイルの読み込み
# 時間測定ライブラリ
library(tictoc)

# データの読み込み・加工
tic()
source("DataCleaning_Brand.R", encoding = "UTF-8")
toc()

# 関数の読み込み
source("J_ht_new_15.R")
source("J_ht_new_support_15.R")



##### パラメータ
delta <- matrix(c(1, 0.5, 1, 1, 2), 5, 1)　# 年齢、結婚、t-1、気温、定数項
alpha <- matrix(c(0.8, 2.4, 1, 0.2), 4, 1) # 年齢、結婚、t-1、定数項
kappa <- 1.2
gamma <- 0.05
mu_demo <- 0.1
mu_brand <- 0.1
mu_ad <- 0.1
## 定数項
beta_tilde <- 0.7
xi_i <- 0 # 本当はカテゴリ別とかにしたいけど、一旦固定で。スポーツ、野菜、サイダー、お茶、豆乳、紅茶、コーヒー、水、コーラ
### 誤差項の分布
ep_sigma <- 1.1

## パラメータベクトルたちを1つの行列にまとめる
parameter <- rbind(delta, alpha, kappa, gamma, mu_demo, mu_brand, mu_ad, beta_tilde, xi_i, ep_sigma)



##### Simulated GMMの目的関数の値を出力する関数
tic()
test_J_ht_new15 <- J_ht_new15(parameters = parameter)
test_J_ht_new15
toc()



##### Simulated Annealing
### まずは軽く推定する。
## Set.
library(GenSA)

## Paramter Min
delta_min <- matrix(rep(1, 5),5,1)
alpha_min <- matrix(rep(-1, 4), 4, 1)
kappa_min <- 1
gamma_min <- 0.00001 # 0 < gamma < 1を忘れずに！
mu_demo_min <- -1
mu_brand_min <- -1
mu_ad_min <- -1
beta_tilde_min <- -1
xi_i_min <- 0
ep_sigma_min <- 1.5

lower <- rbind(delta_min, alpha_min, kappa_min, gamma_min, mu_demo_min,
               mu_brand_min, mu_ad_min, beta_tilde_min, xi_i_min, ep_sigma_min)

### Test
tic()
test_J_ht_new15 <- J_ht_new15(parameters = upper)
test_J_ht_new15
toc()


### 上限決める用
## Paramter Min
delta_min <- matrix(rep(2, 5),5,1)
alpha_min <- matrix(rep(-1, 4), 4, 1)
kappa_min <- 1
gamma_min <- 0.00001 # 0 < gamma < 1を忘れずに！
mu_demo_min <- -1
mu_brand_min <- -1
mu_ad_min <- -1
beta_tilde_min <- -1
xi_i_min <- 0
ep_sigma_min <- 1.5

lower <- rbind(delta_min, alpha_min, kappa_min, gamma_min, mu_demo_min,
               mu_brand_min, mu_ad_min, beta_tilde_min, xi_i_min, ep_sigma_min)

### Test
tic()
test_J_ht_new15 <- J_ht_new15(parameters = lower)
test_J_ht_new15
toc()

## Paramter Max
delta_max <- matrix(rep(2, 5), 5, 1)
alpha_max <- matrix(rep(2, 4), 4, 1)
kappa_max <- 1
gamma_max <- 0.99999 # 0 < gamma < 1を忘れずに！
mu_demo_max <- 1
mu_brand_max <-1
mu_ad_max <- 1
beta_tilde_max <- 1
xi_i_max <- 1
ep_sigma_max <- 1
upper <- rbind(delta_max, alpha_max, kappa_max, gamma_max, mu_demo_max, 
               mu_brand_max, mu_ad_max, beta_tilde_max, xi_i_max, ep_sigma_max)


## Try!
set.seed(123)
out <- GenSA(lower = lower, upper = upper, fn = J_ht_new15,
             control=list(max.time = 108000 ,verbose=TRUE))











