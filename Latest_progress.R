### 最新の進捗状況 ###
## 最終改訂日: 1216 ##

####################### 必要なファイルの読み込み ######################
# 時間測定ライブラリ
library(tictoc)

# データの読み込み・加工
source("DataCleaning.R", encoding = "UTF-8")

# h=1~H, t=1~T での消費機会Jごとの最適な消費量を出力する関数. Epsilon_ht.Rのサポート用．
source("Epsilon_ht_support.R")

# h=1~Hの期間t=1~Tの期待購入量を計算して予測誤差を出力する関数
source("Epsilon_ht.R")

# Simulated GMM の目的関数を出力する関数
source("J_ht.R")

# Draw30回数を考慮したSimulated GMM の目的関数を出力する関数
source("J_ht_new.R")

# J_ht_newの補助
source("Epsilon_ht_support_new.R")


######################## パラメータの設定 ############################

# 任意の家計、時間、パラメータ、定数項を設定
## 人と時間の設定
h <- "3ff4d7cefc3b3d24fb3b"
week <- 6
## パラメータ
delta <- matrix(c(4, 0.5, 2.5, 3.5, 1.9), 5, 1)　# 年齢、結婚、t-1、気温、定数項
alpha <- matrix(c(0.8, 2.4, 4.9, 0.2), 4, 1) # 年齢、結婚、t-1、定数項
kappa <- 1.2
gamma <- 0.05
mu_demo <- -3.0
mu_brand <- 0.1
mu_ad <- 0.1
## 定数項
beta_tilde <- 0.7
xi_i <- 1 # 本当はカテゴリ別とかにしたいけど、一旦固定で。スポーツ、野菜、サイダー、お茶、豆乳、紅茶、コーヒー、水、コーラ
### 誤差項の分布
ep_sigma <- 1.85

## パラメータベクトルたちを1つの行列にまとめる
parameters <- rbind(delta, alpha, kappa, gamma, mu_demo, mu_brand, mu_ad, beta_tilde, xi_i, ep_sigma)



######################## 予測誤差を出力する関数 ###################

# test_Epsilon_ht <- Epsilon_ht(parameters = parameters) # 22.396 



###################### Simulated GMM の目的関数 ###################
tic()
test_J_ht <- J_ht(parameters = parameters) # 24.903
toc()


###################### Simulated GMM の目的関数改良版 ###################

tic()
test_J_ht_new <- J_ht_new(parameters = parameters) # 128.295 sec.
toc()


###################### パラメータ推定 ###################
### 推定1
## Set.
library(GenSA)
global.min <- 0
tol <- 1e-5

## Paramter Min
delta_min <- matrix(rep(0, 5),5,1)
alpha_min <- matrix(rep(-10, 4), 4, 1)
kappa_min <- -10
gamma_min <- 0.00001 # 0 < gamma < 1を忘れずに！
mu_demo_min <- -10
mu_brand_min <- -10
mu_ad_min <- -10
beta_tilde_min <- -10
xi_i_min <- -10
ep_sigma_min <- 0.00001

lower <- rbind(delta_min, alpha_min, kappa_min, gamma_min, mu_demo_min,
               mu_brand_min, mu_ad_min, beta_tilde_min, xi_i_min, ep_sigma_min)

## Paramter Max
delta_max <- matrix(rep(10, 5), 5, 1)
alpha_max <- matrix(rep(10, 4), 4, 1)
kappa_max <- 10
gamma_max <- 0.99999 # 0 < gamma < 1を忘れずに！
mu_demo_max <- 10
mu_brand_max <- 10
mu_ad_max <- 10
beta_tilde_max <- 5
xi_i_max <- 10
ep_sigma_max <- 10

upper <- rbind(delta_max, alpha_max, kappa_max, gamma_max, mu_demo_max, 
               mu_brand_max, mu_ad_max, beta_tilde_max, xi_i_max, ep_sigma_max)

## Try!
tic()
set.seed(123)
out <- GenSA(lower = lower, upper = upper, fn = J_ht_new,
             control=list(threshold.stop=global.min+tol,verbose=TRUE))
toc()






### 推定2