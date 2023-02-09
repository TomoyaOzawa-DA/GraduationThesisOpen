##### Title: J_ht_new.R
##### objective: Simulated GMM の目的関数を出力する関数
##### Update: 2020/12/12

# h=1~H, t=1~T での消費機会Jごとの最適な消費量を出力する関数. 
source("Epsilon_ht_support_new.R")


# h=1の期間ごとの期待購入量を出力する関数

J_ht_new <- function(parameters){
  ################### MaxUtilityの計算に必要ではあるが、消費機会Jにおいて不変であるものたち##################
  ## パラメータを配分する
  # パラメータベクトルから各パラメータを返す
  alpha <- parameters[6:9]
  kappa <- parameters[10]
  gamma <- parameters[11]
  mu <- matrix(rep(0,25*133), 25,133)
  mu_i <- c(parameters[12], rep(parameters[13], 66), rep(parameters[14], 66))
  for (i in 1:25){
    mu[i, ] <- mu_i
  }
  ## 定数項
  beta_tilde <- parameters[15]
  xi_i <- rep(parameters[16], 66)
  ## 誤差項の分布
  ep_mean<- 0
  ep_sigma<- parameters[17]
  
  # S
  S <- d_s %*% alpha
  
  ## m
  # m
  m <- d_m%*% kappa
  
  ## β
  
  # mu*d_beta
  beta_j <- beta_tilde + mu%*%d_beta
  # Xβ
  lambda_beta <- product_attri%*%beta_j + xi_i 
  # 配列にする
  lambda_beta_new <- array(0, dim = c(66, 12933, 30))
  for(i in 1:30){
    lambda_beta_new[,,i]<- lambda_beta
  }
  
  ### 最適消費量
  # Sの形を合わせる
  S_new <- matrix(rep(0, 66*12933), 66, 12933)
  for (i in 1: 66){
    S_new[i, ] <- t(S)
  }
  S_new_1 <- array(0, dim=c(66, 12933, 30))
  for ( i in 1:30){
    S_new_1[,,i]<- S_new
  }
  
  ##### j=1の時の購入量
  j_com=1
  Q_j <- max_utility_UP_new(product_attri=product_attri, lambda_beta_new=lambda_beta_new, m=m, S_new_1=S_new_1, gamma=gamma, price_new=price_new,  ep_sigma=ep_sigma, j_com=j_com)
  Q_j_add <- Q_j
  for (i in 2:9) {
    j_com=i
    Q_j_new <- max_utility_UP_new(product_attri=product_attri, lambda_beta_new=lambda_beta_new, m=m, S_new_1=S_new_1, gamma=gamma, price_new=price_new,  ep_sigma=ep_sigma, j_com=j_com)
    Q_j_add <- Q_j_add + Q_j_new
    Q_j <- cbind(Q_j, Q_j_add)
  }
  toc()
  
  ### これを3次元の配列にする。このQ_jにポアソン確率をかければ、期待購入量が求められる！
  Q_j <- array(Q_j, dim = c(66, 12933, 9))
  
  
  ## 消費機会jの分布パラメータλを決める
  # λ
  lambda <- d_lambda %*% parameters[1:5]
  
  # 行列の形を揃える
  lambda_rep <- t(matrix(rep(lambda, 66), 12933, 66))
  
  # ポアソン密度関数をj=1~10まで作成する
  J_prob <- dpois(1, lambda_rep)
  for (i in 2:9){
    J_prob_add <- dpois(i, lambda_rep)
    J_prob <- cbind(J_prob, J_prob_add)
  }
  
  # 配列にしたる
  J_prob_array <- array(J_prob, dim = c(66, 12933, 9))
  
  
  ##### h=1の期待購入量を計算する
  ### jごとの期待購入量をだす
  EQ_j <- Q_j*J_prob_array
  
  ### 3次元から2次元に落とし込む。つまり和をとればよい。
  EQ <- apply(EQ_j, c(1, 2), sum)
  
  ### 転置させて予測誤差をだす準備
  EQ_t <- t(EQ)
  ### 差分を計算
  epsilon <- EQ_t - answer
  
  # Sample Analog
  epsilon_t <- t(epsilon)
  g_ht <- Z_ht_t[,1]%x%epsilon_t[, 1]
  
  for (i in 2:12933){
    g_ht_add = Z_ht_t[,i]%x%epsilon_t[, i]
    g_ht = g_ht + g_ht_add
  }
  
  g_ht_ave <- g_ht/12933
  
  # ウェイト行列
  # とりあえず単位行列で
  W_ht <- diag(3894)
  
  # GMMの目的関数. これを最小化したい！
  J_ht <- t(g_ht_ave)%*%W_ht%*%g_ht_ave
  
  return(J_ht)
}