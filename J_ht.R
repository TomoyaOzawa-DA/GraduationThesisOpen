##### Title: J_ht.R
##### objective: Simulated GMM の目的関数を出力する関数
##### Update: 2020/12/12

# h=1~H, t=1~T での消費機会Jごとの最適な消費量を出力する関数. EQ_speedUP用.
source("Epsilon_ht_support.R")


# h=1の期間ごとの期待購入量を出力する関数

J_ht <- function(parameters){
  ################### MaxUtilityの計算に必要ではあるが、消費機会Jにおいて不変であるものたち##################
  ## パラメータを配分する
  # パラメータベクトルから各パラメータを返す
  alpha <- parameters[7:10]
  kappa <- parameters[11]
  gamma <- parameters[12]
  mu <- matrix(rep(0,25*135), 25,135)
  mu_i <- c(parameters[13:15], rep(parameters[16], 3), rep(parameters[17], 4), rep(parameters[18], 5), rep(parameters[19], 3), rep(parameters[20], 3), rep(parameters[21], 6), rep(parameters[22], 6),
            rep(parameters[23], 3), rep(parameters[24], 5), rep(parameters[25], 5), rep(parameters[26], 6), rep(parameters[27], 4), rep(parameters[28], 3), rep(parameters[29], 6), rep(parameters[30], 4),
            rep(parameters[31], 3), rep(parameters[32], 4), rep(parameters[33], 5), rep(parameters[34], 3), rep(parameters[35], 3), rep(parameters[36], 6), rep(parameters[37], 6),
            rep(parameters[38], 3), rep(parameters[39], 5), rep(parameters[40], 5), rep(parameters[41], 6), rep(parameters[42], 4), rep(parameters[43], 3), rep(parameters[44], 6), rep(parameters[45], 4))
  for (i in 1:25){
    mu[i, ] <- mu_i
  }
  ## 定数項
  beta_tilde <- parameters[46]
  xi_i <- c(rep(parameters[47], 3), rep(parameters[48], 4), rep(parameters[49],5), rep(parameters[50],3),rep(parameters[51], 3), rep(parameters[52], 6), rep(parameters[50], 6), 
            rep(parameters[53], 3), rep(parameters[53], 5), rep(parameters[50], 5), rep(parameters[53], 6), rep(parameters[50], 4), rep(parameters[54], 3), rep(parameters[55], 6), rep(parameters[50], 4))
  ## 誤差項の分布
  ep_mean<- 0
  ep_sigma<- parameters[56]
  
  
  ## S
  # 年齢、性別、結婚、前回の購買からの期間の順番
  d_s_attri <- as.matrix(subset(df_panelattri_new, select = c(2, 3, 4)))
  d_s_purchase <- as.matrix(subset(df_panelattri_purchase_t1, select = purchase_dummy_lag1))
  d_s <- cbind(d_s_attri, d_s_purchase)
  
  # S
  S <- d_s %*% alpha
  
  ## m
  # 収入
  d_m <- as.matrix(subset(df_panelattri_new,select = 6))
  
  # m
  m <- d_m%*% kappa
  
  ## β
  # 年齢、性別、結婚
  d_beta <- t(d_s_attri)
  # 広告接触
  ad <- as.matrix(subset(df_tvcom_panel,select = -c(1, 2)))
  ad <- t(ad)
  # ブランドロイヤリティ
  br_p <- as.matrix(subset(df_panelpurchase_count_panel,select = -c(1, 2)))
  br_p <- t(br_p)
  # 結合
  d_beta <- rbind(d_beta,ad, br_p)
  
  # 定数項を列分増やす。
  beta_tilde_new <- matrix(rep(beta_tilde, 25*12933), 25, 12933)
  
  # 定数項を列分増やす
  xi_i_new <- matrix(rep(0, 66* 12933),66, 12933)
  for(j in 1:12933){
    xi_i_new[,j] <- xi_i
  }
  
  ### 最適消費量
  # 1mlあたりの価格をp_iとして実験
  price <- as.matrix(subset(df_panelpurchase_price_edit, select = -c(1, 2)))
  # Sの形を合わせる
  S <- t(S)
  S_new <- matrix(rep(0, 66*12933), 66, 12933)
  for (i in 1: 66){
    S_new[i, ] <- S
  }
  
  ##### j=1の時の購入量
  Q_j <- max_utility_UP(product_attri = product_attri,beta_tilde_new = beta_tilde_new,mu = mu,d_beta = d_beta , xi_i = xi_i, m = m, S_new = S_new, gamma = gamma, price = price, ep_mean = ep_mean, ep_sigma = ep_sigma)
  Q_j_add <- Q_j
  for (i in 2:10) {
    Q_j_new <- max_utility_UP(product_attri = product_attri,beta_tilde_new = beta_tilde_new,mu = mu,d_beta = d_beta , xi_i = xi_i, m = m, S_new = S_new, gamma = gamma, price = price, ep_mean = ep_mean, ep_sigma = ep_sigma)
    Q_j_add <- Q_j_add + Q_j_new
    Q_j <- cbind(Q_j, Q_j_add)
  }
  
  ### これを3次元の配列にする。このQ_jにポアソン確率をかければ、期待購入量が求められる！
  Q_j <- array(Q_j, dim = c(66, 12933, 10))
  
  
  ## 消費機会jの分布パラメータλを決める
  # 性別、年齢、結婚
  d_lambda_attri <- as.matrix(subset(df_panelattri_new, select = c(2, 3, 4)))
  # 前回の購買の有無（先行研究では前回購入からの期間を使用している）
  d_lambda_purchase <- as.matrix(subset(df_panelattri_purchase_t1,  select = purchase_dummy_lag1))
  # 気温と休日
  d_lambda_temp_holi <- as.matrix(subset(df_temp_holiday, select = c(3, 4)))
  
  # いざ結合
  d_lambda <- cbind(d_lambda_attri, d_lambda_purchase,d_lambda_temp_holi)
  # λ
  lambda <- d_lambda %*% parameters[1:6]
  
  # 行列の形を揃える
  lambda_rep <- t(matrix(rep(lambda, 66), 12933, 66))
  
  # ポアソン密度関数をj=1~10まで作成する
  J_prob <- dpois(1, lambda_rep)
  for (i in 2:10){
    J_prob_add <- dpois(i, lambda_rep)
    J_prob <- cbind(J_prob, J_prob_add)
  }
  
  # 配列にしたる
  J_prob_array <- array(J_prob, dim = c(66, 12933, 10))
  
  
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