##### Title: Epsilon_ht_support.R
##### objective: h=1~H, t=1~T での消費機会Jごとの最適な消費量を出力する関数. Epsilon_ht.Rのサポート用．
##### Update: 2020/11/30

max_utility_UP <- function(product_attri, beta_tilde_new, mu, d_beta, xi_i, m, S_new, gamma, price, ep_mean, ep_sigma){
  
  # 消費機会jごとの変化をしめす
  # ハルトン行列から抽出
  halton <- ghalton(12933, d=66)
  epsilon_j <- qnorm(t(halton), ep_mean, ep_sigma)
  # mu*d_beta
  beta_j <- beta_tilde_new + mu%*%d_beta
  # Xβ
  lambda_beta_new <- product_attri%*%beta_j + xi_i + epsilon_j
  
  ### Quality Function
  QF <- lambda_beta_new
  for (i in 1: 66){
    for (j in 1:12933) {
      if(QF[i, j]>0){
        QF[i, j] <- QF[i, j]^m[j]
      }else{
        QF[i, j] <- 0
      }
    }
  }
  

  # 最適消費量の計算
  Q_star <- ((gamma[1]*(QF^(gamma[1])))*S_new)^(1/(1-gamma[1]))
  Q_star <- Q_star / t((price)^(1/(1-gamma[1])))
  
  # 小数点を切り捨てて整数に
  Q_star_down <- floor(Q_star)
  Q_star_up <- ceiling(Q_star)
  
  ### 効用関数
  ## Q_star_down消費した場合の効用
  u_star_down <- (QF*Q_star_down)^gamma*S_new
  u_star_up <- ((QF*Q_star_up)^gamma)*S_new
  
  
  ## 改善の余地あり
  # 切り上げverと切り下げverで効用を比較、最大となる商品をこの消費機会Jで消費する
  max_Q_star_j = matrix(rep(0), 66, 12933)
  for (i in 1: 12933){
    if (max(u_star_down[,i], na.rm = TRUE) > max(u_star_up[,i], na.rm = TRUE)){
      max_brand_down <- which(u_star_down[,i] == max(u_star_down[,i], na.rm = TRUE))
      max_Q_star_j[max_brand_down, i] <- Q_star_down[max_brand_down,i]
    }else{
      max_brand_up <- which(u_star_up[,i] == max(u_star_up[,i], na.rm = TRUE))
      max_Q_star_j[max_brand_up, i] <- Q_star_up[max_brand_up,i]
    }
  }
  
  ## その消費量を返す
  return(max_Q_star_j)
  
}