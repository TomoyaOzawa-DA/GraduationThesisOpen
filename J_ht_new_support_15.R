##### Title: J_ht_new_support_15.R
##### objective: h=1~H, t=1~T での消費機会Jごとの最適な消費量を出力する関数. 容量を考慮しないver.
##### Update: 2020/12/19

# 1周28 sec. 
max_utility_UP_new15 <- function(lambda_beta_new, m, S_new_1, gamma, price_new, ep_sigma, j_com){
  
  # 消費機会jごとの変化をしめす
  # ハルトン行列から抽出
  from = 1+30*(j_com-1)
  to = 30*j_com
  epsilon_j <- qnorm(Halton[,,from:to], 0, ep_sigma)
  
  QF <- lambda_beta_new + epsilon_j
  ### Quality Function 
  for (j in 1:13411) {
    QF[, j, ] <- ifelse(QF[, j, ]>0, QF[, j, ]^m[j], 0)
  }
  
  # 最適消費量の計算
  Q_star <- ((gamma[1]*(QF^(gamma[1])))*S_new_1)^(1/(1-gamma[1]))
  Q_star <- Q_star / ((price_new)^(1/(1-gamma[1])))
  
  # 小数点を切り捨てて整数に
  Q_star_down <- floor(Q_star)
  Q_star_up <- ceiling(Q_star)
  
  ### 効用関数
  ## Q_star_down消費した場合の効用
  u_star_down <- (QF*Q_star_down)^gamma*S_new_1
  u_star_up <- (QF*Q_star_up)^gamma*S_new_1
  
  ## 改善の余地あり
  # 切り上げverと切り下げverで効用を比較、最大となる商品をこの消費機会Jで消費する
  max_Q_star_z = array(0, dim = c(15, 13411, 30))
  for (j in 1:30){
    for (i in 1: 13411){
      if (max(u_star_down[,i,j], na.rm = TRUE) > max(u_star_up[,i,j], na.rm = TRUE)){
        max_brand_down <- which(u_star_down[,i,j] == max(u_star_down[,i,j], na.rm = TRUE))
        max_Q_star_z[max_brand_down, i,j] <- Q_star_down[max_brand_down,i,j]
      }else{
        max_brand_up <- which(u_star_up[,i,j] == max(u_star_up[,i,j], na.rm = TRUE))
        max_Q_star_z[max_brand_up, i,j] <- Q_star_up[max_brand_up,i,j]
      }
    }
  }
  
  # 平均をとって2次元へ
  max_Q_star_j <- apply(max_Q_star_z, c(1, 2), mean)
  ## その消費量を返す
  return(max_Q_star_j)
}