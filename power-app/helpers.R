#### Source code for power calculator

power_calculator <- function(mu_t, mu_c, sigma, alpha=0.05, N){
  lowertail <- (abs(mu_t - mu_c)*sqrt(N))/(2*sigma)
  uppertail <- -1*lowertail
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 
    1 - pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE)
  return(beta)
}

#power_calculator(mu_t = 60, mu_c = 50, sigma = 20,N = 100)
#power.t.test(n = 50, delta = 10, sd = 20)

power_calculator_binary <- function(p1, p0, alpha=0.05, N){
  lowertail <- (abs(p1 - p0) * sqrt(N/2))/sqrt(p1*(1-p1) + p0*(1-p0))
  uppertail <- -1*lowertail
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 
     1 - pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE)
  return(beta)
}

#power_calculator_binary(p1=.65, p0=.50, N=100)
#power.prop.test(p1 = .65, p2 = .50, n = 50)

power_calculator_cluster <- function(mu_t, mu_c, sigma, ICC, alpha=0.05, n_clus_per_arm, N){
  n_per_clus <- N/(n_clus_per_arm*2)
  if(n_per_clus < 1){return(NA)}
  lowertail <- (abs(mu_t - mu_c) * sqrt((n_clus_per_arm - 1)*n_per_clus))/
                      sqrt((2*(sigma^2) * (1 + (n_per_clus-1)*ICC)))
  uppertail <- -1*lowertail
  beta <- pnorm(lowertail - qnorm(1-alpha/2), lower.tail=TRUE) + 
    1 - pnorm(uppertail - qnorm(1-alpha/2), lower.tail=FALSE)
  return(beta)
}

power_calculator_binary_cluster <- function(p1, p0, ICC, alpha=0.05, n_clus_per_arm, N){
  n_per_clus <- N/(n_clus_per_arm*2)
  if(n_per_clus < 1){return(NA)}
  lowertail <- (abs(p1 - p0) * sqrt((n_clus_per_arm - 1)*n_per_clus))/
    sqrt((p1*(1-p1) + p0*(1-p0)) * (1 + (n_per_clus-1)*ICC))
  uppertail <- -1*lowertail
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 
    1 - pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE)
  return(beta)
}

