#### Source code ####
library(sandwich)
library(lmtest)
library(xtable)
library(ggplot2)
library(dplyr)
library(shiny)
library(BH)

# Function for obtaining robust standard errors from a model fit
commarobust <- function(fit){
  coeftest(fit,vcovHC(fit, type="HC2"))
}
 
# Function that returns the summary table
table_maker <- function(n_Z1, n_Z0, n_Z1_contacted, 
                        n_Z0_contacted, n_Z1_voted, n_Z0_voted){
   tab <- rbind(c(n_Z1_voted/n_Z1, n_Z0_voted/n_Z0),
         c(n_Z1_contacted/n_Z1, n_Z0_contacted/n_Z0))
   colnames(tab) <- c("Treatment", "Control")
   rownames(tab) <- c("Percent Voting", "Percent Contacted")
   return(round(tab*100,2))
}

# Function that returns the statistical results
statistical_results <-  function(n_Z1, n_Z0, n_Z1_contacted, 
                                 n_Z0_contacted, n_Z1_voted, n_Z0_voted ){
  Y <- rep(c(0,1,0,1), times=c(n_Z1 - n_Z1_voted, n_Z1_voted,
                               n_Z0 - n_Z0_voted, n_Z0_voted))
  D <- rep(c(1,0,0,1), times=c(n_Z1_contacted, n_Z1 - n_Z1_contacted,  
                               n_Z0 - n_Z0_contacted, n_Z0_contacted))
  Z <- rep(c(1,0), times=c(n_Z1, n_Z0))
  
  itt_fit_r <- commarobust(lm(Y~Z))
  itt_d_fit <- lm(D~Z)
  
  contact_rate <-  coef(itt_d_fit)[2]
  itt_hat <- itt_fit_r[2,1] *100
  se_itt <- itt_fit_r[2,2] *100
  cace <- itt_hat/contact_rate
  se_cace <- se_itt/contact_rate
  ui <- cace + 1.96*se_cace
  li <- cace - 1.96*se_cace
  
  
  p <- pnorm(abs(itt_hat), sd = se_itt, lower.tail = FALSE)
  
  mu_t <- mean(Y[Z==1]) ; mu_c <- mean(Y[Z==0])
  power <- power_calculator(mu_t = mu_t, 
                            mu_c = mu_c, 
                            sigma = sqrt(mu_t * (1-mu_t)),
                            alpha = 0.1, N = n_Z1 + n_Z0)
  
  results <- matrix(c(sprintf("%.3f", contact_rate), 
                      sprintf("%.3f", itt_hat), 
                      sprintf("%.3f", se_itt), 
                      sprintf("%.3f", cace), 
                      sprintf("%.3f", se_cace), 
                      paste0(sprintf("%.3f", li), ", ",sprintf("%.3f", ui)), 
                      sprintf("%.3f", p), 
                      sprintf("%.3f", power)))
  
  rownames(results) <-  c("Contact Rate", 
                          "Estimated Intent-to-treat Effect",
                          "Standard Error of Estimated Intent-to-treat Effect",
                          "Estimated Complier Average Causal Effect (CACE)",
                          "Standard Error of Estimated CACE",
                          "95% Confidence Interval for Estimated CACE",
                          "One-tailed p-value",
                          "Statistical Power of this Experiment")
  return(xtable(results))
}


# Function that returns the summary table
table_maker_2 <- function(Y,D,Z){
  tab <- rbind(c(mean(Y[Z==1]), mean(Y[Z==0])),
               c(mean(D[Z==1])*100, mean(D[Z==0])*100))
  colnames(tab) <- c("Treatment", "Control")
  rownames(tab) <- c("Average Outcome", "Percent Contacted")
  return(round(tab,2))
}

# Function that returns the statistical results
statistical_results_2 <-  function(Y,D,Z){
  
  itt_fit_r <- commarobust(lm(Y~Z))
  itt_d_fit <- lm(D~Z)
  
  contact_rate <-  coef(itt_d_fit)[2]
  itt_hat <- itt_fit_r[2,1]
  se_itt <- itt_fit_r[2,2]
  cace <- itt_hat/contact_rate
  se_cace <- se_itt/contact_rate
  ui <- cace + 1.96*se_cace
  li <- cace - 1.96*se_cace
  
  p <- 2*pnorm(abs(itt_hat), sd = se_itt, lower.tail = FALSE)
  
  mu_t <- mean(Y[Z==1]) ; mu_c <- mean(Y[Z==0])
  power <- power_calculator(mu_t = mu_t, 
                            mu_c = mu_c, 
                            sigma = sd(Y),
                            N =length(Y))
  
  pr_at <- mean(D[Z==0])
  E_Y_1_at <- mean(Y[D==1 & Z ==0])
  if(is.nan(E_Y_1_at)){E_Y_1_at <- 0}
  
  pr_nt <- mean(D[Z==1]==0)
  E_Y_0_nt <- mean(Y[D==0 & Z ==1])
  if(is.nan(E_Y_0_nt)){E_Y_0_nt <- 0}
  
  untreated_compliers <- 
    (mean(Y[Z==0]) - pr_at*E_Y_1_at - pr_nt*E_Y_0_nt)/contact_rate
  
  results <- matrix(c(sprintf("%.3f", contact_rate), 
                      sprintf("%.3f", itt_hat), 
                      sprintf("%.3f", se_itt), 
                      sprintf("%.3f", cace), 
                      sprintf("%.3f", se_cace), 
                      paste0(sprintf("%.3f", li), ", ",sprintf("%.3f", ui)), 
                      sprintf("%.3f", p), 
                      sprintf("%.3f", power),
                      sprintf("%.3f", untreated_compliers)))
  
  rownames(results) <-  c("Contact Rate", 
                          "Estimated Intent-to-treat Effect",
                          "Standard Error of Estimated Intent-to-treat Effect",
                          "Estimated Complier Average Causal Effect (CACE)",
                          "Standard Error of Estimated CACE",
                          "95% Confidence Interval for Estimated CACE",
                          "Two-tailed p-value",
                          "Statistical Power of this Experiment",
                          "Estimated Average Outcome Among Untreated Compliers")
  return(xtable(results))
}



# Function that determines statistical power
power_calculator <- function(mu_t, mu_c, sigma, alpha=0.05, N){
  lowertail <- (abs(mu_t - mu_c)*sqrt(N))/(2*sigma)
  uppertail <- -1*lowertail
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 
    1-  pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE)
  return(beta)
}

# Helper for checking the datatype
is_binary <- function(x){
  return(all(x %in% c(0,1)))
}

