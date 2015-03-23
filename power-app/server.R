# server.R
source("helpers.R")

Ns_small <- as.matrix(1:10000)
Ns_big <- as.matrix(c(seq(1, 9999, 1), seq(1000, 100000000, 1000)))
shinyServer(
  function(input, output) {
    
    betas_fun <- reactive({
      sigma <- input$sigma
      tau <- input$tau
      target <- input$target
      maxn <- input$maxn
      alpha <- switch(input$alpha,
                      "Alpha = 0.01" = 0.01, 
                      "Alpha = 0.05" = 0.05, 
                      "Alpha = 0.10" = 0.10)
      betas_small <- apply(X=Ns_small,MARGIN = 1,FUN = power_calculator, mu_t= (60 + tau), mu_c=60, sigma=sigma,alpha=alpha)
      betas_big <- apply(X=Ns_big,MARGIN = 1,FUN = power_calculator, mu_t= (60 + tau), mu_c=60, sigma=sigma,alpha=alpha)
      big <- (sum(betas_small>=target, na.rm = TRUE)==0 | maxn>10000)
      return(list(betas_small=betas_small,betas_big=betas_big,big=big))
    })
    
    betas_fun_binary <- reactive({
      p0 <- input$p0_b
      p1 <- input$p1_b
      target <- input$target_b
      maxn <- input$maxn_b
      alpha <- switch(input$alpha_b,
                      "Alpha = 0.01" = 0.01, 
                      "Alpha = 0.05" = 0.05, 
                      "Alpha = 0.10" = 0.10)
      betas_small <- apply(X=Ns_small,MARGIN = 1,FUN = power_calculator_binary, p1=p1, p0=p0,alpha=alpha)
      betas_big <- apply(X=Ns_big,MARGIN = 1,FUN =power_calculator_binary, p1=p1, p0=p0,alpha=alpha)
      big <- (sum(betas_small>=target, na.rm = TRUE)==0 | maxn>10000)
      return(list(betas_small=betas_small,betas_big=betas_big,big=big))
    })
    
    betas_fun_clus <- reactive({
      sigma <- input$sigma_c
      ICC <- input$ICC_c
      tau <- input$tau_c
      n_clus_per_arm <- input$n_clus_per_arm_c
      target <- input$target_c
      maxn <- input$maxn_c
      alpha <- switch(input$alpha_c,
                      "Alpha = 0.01" = 0.01, 
                      "Alpha = 0.05" = 0.05, 
                      "Alpha = 0.10" = 0.10)
      betas_small <- apply(X=Ns_small,MARGIN = 1,FUN = power_calculator_cluster, mu_t= (60 + tau), mu_c=60, sigma=sigma, ICC=ICC,n_clus_per_arm=n_clus_per_arm,alpha=alpha)
      betas_big <- apply(X=Ns_big,MARGIN = 1,FUN = power_calculator_cluster,mu_t= (60 + tau), mu_c=60, sigma=sigma, ICC=ICC,n_clus_per_arm=n_clus_per_arm,alpha=alpha)
      big <- (sum(betas_small>=target, na.rm = TRUE)==0| maxn>10000)
      return(list(betas_small=betas_small,betas_big=betas_big,big=big))
    })
    
    betas_fun_binary_clus <- reactive({
      p0 <- input$p0_bc
      p1 <- input$p1_bc
      ICC <- input$ICC_bc
      n_clus_per_arm <- input$n_clus_per_arm_bc
      target <- input$target_bc
      maxn <- input$maxn_bc
      alpha <- switch(input$alpha_bc,
                      "Alpha = 0.01" = 0.01, 
                      "Alpha = 0.05" = 0.05, 
                      "Alpha = 0.10" = 0.10)
      betas_small <- apply(X=Ns_small,MARGIN = 1,FUN = power_calculator_binary_cluster, p0=p0, p1=p1, ICC=ICC,n_clus_per_arm=n_clus_per_arm,alpha=alpha)
      betas_big <- apply(X=Ns_big,MARGIN = 1,FUN = power_calculator_binary_cluster, p0=p0, p1=p1, ICC=ICC,n_clus_per_arm=n_clus_per_arm,alpha=alpha)
      big <- (sum(betas_small>=target, na.rm = TRUE)==0| maxn>10000)
      return(list(betas_small=betas_small,betas_big=betas_big,big=big))
    })
    
        
    output$powerplot <- renderPlot({  
      if(input$clustered==FALSE & input$binary==FALSE){
      sigma <- input$sigma
      tau <- input$tau
      target <- input$target
      maxn <- input$maxn
      alpha <- switch(input$alpha,
                      "Alpha = 0.01" = 0.01, 
                      "Alpha = 0.05" = 0.05, 
                      "Alpha = 0.10" = 0.10)
      results <- betas_fun()
      if(!results$big){
      plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",tau, "\nSD of outcome = ", sigma), ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
      lines(Ns_small, results$betas_small, lwd=4,col="blue")
      abline(h=target, col="red", lty=2)
      }
      
      if(results$big){
        plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",tau, "\nSD of outcome = ", sigma), ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
        lines(Ns_big, results$betas_big, lwd=4,col="blue")
        abline(h=target, col="red", lty=2)
      }
      }
      if(input$clustered==TRUE & input$binary==FALSE){
        sigma <- input$sigma_c
        ICC <- input$ICC_c
        tau <- input$tau_c
        n_clus_per_arm <- input$n_clus_per_arm_c
        target <- input$target_c
        alpha <- switch(input$alpha_c,
                        "Alpha = 0.01" = 0.01, 
                        "Alpha = 0.05" = 0.05, 
                        "Alpha = 0.10" = 0.10)
        maxn <- input$maxn_c
        results <- betas_fun_clus()
        if(!results$big){
          plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",tau, "\nSD of outcome = ", sigma, "; ICC = ",ICC), 
               ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
          lines(Ns_small, results$betas_small, lwd=4, col="green")
          lines(Ns_small, apply(X=Ns_small,MARGIN = 1,FUN = power_calculator, mu_t= (60 + tau), mu_c=60, sigma=sigma,alpha=alpha), lwd=4, col="blue")
          abline(h=target, col="red", lty=2)
          legend("bottomright", legend=c("Equivalent Individual-Level Design", "Clustered Design"), col=c("blue", "green"), lwd=c(4,4), lty=c(1,1))
        }
        
        if(results$big){
          plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",tau, "\nSD of outcome = ", sigma, "; ICC = ",ICC), 
               ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
          lines(Ns_big, results$betas_big, lwd=4, col="green")
          lines(Ns_big, apply(X=Ns_big,MARGIN = 1,FUN = power_calculator, mu_t= (60 + tau), mu_c=60, sigma=sigma,alpha=alpha), lwd=4, col="blue")
          abline(h=target, col="red", lty=2)
          legend("bottomright", legend=c("Equivalent Individual-Level Design", "Clustered Design"), col=c("blue", "green"), lwd=c(4,4), lty=c(1,1))
        }
       }
      
      # binary
      if(input$clustered==FALSE & input$binary==TRUE){
        p0 <- input$p0_b
        p1 <- input$p1_b
        target <- input$target_b
        maxn <- input$maxn_b
        alpha <- switch(input$alpha_b,
                        "Alpha = 0.01" = 0.01, 
                        "Alpha = 0.05" = 0.05, 
                        "Alpha = 0.10" = 0.10)
        results <- betas_fun_binary()
        if(!results$big){
          plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",round((p1-p0),3)," Percentage Points"),
               ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
          lines(Ns_small, results$betas_small, lwd=4,col="blue")
          abline(h=target, col="red", lty=2)
        }
        
        if(results$big){
          plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",round((p1-p0),3)," Percentage Points"),
               ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
          lines(Ns_big, results$betas_big, lwd=4,col="blue")
          abline(h=target, col="red", lty=2)
        }
      }
      
      if(input$clustered==TRUE & input$binary==TRUE){
        p0 <- input$p0_bc
        p1 <- input$p1_bc
        ICC <- input$ICC_bc
        n_clus_per_arm <- input$n_clus_per_arm_bc
        target <- input$target_bc
        alpha <- switch(input$alpha_bc,
                        "Alpha = 0.01" = 0.01, 
                        "Alpha = 0.05" = 0.05, 
                        "Alpha = 0.10" = 0.10)
        maxn <- input$maxn_bc
        results <- betas_fun_binary_clus()
        if(!results$big){
          plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",round(abs(p1-p0),3), " Percentage Points; \n ICC = ",ICC), 
               ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
          lines(Ns_small, results$betas_small, lwd=4, col="green")
          lines(Ns_small, apply(X=Ns_small,MARGIN = 1,FUN = power_calculator_binary, p1=p1, p0=p0,alpha=alpha), lwd=4, col="blue")
          abline(h=target, col="red", lty=2)
          legend("bottomright", legend=c("Equivalent Individual-Level Design", "Clustered Design"), col=c("blue", "green"), lwd=c(4,4), lty=c(1,1))
        }
        
        if(results$big){
          plot(NA, ylim=c(0,1), xlim=c(0,maxn), main=paste0("Power Analysis: Hypothetical Treatment Effect = ",round(abs(p1-p0),3), " Percentage Points; \n ICC = ",ICC), 
               ylab="Power (Probability of Statistical Significance)", xlab="Number of Subjects")
          lines(Ns_big, results$betas_big, lwd=4, col="green")
          lines(Ns_big, apply(X=Ns_big,MARGIN = 1,FUN = power_calculator_binary, p1=p1, p0=p0,alpha=alpha), lwd=4, col="blue")
          abline(h=target, col="red", lty=2)
          legend("bottomright", legend=c("Equivalent Individual-Level Design", "Clustered Design"), col=c("blue", "green"), lwd=c(4,4), lty=c(1,1))
        }
      }
      
    })

output$nrequired <- renderUI({  
  if(input$clustered==FALSE & input$binary==FALSE){
    sigma <- input$sigma
    tau <- input$tau
    target <- input$target
    alpha <- switch(input$alpha,
                    "Alpha = 0.01" = 0.01, 
                    "Alpha = 0.05" = 0.05, 
                    "Alpha = 0.10" = 0.10)
    results <- betas_fun()
    if(!results$big){
      nrequired <-Ns_small[which.max(results$betas_small>=target)]
    }
    if(results$big){
      nrequired <-Ns_big[which.max(results$betas_big>=target)]
    }
    
    str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of at least ", nrequired,".")
    if(sum(results$betas_big>=target, na.rm=TRUE)==0){str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of well more than 10,000,000")}
  }
  if(input$clustered==TRUE & input$binary==FALSE){
    sigma <- input$sigma_c
    ICC <- input$ICC_c
    tau <- input$tau_c
    n_clus_per_arm <- input$n_clus_per_arm_c
    target <- input$target_c
    alpha <- switch(input$alpha_c,
                    "Alpha = 0.01" = 0.01, 
                    "Alpha = 0.05" = 0.05, 
                    "Alpha = 0.10" = 0.10)
    maxn <- input$maxn_c
    results <- betas_fun_clus()
    if(!results$big){
      nrequired <-Ns_small[which.max(results$betas_small>=target)]
    }
    if(results$big){
      nrequired <-Ns_big[which.max(results$betas_big>=target)]
    }
    
    str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of at least ", nrequired,", or an average of at least ", round(nrequired/(n_clus_per_arm*2)), " subjects in each of ", n_clus_per_arm*2, " clusters. Right-click to download image.")
    if(sum(results$betas_big>=target, na.rm=TRUE)==0){str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of well more than 10,000,000. You may need to increase the number of clusters.")}
    }
  
  # Binary
  
  if(input$clustered==FALSE & input$binary==TRUE){
    p0 <- input$p0_b
    p1 <- input$p1_b
    target <- input$target_b
    alpha <- switch(input$alpha_b,
                    "Alpha = 0.01" = 0.01, 
                    "Alpha = 0.05" = 0.05, 
                    "Alpha = 0.10" = 0.10)
    results <- betas_fun_binary()
    if(!results$big){
      nrequired <-Ns_small[which.max(results$betas_small>=target)]
    }
    if(results$big){
      nrequired <-Ns_big[which.max(results$betas_big>=target)]
    }
    
    str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of at least ", nrequired,".")
    if(sum(results$betas_big>=target, na.rm=TRUE)==0){str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of well more than 10,000,000")}
  }
  if(input$clustered==TRUE & input$binary==TRUE){
    p0 <- input$p0_bc
    p1 <- input$p1_bc
    ICC <- input$ICC_bc
    n_clus_per_arm <- input$n_clus_per_arm_bc
    target <- input$target_bc
    alpha <- switch(input$alpha_bc,
                    "Alpha = 0.01" = 0.01, 
                    "Alpha = 0.05" = 0.05, 
                    "Alpha = 0.10" = 0.10)
    maxn <- input$maxn_bc
    results <- betas_fun_binary_clus()
    if(!results$big){
      nrequired <-Ns_small[which.max(results$betas_small>=target)]
    }
    if(results$big){
      nrequired <-Ns_big[which.max(results$betas_big>=target)]
    }
    
    str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of at least ", nrequired,", or an average of at least ", round(nrequired/(n_clus_per_arm*2)), " subjects in each of ", n_clus_per_arm*2, " clusters. Right-click to download image.")
    if(sum(results$betas_big>=target, na.rm=TRUE)==0){str1 <- paste0("In order to achieve ", target*100, "% power, you'll need to use a sample size of well more than 10,000,000. You may need to increase the number of clusters.")}
  }
  
  HTML(str1)
  })
})


