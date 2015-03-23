# server.R
source("helpers.R")

shinyServer(function(input, output) {

  # Basic
  
  # Create Plot
  output$out_plot <- renderPlot({
    n_Z1 = input$n_Z1
    n_Z0 = input$n_Z0
    n_Z1_contacted = input$n_Z1_contacted
    n_Z0_contacted = input$n_Z0_contacted
    n_Z1_voted = input$n_Z1_voted
    n_Z0_voted = input$n_Z0_voted
    df <- data.frame(
      Y = rep(c(1,0,1,0), times=c(n_Z1_voted, n_Z1 - n_Z1_voted, 
                                  n_Z0_voted, n_Z0 - n_Z0_voted)),
      D = rep(c(1,0,1,0), times=c(n_Z1_contacted, n_Z1 - n_Z1_contacted, 
                                  n_Z0_contacted, n_Z0 - n_Z0_contacted)),
      Z = rep(c("Treatment","Control"), times=c(n_Z1, n_Z0)))
  
    group_by(df, Z) %>% 
      summarize(y_bar = mean(Y),
                se = sqrt(y_bar*(1-y_bar)/n()),
                ui = y_bar + 1.96*se,
                li = y_bar - 1.96*se) %>%
    ggplot(aes(x=Z, y=y_bar)) + 
      geom_pointrange(aes(ymin=li, ymax=ui), size=2) + ylim(0, NA) +
      ggtitle("Voting Rates by Assigned Experimental Group") + 
      xlab("") + ylab("Average Voting Rate") +
      theme_bw()
  })
  
  # Create summary table  
  output$summary_table <- renderTable(digits=3, expr = {
    n_Z1 = input$n_Z1
    n_Z0 = input$n_Z0
    n_Z1_contacted = input$n_Z1_contacted
    n_Z0_contacted = input$n_Z0_contacted
    n_Z1_voted = input$n_Z1_voted
    n_Z0_voted = input$n_Z0_voted
    xtable(table_maker(n_Z1, n_Z0, n_Z1_contacted, 
                       n_Z0_contacted, n_Z1_voted, n_Z0_voted))
  })
  
  # Create Statistical Results
  output$statistical_results <- renderTable(digits=3, 
                                            include.colnames=FALSE,
                                            expr = {
    n_Z1 = input$n_Z1
    n_Z0 = input$n_Z0
    n_Z1_contacted = input$n_Z1_contacted
    n_Z0_contacted = input$n_Z0_contacted
    n_Z1_voted = input$n_Z1_voted
    n_Z0_voted = input$n_Z0_voted
    xtable(statistical_results(n_Z1, n_Z0, n_Z1_contacted, 
                               n_Z0_contacted, n_Z1_voted, n_Z0_voted))
  })
  
  # Upload a Dataset
  output$outcome <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat<- read.csv(inFile$datapath)
    vars <- names(dat)
    selectInput(inputId = "Y", "Dependent Variable", vars, selected=vars[1])
  })
  
  output$contact <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat<- read.csv(inFile$datapath)
    vars <- names(dat)
    selectInput(inputId = "D", "Treatment Received", vars, selected=vars[2])
  })
  
  output$treatment <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat<- read.csv(inFile$datapath)
    vars <- names(dat)
    selectInput(inputId = "Z", "Treatment Assigned", vars, selected=vars[3])
  })
  
  output$out_plot_2 <- renderPlot({
    inFile <- input$file1
    if(is.null(inFile)) return(NULL)
    
    dat<- read.csv(inFile$datapath)
    
    Y = dat[,input$Y]
    D = dat[,input$D]
    Z = dat[,input$Z]
    if(!is.numeric(Y)) stop("The dependent variable must be numeric.")
    if(!is_binary(D)) stop("The contact variable can only include 0's and 1's.")
    if(!is_binary(Z)) stop("The treatment variable can only include 0's and 1's.")
    
    df <- data.frame(Y, D, Z = ifelse(Z==1, "Treatment", "Control"))

    group_by(df, Z) %>% 
      summarize(y_bar = mean(Y),
                N = n(),
                se = sd(Y)/sqrt(N),
                ui = y_bar + 1.96*se,
                li = y_bar - 1.96*se) %>%
      ggplot(aes(x=Z, y=y_bar)) + 
      geom_pointrange(aes(ymin=li, ymax=ui), size=2) + 
      #ylim(0, NA) +
      ggtitle("Average Outcomes by Assigned Experimental Group") + 
      xlab("") + ylab("Average Outcome") +
      theme_bw()
  })
  
  # Create summary table  
  output$summary_table_2 <- renderTable(digits=3, expr = {
    inFile <- input$file1
    if(is.null(inFile)) return(NULL)
    
    dat<- read.csv(inFile$datapath)
    
    Y = dat[,input$Y]
    D = dat[,input$D]
    Z = dat[,input$Z]
    if(!is.numeric(Y)) stop("The dependent variable must be numeric.")
    if(!is_binary(D)) stop("The contact variable can only include 0's and 1's.")
    if(!is_binary(Z)) stop("The treatment variable can only include 0's and 1's.")
    
    xtable(table_maker_2(Y,D,Z))
  })
  
  # Create Statistical Results
  output$statistical_results_2 <- renderTable(digits=3, 
                                            include.colnames=FALSE,
                                            expr = {
                                              inFile <- input$file1
                                              if(is.null(inFile)) return(NULL)
                                              
                                              dat<- read.csv(inFile$datapath)
                                              Y = dat[,input$Y]
                                              D = dat[,input$D]
                                              Z = dat[,input$Z]
                                              if(!is.numeric(Y)) stop("The dependent variable must be numeric.")
                                              if(!is_binary(D)) stop("The contact variable can only include 0's and 1's.")
                                              if(!is_binary(Z)) stop("The treatment variable can only include 0's and 1's.")
                                              
                                              xtable(statistical_results_2(Y,D,Z))
                                            })
  
  
})