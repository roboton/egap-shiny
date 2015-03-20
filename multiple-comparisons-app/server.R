shinyServer(function(input, output) {
  output$ptable <- renderTable(digits=3, expr = {
    alpha <- switch(input$alpha,
                    "Alpha = 0.01" = 0.01,
                    "Alpha = 0.05" = 0.05,
                    "Alpha = 0.10" = 0.10)
    ps <- c(input$P1, input$P2, input$P3, 
            input$P4, input$P5, input$P6,
            input$P7, input$P8, input$P9, input$P10)
    cor_method <- switch(input$cor_method,
                    "none" = "none",
                    "Bonferroni" = "bonferroni",
                    "Holm" = "holm",
                    "Benjamini and Hochberg" = "fdr")
    adjusted_ps <- p.adjust(p = ps, method = cor_method)
    
    mat <- data.frame(ps, (ps <= alpha), adjusted_ps, (adjusted_ps <= alpha))
    colnames(mat) <- c("Unadjusted p-values",
                       "Unadjusted significance",
                       "Adjusted p-values",
                       "Adjusted significance")
    mat
  })
})