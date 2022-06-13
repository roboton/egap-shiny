library(shiny)
numericInput2<-function (inputId, label, value = 1,...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = value,...))
}


shinyUI(
  fluidPage(    
    titlePanel("Multiple Comparisons Corrections"),
      fluidRow(      
        column(4,
             wellPanel("Pick your alpha level and desired correction",
                       hr(),
                       selectInput("alpha", "Significance Level",c("Alpha = 0.01", "Alpha = 0.05", "Alpha = 0.10"),selected="Alpha = 0.05"),
                       selectInput("cor_method", "Correction",c("None", "Bonferroni", "Holm", "Benjamini and Hochberg"),selected="Bonferroni")
              ),
             wellPanel("Enter your p-values",
                       hr(),
                        numericInput2(inputId="P1", label="1", value = 0.005, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P2", label="2", value = 0.010, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P3", label="3", value = 0.025, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P4", label="4", value = 0.050, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P5", label="5", value = NA, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P6", label="6", value = NA, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P7", label="7", value = NA, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P8", label="8", value = NA, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P9", label="9", value = NA, class="input-small",
                                      min = 0, max = 1),
                        numericInput2(inputId="P10", label="10", value = NA, class="input-small",
                                      min = 0, max = 1)#,
      )),
      column(8,
             wellPanel(tableOutput("ptable")),
             wellPanel(p("The Bonferroni correction is very extreme. It divides the unadjusted p-values by the total number of tests. The Bonferroni correction controls the family-wise error rate (FWER) under the worst-case scenario: when all the tests are independent of one another."),
                       p("The Holm correction also controls the FWER, but is slightly less extreme. It proceeds as follows: Order your m p-values from smallest to largest. Find the smallest p-value that satisfies this condition: p_k > alpha/(m + 1 - k), where k is the p-value’s index. This and all larger p-values are insignificant; all smaller p-values are significant."),
                       p("The Benjamini–Hochberg procedure controls the FDR. Like the Holm correction, you also begin by ordering m p-values. Then you find the largest p-value that satisfies: p_k <= (k/m)*alpha. This test, and all tests with smaller p-values are declared significant."),
                       p("For more information about multiple comparisons, head over to this ", a("EGAP Methods Guide.", href="https://egap.org/resource/10-things-to-know-about-multiple-comparisons/")))
      )
    )
  )
)




