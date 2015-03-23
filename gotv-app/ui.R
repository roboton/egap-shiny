shinyUI(
  navbarPage("Analysis of Field Experiments",
             tabPanel("Basic",
  fluidPage(theme = "bootstrap.css",    
    fluidRow(      
      column(12,
             wellPanel(
               p("The statistical software on this page allows researchers to analyze experiments in which the outcome variable is dichotomous (e.g., voting versus nonvoting). The statistical analysis is designed to accommodate situations in which only some of the subjects assigned to the treatment group actually receive the treatment, as well as situations in which some of the control group is treated inadvertently."),
               p("You supply six numbers: the number of people that you (1) assigned to the treatment group, (2) assigned to the control group, (3) successfully treated in the treatment group, (4) inadvertently treated in the control group, (5) found to have voted in the treatment group, and (6) found to have voted in the control group."),
               p("Melissa Michelson's", a("door-to-door canvassing experiment", href="http://class.csueastbay.edu/faculty/mmichelson/Dos%20Palos.pdf"), "in Dos Palos, California provides a nice illustration. Prior to the 2001 election, she assigned 466 people with Latino surnames to the treatment group and 297 to the control group. 342 of the people in the treatment group were successfully contacted. No one in the control group was contacted. In the treatment group, 86 people voted, whereas 41 people voted in the control group. The six numbers are therefore: 466, 297, 342, 0, 86, 41.")
               )
             ),
      column(4,
             wellPanel(
               numericInput("n_Z1", label = "N in the treatment group", value = 466),
               numericInput("n_Z0", label = "N in the control group", value = 297),
               numericInput("n_Z1_contacted", label = "N in the treatment group who are actually treated", value = 342),
               numericInput("n_Z0_contacted", label = "N in the control group who are inadvertently treated", value = 0),
               numericInput("n_Z1_voted", label = "N who voted in the treatment group", value = 86),
               numericInput("n_Z0_voted", label = "N who voted in the control group", value = 41)
      ),
      wellPanel("Summary of Input",
        tableOutput("summary_table")  
      )
      ),
      column(8,
             wellPanel(
             plotOutput("out_plot"),
             p("Error bars indicate 95% confidence intervals.")
             ),
             wellPanel(
               "Statistical Results",
             tableOutput("statistical_results")  
             )
      ),
      column(12,
             wellPanel("Interpretation of Results",
               p("The intent-to-treat effect is the effect of assigning subjects to the treatment group, irrespective of whether they are actually treated. The intent-to-treat estimate is expressed in terms of percentage-points. An estimate of '5' implies that assignment to the treatment group increases one's probability of voting by 5 percentage-points."),
               p("The standard error of this estimate is the standard deviation of the sampling distribution of this estimate; the larger the standard error, the more uncertainty there is about the value of the intent-to-treat parameter."),
               p("The complier average causal effect (CACE) is the effect of the treatment on 'compliers,' i.e., those who are actually treatable if assigned to the treatment group. This quantity is estimated by dividing the intent-to-treat effect by the contact rate. This estimator is equivalent to instrumental variables or two-stage least squares regression. The standard error expresses the uncertainty of this estimate."),
               p("The 95% confidence interval is a range of values that has a 95% chance of bracketing the true treatment parameter. In other words, if the experiment were repeated a large number of times, 95% of the intervals would encompass the true parameter."),
               p("The one-tailed p-value refers to the probability of obtaining an estimated intent-to-treat effect at least as large as what was observed in this sample, if the true effect were zero. When this level is below .05, the estimates are conventionally referred to as 'statistically significant.'"),
               p("This significance test is calculated based on a large-sample approximation. If you are working with small samples and want to conduct an exact significance test, see Gerber and Green (2012), chapter 3."),
               p("The statistical power of an experiment is its probability of rejecting the null hypothesis using a one-sided test, given that the true CACE were identical to the CACE estimated from this sample. Suppose, for example, that the estimated CACE were 10 and the power of the study is 50%. For this effect size, the experiment has a 50% chance of rejecting the null hypothesis at the 0.05 level using a one-tailed test.")
               ),
             wellPanel("Credits",
                       p(a("Donald Green", href="https://sites.google.com/site/donaldpgreen/")," and ",a("Alan Gerber", href="http://pantheon.yale.edu/~agerber/"), "."),
                       p("Shiny app designed by ", a("Alexander Coppock", href="https://alexandercoppock.wordpress.com/"),".")
                       ),
             wellPanel("Further Reading",
                       p("Angrist, Joshua D., Imbens, Guido W. and Donald B. Rubin. 1996. Identification of Causal Effects Using Instrumental Variables. Journal of the American Statistical Association 91(June): 444-455."),
                       p("Gerber, Alan S., and Donald P. Green.", a("Field Experiments: Design, Analysis, and Interpretation.",href="http://www.amazon.com/Field-Experiments-Design-Analysis-Interpretation/dp/0393979954"), "WW Norton, 2012."),
                       p("Michelson, Melissa. 2003. Getting Out the Latino Vote: How Door-to-Door Canvassing Influences Voter Turnout in Rural Central California. Political Behavior 25(3): 247-263")
         )
       )
    )
  )
             ),
  tabPanel("Upload a Dataset",
           fluidPage(theme = "bootstrap.css",    
       fluidRow(
         column(12,
                wellPanel(
                  p("The statistical software on this page allows researchers to analyze experiments in which the outcome is numeric. The statistical analysis is designed to accommodate situations in which only some of the subjects assigned to the treatment group actually receive the treatment, as well as situations in which some of the control group is treated inadvertently."),
                  p("You upload a dataset in .csv form with three columns: the outcome variable, the treatment received, and the treatment assignment. The outcome variable must be numeric.  The treatment recieved and the treatment assigment variables can only include 0's and 1's. For an example dataset in the correct format, you can download a .csv of the Michelson GOTV experiment ", a("here.", href="http://www.columbia.edu/~ac3242/michelson.csv", target="blank")),
                  p("A tip: if your experiment did not encounter any noncompliance just select the assignment variable in both 'Treatment Assigned' and 'Treatment Recieved'.")
                )
         ),
           column(4,
                  wellPanel("Upload a .CSV",
                            fileInput('file1', 'Choose CSV File',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                            uiOutput("outcome"),
                            uiOutput("contact"),
                            uiOutput("treatment")
            ),
            wellPanel("Summary of Input",
                      tableOutput("summary_table_2")  
            )
            ),
          column(8,
                 wellPanel(
                   plotOutput("out_plot_2"),
                   p("Error bars indicate 95% confidence intervals.")
                 ),
                 wellPanel(
                   "Statistical Results",
                   tableOutput("statistical_results_2")  
                 )
          ),
         column(12,
                wellPanel("Interpretation of Results",
                          p("The intent-to-treat effect is the effect of assigning subjects to the treatment group, irrespective of whether they are actually treated."),
                          p("The standard error of this estimate is the standard deviation of the sampling distribution of this estimate; the larger the standard error, the more uncertainty there is about the value of the intent-to-treat parameter."),
                          p("The complier average causal effect (CACE) is the effect of the treatment on 'compliers,' i.e., those who are actually treatable if assigned to the treatment group. This quantity is estimated by dividing the intent-to-treat effect by the contact rate. This estimator is equivalent to instrumental variables or two-stage least squares regression. The standard error expresses the uncertainty of this estimate."),
                          p("The 95% confidence interval is a range of values that has a 95% chance of bracketing the true treatment parameter. In other words, if the experiment were repeated a large number of times, 95% of the intervals would encompass the true parameter."),
                          p("The two-tailed p-value refers to the probability of obtaining an estimated intent-to-treat effect at least as large in absolute value as what was observed in this sample, if the true effect were zero. When this level is below .05, the estimates are conventionally referred to as 'statistically significant.'"),
                          p("This significance test is calculated based on a large-sample approximation. If you are working with small samples and want to conduct an exact significance test, see Gerber and Green (2012), chapter 3."),
                          p("The statistical power of an experiment is its probability of rejecting the null hypothesis using a two-sided test, given that the true CACE were identical to the CACE estimated from this sample. Suppose, for example, that the estimated CACE were 10 and the power of the study is 50%. For this effect size, the experiment has a 50% chance of rejecting the null hypothesis at the 0.05 level using a two-tailed test."),
                          p("The average outcome among untreated compliers helps aid interpretation of the results as it may be different from the overall average in the control group.")
                ),
                wellPanel("Credits",
                          p(a("Donald Green", href="https://sites.google.com/site/donaldpgreen/")," and ",a("Alan Gerber", href="http://pantheon.yale.edu/~agerber/"), "."),
                          p("Shiny app designed by ", a("Alexander Coppock", href="https://alexandercoppock.wordpress.com/"),".")
                ),
                wellPanel("Further Reading",
                          p("Angrist, Joshua D., Imbens, Guido W. and Donald B. Rubin. 1996. Identification of Causal Effects Using Instrumental Variables. Journal of the American Statistical Association 91(June): 444-455."),
                          p("Gerber, Alan S., and Donald P. Green.", a("Field Experiments: Design, Analysis, and Interpretation.",href="http://www.amazon.com/Field-Experiments-Design-Analysis-Interpretation/dp/0393979954"), "WW Norton, 2012."),
                          p("Michelson, Melissa. 2003. Getting Out the Latino Vote: How Door-to-Door Canvassing Influences Voter Turnout in Rural Central California. Political Behavior 25(3): 247-263"),
                          p("Aronow, Peter A., and Donald P. Green. 2013. Sharp Bounds for Complier Average Potential Outcomes in Experiments with Noncompliance and Incomplete Reporting. Statistics and Probability Letters 83: 677-679.")
                )
         )
        )
      )
    )
  )
)


