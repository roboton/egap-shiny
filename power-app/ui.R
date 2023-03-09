shinyUI(fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  headerPanel("Power Calculator"),
  fluidRow(
    column(12,
           wellPanel(
             helpText("This calculator can help you understand the power of a few simple experimental designs to detect average treatment effects. You can choose between a standard design in which individuals are randomly assigned to treatment or control and a clustered design, in which groups of individuals are assigned to treatment and control together. For other, more complex designs, for example using block or stratified assignment, or more complex causal quantities such as complier average causal effects (also known as local average treatment effects), we suggest you see the DeclareDesign Wizard at https://eos.wzb.eu/ipi/DDWizard/ "),
           checkboxInput(inputId = "clustered",label = "Clustered Design?", value = FALSE),
           checkboxInput(inputId = "binary",label = "Binary Dependent Variable?", value = FALSE)
           )),
    column(4,
           wellPanel(
  conditionalPanel(
    condition = "input.clustered == false & input.binary == false",
    selectInput("alpha", "Significance Level",c("Alpha = 0.01", "Alpha = 0.05", "Alpha = 0.10"),selected="Alpha = 0.05"),
    numericInput("tau", "Treatment Effect Size", value = 5, min = 0, max = 40),
    numericInput("sigma", "Standard Deviation of Outcome Variable", value = 10, min = 0, max = 30),
    sliderInput("target", "Power Target", value = .8, min = 0, max = 1),
    numericInput("maxn", "Maximum Number of Subjects", value = 2000, min = 0, max = 10000000)
  ),
  conditionalPanel(
    condition = "input.clustered == false & input.binary == true",
    selectInput("alpha_b", "Significance Level",c("Alpha = 0.01", "Alpha = 0.05", "Alpha = 0.10"),selected="Alpha = 0.05"),
    numericInput("p0_b", "Proportion (DV = 1) in Control Group", value = .5, min = 0, max = 1),
    numericInput("p1_b", "Proportion (DV = 1) in Treatment Group", value = .65, min = 0, max = 1),
    sliderInput("target_b", "Power Target", value = .8, min = 0, max = 1),
    numericInput("maxn_b", "Maximum Number of Subjects", value = 2000, min = 0, max = 10000000)
  ),
  conditionalPanel(
  condition = "input.clustered == true & input.binary == false",
      selectInput("alpha_c", "Significance Level",c("Alpha = 0.01", "Alpha = 0.05", "Alpha = 0.10"),selected="Alpha = 0.05"),
      numericInput("tau_c", "Treatment Effect Size", value = 5, min = 0, max = 40),
      numericInput("sigma_c", "Standard Deviation of Outcome Variable", value = 10, min = 0, max = 30),
  sliderInput("ICC_c", "Intra-cluster Correlation", value = .5, min = 0, max = 1),
      numericInput("n_clus_per_arm_c", "Number of Clusters per Arm", value = 40, min = 0, max = 200),
      sliderInput("target_c", "Power Target", value = .8, min = 0, max = 1),
      numericInput("maxn_c", "Maximum Number of Subjects", value = 2000, min = 0, max = 10000000)
    ),
  conditionalPanel(
    condition = "input.clustered == true & input.binary == true",
    selectInput("alpha_bc", "Significance Level",c("Alpha = 0.01", "Alpha = 0.05", "Alpha = 0.10"),selected="Alpha = 0.05"),
    numericInput("p0_bc", "Proportion (DV = 1) in Control Group", value = .5, min = 0, max = 1),
    numericInput("p1_bc", "Proportion (DV = 1) in Treatment Group", value = .65, min = 0, max = 1),
    sliderInput("ICC_bc", "Intra-cluster Correlation", value = .5, min = 0, max = 1),
    numericInput("n_clus_per_arm_bc", "Number of Clusters per Arm", value = 40, min = 0, max = 200),
    sliderInput("target_bc", "Power Target", value = .8, min = 0, max = 1),
    numericInput("maxn_bc", "Maximum Number of Subjects", value = 2000, min = 0, max = 10000000)
  )
  )),
    column(8,
           wellPanel(
      plotOutput("powerplot")
           )
    )),
  column(12,
wellPanel(
         htmlOutput("nrequired")
))
)
)



