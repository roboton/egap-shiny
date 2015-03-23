
numericInput2<-function (inputId, label, value = 1,...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = value,...))
}


shinyUI(fluidPage(
  titlePanel("An Exact Fishy Test"),
  sidebarLayout(
    sidebarPanel( "Enter ten random numbers between 01 and 100", br(),
        
                  numericInput2(inputId="a1", label="1", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a2", label="2", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a3", label="3", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a4", label="4", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a5", label="5", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a6", label="6", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a7", label="7", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a8", label="8", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a9", label="9", value = NA, class="input-small",
                                min = 1, max = 100),
                  numericInput2(inputId="a10", label="10", value = NA, class="input-small",
		                  min = 1, max = 100)#,
				  
          
                  ),
    mainPanel( #"the", em("payoff"), "matrix"
    #            , tableOutput("text1")
    #               , textOutput("text2")
    #, 
#    textOutput("g")
    htmlOutput("htext")

               )
  )
  
  

  
))




