# Shiny apps on egap.org

This repository contains the code for research tools deployed as Shiny apps (http://shiny.rstudio.com) and created by Experiments in Governance and Politics (egap.org)

## Deployment

Following https://shiny.rstudio.com/articles/shinyapps.html

```
library(rsconnect)
rsconnect::setAccountInfo(name='egap', token=<Get Token from Shiny App Dashboard>, 
secret=<Get Secret from the Shiny App Dashboard>)
library(shiny)
# From within the working directory
runApp()
# Then if it works do
deployApp()
## (Ideally, you’d start RStudio using the power-app.Rproj file to get you the correct working directory.)
```
