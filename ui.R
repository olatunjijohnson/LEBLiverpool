
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shiny)
library(leaflet)
library(dplyr)

shinyUI(fluidPage(

  titlePanel(p("Spatially continuous prediction of Life Expectancy at Birth (LEB)", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      #fileInput(inputId = "SDALGCPoutput", label = "Upload output from SDALGCPPred:",
       #         multiple = TRUE),
      conditionalPanel(condition = "input.tabselected==1",
                       radioButtons(inputId = "resolution1", label = "Life Expectancy at Birth (LEB):",
                  choices = c("Female"= "female", "Male" = "male"))),
      conditionalPanel(condition = "input.tabselected==2",
                       radioButtons(inputId = "resolution2", label = "Life Expectancy at Birth (LEB)",
                                   choices = c("Female"="female", "Male"= "male")),
                      sliderInput(inputId= "thresholds", "LEB threshold:", value = 70,
                                                    min = 0, max = 100, step = 1)),
      conditionalPanel(condition = "input.tabselected==3",
                       radioButtons(inputId = "resolution3", label = "Life Expectancy at Birth (LEB)",
                                    choices = c("Female"="female", "Male"= "male")),
                       sliderInput(inputId= "quantile", "Quantile level:", value = 0.5,
                                   min = 0, max = 1, step = 0.1))
      

      ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Mean LEB", value=1, conditionalPanel(condition = "input.resolution1=='female'",leafletOutput(outputId = "dmean_risk")), 
                           conditionalPanel(condition = "input.resolution1=='male'",leafletOutput(outputId = "cmean_risk"))),
                  tabPanel("Non-exceedance", value=2, conditionalPanel(condition = "input.resolution2=='female'", leafletOutput(outputId = "dexceed")),
                           conditionalPanel(condition = "input.resolution2=='male'", leafletOutput(outputId = "cexceed"))),
                  tabPanel("Quantile", value=3, conditionalPanel(condition = "input.resolution3=='female'", leafletOutput(outputId = "dquantile")),
                           conditionalPanel(condition = "input.resolution3=='male'", leafletOutput(outputId = "cquantile"))),
                  id="tabselected"
      )
    )
  )
))


