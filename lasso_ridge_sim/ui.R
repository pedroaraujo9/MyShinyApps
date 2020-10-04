#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Lasso and Ridge Simulation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput(
                inputId = "p", 
                label = "Number of covariates:", 
                min = 1, 
                max = 3000,
                value = 100
            ),
            numericInput(
                inputId = "p1",
                label = "Number of covariates with effects different from 0",
                min = 0,
                max = 3000,
                value = 10
            ),
            numericInput(
                inputId = "n",
                label = "Number of observations",
                min = 100,
                max = 10000,
                value = 1000
            ),
            numericInput(
                inputId = "sigma",
                label = "Sigma",
                min = 0.001,
                max = 1000,
                value = 3
            ),
            numericInput(
                inputId = "lambda_max",
                label = "Max lambda",
                min = 0,
                max = 100,
                value = 3
            ),
            submitButton("Simulate")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("lasso_lambda"),
            plotOutput("lasso_nvar")
        )
    )
))
