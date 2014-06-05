library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Heckman model compared with OLS"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    sliderInput("n", "Sample size (exponentiated):",
                min=4, max=8, value=5, step=1),
    sliderInput("cr1w", "Correlation between x1 and w:",
                min=0, max=.9, value=0, step=.1),
    sliderInput("cr1u", "Correlation between x1 and u:",
                min=0, max=.9, value=0, step=.1),
    sliderInput("sel.cri", "selection proportion (low, medium, high):",
                min=-.5, max=.5, value=0, step=.5),
    selectInput("model", "selection model:",
                list("select ~ x1",
                     "select ~ x1 + w"
                     ))),
      mainPanel(plotOutput("plot1")
                )
      ))
