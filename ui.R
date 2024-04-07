library(shiny)
library(ggplot2)
shinyUI(fluidPage(
  titlePanel("Plot MTcars Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", label = "Predictor",
                  choices = c('Number of Cylinders', 'Displacement (cu.in.)', 
                              'Gross horsepower', 'Rear axle ratio', 
                              'Weight (1000 lbs)', '1/4 mile time', 
                              'Engine','Transmission',
                              'Number of forward gears', 
                              'Number of carburetors'), 
                  selected = 'Weight (1000 lbs)'),
      selectInput("plt_type", label = "Plot Type",
                  choices = c('Scatter Plot', 'Box Plot'), 
                  selected = 'Scatter Plot'),
      selectInput("color_type", label = "Color Type",
                  choices = c('Number of Cylinders', 'Engine', 
                              'Transmission',
                              'Number of forward gears', 
                              'Number of carburetors',
                              'None'), 
                  selected = 'Transmission'),
      checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
      checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
      checkboxInput("show_title", "Show/Hide Title"),
      textInput("plt_title",label="Figure Title", value="")
    ),
    mainPanel(
      h3("Graph of MTcars Data"),
      plotOutput("plot1")
    )
  )
))