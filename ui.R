library(shiny)

ui <- fluidPage(
  
  titlePanel("Forecasting Pre-bubble Trends"),
  helpText("S&P/Case-Shiller Home Price Index with January 2000 as baseline"),
  tabsetPanel(type = "tabs",
              tabPanel("Forecasts", plotOutput(outputId = "forecast")),
              tabPanel("Models",
                       verbatimTextOutput(outputId = "summary"), 
                       tableOutput(outputId = "table1")
                       ),
              tabPanel("Residual Diagnostics", plotOutput(outputId = "residuals"))
  ),
  hr(),
  fluidRow(
      column(4,
        sliderInput(
          inputId = "bubyear", label = "Housing bubble start date:",
          min = as.Date("1997-01-01"), value = as.Date("1998-01-15"), max = as.Date("2001-12-01"),
          timeFormat = "%b %Y"),
        sliderInput(inputId = "predInt", label = "Prediction Interval", 
                    min = 0, value = 0.95, max = 0.99, step = 0.01),
        checkboxInput("customize", "Customize Model")
      ),
      column(4,
        conditionalPanel(
           condition = "input.customize == true",
           checkboxGroupInput("diff", "Differencing", c("Lag-1 (Non-seasonal)", "Lag-12 (Seasonal)"), 
                              selected = c("Lag-1 (Non-seasonal)", "Lag-12 (Seasonal)"), inline = TRUE),
           selectInput("ar", "Auto Regressive Component", c(0,1,2,3), selected = 1),
           selectInput("ma", "Moving Average Component", c(0,1,2,3), selected = 0),
           selectInput("sar", "Seasonal Auto Regressive Component", c(0,1,2,3), selected = 0),
           selectInput("sma", "Seasonal Moving Average Component", c(0,1,2,3), selected = 1),
           actionButton("save", "Save Model"),
           actionButton("clear", "Clear All Models")
        )
      ),
      column(4,
         conditionalPanel(
           condition = "input.id > 0",
           selectInput("id", "Load model from ID", character(0)),
           actionButton("load", "Load Model")
        )
      )
    )
  )
              