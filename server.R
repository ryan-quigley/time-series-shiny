library(shiny)
library(ggplot2)
library(lubridate)
library(tseries)

## Non-reactive code
hp.data <- read.table('data/HomePrice.txt', header = TRUE)
hp.data$date <- mdy(hp.data$date)
hp.data$month <- month(hp.data$date)
hp.data$month_label <- month(hp.data$month, label = TRUE)
series.end <- dim(hp.data)[1]

## Reactive code
server <- function(input, output, session) {
  
  user.date <- reactive({temp <- input$bubyear
    day(temp) <- 1
    format(temp, "%Y-%m-%d")
  })
  
  prebub.data <- reactive({hp.data[hp.data$date <= user.date(),]})
  bub.end <- reactive({dim(prebub.data())[1]})
  n <- reactive({series.end - bub.end()})
  
  model <- reactive({
    if (input$customize == TRUE) {
      if (length(input$diff) == 2) {
        d <- 1
        ds <- 1
      } else if (is.null(input$diff)) {
        d <- 0
        ds <- 0
      } else {
        if (input$diff == "Lag-1 (Non-seasonal)") {
          d <- 1
          ds <- 0
        } else {
          d <- 0
          ds <- 1
        }
      }
      
      ord <- c(as.numeric(input$ar), d, as.numeric(input$ma))
      sord <- c(as.numeric(input$sar), ds, as.numeric(input$sma))
      arima(prebub.data()$index, order = ord, seasonal = list(order = sord, period = 12), include.mean = FALSE, method = "ML")
    } else {
      arima(prebub.data()$index, order = c(1,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")
    }
  })
  
  output$summary <- renderPrint({
    model()
  })
  
  output$residuals <- renderPlot({
    tsdiag(model(), gof.lag = 40)
  })
  
  pi.quantile <- reactive({
      p <- (1 - (1 - as.numeric(input$predInt))/2)
      qnorm(p)
  })
  
  
  output$forecast <- renderPlot({
    my.preds.sarima <- predict(model(), n.ahead = n(), se.fit = TRUE)
    preds <- my.preds.sarima$pred
    se <- my.preds.sarima$se
    lower.bound <- preds - pi.quantile()*se
    upper.bound <- preds + pi.quantile()*se
    
    hp.final.preds.df <- data.frame(date = hp.data[bub.end():series.end,1], lb = c(hp.data[bub.end(),2],lower.bound), preds = c(hp.data[bub.end(),2],preds), ub = c(hp.data[bub.end(),2],upper.bound))
    
    ggplot(hp.final.preds.df, aes(x = date, y = preds)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = paste(100*input$predInt,"% Prediction Interval", sep ="")), alpha = 0.6) +
      geom_line(data = hp.data, aes(x=date,y=index, colour = "True Index")) +
      geom_point(data = hp.data, aes(x=date,y=index, colour = "True Index")) +
      geom_line(aes(colour = "Forecasts")) + 
      geom_point(aes(colour = "Forecasts")) +
      scale_colour_manual("", values= c("blue","black"))+
      scale_fill_manual("", values = "light blue") +
      scale_x_date(date_breaks = "1 year", date_labels = "%y") +
      labs(x = "Year", y = "S&P/Case-Shiller Home Price Index") +
      theme_bw() +
      theme(legend.key = element_blank(), legend.position = "bottom")
    })
  
  # Initialize model table
  values <- reactiveValues(data = NULL)
  values$data <- data.frame(Date = numeric(0), AR = numeric(0), MA = numeric(0), AR.Seasonal = numeric(0), MA.Seasonal = numeric(0), 
                            Lag1.Diff = numeric(0), Lag12.Diff = numeric(0), Sigma2 = numeric(0), AIC = numeric(0), LogLik = numeric(0))
  
  observeEvent(input$save, ignoreNULL = TRUE, {
    output$table1 <- renderTable({values$data})
  })
  
  observeEvent(input$save, {
    values$data[nrow(values$data) + 1,] <- c(format(input$bubyear, "%Y-%b"), input$ar, input$ma, input$sar, input$sma, model()$arma[6], model()$arma[7], round(model()$sigma2, 5), round(model()$aic, 5), round(model()$loglik, 5))
    rownames(values$data) <- seq(from = 1, to = nrow(values$data), by = 1)
    updateSelectInput(session, "id", choices = seq(from = 1, to = nrow(values$data), by = 1), selected = 1)
  })
  
  # Destroy model table
  observeEvent(input$clear, ignoreNULL = TRUE, {
    output$table1 <- renderTable({NULL})
    updateSelectInput(session, "id", choices = character(0))
  })
  
  # observeEvent(input$clear, {
  #   values$data <- data.frame(Date = numeric(0), AR = numeric(0), MA = numeric(0), AR.Seasonal = numeric(0), MA.Seasonal = numeric(0), 
  #                             Lag1.Diff = numeric(0), Lag12.Diff = numeric(0), Sigma2 = numeric(0), AIC = numeric(0), LogLik = numeric(0))
  # })
  
  ########################
  ### FIGURE OUT LOAD: error at line 131
  # Load model from id
  observeEvent(input$load, {
    id <- as.numeric(input$id)
    model.data <- values$data[id,1:7]
    
    dt <- ymd(paste(model.data$Date, "-01", sep =""))
    
    ar <- as.numeric(model.data$AR)
    ma <- as.numeric(model.data$MA)
    sar <- as.numeric(model.data$AR.Seasonal)
    sma <- as.numeric(model.data$MA.Seasonal)
    l1diff <- as.numeric(model.data$Lag1.Diff)
    l12diff <- as.numeric(model.data$Lag12.Diff)

    
    prebub.data <- hp.data[hp.data$date <= dt,]
    bub.end <- dim(prebub.data)[1]
    n <- series.end - bub.end
    
    ord <- c(ar, l1diff, ma)
    sord <- c(sar, l12diff, sma)
    model <- arima(prebub.data$index, order = ord, seasonal = list(order = sord, period = 12), include.mean = FALSE, method = "ML")
    
    output$summary <- renderPrint({
      model
    })
    
    output$residuals <- renderPlot({
      tsdiag(model, gof.lag = 40)
    })
    
    output$forecast <- renderPlot({
      my.preds.sarima <- predict(model, n.ahead = n, se.fit = TRUE)
      preds <- my.preds.sarima$pred
      se <- my.preds.sarima$se
      lower.bound <- preds - pi.quantile()*se
      upper.bound <- preds + pi.quantile()*se
      
      hp.final.preds.df <- data.frame(date = hp.data[bub.end:series.end,1], lb = c(hp.data[bub.end,2],lower.bound), preds = c(hp.data[bub.end,2],preds), ub = c(hp.data[bub.end,2],upper.bound))
      
      ggplot(hp.final.preds.df, aes(x = date, y = preds)) +
        geom_ribbon(aes(ymin = lb, ymax = ub, fill = paste(100*input$predInt,"% Prediction Interval", sep ="")), alpha = 0.6) +
        geom_line(data = hp.data, aes(x=date,y=index, colour = "True Index")) +
        geom_point(data = hp.data, aes(x=date,y=index, colour = "True Index")) +
        geom_line(aes(colour = "Forecasts")) + 
        geom_point(aes(colour = "Forecasts")) +
        scale_colour_manual("", values= c("blue","black"))+
        scale_fill_manual("", values = "light blue") +
        scale_x_date(date_breaks = "1 year", date_labels = "%y") +
        labs(x = "Year", y = "S&P/Case-Shiller Home Price Index") +
        theme_bw() +
        theme(legend.key = element_blank(), legend.position = "bottom")
    })
    
    updateSliderInput(session, "bubyear", value = dt)
    updateCheckboxGroupInput(session, "diff", selected = c("Lag-1 (Non-seasonal)", "Lag-12 (Seasonal)"))
    updateSelectInput(session, "ar", selected = ar)
    updateSelectInput(session, "ma", selected = ma)
    updateSelectInput(session, "sar", selected = sar)
    updateSelectInput(session, "sma", selected = sma)
  }) 
}
