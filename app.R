library(shiny)
library(quantmod)
source("global.R")

server <- function(input, output) {
  # get the Stock Data
  sSymbol <- reactive({
    tryCatch({
      suppressWarnings(getSymbols(input$select, auto.assign = FALSE))
    }, error = function(err) {
      return(NULL)
    })
  })
  
  # get the Date Range
  data_range  <- reactive({  
    paste(input$dates, collapse = "::") })
  
  # get the Data Stock Yrange
  data_yrange <- reactive({  
    # calculate subset, mean, std
    symobolmath <- symbol_data(sSymbol(), data_range())
    return(c(min(Cl(symobolmath$subdata)) - input$yrange*symobolmath$sd, 
             max(Hi(symobolmath$subdata)) + input$yrange*symobolmath$sd))
  })
  
  # get all the TA list
  TAList <- reactive({
    taStr <- list("addBBands()")
    if (!is.null(input$ta)) { for (ta in (input$ta)) { taStr <- append(taStr, list(ta)) } }
    return(taStr)
  })# TAList
    
  # plot chartseries
  output$distPlot <- renderPlot({
    # check symbol is exist
    #if(!is.null(sSymbol())) {
      # plot stock chartSeries
     chartSeries(sSymbol(), 
                    name = input$select, 
                  yrange = data_yrange(), 
                  subset = data_range(),  
                      TA = TAList(),
                   theme = "white", up.col = "red", dn.col = "green")
    # get Tech analysis data
    symbol_m   <- symbol_data(sSymbol(), data_range())
    TI         <- TechAnalysis(sSymbol())
    # calculate threshold
    MACD_trade <- TechIndex_trade(TI$MACD, uplimit = input$TI_MACD[2], lowlimit = input$TI_MACD[1])
    KD_trade   <- TechIndex_trade(TI$KD  , uplimit = input$TI_KD[2]  , lowlimit = input$TI_KD[1]  )
    RSI_trade  <- TechIndex_trade(TI$RSI , uplimit = input$TI_RSI[2] , lowlimit = input$TI_RSI[1] )
    MTM_trade  <- TechIndex_trade(TI$MTM , uplimit = input$TI_MTM[2] , lowlimit = input$TI_MTM[1] )
    WMA_trade  <- TechIndex_trade(TI$WMA , uplimit = input$TI_WMA[2] , lowlimit = input$TI_WMA[1] )
    
    MACD_b     <- Cl(sSymbol())[MACD_trade$buy_peak_t ] - (symbol_m$sd+Shift_indicator$MACD)
    MACD_s     <- Hi(sSymbol())[MACD_trade$sell_peak_t] + (symbol_m$sd+Shift_indicator$MACD)
    KD_b       <- Cl(sSymbol())[KD_trade$buy_limit_t  ] - (symbol_m$sd+Shift_indicator$KD)
    KD_s       <- Hi(sSymbol())[KD_trade$sell_limit_t ] + (symbol_m$sd+Shift_indicator$KD)
    RSI_b      <- Cl(sSymbol())[RSI_trade$buy_limit_t ] - (symbol_m$sd+Shift_indicator$RSI)
    RSI_s      <- Hi(sSymbol())[RSI_trade$sell_limit_t] + (symbol_m$sd+Shift_indicator$RSI)
    MTM_b      <- Cl(sSymbol())[MTM_trade$buy_peak_t  ] - (symbol_m$sd+Shift_indicator$MTM)
    MTM_s      <- Hi(sSymbol())[MTM_trade$sell_peak_t ] + (symbol_m$sd+Shift_indicator$MTM)
    WMA_b      <- Cl(sSymbol())[WMA_trade$buy_peak_t  ] - (symbol_m$sd+Shift_indicator$WMA)
    WMA_s      <- Hi(sSymbol())[WMA_trade$sell_peak_t ] + (symbol_m$sd+Shift_indicator$WMA)
    
    # use plot to plot the addTA function
    plot(addTA(MACD_b, pch = 2, type = "p", col = "red"  , on = 1, legend = NULL))
    plot(addTA(MACD_s, pch = 6, type = "p", col = "red"  , on = 1, legend = NULL))
    plot(addTA(KD_b  , pch = 1, type = "p", col = "blue" , on = 1, legend = NULL))
    plot(addTA(KD_s  , pch = 1, type = "p", col = "blue" , on = 1, legend = NULL))
    plot(addTA(RSI_b , pch = 5, type = "p", col = "black", on = 1, legend = NULL))
    plot(addTA(RSI_s , pch = 5, type = "p", col = "black", on = 1, legend = NULL))
    plot(addTA(MTM_b , pch = 8, type = "p", col = "green", on = 1, legend = NULL))
    plot(addTA(MTM_s , pch = 8, type = "p", col = "green", on = 1, legend = NULL))
 #   plot(addTA(TI$MTM))
    plot(addTA(WMA_b , pch = 10, type = "p", col = "purple", on = 1, legend = NULL))
    plot(addTA(WMA_s , pch = 10, type = "p", col = "purple", on = 1, legend = NULL))
 #   plot(addTA(WMA(Cl(sSymbol()), n = 5)))
    plot(addTA(Cl(sSymbol()),on = 1, col = "red"))
 #   plot(addTA(TI$WMA))
    # }
 })# output$distPlot
  
  
  # sliderValues <- reactive({
  #   # Compose data frame
  #   data.frame(
  #     Name  = c("MACD", "KD", "RSI"),
  #     Range = as.character(c(
  #       paste(input$TI_MACD, collapse = ' '),
  #       paste(input$TI_KD  , collapse = ' '),
  #       paste(input$TI_RSI , collapse = ' ')))
  #   )
  # })  
  # sliderValues2 <- reactive({
  #   symbol_m  <- symbol_data(sSymbol(), data_range())
  #   # Compose data frame
  #   dos <- data.frame(
  #         Name  = c("Mean", "Sd"),
  #         Value = c(symbol_m$mean, symbol_m$sd)
  #       )
  #   return(dos)
  # })  
  
  # output$values <- renderTable({
  #   sliderValues()
  # })
  # output$values2 <- renderTable({
  #   sliderValues2()
  # })
}

ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("R quantmod Program"),
             
  # Sidebar with inputs for quantmod graphing capabilities
  sidebarLayout(
    sidebarPanel(height="650px",
      h4("Check BUY and SELL timing"),
      
      
      selectInput("select", label = h6("Select Stock:"), choices  = StockList),
      
      dateRangeInput("dates", label = h6("Date range"), start = "2016-09-01", end = as.character(Sys.Date())),
      
      # scale stock range
      sliderInput("yrange", label = h6("Scale Stock range "),
                  min = 1, max = 5, step = 1, value = 3),
      
      # Get the Thsreshold
      sliderInput("TI_MACD", label = h6("MACD Tech. Indicator Threshold:"),
                  min = -3, max = 3, step = 0.2, value = c(0,0)),
      sliderInput("TI_KD", label = h6("KD Tech. Indicator Threshold:"),
                  min = 0, max = 100, step = 5, value = c(20,80)),
      sliderInput("TI_RSI",label = h6("RSI Tech. Indicator Threshold:"),
                  min = 0, max = 100, step = 5, value = c(30,70)),
      sliderInput("TI_MTM", label = h6("MTM Tech. Indicator Threshold:"),
                  min = -3, max = 3, step = 0.2, value = c(0,0)),
      sliderInput("TI_WMA", label = h6("MTM Tech. Indicator Threshold:"),
                  min = -3, max = 3, step = 0.2, value = c(0,0)),
      hr(),
    #  submitButton("Update View"),
      flowLayout(
        checkboxGroupInput("ta", "Add Optional Technical Analysis Overlays:",
                           c(
                             # "Directional Movement Index" = "addADX()",
                             # "Average True Range" = "addATR()",
                             # "Bollenger Bands" = "addBBands()",
                             # "Commodity Channel Index" = "addCCI()",
                             # "Chaiken Money Flow" = "addCMF()",
                             # "Chande Momentum Oscillator" = "addCMO()",
                             # "Contract Expiration Bars" = "addExpiry()",
                             # "De-trended Price Oscillator" = "addDPO()",
                             # "Simple Moving Average" = "addSMA()",
                             # "Expotential Moving Average" = "addEMA()",
                             "Weighted Moving Average" = "addWMA(col=1)",
                             # "Double Expotential Moving Average" = "addDEMA()",
                             # "Expotential Volume Weighted Moving Average" = "addEVWMA()",
                             # "ZLEMA" = "addZLEMA()",
                             "Moving Average Convergence Divergence" = "addMACD()",
                             "Price Envelope" = "addEnvelope()",
                             "Relative Strength Index" = "addRSI(n = 5)"
                             # "Parabolic Stop and Reversal Indicator" = "addSAR()",
                             # "Rate of Change" = "addROC()",
                             # "Stochastic Momemtum Indicator" = "addSMI()"
                             ),
                           selected = c("addWMA(col=1)","addMACD()","addEnvelope()", "addRSI(n = 5)")
                           )
        )# flowLayout
      ),# sidebarPane
      # Show the demonstration plot
      mainPanel(
        plotOutput("distPlot", height="650px")
        # ,tableOutput("values")
        # ,tableOutput("values2")
        # ,tableOutput("dispPrint")
        
      )# mainPanel
    )#sidebarLayout
))
shinyApp(ui = ui, server = server)
