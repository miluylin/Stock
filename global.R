
library("quantmod")
library("data.table")
# functions
maximums <- function(x) which(x - shift(x, 1, type='lag') > 0  & x - shift(x, 1, type='lead') > 0)
minimums <- function(x) which(x - shift(x, 1, type='lag') < 0  & x - shift(x, 1, type='lead') < 0)
MTM      <- function(x, n = 2) {return((x - shift(x, n, type='lag'))/n)} 
TechIndex_trade <- function(x, uplimit = 0, lowlimit = 0) {
  # NA value set to 0
  x[is.na(x)] <- 0
  if(max(x) < uplimit ) {  uplimit  <- max(x) }
  if(min(x) > lowlimit) {  lowlimit <- min(x) }
  # find local Max/min in MACD
  position_local_Max <- maximums(x)
  position_local_Min <- minimums(x)
  
  # set sell flag is the local maximum
  sell_pos <- x
  sell_pos[ position_local_Max] <- 1
  sell_pos[-position_local_Max] <- 0
  
  # set buy flag is the local minimum
  buy_pos <- x
  buy_pos[ position_local_Min] <- 1
  buy_pos[-position_local_Min] <- 0
  
  # only if x > uplimit => pos_uplimit => 1 
  pos_uplimit <- x
  pos_uplimit[x >  uplimit] <- 1
  pos_uplimit[x <= uplimit] <- 0
  
  # only if x < lowlimit => neg_lowlimit => 1 
  neg_lowlimit <- x
  neg_lowlimit[x <  lowlimit] <- 1
  neg_lowlimit[x >= lowlimit] <- 0
  
  buy_peak_t   <- as.logical(buy_pos & neg_lowlimit)
  sell_peak_t  <- as.logical(sell_pos & pos_uplimit)
  buy_limit_t  <- as.logical(neg_lowlimit)
  sell_limit_t <- as.logical(pos_uplimit)
  return(data.frame(buy_peak_t, sell_peak_t, buy_limit_t, sell_limit_t))
}
TechAnalysis    <- function(x) {
  c_MACD <- MACD(Cl(x), nFast = 12, nSlow = 26, nSig = 9)
  c_KD   <- 100*stoch(HLC(x), nFastK = 9, nFastD = 3, nSlowD = 3,
                      maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
  c_RSI  <- RSI(Cl(x), n = 5)
  c_MTM  <- MTM(runMean(Cl(x), n = 5), n = 2 )
  c_WMA  <- MTM(WMA(Cl(x), n = 5), n =2)
  curve_MACD <- c_MACD[, "macd"] - c_MACD[, "signal"]
  curve_KD   <- c_KD[, "fastK"]
  curve_RSI  <- c_RSI
  curve_MTM  <- c_MTM
  curve_WMA  <- c_WMA
  
  return(list(MACD = curve_MACD, 
              KD   = curve_KD, 
              RSI  = curve_RSI, 
              MTM  = curve_MTM,
              WMA  = curve_WMA))
}
symbol_data     <- function(x, range) {
  mean_price <- mean(Cl(x[range]), na.rm = TRUE)
  sd_price   <- sd(Cl(x[range])  , na.rm = TRUE)
  return(list(subdata = x[range], mean = mean_price, sd = sd_price))
}

# stock list observation
StockList  <<- c("2377.TW", "2439.TW", "2498.TW" )
Shift_MACD <<- 1
Shift_KD   <<- 2
Shift_RSI  <<- 3
Shift_MTM  <<- 4
Shift_WMA  <<- 5
#------------- 
# # Plot buy and sell point
# # MACD
# addTA(envlop_MACD, on = 2, col = "red", legend=NULL)
# addTA(twStock[MACD_trade$buy_peak_t , "Low" ] - 3, pch = 2, type = "p", col = "red", on = 1, legend = NULL)
# addTA(twStock[MACD_trade$sell_peak_t, "High"] + 3, pch = 6, type = "p", col = "red", on = 1, legend = NULL)
# #addTA(6*(envlop_MACD)+mean_prise, on = 1, col = "blue", legend = NULL )
# 
# # KD
# # plot 4. KD
# addTA(c_KD[,"fastK"], col = "red", legend=NULL )
# addTA(c_KD[,"slowD"], on = 4, col = "blue", legend=NULL)
# addTA(twStock[KD_trade$buy_limit_t , "Low" ] - 4, pch = 1, type = "p", col = "blue", on = 1, legend = NULL)
# addTA(twStock[KD_trade$sell_limit_t, "High"] + 4, pch = 1, type = "p", col = "blue", on = 1, legend = NULL)
# 
# # RSI
# addTA(twStock[RSI_trade$buy_limit_t , "Low" ] - 5, pch = 5, type = "p", col = "black", on = 1, legend = NULL)
# addTA(twStock[RSI_trade$sell_limit_t, "High"] + 5, pch = 5, type = "p", col = "black", on = 1, legend = NULL)
#-------------