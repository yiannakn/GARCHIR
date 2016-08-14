# ---------------------------------------------------------------------
# Book:         
# ---------------------------------------------------------------------
# Quantlet:     Breakpoint_detection_in_zoo_variables
# ---------------------------------------------------------------------
# Description:  Detect breakpoints in a zoo time series
# ---------------------------------------------------------------------
# Usage:        
# ---------------------------------------------------------------------
# Inputs:       returns: a "zoo" variable
# ---------------------------------------------------------------------
# Output:       Plot showing breakpoints in a zoo time series
# ---------------------------------------------------------------------
# Example:      Detecting breakpoints in DAX returns and 
#               comparing them with points of interest rate changes
# ---------------------------------------------------------------------
# Data:		      int_rate.csv, Dax Data Comma.csv
# ---------------------------------------------------------------------
# Authors:      Alexander Dautel, Thorsten Disser, Binhui Hu,
#               Nicolas Yiannakou
# ---------------------------------------------------------------------

## Clear history
rm(list = ls(all = TRUE))
graphics.off()

## Intall and load packages
libraries = c("tseries", "stockPortfolio", "zoo", "changepoint")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## Get Data and Returns and Returns demeaned
SPL     = read.csv("DAX Data Comma.csv")
SPL.zoo = zoo(SPL[, -1], 
              order.by = as.Date(strptime(as.character(SPL[, 1]), "%Y-%m-%d")))
prices  = SPL.zoo$Adj.Close
returns = diff(log(prices))
mean       = mean(returns)
returns.dm = returns - mean

int.rate      = read.csv("int_rate.csv", header = T, sep = ",")
int.rate.zoo  = zoo(int.rate[, -1], 
                    order.by = as.Date(strptime(as.character(int.rate[, 1]), 
                                                "%Y-%m-%d")))

## Breakpoint Testing
breakpoints     = cpt.var(returns, penalty = "MBIC", pen.value = 0.05, 
                          know.mean = FALSE, method = "PELT")
breakpoints.vec = breakpoints@cpts
breakpoints.zoo = index(returns[breakpoints.vec])

## Plot the returns and breakpoints
par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(returns.dm, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Returns", xlim = NULL, lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = breakpoints.zoo[-6], col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Demeaned Returns and Breakpoints", cex.main = 1)
