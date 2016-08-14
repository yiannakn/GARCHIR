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
# Authors:      Alexander Dautel, Thorsten Disser, Binhui Hu,
#               Nicolas Yiannakou
# ---------------------------------------------------------------------

## Clear history
rm(list = ls(all = TRUE))
graphics.off()

## Install and load packages
libraries = c("changepoint", "zoo")
lapply(libraries, function(x) if (!(x %in% installed.packages( ))) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## Determine the breakpoints
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