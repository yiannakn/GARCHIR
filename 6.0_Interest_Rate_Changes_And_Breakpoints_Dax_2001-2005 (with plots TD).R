##############################
##  Authors: Alexander Dautel, Thorsten Disser, Binhui Hu, Nicolas Yiannakou
##  Date: 24.06.2016
##  Project: SPL project
##  Script: Interest_Rate_Changes_And_Breakpoints_Dax_2001-2005.R
##  Input Data: Dax Data Comma.csv (Daily Dax Data from 2001-2005)
##############################

# Workspace and Working Directory -----------------------------------------

rm(list = ls())
getwd()

folder.wd = "C:/Users/howtodowtle/Dropbox/studium/stat.de/6_SS_2016/HU_Wiwi_SPL/Code self"
setwd(folder.wd)
getwd()

# Function Definitions ----------------------------------------------------

IPak = function(pkg){
  # Function will install new packages and require already installed ones
  #
  # Args:
  #  pkg: A character string containing the packages to be used in the
  #       project.
  #
  # Returns:
  #  None, apart from the function calls for install.packages() and/or
  #  require(). 
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

BIC = function(n.like, k, n) {
  # Calculates and returns the Bayesian Information Criterion
  #
  # Args:
  #  n.like: negative log-likelihood, as for example produced 
  #          by the 'garch' command from the 'tseries' package
  #  k:      number of free parameters to be estimated
  #  n:      number of data points
  2 * n.like + k * log(n)
}

HQIC = function(n.like, k, n) {
  # Calculates and returns the Hannan-Quinn Information Criterion
  #
  # Args:
  #  n.like: negative log-likelihood, as for example produced 
  #          by the 'garch' command from the 'tseries' package
  #  k:      number of free parameters to be estimated
  #  n:      number of data points
  2 * n.like + 2 * k * log(log(n))
}

# Install and Load Packages -----------------------------------------------

packages = c("tseries", "nlme", "zoo", "ecp", "bfast", 
             "stockPortfolio", "changepoint", "lint")
IPak(packages)

# Read data ---------------------------------------------------------------

SPL     = read.csv("DAX Data Comma.csv")
SPL.zoo = zoo(SPL[, -1], 
              order.by = as.Date(strptime(as.character(SPL[, 1]), "%Y-%m-%d")))
prices  = SPL.zoo$Adj.Close
n       = length(prices)
returns = diff(log(prices))

# Descriptives ------------------------------------------------------------

mean       = mean(returns)
returns.dm = returns - mean
summary(returns.dm)  

##plot of the raw data(begin,add in 13th,August by BH)
par(mfrow = c(1, 1), mar = c(4, 2, 1, 1))
prices  = SPL.zoo$Adj.Close
plot(prices, type = "l", col = "blue3", 
      xlab = "Time", ylab = "Returns", xlim = NULL, lwd = 1)
title(main = "Demeaned Price", cex.main = 1)
##plot of the raw data(end)

par(mfrow = c(2, 1))
plot(returns, type = "l")
plot(returns.dm, type = "l")

sq.ret        = returns.dm ^ 2
sq.ret.cum    = cumsum(sq.ret)
sq.ret.cum.df = diff(sq.ret.cum)

plot(sq.ret.cum, type = "l")
plot(sq.ret.cum.df, type = "l")

acf(coredata(returns.dm), lag.max = NULL, plot = TRUE)
pacf(coredata(returns.dm), lag.max = NULL, plot = TRUE)

Box.test(returns.dm ^ 2, lag = 10, type = c("Ljung-Box"), fitdf = 0)

# Breakpoint Testing ------------------------------------------------------

breakpoints     = cpt.var(returns, penalty = "MBIC", pen.value = 0.05, 
                      know.mean = FALSE, method = "PELT")
breakpoints.vec = breakpoints@cpts
breakpoints.zoo = index(returns[breakpoints.vec])

par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(returns.dm, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Returns", xlim = NULL, lwd = 1)
title(main = "Demeaned Returns", cex.main = 1)

# ARIMA -------------------------------------------------------------------

#AIC for returns#

ltt.arima = 2
aic.arima.matrix = matrix(NA, ncol = ltt.arima + 1, nrow = ltt.arima + 1)
colnames(aic.arima.matrix) = 0:2 #order of MA model (q)
rownames(aic.arima.matrix) = 0:2 #order of AR model (p)

for (i in 0 : ltt.arima) {
  for (j in 0 : ltt.arima) {
    aic.arima.matrix[i + 1, j + 1] = AIC(arima(returns.dm, order = c(i, 0, j)))
  }
}

aic.arim.min = min(aic.arima.matrix)
which(aic.arima.matrix == aic.arim.min, arr.ind = T)

#--> arima(1,0,1) has the smallest AIC --> Best model for returns ARMA(1,1)

# --> ARMA (1, 1)

arima101 = arima(returns.dm, order = c(1, 0, 1)) #-6697.83

Box.test(returns.dm, lag = 5, type = "Ljung-Box", fitdf = 0)

# Generate squared residuals
res.arima101        = arima101$residuals[is.na(arima101$residuals) == FALSE]
rev.date            = rev(as.Date(strptime(as.character(SPL[, 1]), "%Y-%m-%d")))
res.arima101.zoo    = zoo(res.arima101, order.by = rev.date)
sq.res.arima101.zoo = res.arima101.zoo ^ 2
plot(res.arima101.zoo, main = "Residuals", type = "l")
plot(sq.res.arima101.zoo, main = "Squared Residuals", type = "l")
# from the plots we can observe some obvious volatility clusters

acf(coredata(sq.res.arima101.zoo),  
    main = "ACF Squared Residuals",  ylim = c(-0.5, 1))
pacf(coredata(sq.res.arima101.zoo), 
     main = "PACF Squared Residuals", ylim = c(-0.5, 1))
# residuals clearly not independent

# ARCH AIC------------------------------------------------------------------

# ARCH 

# use AIC to test
ltt.arch = 10  # ltt: lags to test (compare models up to this lag)
aic.arch = NA

for(i in 1 : ltt.arch) {
  aic.arch[i] = AIC(garch(res.arima101, order = c(0, i)))
}

arch.min.a = min(aic.arch)
aic.arch
which(aic.arch == min(aic.arch), arr.ind = TRUE)
# result: q = 7 (ARCH 7)
 
# ARCH BIC------------------------------------------------------------------

# ARCH 

# use bic to test
ltt.arch = 10  # ltt: lags to test (compare models up to this lag)
bic.arch = NA

for(i in 1 : ltt.arch) {
  bic.arch[i] = BIC(garch(res.arima101, order = c(0, i))$n.likeli, i + 1, n)
}

arch.min = min(bic.arch)
bic.arch
which(bic.arch == min(bic.arch), arr.ind = TRUE)
# result: q = 7 (ARCH 7)
# ARCH HQIC------------------------------------------------------------------

# ARCH 

# use hqic to test
ltt.arch = 10  # ltt: lags to test (compare models up to this lag)
hqic.arch = NA

for(i in 1 : ltt.arch) {
  hqic.arch[i] = HQIC(garch(res.arima101, order = c(0, i))$n.likeli, i + 1, n)
}

arch.min.h = min(hqic.arch)
hqic.arch
which(hqic.arch == min(hqic.arch), arr.ind = TRUE)
# result: q = 7 (ARCH 7)

# GARCH AIC ---------------------------------------------------------------

# quick comparison
garch.11 = garch(res.arima101, order = c(1, 1))
AIC(garch.11) < min(aic.arch)
# GARCH(1, 1) better than best model with only ARCH component

ltt.garch = 10  # ltt: lags to test (compare models up to this lag)
aic.both = matrix(rep(0, ltt.garch ^ 2), nrow = ltt.garch)

for(p in 1 : ltt.garch) {
  for(q in 1 : ltt.garch) {
    aic.both[p, q] = AIC(garch(res.arima101, order = c(p, q)))
  }
}

aic.both  # row number: GARCH component, col number: ARCH component
which(aic.both == min(aic.both), arr.ind = TRUE)
garch.min.a = min(aic.both)
arch.min.a  > garch.min.a
# result: GARCH(1, 2) has lowest AIC
# if ONLY ARCH, then ARCH(8) seems best,
# but when we include a GARCH part, we settle on 
# GARCH part = 1 and ARCH part = 2 for the GARCH(1, 2) model
# GARCH BIC---------------------------------------------------------------

ltt.garch = 10  # ltt: lags to test (compare models up to this lag)
bic.both = matrix(rep(0, ltt.garch ^ 2), nrow = ltt.garch)

for(p in 1 : ltt.garch) {
  for(q in 1 : ltt.garch) {
    bic.both[p, q] = BIC(garch(res.arima101, order = c(p, q))$n.likeli, 
                         p + q + 1, n)
  }
}

bic.both  # row number: GARCH component, col number: ARCH component
which(bic.both == min(bic.both), arr.ind = TRUE)
garch.min = min(bic.both)
arch.min > garch.min
# result: GARCH(1, 1) has lowest bic
# if ONLY ARCH, then ARCH(7) seems best,
# but when we include a GARCH part, 
# we settle on GARCH part = 1 and ARCH part = 2 for the GARCH(1, 1) model
# GARCH HQIC---------------------------------------------------------------

ltt.garch = 10  # ltt: lags to test (compare models up to this lag)
hqic.both = matrix(rep(0, ltt.garch ^ 2), nrow = ltt.garch)

for(i in 1 : ltt.garch) {
  for(j in 1 : ltt.garch) {
    hqic.both[i, j] = HQIC(garch(res.arima101, order = c(i, j))$n.likeli, 
                           i + j + 1, n)
  }
}

hqic.both  # row number: GARCH component, col number: ARCH component
which(hqic.both == min(hqic.both), arr.ind = TRUE)
garch.min.h = min(hqic.both)
arch.min.h  > garch.min.h
# result: GARCH(1, 1) has lowest hqic
# if ONLY ARCH, then ARCH(7) seems best,
# but when we include a GARCH part, 
# we settle on GARCH part = 1 and ARCH part = 1
# for the GARCH(1, 1) model

# Model Decision ----------------------------------------------------------

# Use a GARCH(1, 1) model.

garch.11 = garch(res.arima101, order = c(1, 1))
# plot(res.arima101, type = "l")
plot(res.arima101.zoo, type = "l")
# plot(garch.11, which = "3", type = "l")
print(garch.11)
garch.11.res        = residuals(garch.11)
garch.11.res.zoo    = zoo(garch.11.res, order.by = rev.date)

Box.test(garch.11.res ^ 2, lag = 10, type = c("Ljung-Box"), fitdf = 0)

par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(garch.11.res.zoo, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
title(main = "GARCH Residuals", cex.main = 1) 

# Get interest rate changes -----------------------------------------------

int.rate      = read.csv("int_rate.csv", header = T, sep = ",")
int.rate.zoo  = zoo(int.rate[, -1], 
                    order.by = as.Date(strptime(as.character(int.rate[, 1]), 
                                                "%Y-%m-%d")))
int.rate.diff        = diff(int.rate.zoo)
int.rate.changes.zoo = index(int.rate.diff)
int.rate.changes.vec = which(index(returns) %in% int.rate.changes.zoo)
print(int.rate.changes.zoo)

# Merge DAX and ECB data --------------------------------------------------

dax.int = merge(returns.dm, sq.ret, sq.ret.cum, 
                sq.ret.cum.df, int.rate.diff, fill = 0)
head(dax.int)

data.class(dax.int)

# dax.int: zoo object with time index and 5 vectors for returns.dm, sq.ret, 
# sq.ret.cum, sq.ret.cum.df, int.rate.diff

# ALT BP/IR ---------------------------------------------------------------

# need
# list where returns divided by breakpoints

split(returns, f = breakpoints.zoo)
aggregate(returns, by = vec)

bp.plus1 = c(1, breakpoints.vec[-6])
Inter = as.list(rep(NA, 6))

for(i in bp.plus1) {
  Inter[i] = scale(returns[bp.plus1[i] : breakpoints.vec[i]])
}

a = 1:100
b = c(rep(1, 10), rep(2, 50), rep(3, 40))
split(a, f = b)

# Breakpoint Part ---------------------------------------------------------

newdf1 = returns[1 : breakpoints.vec[1]]
plot(newdf1, type = "l")
var(newdf1)
mean(newdf1)

Inter1 = scale(newdf1)
plot(Inter1, type = "l")

newdf2  =  returns[(breakpoints.vec[1] + 1) : breakpoints.vec[2]]
plot(newdf2, type = "l")
var(newdf2)

Inter2 = scale(newdf2)
plot(Inter2, type = "l")

newdf3  =  returns[(breakpoints.vec[2] + 1) : breakpoints.vec[3]]
plot(newdf3, type = "l")
var(newdf3)

Inter3 = scale(newdf3)
plot(Inter3, type = "l")

newdf4  =  returns[(breakpoints.vec[3] + 1) : breakpoints.vec[4]]
plot(newdf4, type = "l")
var(newdf4)

Inter4 = scale(newdf4)
plot(Inter4, type = "l")

newdf5  =  returns[(breakpoints.vec[4] + 1) : breakpoints.vec[5]]
plot(newdf5, type = "l")
var(newdf5)

Inter5 = scale(newdf5)
plot(Inter5, type = "l")

newdf6  =  returns[(breakpoints.vec[5] + 1) : breakpoints.vec[6]]
plot(newdf6, type = "l")
var(newdf6)

Inter6 = scale(newdf6)
plot(Inter6, type = "l")

BPInter = c(Inter1, Inter2, Inter3, Inter4, Inter5, Inter6)
par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(BPInter, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = breakpoints.zoo[-6], col = "red3", reg = NULL, 
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (BP)", cex.main = 1)
    
Box.test(BPInter ^ 2, lag = 10, type = c("Ljung-Box"), fitdf = 0)

# IR Change rate ----------------------------------------------------------


ir1  =  returns[1 : int.rate.changes.vec[1]]
plot(ir1, type = "l")
var(ir1)  

irs1 = scale(ir1)
plot(irs1, type = "l")

ir2  =  returns[(int.rate.changes.vec[1] + 1) : int.rate.changes.vec[2]]
plot(ir2, type = "l")
var(ir2)  

irs2 = scale(ir2)
plot(irs2, type = "l")

ir3  =  returns[(int.rate.changes.vec[2] + 1) : int.rate.changes.vec[3]]
plot(ir3, type = "l")
var(ir3)  

irs3 = scale(ir3)
plot(irs3, type = "l")

ir4  =  returns[(int.rate.changes.vec[3] + 1) : int.rate.changes.vec[4]]
plot(ir4, type = "l")
var(ir4)  

irs4 = scale(ir4)
plot(irs4, type = "l")

ir5  =  returns[(int.rate.changes.vec[4] + 1) : int.rate.changes.vec[5]]
plot(ir5, type = "l")
var(ir5)  

irs5 = scale(ir5)
plot(irs5, type = "l")

ir6  =  returns[(int.rate.changes.vec[5] + 1) : int.rate.changes.vec[6]]
plot(ir6, type = "l")
var(ir6)  

irs6 = scale(ir6)
plot(irs6, type = "l")

ir7  =  returns[(int.rate.changes.vec[6] + 1) : int.rate.changes.vec[7]]
plot(ir7, type = "l")
var(ir7)  

irs7 = scale(ir7)
plot(irs7, type = "l")

ir8  =  returns[(int.rate.changes.vec[7] + 1) : int.rate.changes.vec[8]]
plot(ir8, type = "l")
var(ir8)  

irs8 = scale(ir8)
plot(irs8, type = "l")

ir9  =  returns[(int.rate.changes.vec[8] + 1) : 1271]
plot(ir9, type = "l")
var(ir9)  

irs9 = scale(ir9)
plot(irs9, type = "l")

IRInter = c(irs1, irs2, irs3, irs4, irs5, irs6, irs7, irs8, irs9)

par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(IRInter, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = int.rate.changes.zoo , col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (IR)", cex.main = 1)

Box.test(IRInter ^ 2, lag = 10, type = c("Ljung-Box"), fitdf = 0) 
         

# Plots -------------------------------------------------------------------


par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(returns.dm, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Returns", xlim = NULL, lwd = 1)
title(main = "Demeaned Returns", cex.main = 1)

par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(IRInter, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = int.rate.changes.zoo , col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (IR)", cex.main = 1)

par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(BPInter, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = breakpoints.zoo[-6], col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (BP)", cex.main = 1)

par(mfrow = c(1, 1), mar = c(6, 5, 4, 2))
plot(garch.11.res.zoo, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
title(main = "GARCH Residuals", cex.main = 1) 


par(mfrow = c(2, 1), mar = c(2, 4, 3, 3))
plot(IRInter, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = int.rate.changes.zoo, col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (IR)", cex.main = 1)


par(mar = c(3, 4, 2, 3))
plot(BPInter, type = "l", col = "blue3",
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = breakpoints.zoo[-6], col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (BP)", cex.main = 1)

par(mfrow = c(2, 1), mar = c(2, 4, 3, 3))
plot(IRInter, type = "l", col = "blue3",
     xlab = "Time", ylab = "Residuals", lwd = 1)
abline(a = NULL, b = NULL, h = NULL, 
       v = int.rate.changes.zoo, col = "red3", reg = NULL,
       coef = NULL, untf = FALSE, lwd = 2) 
title(main = "Homogeneous Intervals (IR)", cex.main = 1) 

par(mar = c(3, 4, 2, 3))
plot(garch.11.res.zoo, type = "l", col = "blue3", 
     xlab = "Time", ylab = "Residuals", lwd = 1)
title(main = "GARCH Residuals", cex.main = 1) 