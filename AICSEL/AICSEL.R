## Clear history
rm(list = ls(all = TRUE))
graphics.off()

## Intall and load packages
libraries = c("tseries", "stockPortfolio")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## Get Data and Returns
grEx1 = getReturns(c("C"), start = "2004-01-01", end = "2008-12-31")
returns = grEx1$R

## Set the parameters of the model up to which you want to test
p = 2
q = 2
d = 0
aic.arima = matrix(NA, ncol = q + 1, nrow = p + 1)
colnames(aic.arima) = seq(0, q)
rownames(aic.arima) = seq(0, p)

for (i in 0:p) {
  for (j in 0:q) {
    aic.arima[i + 1, j + 1] = AIC(arima(returns, order = c(i, d, j)))
  }
}

## Results
aic.arima.min = min(aic.arima)
print(aic.arima)
which(aic.arima == aic.arima.min, arr.ind = T)
