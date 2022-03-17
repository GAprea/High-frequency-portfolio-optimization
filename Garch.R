#Garch modelling

library(xts)
library(rugarch)
library(aTSA)
library(tseries)
library(FinTS)
library(e1071)
library(readxl)
library(rmgarch)

Dataset3y<- read_excel("R/3y-log_returns.xlsx")
#Dataset5s<- read_excel("R/Log-R-AAPL-5s-BID-NOOPEN.xlsx")

#Univariate Garch procedure
Dataset3y_univ_GARCH<-merge.zoo(Dataset3y[1],Dataset3y[2])
#Converting data from format to xts
Dataset3y_univ_GARCH_xts <- xts(x = Dataset3y_univ_GARCH[, -1], order.by = as.Date.factor(Dataset3y_univ_GARCH$Time))
#Garch fit procedure
Dataset3y_univ_GARCH_vector=ugarchspec(variance.model = list(garchOrder=c(2,1)),mean.model = list(armaOrder=c(2,1)))
Dataset3y_univ_GARCH_vector_fit=ugarchfit(Dataset3y_univ_GARCH_vector,data = Dataset3y_univ_GARCH_xts)
Dataset3y_univ_GARCH_vector_fit
#Impact curve
#news_garch=newsimpact(Dataset3y_univ_GARCH_vector_fit)
#Plot Impact curve
#plot(news_garch$zx,news_garch$zy,ylab=news_garch$yexpr,news_garch$xexpr,main="News Impact Curve")
#Forecasting Garch procedure
Dataset3y_univ_GARCH_xts_forecast=ugarchforecast(Dataset3y_univ_GARCH_vector_fit,n.ahead=5)
Dataset3y_univ_GARCH_xts_forecast
plot(Dataset3y_univ_GARCH_xts_forecast)


#Stationary test ADF
#adf.test(Dataset3y$TRV)
#LM-ARCH test
#ArchTest(Dataset3y$TRV)


#DCC - Garch model
Dataset3y<-Dataset3y[,-1] #eliminating time column
model1=ugarchspec(mean.model=list(armaOrder=c(0,0)),variance.model=list(garchOrder=c(1,1),model="sGARCH"),distribution.model="norm")
modelspec=dccspec(uspec=multispec(replicate(30,model1)),dccOrder = c(1,1),distribution = "mvnorm")
modelfit=dccfit(modelspec,data=data.frame(Dataset3y))
modelfit

#dcca1 -> measures the short term spillover effect
#dccb1 -> measure the long term spillover effect
#Remember dcca1+dccb1 <=1 for dynamic relationship

#Forecasting using DCC Garch model
dccforecast(modelfit,n.ahead = 1)
#Graphs of DCC-Garch model
plot(modelfit)


#garch(x =Dataset3y_univ_GARCH_xts, grad=c("numerical"),trace=FALSE)
#DatasetuGARCH_xts_zero<-DatasetuGARCH_xts[apply(DatasetuGARCH, 1, function(row) all(row !=0)),]
#DatasetuGARCH_zero_xts <- xts(x = DatasetuGARCH_zero[, -1], order.by = as.Date.factor(DatasetuGARCH$time))
