library("rvest", lib.loc="~/R/win-library/3.5")
library("tseries", lib.loc="~/R/win-library/3.5")
library("TTR", lib.loc="~/R/win-library/3.5")
library("forecast", lib.loc="~/R/win-library/3.5")

url <- "https://finance.tut.by/arhiv/?currency=USD&from=01-01-2017&to=01-12-2018"
page <- read_html(url)
tables <- page %>% html_nodes(xpath="//*[@id='content-band']/div[2]/div/div/div[2]/div[1]/table")
table <- tables[[1]]
lmu <- html_table(table)
print(lmu)

df =subset(lmu, select = -c(2,3) )
dc = subset(lmu, select = -c(1,3) )
xDate=c()
yCourse=c()
for (i in dc) {
  i=as.double(i)
  yCourse=c(i)
}
for (i in df) {
  i=as.Date(i, "%d.%m.%Y")
  xDate=c(i)
}
course = data.frame(xDate,yCourse)

#plot(xDate,logXDate,type="l")

plot(course,type = "l",xlab="Äàòà",ylab="Êóðñ ÍÐÁÐ",col="purple",lwd=2)
lines(lowess(course), col="red", lty="dashed",lwd=1)

v2 <- decompose(ts(course), type = c("additive", "multiplicative"), filter = NULL)
plot.ts(SMA(yCourse,n=10),col="red",lwd=3,ylab="Êóðñ ÍÐÁÐ",xlab="Äàòà")



adf.test(yCourse,alternative=c('stationary'))



Lm <- BoxCox.lambda(course, method="loglik")
print(Lm)

short <- ts(yCourse, frequency=20)
fit.nnm <- nnetar(yCourse, lambda=Lm, size=3)
fcast.nnm <- forecast(fit.nnm, h=100, lambda=Lm)

par(mfrow=c(3, 1))
plot(fcast.nnm)

short <- ts(yCourse, frequency=20)

########   ÏÐÎÃÍÎÇ

h <-30
fit.arima <- auto.arima(short[1:30], lambda=Lm)
fcast.arima <- forecast(fit.arima, h, lambda=Lm)

fit.nn <- nnetar(short[1:30], size=7, lambda=Lm)
fcast.nn <- forecast(fit.nn, h, lambda=Lm)

fit.tbats <-tbats(short[1:30], lambda=Lm)
fcast.tbats <- forecast(fit.tbats, h, lambda=Lm)

par(mfrow=c(3, 1))
plot(fcast.arima, include=3*h)
plot(fcast.nn, include=3*h)
plot(fcast.tbats, include=3*h)
##########



par(mfrow=c(1, 1))
plot(short[1:30], type="l", col="red", lwd=3, xlab="Day", ylab="Price, $", main="December prices",
     ylim=c(min(short[1:30], fcast.arima$mean, fcast.tbats$mean, fcast.nn$mean),
            max(short[1:30], fcast.arima$mean, fcast.tbats$mean, fcast.nn$mean)))
     
lines(as.numeric(fcast.nn$mean), col="green", lwd=3,lty=2)
lines(as.numeric(fcast.tbats$mean), col="magenta", lwd=3,lty=2)
lines(as.numeric(fcast.arima$mean), col="blue", lwd=3, lty=2)
legend("topright", legend=c("Real Data","NeuralNet","TBATS", "ARIMA"), 
       col=c("red","green", "magenta","blue"), lty=c(1,2,2,2), lwd=c(5,3,3,3))
grid()

