# 181225 pfaff slides
# pfaff - slides integrated and cointegrated time series

library(urca)
data(npext)
y <- ts(na.omit(npext$realgnp), start = 1909, end = 1988, frequency = 1)
z <- ts(exp(na.omit(npext$unemploy)), start = 1909, end = 1988, frequency = 1)
plot(y, ylab = "logarithm of real gnp")
plot(z, ylab = "unemployment rate in percent")

set.seed(12345)
gwn <- rnorm(100)
layout(matrix(1:4, ncol = 2, nrow = 2))
plot.ts(gwn, xlab = "", ylab = "")
abline(h = 0, col = "red") #h is for horizontal
#abline(v = 55, col = "red") #with vertical line
acf(gwn, main = "ACF")
qqnorm(gwn) 
pacf(gwn, main = "PACF")
layout(matrix(1, ncol = 1, nrow = 1))
par(mar=c(5.1,4.1,4.1,2.1))

#examples ARMA
set.seed(12345)
y.ex <- arima.sim(n = 500, list(ar = c(0.9, -0.4))) 
layout(matrix(1:3, nrow = 3, ncol = 1))
plot(y.ex, xlab = "", main = "Time series plot")
abline(h = 0, col = "red")
acf(y.ex, main = "ACF of y.ex")
pacf(y.ex, main = "PACF of y.ex")
arma20 <- arima(y.ex, order = c(2, 0, 0), include.mean = FALSE)
result <- matrix(cbind(arma20$coef,sqrt(diag(arma20$var.coef))),nrow = 2)
rownames(result) <- c("ar1", "ar2")
colnames(result) <- c("estimate", "s.e.")
result


set.seed(12345)
y.tsar2 <- 5 + 0.5 * seq(250) + arima.sim(list(ar = c(0.8, -0.2)), n = 250)
par(mar=c(5.1,4.1,4.1,2.1))
layout(matrix(1, ncol = 1, nrow = 1))
plot(y.tsar2, ylab="", xlab = "")
abline(a=5, b=0.5, col = "red")


set.seed(12345)
u.ar2 <- arima.sim(list(ar = c(0.8, -0.2)), n = 250)
y1 <- cumsum(u.ar2)
TD <- 5.0 + 0.7 * seq(250)
y1.d <- y1 + TD
layout(matrix(1:2, nrow = 2, ncol = 1))
plot.ts(y1, main = "I(1) process without drift",ylab="", xlab = "")
plot.ts(y1.d, main = "I(1) process with drift",ylab="", xlab = "")
abline(a=5, b=0.7, col = "red")

library(urca)
y1.adf.nc.2 <- ur.df(y1,type = "none", lags = 2)
summary(y1.adf.nc.2)
dy1.adf.nc.2 <- ur.df(diff(y1),type = "none", lags = 1)
plot(y1.adf.nc.2)


library(lmtest)
set.seed(54321)
e1 <- rnorm(500)
e2 <- rnorm(500)
y1 <- cumsum(e1)
y2 <- cumsum(e2)
sr.reg1 <- lm(y1 ~ y2)
summary(sr.reg1)
sr.dw <- dwtest(sr.reg1)
sr.reg2 <- lm(diff(y1) ~ diff(y2))
summary(sr.reg2)


set.seed(12345)
e1 <- rnorm(250, mean = 0, sd = 0.5)
e2 <- rnorm(250, mean = 0, sd = 0.5)
u.ar3 <- arima.sim(model =list(ar = c(0.6, -0.2, 0.1)), n = 250,innov = e1)
y2 <- cumsum(e2)
y1 <- u.ar3 + 0.5*y2
ymax <- max(c(y1, y2))
ymin <- min(c(y1, y2))
layout(matrix(1:2, nrow = 2, ncol = 1))
plot(y1, xlab = "", ylab = "", ylim =c(ymin, ymax), main ="Cointegrated System")
lines(y2, col = "green")
plot(u.ar3, ylab = "", xlab = "", main ="Cointegrating Residuals")
abline(h = 0, col = "red")
adf.test(y1)
adf.test(y2)

set.seed(12345)
e1 <- rnorm(250, 0, 0.5)
e2 <- rnorm(250, 0, 0.5)
e3 <- rnorm(250, 0, 0.5)
u1.ar1 <- arima.sim(model = list(ar=0.75), innov = e1, n = 250)
u2.ar1 <- arima.sim(model = list(ar=0.3), innov = e2, n = 250)
y3 <- cumsum(e3)
y1 <- 0.8 * y3 + u1.ar1
y2 <- -0.3 * y3 + u2.ar1
ymax <- max(c(y1, y2, y3))
ymin <- min(c(y1, y2, y3))
layout(matrix(1:1, nrow = 1, ncol = 1))
plot(y1, ylab = "", xlab = "", ylim = c(ymin, ymax))
lines(y2, col = "red")
lines(y3, col = "blue")

library(dynlm)
lr <- lm(y1 ~ y2)
ect <- resid(lr)[1:249]
dy1 <- diff(y1)
dy2 <- diff(y2)
ecmdat <- cbind(dy1, dy2, ect)
ecm <- dynlm(dy1 ~ L(ect, 1) + L(dy1, 1)+ L(dy2, 1) , data = ecmdat)
