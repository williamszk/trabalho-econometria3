# 181221 example pfaff 2008

library(vars)
data("Canada")
summary(Canada)

# prod = log difference of GDP and employment
# e = log of employment
# U = unemployment rate
# rw = log real wage index

plot(Canada, nc=2, xlab="")
#nc number of columns

adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1

adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift",lags = 1))
adf2

plot(Canada[, "prod"])

VARselect(Canada, lag.max = 8, type = "both")

Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p = 1, type = "both")
p1ct

summary(p1ct, equation = "e")

par(mar=c(1,1,1,1))
plot(p1ct, names = "e")


ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial

norm1 <- normality.test(p1ct)
norm1$jb.mul


summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2, spec = "transitory"))

vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)
