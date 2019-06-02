#181115 projeto codes

#trabalho prova eco3


library(readxl)
library(vars)
library(rmarkdown)
library(aTSA)

etf_data <- read_excel("etf.xlsx") #import file
head(etf_data)
names(etf_data)

#treatment for dates
date <- as.character(etf_data$X__1)  
date <- as.Date(date,"%Y-%m-%d")
class(date)
head(date)

#treatment for etf
etf<-log(as.numeric(etf_data$etfcaixa))
sum(is.na(etf)) #number of missings
plot(density(etf[!is.na(etf)]))
plot(etf,type = 'l') #plot graph of series
table(is.na(etf)*1:length(etf)) #position of missings
#treatment for missings: use flat method, where we input the last observation
#in the place of the missing
for (t in 1:length(etf)) {
  if (is.na(etf[t])==TRUE){etf[t] <- etf[t-1]} 
}

#treatment for ibov data
ibov <- log(as.numeric(etf_data$IBOV))
sum(is.na(ibov)) #number of missings
plot(density(ibov[!is.na(ibov)]))
plot(ibov,type = 'l')
table(is.na(ibov)*1:length(ibov)) #position of missings
#treatment for missings: use flat method where we imput the last observation
#in the place of the missing
for (t in 1:length(ibov)) {
  if (is.na(ibov[t])==TRUE){ibov[t] <- ibov[t-1]} 
}

#check the length of vectors
length(date)
length(ibov)
length(etf)

#merge the vectors
data_ibov <- cbind(date,ibov,etf)
head(data_ibov)


#########################
#editing data of industry
industria <- read_excel("prodindustrialcapital.xls") #import data
head(industria)
names(industria)

#treatment for dates
date <- industria$Data
head(date)
class(date)
date2 <- paste(date,"01",sep='.') #we need to include the day otherwise it does not work
head(date2)
date3 <- as.Date(date2,"%Y.%m.%d")
head(date3)
class(date3)

#treatment for data about industrial production
indtr <- log(industria$Prodindustrialcapital)
head(indtr)
plot(density(indtr))
plot(indtr,type = 'l')

#check length of vectors
length(indtr)
length(date3)

#merge vectors
data_ind <- cbind(date3,indtr)
head(data_ind)

#########################
##analysis
#########################
#build a VAR model with ibov and etf

#selecting number of lags for VAR 
VARselect(cbind(etf, ibov), lag.max =10, type="both")$selection
VARselect(cbind(etf, ibov), lag.max =10, type="trend")$selection
VARselect(cbind(etf, ibov), lag.max =10, type="const")$selection
VARselect(cbind(etf, ibov), lag.max =10, type="none")$selection
#using Schwartz criterion we use VAR(2)




#apply to a model VAR
model1 <- VAR(cbind(etf,ibov), p=2, type="both")
summary(model1)
model1$varresult


m1 <- ca.jo(cbind(etf,ibov), type = "trace", ecdet = "trend", K = 2, spec = "transitory") 
summary(m1)



vec <- cajorls(m1, r = 1)
coef(summary(vec$rlm))
vec$rlm[1]
class(as.matrix(vec$rlm))
as.matrix(vec$rlm)[[1]][1,1]



coef(summary(vec$rlm))[[2]][1,4]

SVEC(m1,r=1,LR=NULL,SR=NULL)


#############################
#set beta = (1, -1) for test of cointegration
res <- etf - (ibov-ibov[1])
plot(res,type="l")
acf(res)
pacf(res)
adf.test(res)



