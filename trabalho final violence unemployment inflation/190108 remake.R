# 190108 remake


# build the base#################
library(ggplot2)
library(readxl)  
library(aTSA)
library(data.table)
library(xts)
library(urca)
library(vars)
library(gridExtra)


setwd("C:/Users/willi/Desktop/working/Projects/RAW_DATA")
tx.desem <- fread(input='Taxa de desemprego.csv', sep='auto', 
                  sep2='auto', integer64='double')

#treatment of data
#drop the "Fonte IBGE" from the last line
tx.desem <- tx.desem[-length(tx.desem$Data),]

#different variables for unemployment
#change names of variables
names(tx.desem) <- c("date","rt.ch.unem")

#unemployment series are in character format
#unemployment rate
tx.desem$rt.ch.unem <- as.numeric( gsub(",",".",tx.desem$rt.ch.unem))

ind1 <- tx.desem$rt.ch.unem

rt.ch.unem.m <- diff(ind1)/ind1[-length(ind1)]
rt.ch.unem.m <- c(NA,rt.ch.unem.m)
tx.desem$rt.ch.unem <- rt.ch.unem.m

#make dates in date format
tx.desem$date <-  as.Date(paste("01", #day 1 for all months
                                substr(tx.desem$date,1,2), #month 
                                substr(tx.desem$date,4,7), #year
                                sep = '.')  
                          ,"%d.%m.%Y")  

#import data of inflation
setwd("C:/Users/willi/Desktop/working/Projects/RAW_DATA")
inflation <- read_excel("ipca.xls")
inflation <- inflation[,1:3]
year <- as.character(inflation$ano) 
#fix year column
for (t in 1:length(year)) {
  if (is.na(year[t])==TRUE){year[t] <- year[t-1]} 
}
inflation$ano <- year
#change "mes" form
month <- inflation$mês
for (t in 1:length(month)) {
  if (month[t]=="JAN"){month[t] <- "01"}
  if (month[t]=="FEV"){month[t] <- "02"} 
  if (month[t]=="MAR"){month[t] <- "03"} 
  if (month[t]=="ABR"){month[t] <- "04"} 
  if (month[t]=="MAI"){month[t] <- "05"} 
  if (month[t]=="JUN"){month[t] <- "06"} 
  if (month[t]=="JUL"){month[t] <- "07"} 
  if (month[t]=="AGO"){month[t] <- "08"} 
  if (month[t]=="SET"){month[t] <- "09"} 
  if (month[t]=="OUT"){month[t] <- "10"} 
  if (month[t]=="NOV"){month[t] <- "11"} 
  if (month[t]=="DEZ"){month[t] <- "12"} 
}
inflation$mês <- month

#create variable for date
inflation$date <- as.Date(paste("01",month,year,sep='.'),"%d.%m.%Y")
inflation <-as.data.frame(inflation[,c(3,4)])  
inflation <- inflation[,c(2,1)]

#create variable for rate of change of price index
#rate of inflation per month
rt.inflation.m <- diff(inflation$indice)/inflation$indice[-nrow(inflation)]
rt.inflation.m <- c(c(NA),rt.inflation.m)
inflation$rt.month <- rt.inflation.m


#build data for violence
setwd("C:/Users/willi/Desktop/working/Projects/RAW_DATA/SINESPJC")
#data from Ministry of Justice of Brazil
#it contains data in months per municipality and type of crime

#import data about violence 2017
function.violence.base <- function(){
  myFiles <- c("ocorrenciasmun-brasil2017.xlsx",
               "ocorrenciasmun-brasil2016.xlsx",
               "ocorrenciasmun-brasil2015.xlsx",
               "ocorrenciasmun-brasil2014.xlsx",
               "ocorrenciasmun-brasil2013.xlsx",
               "ocorrenciasmun-brasil2012.xlsx",
               "ocorrenciasmun-brasil2011.xlsx",
               "ocorrenciasmun-brasil2010.xlsx",
               "ocorrenciasmun-brasil2009.xlsx",
               "ocorrenciasmun-brasil2008.xlsx",
               "ocorrenciasmun-brasil2007.xlsx",
               "ocorrenciasmun-brasil2006.xlsx",
               "ocorrenciasmun-brasil2005.xlsx",
               "ocorrenciasmun-brasil2004.xlsx")
  base_violence <- data.frame(type_crime=NA,date=NA,qt_violence=NA)
  i <- "ocorrenciasmun-brasil2015.xlsx"
  for(i in myFiles){
    violence.h <- as.data.frame(read_excel(i)) 
    violence.h <- violence.h[,c(6,8,9)]
    violence.h$`Mês/Ano`<-as.character(violence.h$`Mês/Ano`)  
    names(violence.h) <- c("type_crime","date","qt_violence")
    violence.h <- aggregate(violence.h[,-c(1,2)], 
                            by=list(violence.h$type_crime,violence.h$date), 
                            FUN=sum, na.rm=TRUE)
    names(violence.h) <- c("type_crime","date","qt_violence")
    
    base_violence <- rbind(base_violence,violence.h)
  }
  
  
  base_violence <- base_violence[-1,]
}

base.violence<-function.violence.base()

#change date for Date type
date <- as.Date(base.violence$date,"%Y-%m-%d")  
base.violence$date <- date





#Many of the variables are non-stationary and have significant results.
# preparing data for level analysis

setwd("C:/Users/willi/Desktop/working/Projects/RAW_DATA")
tx.desem1 <- fread(input='Taxa de desemprego.csv', sep='auto', 
                   sep2='auto', integer64='double')

#treatment of data
#drop the "Fonte IBGE" from the last line
tx.desem1 <- tx.desem1[-length(tx.desem1$Data),]

#different variables for unemployment
#change names of variables
names(tx.desem1) <- c("date","rt.unem")

#unemployment series are in character format
#unemployment rate
tx.desem1$rt.unem <- as.numeric( gsub(",",".",tx.desem1$rt.unem))

#make dates in date format
tx.desem1$date <-  as.Date(paste("01", #day 1 for all months
                                 substr(tx.desem1$date,1,2), #month 
                                 substr(tx.desem1$date,4,7), #year
                                 sep = '.')  
                           ,"%d.%m.%Y")  

#estupro
rape1 <- 
  base.violence[base.violence$type=='Estupro',][,c(2,3)]
names(rape1) <- c("date",'rape')

#homicidio doloso
homicide1<-
  base.violence[base.violence$type=='Homicídio doloso',][,c(2,3)]
names(homicide1) <- c("date","homicide")

#roube de veiculo
vehicle_theft1<-
  base.violence[base.violence$type=='Roubo de veículo',][,c(2,3)] 
names(vehicle_theft1) <- c("date","veh.theft")

#assalto a veiculo  
vehicle_robbery1 <-
  base.violence[base.violence$type=='Furto de veículo',][,c(2,3)] 
names(vehicle_robbery1) <- c("date","veh.robbery")

#lesao corporal seguida de morte
personal_injury_death1 <-
  base.violence[base.violence$type=='Lesão corporal seguida de morte',][,c(2,3)] 
names(personal_injury_death1) <- c("date","per.injury")

#roubo seguido de morte (latrocinio)
felony_murder1 <-
  base.violence[base.violence$type=='Roubo seguido de morte (latrocínio)',][,c(2,3)] 
names(felony_murder1) <- c("date","fel.murder")


build.base.ts1 <- 
  function(){
    #rate of unemployment
    unem.ts <- as.xts(tx.desem1$rt.unem, order.by = tx.desem1$date)
    names(unem.ts) <- c("unem")
    
    #inflation rate per month
    inflation.m.ts1 <- as.xts(inflation$rt.month,order.by = inflation$date)
    names(inflation.m.ts1) <- c("rt.inflation.month")
    
    #rape quantity of cases
    rape.ts <- as.xts(rape1$rape ,order.by = rape1$date)
    names(rape.ts) <- c("rape")
    
    #homicides quantity of cases
    homicide.ts <- as.xts(homicide1$homicide,
                          order.by = homicide1$date)
    names(homicide.ts) <- c("homicide")
    
    #vehicle theft quantity of cases
    vehicle_theft.ts <- as.xts(vehicle_theft1$veh.theft,
                               order.by = vehicle_theft1$date)
    names(vehicle_theft.ts) <- c("vehicle_theft")
    
    #vehicle robbery quantity of cases
    vehicle_robbery.ts <- as.xts(vehicle_robbery1$veh.robbery,
                                 order.by = vehicle_robbery1$date)
    names(vehicle_robbery.ts) <- c("vehicle_robbery")
    
    #personal injury and death quantity of cases
    personal_injury_death.ts <- as.xts(personal_injury_death1$per.injury,
                                       order.by = personal_injury_death1$date)
    names(personal_injury_death.ts) <- c("personal_injury_death")
    
    #felony murder quantity of cases
    felony_murder.ts <- as.xts(felony_murder1$fel.murder,
                               order.by = felony_murder1$date)
    names(felony_murder.ts) <- c("felony_murder")
    
    #merging xts
    base.ts <- merge.xts(
      unem.ts,
      inflation.m.ts1,
      rape.ts,
      homicide.ts,
      vehicle_theft.ts,
      vehicle_robbery.ts,
      personal_injury_death.ts,
      felony_murder.ts)
    
    base.ts
}

base.ts1 <- build.base.ts1()

#leave only dates that are common to all
base.ts2 <- base.ts1[!is.na(base.ts1[,1]) & !is.na(base.ts1[,2]) & !is.na(base.ts1[,3]),]

# Analsysis on level ############# 

## Unemployment
#Rate of unemployment is not stationary. 
##Inflation
## Rape variable
rape.ml <- VAR(base.ts2[,c(1,2,3)], lag.max=5, ic="SC", type="none")
summary(rape.ml, equation="rape")
normality.test(rape.ml)
serial.test(rape.ml)
causality(rape.ml, cause = c("unem","rt.inflation.month"),boot.runs=100)


## Homicide variable

#homicide, unemployment and monthly inflation rate

hom.ml <- VAR(base.ts2[,c(1,2,4)], type = "none", lag.max = 5, ic = "SC") 
summary(hom.ml, equation="homicide")

normality.test(hom.ml)
serial.test(hom.ml)

par(mar=c(1,1,1,1))

causality(hom.ml, cause = c("unem","rt.inflation.month"),boot.runs=100)
plot(fevd(hom.ml, n.ahead=20))




## Vehicle theft variable
veh.t.ml <- VAR(base.ts2[,c(1,2,5)], type = "none", lag.max = 5, ic = "SC") 
summary(veh.t.ml, equation="vehicle_theft")


normality.test(veh.t.ml)
serial.test(veh.t.ml)

causality(veh.t.ml, cause = c("unem","rt.inflation.month"),boot.runs=100)



## Vehicle robbery variable
veh.r.ml <- VAR(base.ts2[,c(1,2,6)], type="none", lag.max = 5, ic = "SC") 
summary(veh.r.ml, equation="vehicle_robbery")

normality.test(veh.r.ml)
serial.test(veh.r.ml)

par(mar=c(1,1,1,1))

causality(veh.r.ml, cause = c("unem","rt.inflation.month"),boot.runs=100)

plot(fevd(veh.r.ml, n.ahead = 20))


## Personal injury and death
pers.ml <- VAR(base.ts2[,c(1,2,7)], type="none", lag.max = 5, ic = "SC") 
summary(pers.ml, equation="personal_injury_death")
causality(pers.ml, 
          cause = c("unem","rt.inflation.month"),boot.runs=100)
normality.test(pers.ml)
serial.test(pers.ml)
par(mar=c(1,1,1,1))
plot(fevd(pers.ml, n.ahead =20))





## Felony murder variable

felo.ml <- VAR(base.ts2[,c(1,2,8)], type="none", p=1) 
summary(felo.ml, equation="felony_murder")

causality(felo.ml, 
          cause = c("unem","rt.inflation.month"),boot.runs=100)
normality.test(felo.ml)
serial.test(felo.ml)
par(mar=c(1,1,1,1))
plot(fevd(felo.ml, n.ahead =20))

plot(stability(felo.ml))




##Personal Injury and Death more lags

pers.ml2 <- VAR(base.ts2[,c(1,2,7)], type="none", p=3, ic = "SC") 
summary(pers.ml2, equation="personal_injury_death")
causality(pers.ml2, 
          cause = c("unem","rt.inflation.month"),boot.runs=100)
normality.test(pers.ml2)
serial.test(pers.ml2)


## Felony Murder more lags


felo.mlv4 <- VAR(base.ts2[,c(1,2,8)], type="none", p=3) 
summary(felo.mlv4, equation="felony_murder")
causality(felo.mlv4, 
          cause = c("unem","rt.inflation.month"),boot.runs=100)
normality.test(felo.mlv4)
serial.test(felo.mlv4)


felo.mlv3 <- VAR(base.ts2[,c(1,2,8)], type="none", p=5) 
summary(felo.mlv3, equation="felony_murder")
causality(felo.mlv3, 
          cause = c("unem","rt.inflation.month"),boot.runs=100)
normality.test(felo.mlv3)
serial.test(felo.mlv3)
#par(mar=c(5.1,4.1,4.1,2.1))
par(mar=c(1,1,1,1))
plot(fevd(felo.mlv3, n.ahead =20))
plot(stability(felo.mlv3))



# irf graphs #########################

#rape - unemployment
h1 <- irf(rape.ml, n.ahead =20, impulse=c("unem"), response="rape")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.rape.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("rape")

#rape - inflation
h1 <- irf(rape.ml, n.ahead =20, impulse=c("rt.inflation.month"), response="rape")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.rape.inf  <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("rape")

#homicide - unemployment
h1 <- irf(hom.ml, n.ahead =20, impulse=c("unem"), response="homicide")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.homicide.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("homicide")

#homicide - inflation
h1 <- irf(hom.ml, n.ahead =20, impulse=c("rt.inflation.month"), response="homicide")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.homicide.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("homicide")

#vehicle theft - unemployment
h1 <- irf(veh.t.ml, n.ahead =20, impulse=c("unem"), response="vehicle_theft")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.v.theft.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("vehicle theft")

#vehicle theft - inflation
h1 <- irf(veh.t.ml, n.ahead =20, impulse=c("rt.inflation.month"), response="vehicle_theft")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.v.theft.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("vehicle theft")

#vehicle robbery - unemployment
h1 <- irf(veh.r.ml, n.ahead =20, impulse=c("unem"), response="vehicle_robbery")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.v.robbery.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("vehicle robbery")

#vehicle robbery - inflation
h1 <- irf(veh.r.ml, n.ahead =20, impulse=c("rt.inflation.month"), response="vehicle_robbery")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21
h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)

graph.v.robbery.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("vehicle robbery")

setwd("C:/Users/willi/Desktop/working/Projects/181201 trabalho final econometria3/texto")
pdf("graph_irf1.pdf") 
grid.arrange(graph.rape.unem,
             graph.rape.inf,
             graph.homicide.unem,
             graph.homicide.inf, 
             graph.v.theft.unem,
             graph.v.theft.inf,
             graph.v.robbery.unem,
             graph.v.robbery.inf,
             ncol=2, nrow = 4)
dev.off()



# personal injury and death - unemployment
h1 <- irf(pers.ml, n.ahead =20, impulse=c("unem"), response="personal_injury_death")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.personal.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("personal injury and death")

# personal injury and death - inflation
h1 <- irf(pers.ml, n.ahead =20, impulse=c("rt.inflation.month"), response="personal_injury_death")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.personal.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("personal injury and death")

# felony murder - unemployment
h1 <- irf(felo.ml, n.ahead =20, impulse=c("unem"), response="felony_murder")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.felony.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("felony murder")

# felony murder - inflation
h1 <- irf(felo.ml, n.ahead =20, impulse=c("rt.inflation.month"), response="felony_murder")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.felony.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("felony murder")

# personal injury and death 3 lags - unemployment
h1 <- irf(pers.ml2, n.ahead =20, impulse=c("unem"), response="personal_injury_death")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.personal.3l.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("personal injury and death 3 lags")

# personal injury and death 3 lags - inflation
h1 <- irf(pers.ml2, n.ahead =20, impulse=c("rt.inflation.month"), response="personal_injury_death")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.personal.3l.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("personal injury and death 3 lags")


# felony murder 3 lags - unemployment
h1 <- irf(felo.mlv4, n.ahead =20, impulse=c("unem"), response="felony_murder")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.felony.3l.unem <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock unemployment")+ggtitle("felony murder 3 lags")

# felony murder 3 lags - inflation
h1 <- irf(felo.mlv4, n.ahead =20, impulse=c("rt.inflation.month"), response="felony_murder")
h1.1 <- as.data.frame(h1[1][[1]][[1]]); names(h1.1)<-c("value")   
h1.2 <- as.data.frame(h1[2][[1]][[1]]); names(h1.2)<-c("value")     
h1.3 <- as.data.frame(h1[3][[1]][[1]]); names(h1.3)<-c("value")     
h1.4 <- as.data.frame(rep(0,21)); names(h1.4) <- c("value")
h1.1$type <- "irf"; h1.2$type <- "lower"; h1.3$type <- "upper"; h1.4$type <- "zero"
h1.1$period <- 1:21;h1.2$period <- 1:21;h1.3$period <- 1:21;h1.4$period <- 1:21;h1.f <- rbind(h1.1,h1.2,h1.3,h1.4)
graph.felony.3l.inf <-ggplot(h1.f,aes(x=period,y=value))+geom_line(aes(color=type),size=1)+
  scale_color_manual(values=c("#00AFBB","red","red","black"))+
  geom_abline(intercept = 0,slope=0)+scale_x_continuous(breaks = seq(0,20,4))+
  theme(legend.position="none")+ylab("shock inflation")+ggtitle("felony murder 3 lags")

setwd("C:/Users/willi/Desktop/working/Projects/181201 trabalho final econometria3/texto")
pdf("graph_irf2.pdf") 
grid.arrange(graph.personal.unem,
             graph.personal.inf,
             graph.felony.unem,
             graph.felony.inf,
             graph.personal.3l.unem,
             graph.personal.3l.inf,
             graph.felony.3l.unem,
             graph.felony.3l.inf,
             ncol=2, nrow = 4)
dev.off()


# UR analysis ###################
adf.test(as.numeric(base.ts2$unem))
adf.test(as.numeric(base.ts2$rt.inflation.month))
adf.test(as.numeric(base.ts2$rape))
adf.test(as.numeric(base.ts2$homicide))
adf.test(as.numeric(base.ts2$vehicle_theft))
adf.test(as.numeric(base.ts2$vehicle_robbery))
adf.test(as.numeric(base.ts2$personal_injury_death))
adf.test(as.numeric(base.ts2$felony_murder))


# series graphs ####################

setwd("C:/Users/willi/Desktop/working/Projects/181201 trabalho final econometria3/texto")

pdf("graph1.pdf") 
par(mar=c(1,1,1,1))
layout(matrix(1:4, ncol = 1, nrow = 4))
plot.xts(base.ts2$unem, main = "rate of unemployment" , yaxis.right = FALSE, font=1)
plot.xts(base.ts2$rt.inflation.month, main = "rate of inflation per month" , yaxis.right = FALSE)
plot.xts(base.ts2$rape, main = "rape nº of cases" , yaxis.right = FALSE)
plot.xts(base.ts2$homicide, main = " homicide nº of cases" , yaxis.right = FALSE)
dev.off() 


pdf("graph2.pdf") 
par(mar=c(1,1,1,1))
layout(matrix(1:4, ncol = 1, nrow = 4))
plot.xts(base.ts2$vehicle_theft, main = "vehicle theft nº of cases" , yaxis.right = FALSE)
plot.xts(base.ts2$vehicle_robbery, main = "vehicle robbery nº of cases" , yaxis.right = FALSE)
plot.xts(base.ts2$personal_injury_death, main= "personal injury and death nº of cases", yaxis.right = FALSE )
plot.xts(base.ts2$felony_murder, main = "felony murder nº of cases", yaxis.right = FALSE)
dev.off() 









## Felony murder VECM ######################

#we choose to test cointegration in felony murder because the variable compared to the other variables
#showed more significance

vecm.personal <-  ca.jo(base.ts2[,c(7,2,1)], type="trace", ecdet="none", K=3, spec="transitory")
summary(vecm.personal)

vecm.personal.r1 <- cajorls(vecm.personal, r=2)
vecm.personal.r1$beta
summary(vecm.personal.r1$rlm)

vecm.felony <-  ca.jo(base.ts2[,c(8,2,1)], type="trace", ecdet="none", K=3, spec="transitory")
summary(vecm.felony)

vecm.felony.r1 <- cajorls(vecm.felony, r=2)
vecm.felony.r1$beta
summary(vecm.felony.r1$rlm)




