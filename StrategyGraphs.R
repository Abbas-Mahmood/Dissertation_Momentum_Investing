library(readxl)
library(e1071)
setwd("/Users/abbasmahmood/Desktop/Dissertation/Portfolios")
Returns1_1 <- read_excel('XMOMReturns.xlsx',sheet ='MOM1_1' )
Returns3_1 <- read_excel('XMOMReturns.xlsx',sheet ='MOM3_1' )
Returns6_1 <- read_excel('XMOMReturns.xlsx',sheet ='MOM6_1' )

#CUMULATIVE PLOTS
par(mfrow = c(1,2))
plot(Returns1_1$Dates,cumsum(Returns1_1$TotalMom),type = 'l',ylim = c(-20,70),xlab='Date',ylab='Cumulative Excess Return')
lines(Returns1_1$Dates,cumsum(Returns1_1$LongMom),col = 'blue')
lines(Returns1_1$Dates,cumsum(Returns1_1$ShortMom),col = 'red')
legend("topleft",c("Total", "Long", "Short"),lty = 1,col = c('black','blue','red'),cex=0.7)
plot(Returns1_1$Dates,cumsum(Returns1_1$SpotTotalMom),type = 'l',ylim = c(-20,70),xlab='Date',ylab='Cumulative Spot Return')
lines(Returns1_1$Dates,cumsum(Returns1_1$SpotLongMom),col = 'blue')
lines(Returns1_1$Dates,cumsum(Returns1_1$SpotShortMom),col = 'red')
legend("topleft",c("Total", "Long", "Short"),lty = 1,col = c('black','blue','red'),cex=0.7)

par(mfrow = c(1,2))
plot(Returns3_1$Dates,cumsum(Returns3_1$TotalMom),type = 'l',xlab = 'Date',ylab = 'Cumulative Excess Return')
lines(Returns3_1$Dates,cumsum(Returns3_1$LongMom),col = 'blue')
lines(Returns3_1$Dates,cumsum(Returns3_1$ShortMom),col = 'red')
legend("topleft",c("Total", "Long", "Short"),lty = 1,col = c('black','blue','red'),cex=0.7)
plot(Returns3_1$Dates,cumsum(Returns3_1$SpotTotalMom),type = 'l',xlab = 'Date',ylab = 'Cumulative Spot Return')
lines(Returns3_1$Dates,cumsum(Returns3_1$SpotLongMom),col = 'blue')
lines(Returns3_1$Dates,cumsum(Returns3_1$SpotShortMom),col = 'red')
legend("topleft",c("Total", "Long", "Short"),lty = 1,col = c('black','blue','red'),cex=0.7)

par(mfrow = c(1,2))
plot(Returns6_1$Dates,cumsum(Returns6_1$TotalMom),type = 'l',xlab = 'Date',ylab = 'Cumulative Excess Return')
lines(Returns6_1$Dates,cumsum(Returns6_1$LongMom),col = 'blue')
lines(Returns6_1$Dates,cumsum(Returns6_1$ShortMom),col = 'red')
legend("topleft",c("Total", "Long", "Short"),lty = 1,col = c('black','blue','red'),cex=0.7)
plot(Returns6_1$Dates,cumsum(Returns6_1$SpotTotalMom),type = 'l',xlab = 'Date',ylab = 'Cumulative Spot Return')
lines(Returns6_1$Dates,cumsum(Returns6_1$SpotLongMom),col = 'blue')
lines(Returns6_1$Dates,cumsum(Returns6_1$SpotShortMom),col = 'red')
legend("topleft",c("Total", "Long", "Short"),lty = 1,col = c('black','blue','red'),cex=0.7)


#ABSOLUTE PLOTS
par(mfrow=c(1,2)) 
plot(Returns1_1$Dates,Returns1_1$TotalMom,type = 'l',xlab = 'Date',ylab = 'Excess Return')
abline(h = c(mean(Returns1_1$TotalMom)),col=c("blue"), lty=c(2,2), lwd=c(3, 3))
qqnorm(Returns1_1$TotalMom,ylab = 'Excess Returns')
qqline(Returns1_1$TotalMom,col='red')

skewness(Returns1_1$SpotShortMom)
kurtosis(Returns1_1$SpotShortMom)
sd(Returns1_1$SpotShortMom)
mean(Returns1_1$SpotShortMom)
min(Returns1_1$SpotShortMom)
t.test(Returns1_1$TotalMom)
#hist(Returns1_1$TotalMom)

par(mfrow=c(1,2)) 
plot(Returns3_1$Dates,Returns3_1$TotalMom,type = 'l',xlab = 'Date',ylab = 'Excess Return')
abline(h = c(mean(Returns3_1$TotalMom)),col=c("blue"), lty=c(2,2), lwd=c(3, 3))
qqnorm(Returns3_1$TotalMom,ylab = 'Excess Returns')
qqline(Returns3_1$TotalMom,col='red')

skewness(Returns3_1$SpotShortMom)
kurtosis(Returns3_1$SpotShortMom)
sd(Returns3_1$SpotShortMom)
mean(Returns3_1$SpotShortMom)
t.test(Returns3_1$TotalMom)
#hist(Returns3_1$TotalMom)

par(mfrow=c(1,2)) 
plot(Returns6_1$Dates,Returns6_1$TotalMom,type = 'l',xlab = 'Date',ylab = 'Excess Return')
abline(h = c(mean(Returns6_1$TotalMom)),col=c("blue"), lty=c(2,2), lwd=c(3, 3))
qqnorm(Returns6_1$TotalMom,ylab = 'Excess Returns')
qqline(Returns6_1$TotalMom,col='red')

skewness(Returns6_1$SpotTotalMom)
kurtosis(Returns6_1$SpotTotalMom)
sd(Returns6_1$SpotTotalMom)
mean(Returns6_1$SpotTotalMom)
#t.test(Returns6_1$SpotTotalMom)
#kurtosis(((((Returns6_1$TotalMom/100)+1)**12)-1)*100)
#hist(Returns6_1$TotalMom)


par(mfrow=c(2,2)) 
#FACTORS AND CORRELATION WITH MOM3_1
FactorReturns <- read_excel('Complete/regression_3_1.xlsx',sheet ='TotalData' )
plot(FactorReturns[2:6])


XMOM_TSMOMregession <- lm(XMOM_3_1~TSMOM,data = FactorReturns)
#summary(XMOM_TSMOMregession)
plot(FactorReturns$XMOM_3_1,FactorReturns$TSMOM,xlab = "XMOM(3,1)",ylab = "TSMOM(3,1)")
abline(XMOM_TSMOMregession,col = 'blue',lwd = 3)

XMOM_DOLregession <- lm(XMOM_3_1~DOL,data = FactorReturns)
#summary(XMOM_TSMOMregession)
plot(FactorReturns$XMOM_3_1,FactorReturns$DOL,xlab = "XMOM(3,1)",ylab = "DOL")
abline(XMOM_DOLregession,col = 'blue',lwd = 3)

XMOM_HMLregession <- lm(XMOM_3_1~HML,data = FactorReturns)
#summary(XMOM_TSMOMregession)
plot(FactorReturns$XMOM_3_1,FactorReturns$HML,xlab = "XMOM(3,1)",ylab = "HML")
abline(XMOM_HMLregession,col = 'blue',lwd = 3)

XMOM_VALregession <- lm(XMOM_3_1~VAL,data = FactorReturns)
#summary(XMOM_TSMOMregession)
plot(FactorReturns$XMOM_3_1,FactorReturns$VAL,xlab = "XMOM(3,1)",ylab = "VAL")
abline(XMOM_VALregession,col = 'blue',lwd = 3)


#qqplots factors
par(mfrow=c(2,2)) 
qqnorm(FactorReturns$TSMOM,main = 'TSMOM Q-Q Plot', ylab = 'Excess Returns')
qqline(FactorReturns$TSMOM,col='red')

qqnorm(FactorReturns$DOL,main = 'DOL Q-Q Plot', ylab = 'Excess Returns')
qqline(FactorReturns$DOL,col='red')

qqnorm(FactorReturns$HML,main = 'HML Q-Q Plot', ylab = 'Excess Returns')
qqline(FactorReturns$HML,col='red')

qqnorm(FactorReturns$VAL,main = 'VAL Q-Q Plot', ylab = 'Excess Returns')
qqline(FactorReturns$VAL,col='red')

#PCA plot
FactorCor <- cor(FactorReturns[2:6])
pcaFactors <- princomp(FactorCor)
plot(pcaFactors,main = "Principal Component Analysis")
pcaFactors$loadings

#BREAKPOINTS IN MOM RETURNS 
library('strucchange')
library(tseries)

TS_XMOM_3_1 <- ts(cumsum(Returns3_1$TotalMom),start = c(1999,4),end=c(2020,12),frequency = 12)
BP_XMOM_3_1 <- breakpoints(TS_XMOM_3_1~c(1:nrow(Returns3_1)),h=round(nrow(Returns3_1)*0.15),breaks = 3)
add_BPi <- efp(TS_XMOM_3_1~breakfactor(BP_XMOM_3_1), type ="OLS-CUSUM")
plot(add_BPi,alpha = 0.05, boundary = TRUE)

plot(Returns3_1$Dates,cumsum(Returns3_1$TotalMom),type = 'l',xlab = 'Date',ylab = 'Cumulative Excess Return')
abline(v=c(FactorReturns$Dates[BP_XMOM_3_1$breakpoints[1]],
           FactorReturns$Dates[BP_XMOM_3_1$breakpoints[2]],
           FactorReturns$Dates[BP_XMOM_3_1$breakpoints[3]]),
            col=c("red","red","red"),lty=3, lwd=1)

TS_XMOM_1_1 <- ts(cumsum(Returns1_1$TotalMom),start = c(1999,2),end=c(2020,12),frequency = 12)
BP_XMOM_1_1 <- breakpoints(TS_XMOM_1_1~c(1:nrow(Returns1_1)),h=round(nrow(Returns1_1)*0.15))
BP_XMOM_1_1

padd_BPi <- efp(TS_XMOM_1_1~breakfactor(BP_XMOM_1_1), type ="OLS-CUSUM")
plot(add_BPi,alpha = 0.05, boundary = TRUE)

plot(Returns3_1$Dates,cumsum(Returns3_1$TotalMom),type = 'l',xlab = 'Date',ylab = 'Cumulative Excess Return')
abline(v=c(FactorReturns$Dates[BP_XMOM_3_1$breakpoints[1]],
           FactorReturns$Dates[BP_XMOM_3_1$breakpoints[2]],
           FactorReturns$Dates[BP_XMOM_3_1$breakpoints[3]]),
       col=c("red","red","red"),lty=3, lwd=1)

