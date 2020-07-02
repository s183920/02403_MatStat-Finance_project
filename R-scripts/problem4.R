# read data
setwd("D:/OneDrive/Dokumenter/DTU/Statistik/finans_project/R-scripts")
etfSum <- read.table("finans2_data.csv",header=TRUE, sep=",")
str(etfSum)

#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk

## Bestem den empiriske korrelation for de udvalgte variable og
## undersøg afhængighed

print(cor(etfSum[,2:7], use="everything", method="pearson"), digits = 3)
cov(etfSum$Geo.mean,etfSum$maxTuW)/sd(etfSum$Geo.mean)/sd(etfSum$maxTuW)


## Først trimmes pladsen rundt om selve plottet. Se mere på ?par
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
par(mfrow=c(2,2))

plot(etfSum$Volatility, etfSum$CVaR, pch=16, cex=0.7,
     xlab="Volatilitet [Ugentligt Pct.]",
     ylab="Conditional Value at Risk [Ugentligt Pct.]", cex.lab=0.8,
     main="Sammenhæng mellem Volatilitet og CVaR", cex.main=0.8)

plot(etfSum$Geo.mean, etfSum$maxTuW, pch=16, cex=0.7,
     xlab="Geometric mean [Ugentligt Pct.]",
     ylab="Maximum time under water [Ugentligt Pct.]", cex.lab=0.8,
     main="Sammenhæng mellem Geo.mean og maxTuW", cex.main=0.8)

plot(etfSum$Volatility, etfSum$maxDD, pch=16, cex=0.7,
     xlab="Volatilitet [Ugentligt Pct.]",
     ylab="Maximum Draw Down [Ugentligt Pct.]", cex.lab=0.8,
     main="Sammenhæng mellem Volatilitet og maxDD", cex.main=0.8)

plot(etfSum$maxTuW, etfSum$Volatility, pch=16, cex=0.7,
     xlab="Maximum time under water [Ugentligt Pct.]",
     ylab="Volatilitet [Ugentligt Pct.]", cex.lab=0.8,
     main="Sammenhæng mellem maxTuW og Volatilitet", cex.main=0.8)


# llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
#lineær model
fit <- lm(Geo.mean~VaR, data=etfSum) # vælger den med højest correlation
summary <- summary(fit)

ggplot(data = etfSum, aes(x = VaR, y = Geo.mean))+
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE)


# manuelt udregnet
x <- etfSum$VaR
y <- etfSum$Geo.mean

mux <- mean(x)
muy <- mean(y)

sxx <- sum((x-mux)^2)

beta1 <- sum((y-muy)*(x-mux))/sxx
beta0 <- muy-beta1*mux
  
rss <- sum((y-(beta0+beta1*x))^2)
n <- length(x)

sigma <- rss/(n-2)

#nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
# forklaret varians
summary$r.squared

#forklaret korrelation
sqrt(summary$r.squared)

       