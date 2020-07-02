AGG <- data$AGG
VAW <- data$VAW
IWN <- data$IWN
SPY <- data$SPY

#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
# model kontrol, følger normal fordeling, z = antal std væk
par(mfrow= c(2,2))

qqnorm(AGG,main='Vurdering af normalfordelingsantagelse for AGG',
       xlab='z-fraktil',ylab='Afkast')
qqline(AGG, col = "red")

qqnorm(VAW,main='Vurdering af normalfordelingsantagelse for VAW',
       xlab='z-fraktil',ylab='Afkast')
qqline(VAW, col = "red")

qqnorm(IWN,main='Vurdering af normalfordelingsantagelse for IWN',
       xlab='z-fraktil',ylab='Afkast')
qqline(IWN, col = "red")

qqnorm(SPY,main='Vurdering af normalfordelingsantagelse for SPY',
       xlab='z-fraktil',ylab='Afkast')
qqline(SPY, col = "red")

# modeller, varians og mu ses i tabel fra opgave b
gg1 <- ggplot(data = data, aes(x = AGG)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = 2, color = 1) +
  #facet_wrap(~stock_name, nrow = 2, scale = "free") +
  xlab("Afkast") + ylab("Tæthed") + ggtitle("Normalfordeling for AGG") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))+
  stat_function(fun = dnorm, args = list(mean = mean(data$AGG), sd = sd(data$AGG)), size = 2)

gg2 <- ggplot(data = data, aes(x = VAW)) +
  geom_histogram(aes(y = ..density..), bins = 50, , fill = "purple", color = 1) +
  #facet_wrap(~stock_name, nrow = 2, scale = "free") +
  xlab("Afkast") + ylab("Tæthed") + ggtitle("Normalfordeling for VAW") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))+
  stat_function(fun = dnorm, args = list(mean = mean(data$VAW), sd = sd(data$VAW)), size = 2)

gg3 <- ggplot(data = data, aes(x = IWN)) +
  geom_histogram(aes(y = ..density..), bins = 50, , fill = 3, color = 1) +
  #facet_wrap(~stock_name, nrow = 2, scale = "free") +
  xlab("Afkast") + ylab("Tæthed") + ggtitle("Normalfordeling for IWN") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))+
  stat_function(fun = dnorm, args = list(mean = mean(data$IWN), sd = sd(data$IWN)), size = 2)

gg4 <- ggplot(data = data, aes(x = SPY)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = 5, color = 1) +
  #facet_wrap(~stock_name, nrow = 2, scale = "free") +
  xlab("Afkast") + ylab("Tæthed") + ggtitle("Normalfordeling for SPY") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))+
  stat_function(fun = dnorm, args = list(mean = mean(data$SPY), sd = sd(data$SPY)), size = 2)

multiplot(gg1, gg2, gg3, gg4, cols =2)

# ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg

# 95 % konfidens for gennemsnit
t.test(data$AGG, conf.level = 0.95)$conf.int
t.test(data$VAW, conf.level = 0.95)$conf.int
t.test(data$IWN, conf.level = 0.95)$conf.int
t.test(data$SPY, conf.level = 0.95)$conf.int

conf_mu <- function(data){
  k <- c(mean(data) - qt(0.975, df = length(data)-1)*sd(data)/sqrt(length(data)),
         mean(data) + qt(0.975, df = length(data)-1)*sd(data)/sqrt(length(data)))
  return(k)
}

conf_mu(data$AGG)*10^4
conf_mu(data$VAW)*10^4
conf_mu(data$IWN)*10^4
conf_mu(data$SPY)*10^4

# konfidens for varians
alpha <- 1-0.95
conf_sigma <- function(data){
  k <- c((length(data)-1)*var(data)/qchisq(1-alpha/2, df = length(data)-1),
         (length(data)-1)*var(data)/qchisq(alpha/2, df = length(data)-1))
  return(k)
}

conf_sigma(data$AGG)*10^4
conf_sigma(data$VAW)*10^4
conf_sigma(data$IWN)*10^4
conf_sigma(data$SPY)*10^4

# hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
set.seed(7285)
k <- 10000
n <- 454


bootAGG <- replicate(k, sample(AGG, n, replace = TRUE))
bootVAW <- replicate(k, sample(VAW, n, replace = TRUE))
bootIWN <- replicate(k, sample(IWN, n, replace = TRUE))
bootSPY <- replicate(k, sample(SPY, n, replace = TRUE))

# middelværdis konfidens
options(scipen = 999)
bootmuAGG <- apply(bootAGG, 2, mean)
bootmuVAW <- apply(bootVAW, 2, mean)
bootmuIWN <- apply(bootIWN, 2, mean)
bootmuSPY <- apply(bootSPY, 2, mean)

print(quantile(bootmuAGG, c(0.025, 0.975))*10^4, digits = 4)
print(quantile(bootmuVAW, c(0.025, 0.975))*10^4, digits = 4)
print(quantile(bootmuIWN, c(0.025, 0.975))*10^4, digits = 4)
print(quantile(bootmuSPY, c(0.025, 0.975))*10^4, digits = 4)

# konfidens af varians
bootvarAGG <- apply(bootAGG, 2, var)
bootvarVAW <- apply(bootVAW, 2, var)
bootvarIWN <- apply(bootIWN, 2, var)
bootvarSPY <- apply(bootSPY, 2, var)

print(quantile(bootvarAGG, c(0.025, 0.975))*10^4, digits = 4)
print(quantile(bootvarVAW, c(0.025, 0.975))*10^4, digits = 4)
print(quantile(bootvarIWN, c(0.025, 0.975))*10^4, digits = 4)
print(quantile(bootvarSPY, c(0.025, 0.975))*10^4, digits = 4)

options(scipen = 0)

#iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

t.test(data$AGG, df = length(data$AGG)-1, mu = 0)
t.test(data$VAW, df = length(data$VAW)-1, mu = 0)
t.test(data$IWN, df = length(data$IWN)-1, mu = 0)
t.test(data$SPY, df = length(data$SPY)-1, mu = 0)


