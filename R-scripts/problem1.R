
#make data frame
port <- data[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")]

#kovarians matrice
options(scipen=999)
print(cov(port)*10^4, digits = 3)
options(scipen=0)

# EFTs
AGG <- port$AGG
VAW <- port$VAW
IWN <- port$IWN
SPY <- port$SPY
EWG <- port$EWG
EWW <- port$EWW

# stochastic variables

P1 <- function(a){a*EWG+(1-a)*EWW}
P2 <- function(a){a*AGG+(1-a)*SPY}
P3 <- function(a){a*VAW+(1-a)*IWN}
P4 <- function(a){a*VAW+(1-a)*EWG}
P5 <- function(a){a*VAW+(1-a)*EWW}
P6 <- function(a){a*IWN+(1-a)*EWG}


# theroem 2.60 cov(x,x) = var(x)
varP1 <- function(a){a^2*var(EWG)+(1-a)^2*var(EWW)+2*a*(1-a)*cov(EWG, EWW)}
varP2 <- function(a){a^2*var(AGG)+(1-a)^2*var(SPY)+2*a*(1-a)*cov(AGG, SPY)}
varP3 <- function(a){a^2*var(VAW)+(1-a)^2*var(IWN)+2*a*(1-a)*cov(VAW, IWN)}
varP4 <- function(a){a^2*var(VAW)+(1-a)^2*var(EWG)+2*a*(1-a)*cov(VAW, EWG)}
varP5 <- function(a){a^2*var(VAW)+(1-a)^2*var(EWW)+2*a*(1-a)*cov(VAW, EWW)}
varP6 <- function(a){a^2*var(IWN)+(1-a)^2*var(EWG)+2*a*(1-a)*cov(IWN, EWG)}

# grafer for V(a)
par(mfrow = c(3,2))
curve(varP1, from = 0, to=1, main = "Varians af P1, som funktion af andelen af EWG", 
      xlab = expression(paste("Værdi af ", alpha)), ylab = "Varians af P1")
curve(varP2, from = 0, to=1, main = "Varians af P2, som funktion af andelen af AGG", 
      xlab =expression(paste("Værdi af ", alpha)), ylab = "Varians af P2")
curve(varP3, from = 0, to=1, main = "Varians af P3, som funktion af andelen af VAW", 
      xlab = expression(paste("Værdi af ", alpha)), ylab = "Varians af P3")
curve(varP4, from = 0, to=1, main = "Varians af P4, som funktion af andelen af VAW", 
      xlab = expression(paste("Værdi af ", alpha)), ylab = "Varians af P4")
curve(varP5, from = 0, to=1, main = "Varians af P5, som funktion af andelen af VAW", 
      xlab = expression(paste("Værdi af ", alpha)), ylab = "Varians af P5")
curve(varP6, from = 0, to=1, main = "Varians af P6, som funktion af andelen af IWN", 
      xlab = expression(paste("Værdi af ", alpha)), ylab = "Varians af P6")

# optimixation - a værdier der giver mindst varians
a1 <- optimize(varP1, maximum = FALSE, interval = c(0,1))$minimum
a2 <- optimize(varP2, maximum = FALSE, interval = c(0,1))$minimum
a3 <- optimize(varP3, maximum = FALSE, interval = c(0,1))$minimum
a4 <- optimize(varP4, maximum = FALSE, interval = c(0,1))$minimum
a5 <- optimize(varP5, maximum = FALSE, interval = c(0,1))$minimum
a6 <- optimize(varP6, maximum = FALSE, interval = c(0,1))$minimum

A <- c(t(c(a1, a2, a3, a4, a5, a6)))

# funktionsværdier og varianser for optimale alpha
P <- data.frame(P1(a1),P2(a2),P3(a3),P4(a4),P5(a5),P6(a6))
meanP <-  c(mean(P1(a1)),mean(P2(a2)),mean(P3(a3)),mean(P4(a4)),mean(P5(a5)),mean(P6(a6)))*10^3
min_varP <- c(varP1(a1), varP2(a2), varP3(a3), varP4(a4), varP5(a5), varP6(a6)) *10^4


tabel <- data.frame(A, meanP, min_varP)
