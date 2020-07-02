library("ggplot2")
library("tidyr")

# statistics
stats <- function(x){
  k = 10^4
  obs <- length(x)
  mean <- mean(x)*k
  var <- var(x)*k
  sd <- sd(x)*k
  sum <- summary(x)*k
  
  d <- c("Obs:",obs, "mean:",mean, "var:", var, "sd:", sd)
  
  
  print(d, digits = 6)
  print(sum)
}

options(scipen=999)
stats(data$SPY)
options(scipen=0)

# Plots

# transform data
etf4 <- data[, c("AGG", "VAW", "IWN", "SPY")]
df <- gather(etf4, stock_name, stock_data)

# boxplots
ggplot(data = df, aes(y=stock_data, x = stock_name, fill = stock_name))+
  geom_boxplot() +
  xlab("Navn på ETF") + ylab("Afkast") + ggtitle("Boxplot for fire udvalgte ETF'er") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))


#density plots
ggplot(data = df, aes(x = stock_data)) +
  geom_histogram(aes(y = ..density.., fill = stock_name), bins = 50) +
  geom_density(color = "black") +
  facet_wrap(~stock_name, nrow = 2, scale = "free") +
  xlab("Afkast") + ylab("Tæthed") + ggtitle("Empiriske tæthedsfunktioner \n for de fire udvalgte ETF'er") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))


#ecdf
ggplot(data = df, aes(x = stock_data)) +
  stat_ecdf(aes(colour = stock_name)) +
  facet_wrap(~stock_name, nrow = 2, scale = "free") +
  xlab("Afkast") + ylab("Akkumuleret tæthed") + ggtitle("Empiriske fordelingsfunktioner \n for de fire udvalgte ETF'er") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))

ggplot(data = df, aes(x = stock_data)) +
  stat_ecdf(aes(colour = stock_name)) +
  xlab("Afkast") + ylab("Akkumuleret tæthed") + ggtitle("Empiriske fordelingsfunktioner \n for de fire udvalgte ETF'er") +
  guides(fill=guide_legend(title="ETF")) +
  theme(plot.title = element_text(hjust = .5))

