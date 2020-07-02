#jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj

#aflæs fra tabel max og min gennemsnitsværdier, SPY, AGG

etf_high <- data$VAW
etf_low <- data$AGG

cor(etf_high, etf_low)

t.test(etf_low,etf_high, mu = 0, conf.level = 0.95)
#t.test(etf_high, etf_low, paired = TRUE)
 # signifikans = 5% -> kan ikke forkaste da p-værdi er højere

# kritiske værdier
qt(0.025, df=477.83)
qt(0.975, df=477.83)
mean(etf_high)-mean(etf_low)

(qt(0.025, df=477.83) < mean(etf_high)-mean(etf_low)) & (mean(etf_high)-mean(etf_low) < qt(0.975, df=477.83)) # true bekræftning
                                                                                   
