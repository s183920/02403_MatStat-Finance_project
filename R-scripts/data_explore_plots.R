library("ggplot2")
setwd("D:/OneDrive/Dokumenter/DTU/Statistik/finans_project/R-scripts")

# day difference plot
levels_dat_diff <- levels(factor(day_diff, levels = c(min(day_diff):max(day_diff))))

ggplot(data = time_var, aes(x = day_diff)) +
  geom_histogram(bins = length(levels_dat_diff), binwidth = .5, fill = "midnightblue") +
  #ggtitle("Antal dage mellem hver observation i datasættet 'finans1'") +
  xlab("Dage mellem observationer") +
  ylab("Antal observationer") +
  stat_bin(aes(y = ..count.., label = ..count..), color = "red", 
           geom = "text", vjust = -.5, bins = length(levels_dat_diff)) +
  ylim(0, max(table(day_diff)+50))

