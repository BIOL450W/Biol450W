pred <-read.csv("Powdermill_2019.csv", header=TRUE)

names(pred)
library(dplyr)
library(ggplot2)
pred_summary <- pred  # the names of the new data frame and the data frame to be summarised
  group_by(hab)    # the grouping variable
  summarise(mean_pred = mean(predated),  # calculates the mean of each group
            sd_pred = sd(predated), # calculates the standard deviation of each group
            n_pred = n(),  # calculates the sample size per group
            SE_pred = sd(predated)/sqrt(n())) # calculates the standard error of each group

PredPlot <- ggplot(pred_summary, aes(hab, mean_pred)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_pred - SE_pred, ymax = mean_pred + SE_pred), width=0.2)

PredPlot + labs(y="seed predates ± SE", x = "Habitat") + theme_set(theme_classic(base_size = 18)) 


ggplot(pred_summary, aes(x=hab, y=mean_pred)) +
  geom_bar(stat="identity", fill="blue", alpha=0.7) +  
  geom_errorbar( aes( x=hab,ymin=mean_pred - SE_pred, ymax=mean_pred + SE_pred), width=0.2, colour="orange", alpha=1.0, size=1.3) +
  theme_set(theme_classic(base_size = 10)) +  labs(y="seed predation ± SE", x = "Habitat") 
