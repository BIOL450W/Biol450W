
cact <-read.csv(text=getURL("https://raw.githubusercontent.com/BIOL450W/Biol450W/master/Island_Cactus.csv"), header=T)
head(cact)


boxplot(Flowers_fruits~Site, data=cact, 
        col=(c("blue")),
        main="Fecundity", xlab="Site", cex.axis = 0.6)


boxplot(Height~Site, data=cact, 
        col=(c("orange")),
        main="Fecundity", xlab="Site", cex.axis = 0.6)



mean.fec = tapply(Height,Site,mean)
sd.fec = tapply(Height,Site, sd)
n.fec = tapply(Height,Site, length)
sem.fec = tapply(Height, Site, sd)/sqrt(tapply(Height,Site, length))


mean.fec
sd.fec
n.fec
sem.fec

mids = barplot(mean.fec, xlab = "Site", ylab = "cactus height", ylim = c(0,250), col=(c("orange")))
arrows(mids, mean.fec - sem.fec, mids, mean.fec + sem.fec, code = 3, angle = 90, length = 0.1)
text(mids, 10, paste("n =", n.fec))


attach(cact_summary)
attach(cact)
library(sjPlot)
library(sjmisc)
library(ggplot2)

theme_set(theme_sjplot())

# make categorical
cact$Site <- to_factor(cact$Site)

# fit model with interaction
fit <- lm(Flowers_fruits ~ Site * Height, data = cact)
summary(fit)
plot_model(fit, type = "pred", terms = c("Height", "Site"))