
cact <-read.csv(text=getURL("https://raw.githubusercontent.com/BIOL450W/Biol450W/master/Island_Cactus.csv"), header=T)
head(cact)

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
