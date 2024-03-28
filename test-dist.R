library(tidyverse)
library(fitdistrplus)


#set.seed(1234)
x1 <- rbinom(n=100, 100, 0.5)
fitdist(x1, "norm")
plotdist(x1,demp = TRUE)

fg <- fitdist(x1, "gamma")
fln <- fitdist(x1, "lnorm")
n <- fitdist(x1, "norm")
poiss <- fitdist(x1, "pois")
exp <- fitdist(x1, "exp")
bi <- fitdist(x1, "nbinom")
plot.legend <- c("normal", "exp", "poisson")
denscomp(list(n,exp, poiss), legendtext = plot.legend, plotstyle = "ggplot")