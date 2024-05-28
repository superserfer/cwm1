library(tidyverse)
library(fitdistrplus)

# Read Data
zone_raw <- read.csv("out_2.csv")

# Data Wrangling
zone <- zone_raw %>% 
  filter(Date != "",) %>% 
  mutate(
    datetime = ymd_hm(paste(Date, Uhrzeit, sep = " ")), 
    masse = m..kg., 
    velocity = v..m.s.) %>% 
  arrange(datetime) %>% 
  filter(masse > 0) %>% 
  dplyr::select(datetime, masse, velocity)

# Data Wrangling for Time Difference
time_diff <- data.frame(
  stunden = as.numeric(diff(zone$datetime))
)

# Calc Distribution
mass.norm <- fitdist(zone$masse, "norm")
mass.expo <- fitdist(zone$masse, "exp")
mass.unif <- fitdist(zone$masse, "unif")
mass.lnorm <- fitdist(zone$masse, "lnorm")
mass.gamm <- fitdist(zone$masse, "gamma")
mass.legend <- c("Normal", "Exponential", "Unif", "Lognormal", "Gamma")

velocity.norm <- fitdist(zone$velocity, "norm")
velocity.expo <- fitdist(zone$velocity, "exp")
velocity.unif <- fitdist(zone$velocity, "unif")
velocity.lnorm <- fitdist(zone$velocity, "lnorm")
velocity.gamm <- fitdist(zone$velocity, "gamma")
velocity.legend <- c("Normal", "Exponential", "Unif", "Lognormal", "Gamma")


time_diff.norm <- fitdist(time_diff$stunden, "norm")
time_diff.expo <- fitdist(time_diff$stunden, "exp")
time_diff.unif <- fitdist(time_diff$stunden, "unif")
time_diff.lnorm <- fitdist(time_diff$stunden, "lnorm")
time_diff.gamm <- fitdist(time_diff$stunden, "gamma")
time_diff.legend <- c("Normal", "Exponential", "Unif", "Lognormal", "Gamma")

# Plot different Distributions for mass
denscomp(list(mass.norm, mass.expo, mass.unif, mass.lnorm, mass.gamm), legendtext = mass.legend, plotstyle = "ggplot")
descdist(zone$mass, boot = 10000, discrete = FALSE)

# Die passen am besten (gamma)
plot(mass.expo)
plot(mass.gamm)

# Plot different Distributions for velocity
denscomp(list(velocity.norm, velocity.expo, velocity.unif, velocity.lnorm, velocity.gamm), legendtext = velocity.legend, plotstyle = "ggplot")
descdist(zone$velocity, boot = 10000, discrete = FALSE)

# Die passen am besten (gamma)
plot(velocity.norm)
plot(velocity.lnorm)
plot(velocity.gamm)

# Plot different Distributions for velocity
denscomp(list(time_diff.norm, time_diff.expo, time_diff.unif, time_diff.lnorm, time_diff.gamm), legendtext = time_diff.legend, plotstyle = "ggplot")
descdist(time_diff$stunden, boot = 10000)

# Die passen am besten (gamma)
plot(time_diff.gamm)

print(velocity.gamm$estimate)


set.seed(54321)
amount <- 1e+06
zone_gen <- data.frame(
  masse = rgamma(amount, shape = mass.gamm$estimate[1], rate = mass.gamm$estimate[2]),
  velocity = rgamma(amount, shape = velocity.gamm$estimate[1], rate = velocity.gamm$estimate[2]),
  time_diff_stunden = rgamma(amount, shape = time_diff.gamm$estimate[1], rate = time_diff.gamm$estimate[2])
) %>% 
  mutate(kin_energy = masse * velocity * velocity * 0.5 / 1000,)

hist(zone_gen$time_diff_stunden)
