library(tidyverse)
library(fitdistrplus)

# Read Data
zone_raw_1 <- read.csv("out_1.csv")

# Data Wrangling
zone_1 <- zone_raw_1 %>% 
  filter(Datum != "",) %>% 
  mutate(
    datetime = ymd_hm(paste(Datum, Uhrzeit, sep = " ")), 
    masse = Masse..kg., 
    velocity = Geschwindigkeit..m.s.) %>% 
  arrange(datetime) %>% 
  filter(masse > 0) %>% 
  dplyr::select(datetime, masse, velocity)

# Data Wrangling for Time Difference
time_diff_1 <- data.frame(
  stunden = as.numeric(diff(zone_1$datetime)))

# Calc Distribution
mass.norm_1 <- fitdist(zone_1$masse, "norm")
mass.unif_1 <- fitdist(zone_1$masse, "unif")
mass.lnorm_1 <- fitdist(zone_1$masse, "lnorm")
mass.legend_1 <- c("Normal", "Unif", "Lognormal")

velocity.norm_1 <- fitdist(zone_1$velocity, "norm")
velocity.expo_1 <- fitdist(zone_1$velocity, "exp")
velocity.unif_1 <- fitdist(zone_1$velocity, "unif")
velocity.lnorm_1 <- fitdist(zone_1$velocity, "lnorm")
velocity.gamm_1 <- fitdist(zone_1$velocity, "gamma")
velocity.legend_1 <- c("Normal", "Exponential", "Unif", "Lognormal", "Gamma")


time_diff.norm_1 <- fitdist(time_diff_1$stunden, "norm")
time_diff.unif_1 <- fitdist(time_diff_1$stunden, "unif")
time_diff.legend_1 <- c("Normal", "Unif")

# Plot different Distributions for mass
denscomp(list(mass.norm_1, mass.unif_1, mass.lnorm_1), legendtext = mass.legend_1, plotstyle = "ggplot")
descdist(zone_1$mass, boot = 10000, discrete = FALSE)

# Die passen am besten (gamma oder exponential)

"Gamma oder exponential
"
# Plot different Distributions for velocity
denscomp(list(velocity.norm_1, velocity.expo_1, velocity.unif_1, velocity.lnorm_1, velocity.gamm_1), legendtext = velocity.legend_1, plotstyle = "ggplot")
descdist(zone_1$velocity, boot = 10000, discrete = FALSE)

# Die passen am besten (Normalverteiilung)

"Normalverteiilung"

# Plot different Distributions for velocity
denscomp(list(time_diff.norm_1, time_diff.unif_1), legendtext = time_diff.legend_1, plotstyle = "ggplot")
descdist(time_diff_1$stunden, boot = 10000)

# Die passen am besten (Gamma)

"gamma"


#der Teil muss im steinschlag 1 nochmals überschaut werden

set.seed(54321)
amount <- 1e+06
zone_gen_1 <- data.frame(
  masse = rgamma(amount, shape = mass.gamm_1$estimate[1], rate = mass.gamm_1$estimate[2]),
  velocity = rgamma(amount, shape = velocity.gamm_1$estimate[1], rate = velocity.gamm_1$estimate[2]),
  time_diff_stunden = rgamma(amount, shape = time_diff.gamm_1$estimate[1], rate = time_diff.gamm_1$estimate[2])
) %>% 
  mutate(kin_energy = masse * velocity * velocity * 0.5 / 1000,)

hist(zone_gen_1$time_diff_stunden)



