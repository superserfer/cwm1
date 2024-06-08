library(tidyverse)
library(fitdistrplus)
library(future.apply)

# Set up parallel processing
start_time_1 <- Sys.time()
plan(multisession)

# Read Data
zone_raw_1 <- read.csv("out_1.csv")

# Data Wrangling
zone_1 <- zone_raw %>% 
  filter(Date != "") %>% 
  mutate(
    datetime = ymd_hm(paste(Date, Uhrzeit, sep = " ")), 
    masse = m..kg., 
    velocity = v..m.s.) %>% 
  arrange(datetime) %>% 
  filter(masse > 0) %>% 
  dplyr::select(datetime, masse, velocity)

# Data Wrangling for Time Difference
time_diff_1 <- data.frame(
  stunden = as.numeric(diff(zone$datetime))
)

# Calc Distribution
mass.weibull_1 <- fitdist(zone$masse, "gamma")
velocity.norm_1 <- fitdist(zone$velocity, "gamma")
time_diff.expo_1 <- fitdist(time_diff$stunden, "gamma")

# Generate Data from Exponential Distribution
set.seed(4321)
amount <- 1e+07
# if you want to simulate more than 1e+07 you have to give more RAM to with this statement 
# options(future.globals.maxSize = really big number) -> tested with 20 GB
masse = rgamma(amount, shape = mass.gamm$estimate[1], rate = mass.gamm$estimate[2])
velocity = rgamma(amount, shape = velocity.gamm$estimate[1], rate = velocity.gamm$estimate[2])
time_diff_stunden = rgamma(amount, shape = time_diff.gamm$estimate[1], rate = time_diff.gamm$estimate[2])
kin_energy = masse * velocity * velocity * 0.5 / 1000

# Function to check if net exceeds maximum
has_net_exceed_maxium <- function(index, mass_net, time_since_emptying) {
  if (index == 1) {
    return(FALSE)
  }
  time_since_emptying <- time_since_emptying + time_diff_stunden[index]
  if (time_since_emptying > 24) {
    return(FALSE)
  }
  mass_net <- mass_net + masse[index - 1]
  if (mass_net >= 2000) {
    return(TRUE)
  }
  return(has_net_exceed_maxium(index-1, mass_net, time_since_emptying))
}

# Paralleled loop
steinschlaege <- future_lapply(1:amount, function(i) {
  return(kin_energy[i] >= 1200 || (kin_energy[i] >= 600 && has_net_exceed_maxium(i, 0, 0)))
}) %>% 
  unlist() %>% 
  sum()

# Calculate simulation time
simulation_time <- sum(time_diff_stunden) / 24 / 365

# 10s the time an car is in the danger zone, 600the amount of cars per day -> Check for more Correct Number
prob_auto_presenc <- 10 * 600 / (24 * 60 * 60)
prob_net_fails <-  steinschlaege / amount
steinschlaege_per_year <- amount / simulation_time
prob_per_year <- steinschlaege_per_year * prob_auto_presenc * prob_net_fails
end_time <- Sys.time()

# Print results
print(paste("Dauer", end_time - start_time, sep = ": "))
print(paste("Steinschläge gesammt",amount, sep = ": "))
print(paste("Steinschläge die durch das Netzkommen", steinschlaege, sep = ": "))
print(paste("Simuliertezeit in Jahren", simulation_time, sep=": "))
print(paste("Chance pro Steinschlag", sprintf("%.10f", prob_net_fails * prob_auto_presenc), sep = ": "))
print(paste("Mean Steinschläge pro Jahr", steinschlaege_per_year, sep = ": "))
print(paste("Trefferwahrscheinlickkeit in einem Jahr",sprintf("%.10f", prob_per_year), sep = ": "))
