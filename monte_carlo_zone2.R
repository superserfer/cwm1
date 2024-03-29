library(tidyverse)
library(fitdistrplus)
library(future.apply)

# Set up parallel processing
start_time <- Sys.time()
plan(multisession)

# Read Data
zone_raw <- read.csv("out_2.csv")

# Data Wrangling
zone <- zone_raw %>% 
  filter(Date != "") %>% 
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
mass.gamm <- fitdist(zone$masse, "gamma")
velocity.gamm <- fitdist(zone$velocity, "gamma")
time_diff.gamm <- fitdist(time_diff$stunden, "gamma")

# Generate Data from Exponential Distribution
set.seed(4321)
amount <- 1e+07
zone_gen <- data.frame(
  masse = rgamma(amount, shape = mass.gamm$estimate[1], rate = mass.gamm$estimate[2]),
  velocity = rgamma(amount, shape = velocity.gamm$estimate[1], rate = velocity.gamm$estimate[2]),
  time_diff_stunden = rgamma(amount, shape = time_diff.gamm$estimate[1], rate = time_diff.gamm$estimate[2])
) %>% 
  mutate(kin_energy = masse * velocity * velocity * 0.5 / 1000,)

# Function to check if net exceeds maximum
has_net_exceed_maxium <- function(df, index, masse, time) {
  if (index == 1) {
    return(FALSE)
  }
  time <- time + df[index, 3]
  if (time > 24) {
    return(FALSE)
  }
  masse <- masse + df[index - 1, 1]
  if (masse >= 2000) {
    return(TRUE)
  }
  return(has_net_exceed_maxium(df, index-1, masse, time))
}

# Parallelized loop
steinschlaege <- future_lapply(1:amount, function(i) {
  count <- 0
  if (zone_gen[i, 4] >= 1200) {
    count <- count + 1
  }
  if (has_net_exceed_maxium(zone_gen, i, 0, 0) && zone_gen[i, 4] >= 600) {
    count <- count + 1
  }
  return(count)
}) %>% 
  unlist() %>% 
  sum()

# Calculate simulation time
simulation_time <- zone_gen %>% 
  summarise(
    years = sum(time_diff_stunden) / 24 / 365
  )

# 10s the time an car is in the danger zone, 600the amount of cars per day -> Check for more Correct Number
prob_auto_presenc <- 10 * 600 / (24 * 60 * 60)
prob_net_fails <-  steinschlaege / amount
steinschlaege_per_year <- amount / simulation_time$years
prob_per_year <- steinschlaege_per_year * prob_auto_presenc * prob_net_fails
end_time <- Sys.time()

# Print results
print(paste("Dauer", end_time - start_time, sep = ": "))
print(paste("Steinschläge gesammt",amount, sep = ": "))
print(paste("Steinschläge die durch das Netzkommen", steinschlaege, sep = ": "))
print(paste("Simuliertezeit in Jahren", simulation_time$years, sep=": "))
print(paste("Chance pro Steinschlag", sprintf("%.10f", prob_net_fails * prob_auto_presenc), sep = ": "))
print(paste("Mean Steinschläge pro Jahr", steinschlaege_per_year, sep = ": "))
print(paste("Trefferwahrscheinlickkeit in einem Jahr",sprintf("%.10f", prob_per_year), sep = ": "))
