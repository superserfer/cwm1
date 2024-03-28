library(tidyverse)
library(MASS)
library(future.apply)
start_time <- Sys.time()

# Set up parallel processing
plan(multisession)

# Read Data
zone1_raw <- read.csv("out_2.csv")

# Data Wrangling
zone1 <- zone1_raw %>% 
  filter(Date != "") %>% 
  mutate(
    datetime = ymd_hm(paste(Date, Uhrzeit, sep = " ")), 
    masse = m..kg., 
    velocity = v..m.s.) %>% 
  arrange(datetime) %>% 
  dplyr::select(datetime, masse, velocity)

# Data Wrangling for Time Difference
time_diff <- data.frame(
  stunden = as.numeric(diff(zone1$datetime))
)

# Calc Distribution
fit_exp_masse <- fitdistr(zone1$masse, "exponential")
fit_norm_velocity <- fitdistr(zone1$velocity, "normal")
fit_exp_velocity <- fitdistr(zone1$velocity, "exponential")
fit_norm_time_diff <- fitdistr(time_diff$stunden, "normal")
fit_exp_time_diff <- fitdistr(time_diff$stunden, "exponential")

# Generate Data from Exponential Distribution
set.seed(4321)
amount <- 1e+07
zone1_generated <- data.frame(
  masse = rexp(amount, rate = fit_exp_masse$estimate[1]) %>% ceiling,
  velocity = rnorm(amount, mean = fit_norm_velocity$estimate[1], sd = fit_norm_velocity$estimate[2]),
  time_diff_stunden = rnorm(amount, mean = fit_norm_time_diff$estimate[1], sd = fit_norm_time_diff$estimate[2])
) %>% 
  mutate(kin_energy = masse * velocity * velocity * 0.5 / 1000,)

# Function to check if net exceeds maximum
has_net_exceed_maxium <- function(df, index, masse, time) {
  if (index == 1) {
    return(FALSE)
  }
  time <- time + abs(df[index, 3])
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
  if (zone1_generated[i, 4] >= 1200) {
    count <- count + 1
  }
  if (has_net_exceed_maxium(zone1_generated, i, 0, 0) && zone1_generated[i, 4] >= 600) {
    count <- count + 1
  }
  return(count)
}) %>% 
  unlist() %>% 
  sum()

# Calculate simulation time
simulation_time <- zone1_generated %>% 
  summarise(
    years = sum(abs(time_diff_stunden)) / 24 / 365
  )

# Print results
print("Steinschläge gesammt:")
print(amount)

print("Steinschläge die durch das Netzkommen:")
print(steinschlaege)

print("Simuliertezeit in Jahren:")
print(simulation_time$years)

end_time <- Sys.time()
print("Dauer:")
print(end_time - start_time)

# 4s the time an car is in the dangerzone, 600the amount of cars per day
prob_auto_presenc <- 10 * 600 / 24 / 60 / 60
prob_net_fails <-  steinschlaege / amount


print("Chance pro Steinschlag")
print(prob_net_fails * prob_auto_presenc)

steinschlaege_per_year <- amount / simulation_time$years
print("Mean Steinschläge pro Jahr")
print(steinschlaege_per_year)

print("Chance pro Jahr")
print(sprintf("%.10f", steinschlaege_per_year * prob_auto_presenc * prob_net_fails))

