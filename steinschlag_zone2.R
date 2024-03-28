library(tidyverse)
library(MASS)

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

# Plot Mass with Exponential Distribution
ggplot(data = zone1, aes(x = masse)) +
  geom_histogram(aes(y = ..density..), fill = "lightgreen") +
  stat_function(fun = dexp, args = list(rate = fit_exp_masse$estimate[1]), color = "red") +
  ylab("Dichte") +
  xlab("Masse") +
  ggtitle("Massenverteilung")

ggplot(data = zone1, aes(x = velocity)) +
  geom_histogram(aes(y = ..density..), fill = "lightgreen") +
  stat_function(fun = dnorm, args = list(mean = fit_norm_velocity$estimate[1], sd = fit_norm_velocity$estimate[2]), color = "black") +
  stat_function(fun = dexp, args = list(rate = fit_exp_velocity$estimate[1]), color = "red") +
  ylab("Dichte") +
  xlab("velocity") +
  ggtitle("Geschwindigkeitsverteilung")

ggplot(data = time_diff, aes(x = stunden)) +
  geom_histogram(aes(y = ..density..), fill = "lightgreen") +
  stat_function(fun = dnorm, args = list(mean = fit_norm_time_diff$estimate[1], sd = fit_norm_time_diff$estimate[2]), color = "black") +
  stat_function(fun = dexp, args = list(rate = fit_exp_time_diff$estimate[1]), color = "red") +
  
  ylab("Dichte") +
  xlab("Zeitunterschied") +
  ggtitle("Zeitunterschied Verteilung")

# Generate Data from Exponential Distribution
set.seed(1234)
amount <- 1000000
zone1_generated <- data.frame(
  masse = rexp(amount, rate = fit_exp_masse$estimate[1]) %>% ceiling,
  velocity = rnorm(amount, mean = fit_norm_velocity$estimate[1], sd = fit_norm_velocity$estimate[2]),
  time_diff_stunden = rnorm(amount,mean = fit_norm_time_diff$estimate[1], sd = fit_norm_time_diff$estimate[2])
) %>% 
  mutate(kin_energy = masse * velocity * velocity * 0.5 / 1000,)

# Plot generated Data
ggplot(data = zone1_generated, aes(x = masse)) +
  geom_histogram(bins = 100) +
  ylab("Anzahl") +
  xlab("Masse") +
  ggtitle("Masse generiert")

ggplot(data = zone1_generated, aes(x = velocity)) +
  geom_histogram() +
  ylab("Anzahl") +
  xlab("Velocity") +
  ggtitle("Geschwindigkeit generiert")

# Plot generated Data
ggplot(data = zone1_generated, aes(x = time_diff_stunden)) +
  geom_histogram() +
  ylab("Anzahl") +
  xlab("Zeit") +
  ggtitle("Zeitunterschied generiert")
