rm(list = ls())
library(tidyverse)
library(ggplot2)
data <- read.csv("dataset.csv") %>% 
  filter(!is.na(SEED)) %>% 
  select(TEAM, ADJOE, SEED) %>%
  rename(team = "TEAM", adjusted_off = "ADJOE", seed = "SEED")
N <- nrow(data)
pop_mean <- mean(data$adjusted_off)
pop_var <- var(data$adjusted_off)
pop_sd <- sqrt(pop_var)
se <- pop_sd/sqrt(N)
alpha <- 0.05
z <- qnorm(alpha/2, lower.tail = FALSE)
margin_of_error <- se * z

histogram <- data %>% 
  ggplot(aes(x = adjusted_off)) +
  geom_histogram(bins = 20)
histogram

# determining sample size using pre-specified estimation error
n <- (z * pop_sd/margin_of_error)^2/
  (1 + (z * pop_sd/margin_of_error)^2/N)

# SRS without replacement
set.seed(10)
sample_idx_srs <- sample(1:N, n, replace=FALSE)
sample_srs <- data[sample_idx_srs, ]
# normality test
qqnorm(sample_srs$adjusted_off)
qqline(sample_srs$adjusted_off, col="red")
shapiro.test(sample_srs$adjusted_off)

# we know that our sample data is not significantly different
# from normal distribution
mean_srs <- mean(sample_srs$adjusted_off)
var_srs <- var(sample_srs$adjusted_off)
sd_srs <- sqrt(var_srs)
var_ybar_srs <- (N - n) * pop_var/(n * N)
se_ybar_srs <- sqrt(var_ybar_srs)
histogram_srs <- sample_srs %>% 
  ggplot(aes(x = adjusted_off)) +
  geom_histogram(bins = 20)
histogram_srs

# stratified random sampling with allocation
# stratum 1: seed 1-5, stratum 2: seed 6-10
# stratum 3: seed 11-16
data_strat_1 <- data[data$seed >= 1 & data$seed <= 5, ]
data_strat_2 <- data[data$seed >= 6 & data$seed <= 10, ]
data_strat_3 <- data[data$seed >= 11 & data$seed <= 16, ]
rownames(data_strat_1) <- 1:nrow(data_strat_1) 
rownames(data_strat_2) <- 1:nrow(data_strat_2) 
rownames(data_strat_3) <- 1:nrow(data_strat_3) 

pop_mean_strat_1 <- mean(data_strat_1$adjusted_off)
pop_mean_strat_2 <- mean(data_strat_2$adjusted_off)
pop_mean_strat_3 <- mean(data_strat_3$adjusted_off)

pop_var_strat_1 <- var(data_strat_1$adjusted_off)
pop_var_strat_2 <- var(data_strat_2$adjusted_off)
pop_var_strat_3 <- var(data_strat_3$adjusted_off)

pop_sd_strat_1 <- sqrt(pop_var_strat_1)
pop_sd_strat_2 <- sqrt(pop_var_strat_2)
pop_sd_strat_3 <- sqrt(pop_var_strat_3)

N1 <- nrow(data_strat_1)
N2 <- nrow(data_strat_2)
N3 <- nrow(data_strat_3)
n1 <- N1/N * n
n2 <- N2/N * n
n3 <- N3/N * n

sample_idx_prop_1 <- sample(1:N1, n1, replace=FALSE)
sample_idx_prop_2 <- sample(1:N2, n2, replace=FALSE)
sample_idx_prop_3 <- sample(1:N3, n3, replace=FALSE)
sample_prop_1 <- data_strat_1[sample_idx_prop_1, ]
sample_prop_2 <- data_strat_2[sample_idx_prop_2, ]
sample_prop_3 <- data_strat_3[sample_idx_prop_3, ]

mean_prop_1 <- mean(sample_prop_1$adjusted_off)
mean_prop_2 <- mean(sample_prop_2$adjusted_off)
mean_prop_3 <- mean(sample_prop_3$adjusted_off)

yst_prop <- 1/N * (mean_prop_1 * N1 + mean_prop_2 * N2 + mean_prop_3 * N3)
var_yst_prop <- (N - n)*(N1/N * pop_var_strat_1 + N2/N * pop_var_strat_2 + N3/N * pop_var_strat_3)/(N*n)
se_yst_prop <- sqrt(var_yst_prop)

# neyman allocation
n1_neyman <- round((n * N1 * pop_sd_strat_1)/(N1 * pop_sd_strat_1 + N2 * pop_sd_strat_2 + N3 * pop_sd_strat_3))
n2_neyman <- round((n * N2 * pop_sd_strat_2)/(N1 * pop_sd_strat_1 + N2 * pop_sd_strat_2 + N3 * pop_sd_strat_3))
n3_neyman <- round((n * N3 * pop_sd_strat_3)/(N1 * pop_sd_strat_1 + N2 * pop_sd_strat_2 + N3 * pop_sd_strat_3))

sample_idx_neyman_1 <- sample(1:N1, n1_neyman, replace=FALSE)
sample_idx_neyman_2 <- sample(1:N2, n2_neyman, replace=FALSE)
sample_idx_neyman_3 <- sample(1:N3, n3_neyman, replace=FALSE)
sample_neyman_1 <- data_strat_1[sample_idx_neyman_1, ]
sample_neyman_2 <- data_strat_2[sample_idx_neyman_2, ]
sample_neyman_3 <- data_strat_3[sample_idx_neyman_3, ]

mean_neyman_1 <- mean(sample_neyman_1$adjusted_off)
mean_neyman_2 <- mean(sample_neyman_2$adjusted_off)
mean_neyman_3 <- mean(sample_neyman_3$adjusted_off)

yst_neyman <- 1/N * (mean_neyman_1 * N1 + mean_neyman_2 * N2 + mean_neyman_3 * N3)
var_yst_neyman <- (N1/N * pop_sd_strat_1 + N2/N * pop_sd_strat_2 + N3/N * pop_sd_strat_3)^2/n - (N1/N * pop_var_strat_1 + N2/N * pop_var_strat_2 + N3/N * pop_var_strat_3)/N
se_yst_neyman <- sqrt(var_yst_neyman)
