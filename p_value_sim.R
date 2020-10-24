##########################
### P-VALUE SIMULATION ###
##########################

# This simulation demonstrates what happens with p-values of two-group 
# comparison when null is true or not true. It shows what statistical power 

library(patchwork)
library(pwr)
library(tidyverse)


## Tunable parameters

n = 50 # Sample size (per group)
d = 0.2 # Group differences (effect size)

# Computed power for t-test
pwr.t.test(n = n, d = d, sig.level = 0.05)

# Simulate 50000 t-tests when null is true
pvals1 <- NULL
for(i in 1:50000) {

  # Simulate two variables with same mean
  x1 <- rnorm(n, mean = 0, sd = 1)
  x2 <- rnorm(n, mean = 0, sd = 1)
  
  # Take p-value from t-test
  pvals1[i] <- t.test(x1, x2)$p.value
}

# Simulate 50000 t-tests when null is not true
pvals2 <- NULL
for(i in 1:50000) {
  
  # Simulate two variables with different mean 
  x1 <- rnorm(n, mean = 0, sd = 1)
  x2 <- rnorm(n, mean = d, sd = 1)

  # Take p-value from t-test
  pvals2[i] <- t.test(x1, x2)$p.value
}

# Bind p-values into data frame
dat <- data.frame(p1 = pvals1, p2 = pvals2)

# Power observed in simulation
sim_power <- mean(dat$p2 < 0.05)

# Histogram for H0 p-values
g1 <- ggplot(dat, aes(x = p1)) + 
  geom_histogram(fill="white", color="black", bins=30) +
  geom_vline(xintercept = 0.05, col="red") +
  ylim(0, 10000) +
  xlab("P-value") +
  ylab("Frequency") 

# Histogram for H1 p-values
g2 <- ggplot(dat, aes(x = p2)) + 
  geom_histogram(fill="white", color="black", bins=30) +
  geom_vline(xintercept = 0.05, col="red") +
  ylim(0, 10000) +
  xlab("P-value") +
  ylab("Frequency") +
  labs(subtitle = paste("Simulated power: ",
                        as.character(round(sim_power, 3))))

# Density plot for vars under H0
g3 <- ggplot() +
  stat_function(fun=dnorm, geom="area", fill="red", alpha=0.25) +
  stat_function(fun=dnorm, geom="area", fill="blue", alpha=0.25) +
  xlim(-3,3) +
  ggtitle("Null hypothesis true: 2 distrubutions overlap perfectly") +
  xlab("Value") +
  ylab("Density")

# Density plot for vars under H1
g4 <- ggplot() +
  stat_function(fun=dnorm, geom="area", fill="red", alpha=0.25) +
  stat_function(fun=dnorm, geom="area", args = list(mean=d), fill="blue", alpha=0.25) +
  xlim(-3,3) +
  ggtitle("Null hypothesis not true: 2 distributions don't overlap perfectly") +
  xlab("Value") +
  ylab("Density")

# Combine plots
g_final <- (g3 + g4) / (g1 + g2)

ggsave("powerdens.png", g_final)