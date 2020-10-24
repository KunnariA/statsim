
library(patchwork)
library(tidyverse)

n = 5

t1 <- rnorm(n, 0)
t2 <- rnorm(n, 2)

dat <- data.frame(ID = 1:n, t1 = t1, t2 = t2)

dat_long <- dat %>%
  pivot_longer(cols=c(t1,t2), names_to = "time")


g1 <- ggplot(dat_long, aes(x = time, y = value, group=ID)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Low interindividual stability, systemic change") +
  labs(subtitle = "Low test-retest correlation, mean change")

####

t1 <- rnorm(n, 0)
t2 <- rnorm(n, 0)

dat <- data.frame(ID = 1:n, t1 = t1, t2 = t2)

dat_long <- dat %>%
  pivot_longer(cols=c(t1,t2), names_to = "time")


g2 <- ggplot(dat_long, aes(x = time, y = value, group=ID)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Low interindividual stability, no systemic change") +
  labs(subtitle = "Low test-retest correlation, no mean change")


t1 <- rnorm(n, 0)
t2 <- t1 + 2 + rnorm(n, 0, 0.5)

dat <- data.frame(ID = 1:n, t1 = t1, t2 = t2)

dat_long <- dat %>%
  pivot_longer(cols=c(t1,t2), names_to = "time")


g3 <- ggplot(dat_long, aes(x = time, y = value, group=ID)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("High interindividual stability, systemic change") +
  labs(subtitle = "High test-retest correlation, mean difference")

t1 <- rnorm(n, 0)
t2 <- t1 + rnorm(n, 0, 0.5)

dat <- data.frame(ID = 1:n, t1 = t1, t2 = t2)

dat_long <- dat %>%
  pivot_longer(cols=c(t1,t2), names_to = "time")


g4 <- ggplot(dat_long, aes(x = time, y = value, group=ID)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("High interindividual stability, no systemic change") +
  labs(subtitle = "High test-retest correlation, no mean difference")

(g1 + g2) / (g3 + g4)
