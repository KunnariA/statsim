#######################################
### CELLULAR AGENT BASED SIMULATION ###
#######################################

# This script produces a simple agent-based simulation where the agents are 
# represented as nodes in a grid. The agents belong into one of k groups
# At each step the agent assume the group that is most represented in 
# neighboring nodes

library(gganimate)
library(tidyverse)

# Function to visualize the simulation
animate_sim <- function(matrices, end=NULL) {

  if(is.null(end)) {
    end = length(matrices)
  }
  
  long_dat_all <- NULL
  for(i in 1:end) {
  
    dat <- as.data.frame(matrices[[i]])
    dat$ID1 <- 1:dim(mat)[1]

    dat_long <- dat %>%
      tidyr::pivot_longer(cols = -ID1)
  
    dat_long$iter <- i

    dat_long$ID1 <- factor(dat_long$ID1, levels = as.character(sq:1))
    dat_long$ID2 <- factor(dat_long$name, levels = colnames(dat[,1:sq]))
    dat_long$value <- as.factor(dat_long$value)

    long_dat_all <- rbind(long_dat_all, dat_long)  
    long_dat_all <- as.data.frame(long_dat_all)
  
    }

  
  g <- ggplot(long_dat_all, aes(x=ID2, y = ID1, fill = value)) + 
    geom_tile() +
    xlab("") +
    ylab("") +
    theme_bw() + 
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    gganimate::transition_time(iter)
  
  return(g)
}


## The simulation

# Adjustable parameters
sq = 15 # Grid size (one side of square)
choices = 4 # Number of groups
iterations = 20 # Number of steps in simulation

# Initialize agent states
mat <- runif(sq^2, min = 0, max = choices) %>%
  ceiling %>%
  matrix(nrow=sq)

# Placeholder for agent states
matrices <- list(mat)

# Main loop
for(k in 2:iterations) {

  mat_new <- matrix(NA, nrow=sq, ncol=sq)
  # X axis
  for(i in 1:sq) {
    # Y axis
    for(j in 1:sq) {
    
      temp_mat1 <- matrices[[k-1]]
      
      # Unhash for lesser temporal stability
      cur_value <- temp_mat1[j, i]
      temp_mat1[j, i] <- NA

      y <- c(j, j, j+1, j-1, j)
      x <- c(i, i-1, i, i, i+1)
      
      yind1 <- y < 1
      yind2 <- y > sq
    
      xind1 <- x < 1
      xind2 <- x > sq
      
      y[yind1 | yind2] <- NA
      x[xind1 | xind2] <- NA

      temp_mat2 <- temp_mat1[cbind(y,x)]
      
      freqs <- table(temp_mat2)

      winner <- names(freqs)[freqs==max(freqs)]
      winner <- as.numeric(winner)

      # Resolve ties
      if(length(winner) > 1) {
        if(cur_value %in% winner) {
          winner <- cur_value
        } else {
          winner <- sample(winner, 1) }
      }

      mat_new[j, i] <- winner
      
    }
  }
  
  matrices[[k]] <- mat_new
}


### ANIMATE
g <- animate_sim(matrices)

g

ggsave("cellautomata", g)
