#####################################
### SIMPLE AGENT BASED SIMULATION ###
#####################################

library(gganimate)
library(tidyverse)

# Function to calculate distance between agents
calc_distances <- function(list) {
  
  mat_x1 = matrix(rep(list$locations_x, list$n_agents), 
         ncol=list$n_agents, nrow=list$n_agents, byrow=T)
  mat_x2 = matrix(rep(list$locations_x, list$n_agents), 
                ncol=list$n_agents, nrow=list$n_agents, byrow=F)
  
  mat_y1 = matrix(rep(list$locations_y, list$n_agents), 
                  ncol=list$n_agents, nrow=list$n_agents, byrow=T)
  mat_y2 = matrix(rep(list$locations_y, list$n_agents), 
                 ncol=list$n_agents, nrow=list$n_agents, byrow=F)
  
  Dx = mat_x1-mat_x2
  Dy = mat_y1-mat_y2
  
  D = sqrt(Dx^2 + Dy^2)
  diag(D) <- NA
  D[upper.tri(D)] <- NA
  
  list$distances = D
  
  return(list)
}

check_adjacacy <- function(list, radius) {
  
  check = list$distances < radius
  list$adjacant = which(check, arr.ind = T)
  
  return(list)
}

# Moves agents
move <- function(list, ind=0.05) {

  counts = rep(1, list$n_agents)
  
  move_x <- rnorm(list$n_agents, sd=ind*15)
  move_y <- rnorm(list$n_agents, sd=ind*15)
  
  cum_x <- move_x
  cum_y <- move_y
  
  if(nrow(list$adjacant) > 0) {
    
    for(i in 1:nrow(list$adjacant)) {
    
      m = list$adjacant[i, 1]
      n = list$adjacant[i, 2]
    
      cum_x[m] = cum_x[m] + move_x[n]
      cum_y[n] = cum_y[n] + move_y[m]
      cum_x[n] = cum_x[n] + move_x[m]
      cum_y[m] = cum_y[m] + move_y[n]
    
      counts[n] = counts[n] + 2
      counts[m] = counts[m] + 2
    }
}

  cum_x = cum_x / counts
  cum_y = cum_y / counts
  
  list$locations_x = list$locations_x + cum_x + rnorm(list$n_agents, sd=ind)
  list$locations_y = list$locations_y + cum_y + rnorm(list$n_agents, sd=ind)
  
  return(list)
}


## The simulation

# Adjustable parameters
number_of_agents = 300
adj_radius <- 0.8


agents = list()
agents$n_agents <- number_of_agents

# Starting positions
agents$locations_x <- runif(agents$n_agents, -5, 5)
agents$locations_y <- runif(agents$n_agents, -5, 5)

# Placeholders for dataframes
dats = NULL
all_dats = NULL

# Main loop
for(i in 1:100) {
  
  agents = calc_distances(agents)
  agents = check_adjacacy(agents, adj_radius)
  
  new_dat = data.frame(ID = 1:agents$n_agents, 
                       x = agents$locations_x, 
                       y = agents$locations_y,
                       iteration = i)
  dats[[i]] = new_dat
  
  # Bind new data to total data
  all_dats = as.data.frame(rbind(all_dats, new_dat))
  
  agents = move(agents, ind=0.06)

}

# Visualize simulation
g <- ggplot(all_dats, aes(x = x, y = y)) + 
  geom_point(aes(color=ID)) +
  xlim(-10, 10) +
  ylim(-10, 10) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  gganimate::transition_time(iteration)
  
