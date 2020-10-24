######################### 
### BERKSON'S PARADOX ### 
#########################

# This script visually demonstrates Berkson's paradox i.e. what happens when two
# uncorrelated variables are subsetted based on their common consequence


### Simulation with binary variables

# Binary variables A & B
A <- round(runif(1000,0,1),0)
B <- round(runif(1000,0,1),0)

# They are not correlated at first
cor(A,B)
table(A,B)

# Select observartions where neither A or B is 0
i <- A != 0 | B != 0

# Remaining observartions show a large correlation
cor(A[i], B[i])
table(A[i], B[i])


### Simulation with Likert-type normal variables

# Normal variables x & y 
x <- round(rnorm(10000,3,0.8))
y <- round(rnorm(10000,3,0.8))

# Bound them between 1 and 5
i <- x < 6 & y < 6 & x > 0 & y > 0
x <- x[i]
y <- y[i]

# They are uncorrelated
cor(x, y)
plot(jitter(x[i]), jitter(y[i]))

# Median of each variable
q1 <- quantile(x)[3]
q2 <- quantile(y)[3]

# Take observations that are above median in both
i <- x > q1 | y > q2

# Remaining observartions show a large correlation
cor(x[i],y[i])
plot(x[i], y[i])
