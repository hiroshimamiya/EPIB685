
# Experiment 1, sampling of a target population in city x 


# Create hypothetical population with height, in centimeter -- When statements follow "#" sign, it is treated as comment and not interpreted as part of codes 
# sample size of 10 people 
# Case sensitive!!


# Target population, 400,000 people and their true height, mean height is 160cm and standard deviation is 10.0
height_population =  rnorm(n = 400000, mean=160, sd = 10) 
summary(height_population)
# Histogram again 
hist(height_population)
# show average and standard deviation 
mean(height_population)
sd(height_population)








#################### Small sample size  ######################################
# your sample of 10 ppl from some enumeration (source population) representing people in the target population. 
height_10ppl =  sample(size = 10, height_population) # sample 10 ppl from the population above
# lets display the list of these ppl 
height_10ppl
# Some summary statistics 
summary(height_10ppl)
# Histogram again 
hist(height_10ppl)
# show average 
mean(height_10ppl)
# show standard deviation
sd(height_10ppl)
# plot by height
plot(sort(height_10ppl))







#################### Biggr sample size  ######################################
# Lets get 100 people in your sample
height_100ppl = sample(size = 100, height_population)
# lets display the list of these ppl 
height_100ppl
# Some summary statistics 
summary(height_100ppl)
# Histogram again 
hist(height_100ppl)
# show average 
mean(height_100ppl)
# show standard deviation
sd(height_100ppl)

# Plot 
plot(sort(height_100ppl))







#################### BIASED ######################################
# Lets get 100 people in your sample, with biased measure of height 
height_100ppl = sample(size = 100, height_population)
# lets display the list of these ppl 
height_100ppl_biased <- height_100ppl + 20
# Some summary statistics 
summary(height_100ppl_biased)
# Histogram again 
hist(height_100ppl_biased)
# show average 
mean(height_100ppl_biased)
# show standard deviation
sd(height_100ppl_biased)

# Plot 
plot(sort(height_100ppl_biased))




