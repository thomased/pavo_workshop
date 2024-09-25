## ------- Question 3: Perceptual distance -------  ##
# Aim: Estimate how different floral communities are to potential pollinators

# Clear environment
rm(list = ls())

# Libraries
library(pavo)

# Load floral data
data(flowers)

# Split the data in two, to pretend they're from two separate communities
specs_a <- as.rspec(flowers[1:18])
specs_b <- as.rspec(cbind(flowers[1], flowers[19:36]))

# Visualise the two floral communities
plot(specs_a)
plot(specs_b)




