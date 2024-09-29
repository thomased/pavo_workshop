## ------- Question 2: Discrimination -------  ##
# Aim: Predict whether animals should be discriminable from viewing backgrounds
# for a representative tri- and tetrachromat viewer

# Clear environment
rm(list = ls())

# Libraries
library(pavo)
library(vegan)
library(tidyverse)

# Load real data (to be used in place of the simulated spectra during real analysis)
dat <- getspec('../data/q2_discrimination/', ext = '.jaz')

# Plot specs
plot(dat, col = rep(c('forestgreen', 'brown'), each = 10), 
     main = "Frog and Background Spectra")
legend('topright', legend = c('Frog', 'Background'), col = c('forestgreen', 'brown'), lty = 1)

# Process spectra: smoothing and zero-ing negative values
dat <- procspec(dat, opt = 'smooth', fixneg = 'zero', span = 0.2)

# Visualise frogs and backgrounds separately
plot(subset(dat, 'F'), col = 'forestgreen', 
     main = "Frogs",
     lwd = 1.5)

plot(subset(dat, 'B'), col = 'brown', 
     main = "Backgrounds",
     lwd = 1.5)

# Run visual models for tri- and tetrachromat viewers using the 'receptor noise limited model'
# Trichromat model for jumping spiders ('habronattus')
# Tetrachromat model for birds (using UV sensitivity 'avg.uv')

# Tri-chromat viewer (Habronattus spiders)
vis_tri <- vismodel(dat, 
                    visual = 'habronattus', 
                    achromatic = 'l', 
                    qcatch = 'fi', 
                    illum = 'bluesky', 
                    scale = 10000, 
                    relative = FALSE)

# Tetrachromat viewer (average UV-sensitive bird)
vis_tetra <- vismodel(dat, 
                      visual = 'avg.uv', 
                      achromatic = 'bt.dc', 
                      qcatch = 'fi', 
                      illum = 'bluesky', 
                      scale = 10000,
                      relative = FALSE)

# Visualise them in a simple colourspace

# Tri-chromat: custom space for 'habronattus'
tcs_tri <- colspace(vis_tri, space = 'tri')
plot(tcs_tri, col = rep(c('forestgreen', 'brown'), each = 10))

# Tetrachromat: Tetrahedral color space
tcs_tetra <- colspace(vis_tetra, space = 'tcs')
plot(tcs_tetra, col = rep(c('forestgreen', 'brown'), each = 10))

# Calculate chromatic contrasts (JNDs) using receptor noise models for each viewer

# Tri-chromat contrasts (Habronattus)
dist_tri <- coldist(vis_tri, 
                    noise = 'neural',
                    n = c(1, 2, 2),
                    achromatic = TRUE)

# Tetrachromat contrasts (Birds)
dist_tetra <- coldist(vis_tetra, 
                      noise = 'neural', 
                      n = c(1, 2, 2, 4),
                      achromatic = TRUE)

# Can also visualise them in 'rn-space' or 'JND-space'
plot(jnd2xyz(dist_tri), col = rep(c('forestgreen', 'brown'), each = 10))
plot(jnd2xyz(dist_tetra), col = rep(c('forestgreen', 'brown'), each = 10))

# Two-step procedure outlines in Maia & White (2018) to test for 
# statistical and 'perceptual' discrimination
# Step (1) Are colours *statistically* separable?
# PERMANOVA to test for overall color separation

# Prepare inter-sample 'distance matrix' for PERMANOVA 
matrix_tri <- dist(coldist2mat(dist_tri)[['dS']]) # Trichromat
matrix_tetra <- dist(coldist2mat(dist_tetra)[['dS']])  # Tetrachromat
groups <- substring(rownames(as.matrix(matrix_tri)), 1, 1)  # Create group labels

# Use the 'adonis' function from the vegan package for PERMANOVA
adonis2(matrix_tri ~ groups)  # trichromat
adonis2(matrix_tetra ~ groups)  # tetrachromat

# Step (2) Are colours *perceptually* separable?
# Use bootcoldist() to estimate bootstrapped color distances between frogs and backgrounds

# Our trichromat
boot_tri <- 
  bootcoldist(
    vis_tri,
    by = substr(rownames(vis_tri), 1, 1),
    weber = 0.1,
    weber.achro = 0.1,
    n = c(1, 2, 2)
  )

boot_tri

# Our tetrachromat
boot_tetra <- 
  bootcoldist(
    vis_tetra,
    by = substr(rownames(vis_tetra), 1, 1),
    weber = 0.1,
    weber.achro = 0.1,
    n = c(1, 2, 2, 4)
)

boot_tetra


## How about an adjacency analysis to complement?

# Run the adjacency analysis on a single frog image of a butterfly
frog <- getimg()

# Colour classify the image
frog_class <- classify(frog, kcols = 2)

# Visualise the results of classification (could also use plot())
summary(frog_class, plot = TRUE)

# Generate a colour distance matrix for our frog-background contrast
# distances <- data.frame(
#   c1 = c(1),
#   c2 = c(2),
#   dS = c(),
#   dL = c()
# )

# Run the adjacency analysis
frog_adj <- adjacent(frog_class, xscale = 100, coldists = distances)



