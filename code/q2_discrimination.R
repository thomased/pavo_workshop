## ------- Question 2: Discrimination -------  ##
# Aim: Predict whether animals should be discriminable from viewing backgrounds
# for a representative tri- and tetrachromat viewer

# Clear environment
rm(list = ls())

# Libraries
library(pavo)
library(vegan)
library(tidyverse)

## --------------------------------------------------------------------------------------------------------
# FOR TESTING ONLY
# Simulate spectra for 10 frogs and 10 backgrounds using simulate_spec 
# Frogs and backgrounds have variation in Gaussian and sigmoidal peaks

# Simulate frog spectra with a combination of Gaussian and sigmoidal features
spec_frog <- lapply(1:10, function(x) simulate_spec(wl_inflect = 550 + runif(1, -20, 20),
                                                    wl_peak = c(400 + runif(1, -30, 30), 500 + runif(1, -30, 30)),
                                                    width_gauss = 50 + runif(1, -10, 10)))

# Simulate background spectra with broader Gaussian peaks
spec_bkg <- lapply(1:10, function(x) simulate_spec(wl_peak = c(450 + runif(1, -40, 40), 650 + runif(1, -50, 50)),
                                                   width_gauss = 100 + runif(1, -20, 20)))

# Name the frog spectra F1, F2, ... and background spectra B1, B2, ...
names_frog <- paste0('F', 1:10)
names_bkg <- paste0('B', 1:10)

# Assign names to spectra and combine
for (i in seq_along(spec_frog)) {
  colnames(spec_frog[[i]])[2] <- names_frog[i]
  colnames(spec_bkg[[i]])[2] <- names_bkg[i]
}

# Combine frog and background spectra
spec_frog <- Reduce(merge, spec_frog)  # Combine frog spectra
spec_bkg <- Reduce(merge, spec_bkg)    # Combine background spectra
dat <- merge(spec_frog, spec_bkg)      # Combine frog and background spectra

## --------------------------------------------------------------------------------------------------------

# Load real data (to be used in place of the simulated spectra during real analysis)
# dat <- getspec('../data/q2_discrimination/', ext = '.jaz')

# Plot specs
plot(dat, col = rep(c('green', 'brown'), each = 10), 
     main = "Frog and Background Spectra")
legend('topright', legend = c('Frog', 'Background'), col = c('green', 'brown'), lty = 1)

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
vis_tri

# Tetrachromat viewer (average UV-sensitive bird)
vis_tetra <- vismodel(dat, 
                      visual = 'avg.uv', 
                      achromatic = 'bt.dc', 
                      qcatch = 'fi', 
                      illum = 'bluesky', 
                      scale = 10000,
                      relative = FALSE)

# Map the colors into 'receptor noise' space for visualisation
# Tri-chromat: custom space for 'habronattus'
tcs_tri <- colspace(vis_tri, space = 'tri')

# Tetrachromat: Tetrahedral color space
tcs_tetra <- colspace(vis_tetra, space = 'tcs')

# Plot the colors in their respective color spaces

# Plot for tri-chromatic viewer (Habronattus)
plot(tcs_tri, col = rep(c('green', 'brown'), each = 10), main = "Frog vs Background Colors in Habronattus Vision")

# Plot for tetrachromatic viewer (Tetrahedral color space)
plot(tcs_tetra, col = rep(c('green', 'brown'), each = 10), main = "Frog vs Background Colors in Tetrahedral Space (Tetrachromat Vision)")

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

# Two-step procedure outlines in Maia & White (2018) to test for 
# statistical and 'perceptual' discrimination
# Step (1) Are colours *statistically* separable?
# PERMANOVA to test for overall color separation

# Prepare inter-sample 'distance matrix' for PERMANOVA 
# Trichromat first
matrix_tri <- dist(coldist2mat(dist_tri)[['dS']])  
groups_tri <- substring(rownames(as.matrix(matrix_tri)), 1, 1)  # Create group labels

# Tetrachromat
matrix_tetra <- dist(coldist2mat(dist_tetra)[['dS']])  
groups_tetra <- substring(rownames(as.matrix(matrix_tetra)), 1, 1)

# Use the 'adonis' function from the vegan package for PERMANOVA
adonis2(matrix_tri ~ groups_tri)  # trichromat
adonis2(matrix_tetra ~ groups_tetra)  # tetrachromat

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
