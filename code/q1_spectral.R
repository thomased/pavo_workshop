## ------- Question 1: Spectral -------  ##
# Aim: Examine the anatomical basis of sex-based variation in wing colouration
# in the butterfly Eurema hecabe.
# 1a) Are males and females sexually dimorphic/dichromatic?
# 1b) Does wing-scale ridge density explain sex-based variation in structural UV 'brightness'? 

# Clear environment
rm(list = ls())

# Load libraries
library(pavo)
library(tidyverse)
library(patchwork)

# Load collected reflectance spectra of male and female Eurema hecabe butterflies
specs <- read.csv('../data/q1_spectral/hecabe_reflectance.csv')

# Load measures of average 'ridge density' per individual. 
# This is a measure of the density of the ridges on the wing-scales, which underlie
# structural (UV) component of their wing colouration
ridges <- read.csv('../data/q1_spectral/hecabe_ridge_density.csv')
  
# Convert spectra to 'rspec' objects
specs <- as.rspec(specs, lim = c(300, 700))

# Process the spectra to zero spurious 0's and smooth slightly
specs <- procspec(specs, opt = 'smooth', fixneg = 'zero', span = 0.1)

# Visualize the processed reflectance spectra
plot(specs, type = 'heatmap')
?plot.rspec  # Have a look at the help file to see what other plots are available

# Visualise the spectra by sex, based on 'm' or 'f' in the names of the individuals
par(mfrow = c(1, 2))
plot(subset(specs, 'm'), 
     col = 'purple',
     main = 'males',
     ylim = c(0, 100))
plot(subset(specs, 'f'), 
     col = 'forestgreen', 
     main = 'females',
     ylim = c(0, 100))
par(mfrow = c(1, 1))

# Extract 21 common 'colourimetric' variables which describe various
# aspects of spectra reflectance in terms of 'hue', 'saturation', and 'brightness'
specs_summary <- summary(specs)
?summary.rspec

# Choose a sensible measure of 'hue' 'chroma' and 'brightness' for 'yellow' (pigmentary) component, using
# summary(). Then some tidying.
specs_summary_yel <- summary(specs, 
                             subset = c('B2', 'S7', 'H3'), 
                             lim = c(460, 700))

# Tidyind: create an explicit 'id' column from the rownames, and rename the variables for convenience
specs_summary_yel <- 
  specs_summary_yel |> rownames_to_column('id')  |> 
  rename(
    yel_B = B2,
    yel_S = S7,
    yel_H = H3
  )

# Choose a sensible measure of 'hue' 'chroma' and 'brightness' for UV (structural) component
# using peakshape(). Then do a little tidying.
specs_summary_uv <- peakshape(specs, 
                              lim = c(300, 460),
                              plot = TRUE)
specs_summary_uv <- 
  specs_summary_uv |> 
  select('id', 'B3', 'H1', 'FWHM')  |> 
  rename(
    uv_B = B3,
    uv_S = FWHM,
    uv_H = H1
  )

# Merge all our data into one lovely dataframe! And add an explicit 'sex' column
# while we're at it.
dat_compiled <- 
  ridges |> 
  left_join(specs_summary_yel, by = 'id') |>  
  left_join(specs_summary_uv, by = 'id') |> 
  mutate(sex = ifelse(substr(id, 1, 1) == "f", "female", "male"))

dat_compiled

### ANSWER (1a) Are males and females sexually dimorphic/dichromatic?

# Pivot longer to get uv_* and yel_* in one column for plotting
dat_long <- 
  dat_compiled |> 
  pivot_longer(cols = starts_with("uv") | starts_with("yel"), 
               names_to = "measure", values_to = "value") |> 
  mutate(measure = factor(measure, levels = c("uv_B", "yel_B", "uv_S", "yel_S", "uv_H", "yel_H")))

# Create the plot with the specific ordering of UV and Yellow measurements
dat_long |>
  ggplot(aes(x = value, fill = sex)) +
  geom_histogram(color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ measure, scales = "free_x", ncol = 2) +  # 2 columns for UV and Yellow
  labs(title = "Histograms of UV and Yellow Measurements", x = "Value", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Inset your own stats as you like :)  
  
### ANSWER (1b) Does ridge density explain sex-based variation in structural UV 'brightness'? 
  
# Filter for only uv_B from the previously created dat_long
dat_uv_b <- 
  dat_long |>
  filter(measure == "uv_B")  # Focus only on uv_B

# Create the plot with lines fit by sex, consistent colors, and no legend
dat_uv_b |>
  ggplot(aes(x = ridge_density, y = value, color = sex)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +  # Fit lines by sex
  labs(
    x = "Wing Scale Ridge Density (per micron)", 
    y = "UV Brightness"
  ) +
  theme_minimal()
  
# Inset your own stats as you like :) 