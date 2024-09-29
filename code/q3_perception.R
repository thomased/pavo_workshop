## ------- Question 3: Perceptual distance -------  ##
# Aim: Estimate how different floral communities are to potential pollinators

# Clear environment
rm(list = ls())

# Libraries
library(pavo)

# Simulate some reflectance data from two populations of (let's say) flowers
set.seed(123)
inflects_a <- round(runif(15, 500, 600), 1)  # Inflection point locations for population a
inflects_b <- round(runif(15, 400, 600), 1)  # Inflection point locations for population b
inflects_c <- round(runif(15, 340, 500), 1)  # Inflection point locations for population c
y_lims <- 
specs_a <- lapply(seq_along(inflects_a), function(x) simulate_spec(wl_inflect = inflects_a[x]))  # Simulate
specs_b <- lapply(seq_along(inflects_b), function(x) simulate_spec(wl_inflect = inflects_b[x]))  # Simulate
specs_c <- lapply(seq_along(inflects_c), function(x) simulate_spec(wl_inflect = inflects_c[x]))  # Simulate

# Combine and amalgamate
specs_a <- Reduce(merge, specs_a)
specs_b <- Reduce(merge, specs_b)
specs_c <- Reduce(merge, specs_c)

# Add a little population identifier to their names, and merge into one data set
colnames(specs_a)[-1] <- paste0('popA_', colnames(specs_a)[-1])
colnames(specs_b)[-1] <- paste0('popB_', colnames(specs_b)[-1])
colnames(specs_c)[-1] <- paste0('popC_', colnames(specs_c)[-1])
specs <- merge(specs_a, specs_b) |> merge(specs_c)

# Visualise the three floral communities
par(mfrow = c(3, 1))
plot(specs_a)
plot(specs_b)
plot(specs_c)
par(mfrow = c(1, 1))

# Create a simple colourspace model for a bird pollinator (tetrachromat)
# Tetrachromat viewer (average UV-sensitive bird). First, the cone 
# catches
vis_tetra_a <- vismodel(specs_a, 
                        visual = 'avg.uv', 
                        achromatic = 'bt.dc', 
                        illum = 'bluesky', 
                        relative = TRUE)

vis_tetra_b <- vismodel(specs_b, 
                        visual = 'avg.uv', 
                        achromatic = 'bt.dc', 
                        illum = 'bluesky', 
                        relative = TRUE)

vis_tetra_c <- vismodel(specs_c, 
                        visual = 'avg.uv', 
                        achromatic = 'bt.dc', 
                        illum = 'bluesky', 
                        relative = TRUE)

# Then the colourspace. colspace() being the 'gateway' to colourspace (i.e. non-RN) models.
col_tetra_a <- colspace(vis_tetra_a)
col_tetra_b <- colspace(vis_tetra_b)
col_tetra_c <- colspace(vis_tetra_c)

## Visualise them in a tetrahedral coloursapce
plot(col_tetra_a, col = 'forestgreen')
points(col_tetra_b, col = 'goldenrod')
points(col_tetra_c, col = 'purple')

# Or plot interactively
tcsplot(col_tetra_a, col = 'forestgreen')
tcsvol(col_tetra_a, col = 'forestgreen', type = 'alpha')
tcspoints(col_tetra_b, col = 'goldenrod')
tcsvol(col_tetra_b, col = 'goldenrod', type = 'alpha')
tcspoints(col_tetra_c, col = 'purple')
tcsvol(col_tetra_c, col = 'purple', type = 'alpha')

# Explore some individual-level measures of colour, structure
head(col_tetra_a)
head(col_tetra_b)
head(col_tetra_c)

# Explore some group-level measures of structure, difference/similarity

# Descriptive measures
summary(col_tetra_a)
summary(col_tetra_b)
summary(col_tetra_c)

# Volume overlap
voloverlap(col_tetra_a, 
           col_tetra_b)

# Noise weighted-distances?!
# Run our model
vis_tetra <- vismodel(specs, 
                      visual = 'bluetit', 
                      achromatic = 'bt.dc', 
                      relative = FALSE)

# Estimate bootstrapped distances
dist_tetra <- coldist(vis_tetra)
  
bootcoldist(
  dist_tetra,
  qcatch = 'Qi',
  by = substr(names(specs[-1]), 1, 4),
  weber = 0.1,
  weber.achro = 0.1,
  n = c(1, 2, 2, 4),
  achromatic = FALSE
)


## Other colourspace models so achieve similar ends. 
# Note that these aren't limited to 'perceptual distance' questions, 
# that's just what we're using them for here.

# Hexagon model

# Model the cone catches for our bee viewer
vis_bee <- vismodel(specs, 
                    visual = "apis", 
                    qcatch = "Ei", 
                    relative = FALSE,
                    vonkries = TRUE, 
                    achromatic = "l", 
                    bkg = "green")

# Model them in the hexagon
space_bee <- colspace(vis_bee, space = 'hexagon')

# Plot
plot(space_bee, col = c(rep('forestgreen', 10), rep('goldenrod', 10), rep('purple', 10)))

# Calculate colour distances
bootcoldist(
  space_bee,
  by = substr(names(specs[-1]), 1, 4),
)
