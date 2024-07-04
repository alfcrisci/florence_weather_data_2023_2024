library(elevatr)
set.seed(65.7)

crs_dd <- 4326

# Create and example data.frame with additional columns


# Create an example
examp_sf <- sf::st_as_sf(examp_df2, coords = c("x", "y"), crs = crs_dd)