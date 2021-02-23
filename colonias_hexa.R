
require(pacman)
p_load(sf, tidyverse, hexbin, geogrid, here)

col <- st_read(here("coloniascdmx", "coloniascdmx.shp"), stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 

plot(col)


col <-  col %>% 
       filter(!st_is_empty(.))

# sf::st_is_empty()
# 
# col <- col %>% 
#        slice(-421) %>% 
#        slice(-885)
# sf::st_is_empty(col)

#first make our sf object into an sp object
eu_sp <- as(col, 'Spatial')

#then use the calculate_grid function. Note how we specify grid type to be "regular".
eu_reg <- calculate_grid(shape = eu_sp, grid_type = "hexagonal", seed = 1)
#assign the polygons
eu_reg <- assign_polygons(eu_sp, eu_reg)

#now turn it back into sf object for easy ggplot plotting
eu_reg <- st_as_sf(eu_reg)


