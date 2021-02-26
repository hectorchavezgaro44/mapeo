require(pacman)
p_load(sf, tidyverse, hexbin, geogrid, here)


colo <- st_read(here("muni_2018gw",  "muni_2018gw.shp" ), stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 


col <- filter(colo, NOM_ENT=="Ciudad de México") %>% 
       mutate(puntos=1012)

col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
  st_sf() 

col <- col %>% 
mutate(centroide=st_centroid(geometry))


nom <- as.data.frame(sf::st_coordinates(col$centroide))


save(nom, file=here("municipios", "puntos.rda"))
write_csv(nom, here("municipios", "puntos_2.csv"))
