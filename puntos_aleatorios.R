require(pacman)
p_load(sf, tidyverse, hexbin, geogrid, here)


colo <- st_read(here("muni_2018gw",  "muni_2018gw.shp" ), stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 


col <- filter(colo, NOM_ENT=="Jalisco") %>% 
     #  mutate(puntos=1012)

col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
  st_sf() 

col <- col %>% 
mutate(centroide=st_centroid(geometry))


nom <- as.data.frame(sf::st_coordinates(col$centroide))



write_csv(nom, here("municipios", "puntos_jal.csv"))



col <- filter(colo, NOM_ENT=="Nuevo León") %>% 
  #  mutate(puntos=1012)
  
  col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
  st_sf() 

col <- col %>% 
  mutate(centroide=st_centroid(geometry))


nom <- as.data.frame(sf::st_coordinates(col$centroide))



write_csv(nom, here("municipios", "puntos_nl.csv"))


