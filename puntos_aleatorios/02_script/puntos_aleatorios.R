require(pacman)
p_load(sf, tidyverse, hexbin, geogrid, here)
"puntos_aleatorios/03_out"

colo <- st_read(here("puntos_aleatorios",  "01_inp", 
                     "muni_2018gw", "muni_2018gw.shp" ), 
                stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 




col <- filter(colo, NOM_ENT=="Ciudad de México") %>% 
  mutate(puntos=438)

col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
  st_sf() 

col <- col %>% 
  mutate(centroide=st_centroid(geometry))


nom <- as.data.frame(sf::st_coordinates(col$centroide))



write_csv(nom, here("puntos_aleatorios", "03_out", "puntos_cdmx.csv"))




col <- filter(colo, NOM_ENT=="Jalisco") %>% 
       mutate(puntos=56)

col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
  st_sf() 

col <- col %>% 
mutate(centroide=st_centroid(geometry))


nom <- as.data.frame(sf::st_coordinates(col$centroide))



write_csv(nom, here("municipios", "puntos_jal.csv"))



col <- filter(colo, NOM_ENT=="Nuevo León") %>% 
    mutate(puntos=137)
  
  col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
  st_sf() 

col <- col %>% 
  mutate(centroide=st_centroid(geometry))


nom <- as.data.frame(sf::st_coordinates(col$centroide))



write_csv(nom, here("municipios", "puntos_nl.csv"))



