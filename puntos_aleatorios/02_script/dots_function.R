require(pacman)
p_load(sf, tidyverse, hexbin, geogrid, here)
"puntos_aleatorios/03_out"

colo <- st_read(here("puntos_aleatorios",  "01_inp", 
                     "muni_2018gw", "muni_2018gw.shp" ), 
                stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 

colo %>% pull(NOM_ENT)


dots <-  function(DB, estado, z){

  col <- filter(DB, NOM_ENT==sym(estado)) %>% 
    mutate(puntos=nrow(.)*z)
  
  col <-  st_sample(col, size=col$puntos) %>% # random points, as a list ...
    st_sf() 
  
  col <- col %>% 
    mutate(centroide=st_centroid(geometry))
  
  
  nom <- as.data.frame(sf::st_coordinates(col$centroide))
  
  
  return(nom)
}



write_csv(nom, here("puntos_aleatorios", "03_out", "puntos_cdmx.csv"))