require(pacman)
p_load(sf, tidyverse, hexbin, geogrid,classInt, here)
X11(type="cairo")
colo <- st_read(here("puntos_aleatorios",  "01_inp", 
                     "muni_2018gw", "muni_2018gw.shp" ), 
                stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 

edo <- st_read(here("estados",  "ESTADOS.shp"), 
               stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 


nl <- filter(colo, CVE_ENT=="19") %>% 
      select(CVEGEO, NOM_MUN)

nl_coneval <- read_csv(here("nl", "inp", "Libro1.csv"))
             


breaks_qt <- classIntervals(c(min(nl_coneval$mas15_analfabeta)- .00001 , nl_coneval$mas15_analfabeta), n = 5, style = "quantile")
nl_coneval <- nl_coneval %>% 
             mutate(analfabeta_cat = cut(mas15_analfabeta, breaks_qt$brks), 
                    cve_mun=as.character(cve_mun)) 

analfabeta <- nl_coneval %>% 
              select(cve_mun, analfabeta_cat)

nl <- left_join(nl, analfabeta, by=c("CVEGEO"="cve_mun"))

ggplot() + 
  geom_sf(data=nl, aes(fill=analfabeta_cat), size=0.3, color="#808080") +
  geom_sf(data=edo, fill=NA, size=1, color="#808080") +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill="")+
  xlim(101.5, 98.5)+
  ylim(23, 28)+
  theme_void(base_size = 55)


ggsave(here("nl","out", "analfabet_15mas.png"), dpi = 320, width = 70, height = 70, units = "cm")
       


rm(analfabeta, breaks_qt, colo, compro, df,nl,nl_coneval)
# No asiste a la escuela --------------------------------------------------
colo <- st_read(here("puntos_aleatorios",  "01_inp", 
                     "muni_2018gw", "muni_2018gw.shp" ), 
                stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 

nl_coneval <- read_csv(here("nl", "inp", "Libro1.csv"))
nl <- filter(colo, CVE_ENT=="19") %>% 
  select(CVEGEO, NOM_MUN)



breaks_qt <- classIntervals(c(min(nl_coneval$noasiste_6a14)- .00001 , nl_coneval$noasiste_6a14), n = 5, style = "quantile")
nl_coneval <- nl_coneval %>% 
  mutate(cat = cut(noasiste_6a14, breaks_qt$brks), 
         cve_mun=as.character(cve_mun)) 

df <- nl_coneval %>% 
  select(cve_mun, cat)

nl <- left_join(nl, df, by=c("CVEGEO"="cve_mun"))

ggplot() + 
  geom_sf(data=nl, aes(fill=cat), size=0.3, color="#808080") +
  geom_sf(data=edo, fill=NA, size=1, color="#808080") +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill="")+
  xlim(101.5, 98.5)+
  ylim(23, 28) +
  theme_void(base_size = 55)


ggsave(here("nl","out", "no_asiste6a14.png"), dpi = 320, width = 70, height = 70, units = "cm")

rm(analfabeta, breaks_qt, colo, compro, df,nl,nl_coneval)


# educacion basica incompleta ---------------------------------------------

colo <- st_read(here("puntos_aleatorios",  "01_inp", 
                     "muni_2018gw", "muni_2018gw.shp" ), 
                stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) 

nl_coneval <- read_csv(here("nl", "inp", "Libro1.csv"))
nl <- filter(colo, CVE_ENT=="19") %>% 
  select(CVEGEO, NOM_MUN)



breaks_qt <- classIntervals(c(min(nl_coneval$basicincomp_15omas)- .00001 , nl_coneval$basicincomp_15omas), n = 5, style = "quantile")
nl_coneval <- nl_coneval %>% 
  mutate(cat = cut(basicincomp_15omas, breaks_qt$brks), 
         cve_mun=as.character(cve_mun)) 

df <- nl_coneval %>% 
  select(cve_mun, cat)

nl <- left_join(nl, df, by=c("CVEGEO"="cve_mun"))

ggplot() + 
  geom_sf(data=nl, aes(fill=cat), size=0.3, color="#808080") +
  geom_sf(data=edo, fill=NA, size=1, color="#808080") +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill="")+
  xlim(101.5, 98.5)+
  ylim(23, 28) +
  theme_void(base_size = 55)


ggsave(here("nl","out", "basica_incompleta.png"), dpi = 320, width = 70, height = 70, units = "cm")
