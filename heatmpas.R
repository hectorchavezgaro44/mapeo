rm(list=ls())
setwd("~")
library(pacman)
p_load(tidyverse,readxl, ggmosaic, devtools, RColorBrewer, treemapify, geofacet)

inp <- "/Users/hectorchavez/xime"
fa <- read_excel(paste(inp, "00529-Monitoreo ITAM- BD_24062020.xlsx", sep="/"))


gatel <- fa %>%
  select(1:8, "p20_a", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p20_a)) %>%
  group_by(p20_a, amaifinal) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(p20_a) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  filter(p20_a!="No responde") 

gatel$p20_a = gsub("Sí", "Mantiene su trabajo", gatel$p20_a )

gatel$porc <- round((gatel$totales/gatel$total)*100, 2)

tempo <- gatel

tempo <- filter(tempo, p20_a=="Mantiene su trabajo")
display.brewer.pal(5, "Spectral")
brewer.pal(5, "Spectral")

gatel$amaifinal = factor(gatel$amaifinal, 
                          levels=c("AB", "C+", "C", "C-",
                                   "D+", "D","E"))


ggplot(gatel, aes(x=amaifinal, y=p20_a, fill=porc)) +
  geom_tile(color="black") +
  geom_text(aes(label=porc), size=4, color="black") + 
  scale_fill_continuous(low="#FFFFBF", high="#D7191C") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Porcentaje de personas según estatus laboral y \nnivel socioeconómico", 
       x="", y="", fill="%") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text.x = element_text(size = 12, face = "bold", family = "Trebuchet MS"),
        axis.text.y= element_text(size = 12, face = "bold", family = "Trebuchet MS"))+
coord_fixed()

ggsave(paste(inp, "7_heatmaps.png", sep="/"), width=18, height=12) 

gatel <- fa %>%
  select(1:8, "p20_a", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p20_a)) %>%
  group_by(p20_a, amaifinal) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(amaifinal) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  filter(p20_a!="No responde") 

gatel$p20_a = gsub("Sí", "Mantiene su trabajo", gatel$p20_a )

gatel$porc <- round((gatel$totales/gatel$total)*100, 2)

tempo <- gatel

tempo <- filter(tempo, p20_a=="Mantiene su trabajo")
display.brewer.pal(5, "Spectral")
brewer.pal(5, "Spectral")

gatel$amaifinal = factor(gatel$amaifinal, 
                         levels=c("AB", "C+", "C", "C-",
                                  "D+", "D","E"))


ggplot(gatel, aes(x=amaifinal, y=p20_a, fill=porc)) +
  geom_tile(color="black") +
  geom_text(aes(label=porc), size=4, color="black") + 
  scale_fill_continuous(low="#FFFFBF", high="#D7191C") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Porcentaje de personas por nivel socioeconómico, \nsegún su estatus laboral durante la pandemia", 
       x="", y="", fill="%") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text.x = element_text(size = 12, face = "bold", family = "Trebuchet MS"),
        axis.text.y= element_text(size = 12, face = "bold", family = "Trebuchet MS"))+
  coord_fixed()

ggsave(paste(inp, "8_heatmaps.png", sep="/"), width=18, height=12) 

####trabajdoras del hogar
gatel <- fa %>%
  select(1:8, "p45", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p45)) %>%
  group_by(p45, amaifinal) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(p45) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  filter(p45!="No responde") 

gatel$porc <- round((gatel$totales/gatel$total)*100, 2)
gatel$amaifinal = factor(gatel$amaifinal, 
                         levels=c("AB", "C+", "C", "C-",
                                  "D+", "D","E"))


ggplot(gatel, aes(x=amaifinal, y=p45, fill=porc)) +
  geom_tile(color="black") +
  geom_text(aes(label=porc), size=4, color="black") + 
  scale_fill_continuous(low="#FFFFBF", high="#D7191C") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Porcentaje de personas por nivel socioeconómico, \nsegún el sueldo que reciben las trabajadoras del hogar", 
       subtitle="después de la contingencia", 
       x="", y="", fill="%") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text.x = element_text(size = 12, face = "bold", family = "Trebuchet MS"),
        axis.text.y= element_text(size = 12, face = "bold", family = "Trebuchet MS"))+
  coord_fixed()


ggsave(paste(inp, "8_th_heatmaps.png", sep="/"), width=18, height=12) 


##########################
gatel <- fa %>%
  select(1:8, "p44", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p44)) %>%
  group_by(p44, amaifinal) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(p44) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  filter(p44!="No responde") 

gatel$porc <- round((gatel$totales/gatel$total)*100, 2)
gatel$amaifinal = factor(gatel$amaifinal, 
                         levels=c("AB", "C+", "C", "C-",
                                  "D+", "D","E"))


ggplot(gatel, aes(x=amaifinal, y=p44, fill=porc)) +
  geom_tile(color="black") +
  geom_text(aes(label=porc), size=4, color="black") + 
  scale_fill_continuous(low="#FFFFBF", high="#D7191C") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Porcentaje de personas por nivel socioeconómico, \nque sí/no siguen contratando una trabajadora del hogar", 
       subtitle="después de la contingencia", 
       x="", y="", fill="%") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text.x = element_text(size = 12, face = "bold", family = "Trebuchet MS"),
        axis.text.y= element_text(size = 12, face = "bold", family = "Trebuchet MS"))+
  coord_fixed()


ggsave(paste(inp, "9_th_heatmaps.png", sep="/"), width=18, height=12) 


gatel <- fa %>%
  select(1:8, "p44", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p44)) %>%
  group_by(p44, edad_recod) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(p44) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  filter(p44!="No responde") 

gatel$porc <- round((gatel$totales/gatel$total)*100, 2)

gatel$p44 = factor(gatel$p44, 
                         levels=c("Sí", "No"))


ggplot(data=gatel, aes(x = p44, y= porc, fill=edad_recod)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=porc), position=position_stack(vjust = 0.5), color="grey35", size=6) +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A","#33A02C", "#FB9A99")) +
  labs(title = "Perfil de personas que sí/no siguen contratado \na una trabajadora del hogar",
       x = "Después de la pandemia",
       y = "%", fill="Edad") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text = element_text(size = 16, face = "bold", family = "Trebuchet MS"))


ggsave(paste(inp, "10_th_heatmaps.png", sep="/"), width=18, height=12) 


gatel <- fa %>%
  select(1:8, "p43")%>%
  mutate(tot=1) %>%
  group_by(p43) %>%
  summarise(totales=sum(tot, na.rm=T)) %>%
  ungroup() %>%
  mutate(total=sum(totales, na.rm=T))%>%
  mutate(porc=(totales/total)*100)
############################

gatel <- fa %>%
  select(1:8, "p10_2", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p10_2)) %>%
  group_by(p10_2, edad_recod) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(p44) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  filter(p44!="No responde") 

gatel <- fa %>%
  select(1:8, "p10_2", "amaifinal")%>%
  mutate(total=1) %>%
  filter(!is.na(p10_2)) %>%
  filter(p10_2!="Ns/nc") %>%
  group_by(p10_2, amaifinal) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(p10_2) %>%
  mutate(total=sum(totales, na.rm=T)) %>%
  ungroup() 


gatel$porc <- round((gatel$totales/gatel$total)*100, 2)



gatel$p10_2 = factor(gatel$p10_2, 
                         levels=c("Nada confiable", "Algo confiable", "Poco confiable", "Muy confiable"))

gatel$amaifinal = factor(gatel$amaifinal, 
                         levels=c("AB", "C+", "C", "C-",
                                  "D+", "D","E"))

display.brewer.pal(5, "Spectral")
brewer.pal(5, "Spectral")


ggplot(gatel, aes(x=amaifinal, y=p10_2, fill=porc)) +
  geom_tile(color="black") +
  geom_text(aes(label=porc), size=4, color="black") + 
  scale_fill_continuous(low="#FFFFBF", high="#D7191C") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Porcentaje de personas por nivel socioeconómico \ny confianza a las redes sociales", 
       x="", y="", fill="%") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text.x = element_text(size = 12, face = "bold", family = "Trebuchet MS"),
        axis.text.y= element_text(size = 12, face = "bold", family = "Trebuchet MS"))+
  coord_fixed()
ggsave(paste(inp, "11_redes_heatmaps.png", sep="/"), width=18, height=12) 
######

gatel <- fa %>%
  select(1:8, "p10_2")%>%
  mutate(total=1) %>%
  filter(p10_2=="Muy confiable") %>%
  group_by(a, edad_recod) %>%
  summarise(totales=sum(total, na.rm=T)) %>%
  ungroup() %>%
  group_by(a) %>%
  mutate(total_sexo=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  mutate(porc=round((totales/total_sexo)*100, 1))
names(gatel)[1] <- "sexo"
gatel$sexo <- as.factor(gatel$sexo)
display.brewer.pal(5, "Paired")
brewer.pal(5, "Paired")

ggplot(data=gatel, aes(x = sexo, y= porc, fill=edad_recod)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=porc), position=position_stack(vjust = 0.5), color="grey35", size=6) +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A","#33A02C", "#FB9A99")) +
  labs(title = "Perfil de personas que consideran a las redes sociales \nmuy confiables ",
       x = "",
       y = "%", fill="Edad") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "white"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text = element_text(size = 16, face = "bold", family = "Trebuchet MS"))

ggsave(paste(inp, "13_redes.png", sep="/"), width=12, height=12)

#####
sex <- gatel %>%
  group_by(sexo) %>%
  summarise(total=sum(totales, na.rm=T)) %>%
  ungroup() %>%
  mutate(totales=sum(total, nar.m=T)) %>%
  mutate(porc=round((total/totales)*100, 2))


ggplot(sex, aes(area=porc, fill=sexo, label=paste(porc, "%", sep=" "))) + 
  geom_treemap() +
  geom_treemap_text(place = "centre", grow = F, color ="black")+
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3")) +
  labs(title = "Perfil de personas que consideran a las redes sociales \nmuy confiables ",
       x = "",
       y = "", fill="Sexo") +
  theme_bw()+
  theme(text = element_text(family = "Trebuchet MS", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Trebuchet MS", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Trebuchet MS"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold",  family = "Trebuchet MS"),
        axis.text = element_text(size = 16, face = "bold", family = "Trebuchet MS"))

ggsave(paste(inp, "13_redes_sex.png", sep="/"), width=12, height=12)

