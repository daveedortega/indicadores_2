# Indicadores por Estado
# Indicadores de Salud, CLUES

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales,cowplot)
rm(list=ls())
dev.off()

# Cargar Bases ----
clues_12 <- read_csv("salud/CLUES_2012.csv",
                     locale = locale(encoding = "ISO-8859-2")) 

for (i in 13:21){
  clues_12 <- rbind(clues_12,read_csv(paste0("salud/CLUES_20",i,".csv"), locale = locale(encoding = "ISO-8859-2")))
}
clues_12 <- clues_12 %>% clean_names()
# Clues 21 ----

clues_12 %>% count(nombre_de_la_entidad) %>% as.data.frame()
estado <- "BAJA CALIFORNIA" ############################################################################# elegir estado

clues_12 %>% glimpse()

# CLUES tipo establecimiento por estado, serie de tiempo -----

clues_historico <-clues_12 %>% filter(nombre_de_la_entidad ==estado) %>% filter(estatus_de_operacion != "FUERA DE OPERACION")

labs <- clues_historico%>% group_by(ano_cierre) %>% filter(estatus_de_operacion == "EN OPERACION") %>% 
  count(nombre_de_la_institucion) %>% rename(hospitales = n) %>% filter(ano_cierre %in% c(2021,2018,2012,2015))

clues_historico %>% group_by(ano_cierre) %>% filter(estatus_de_operacion == "EN OPERACION") %>% 
  count(nombre_de_la_institucion) %>% rename(hospitales = n) %>% filter(hospitales>0) %>% 
  ggplot(aes(ano_cierre,hospitales,color = nombre_de_la_institucion))+
  geom_line(size = 1, key_glyph = rectangle_key_glyph(fill = color,color = "black", padding = margin(1, 1, 1, 1)))+
  geom_label(data = labs,aes(label = comma(hospitales)),show.legend = F)+
  labs(x="",y="Establecimientos",
       title = str_wrap(paste0("Número de Establecimientos por Institución responsable en ",estado),60),
       subtitle = "Entre 2012 y 2021", caption = "Fuente: SSA - SICUENTAS",color = "Institución Responsable")+
  theme(legend.position = "left",
        plot.title = element_text(size = 30,face = "bold",color = "#9f2241"),
        plot.subtitle = element_text(size = 22,face = "bold"),
        legend.box.background = element_rect(size=2),
        legend.title = element_text(size = 12,face = "bold")) +
  scale_y_sqrt()+
  guides(fill=guide_legend(
    keywidth=0.5,
    keyheight=0.5,
    default.unit="inch"))+
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 20))


#









