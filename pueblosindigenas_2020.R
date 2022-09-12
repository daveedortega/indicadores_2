### Indicadores por Estado
# Indicadores de Pueblos Indigenas

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales,cowplot,readxl,scales)
rm(list=ls())
dev.off()
# sicuentas, Recursos SSA ----
inpi_2020 <- read_xlsx("pueblos indigenas 2020/7-poblacion-indigena-en-hogares-segun-pueblo-por-entidad-federativa-censo-2020-100122.xlsx",skip = 2) %>% 
  clean_names() %>% mutate(entidad = toupper(entidad))


poblacion_estatal <- read_csv("/Users/NSMJ01TM8X/Desktop/inegi/poblacion_censo2020.csv") %>% 
  clean_names() %>% select(!grupo_quinquenal_de_edad)  %>% mutate(nombre_entidad = toupper(entidad_federativa)) %>% select(!entidad_federativa)


# ELEGIR UN NOMBRE ----

inpi_2020 %>% count(entidad) %>% as.data.frame()
estado <- "GUANAJUATO" ############################################################################# elegir estado

inpi_2020 %>% glimpse()

# Top 10 Pueblos Indigenas en el Estado ----

inpi_2020 %>% filter(entidad == estado) %>% group_by(pueblo) %>% 
  mutate(pueblo = ifelse(pihogares<1200,"otros", pueblo)) %>%  summarise(pihogares = sum(pihogares)) %>% 
  ggplot(aes(reorder(pueblo,pihogares),pihogares,fill = pueblo))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(pihogares)),size = 10)+
  labs(x="", y= "Poblacion", title = paste0("Pueblos indigenas con mas presencia en el Estado de ",estado),
       subtitle = "Censado en el año 2020", caption = "Fuente: Instituto Nacional de los Pueblos Indigenas (INPI) - INEGI - 2020")+
  theme(plot.title = element_text(size = 30, color = "#9f2441", face = "bold"), 
        plot.subtitle = element_text(size = 22, face = "bold"), 
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 12,color = "black"),
        legend.position = "none")+
  scale_y_sqrt()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# Top 10 Lenguas Indigenas en el Estado ----

# ES LO MISMO EN CAMPECHE, LO MIDIERON POR HABLANTES? 

inpi_2020 %>% filter(entidad == estado) %>% group_by(lengua) %>% 
  mutate(lengua = ifelse(pihogares<1000,"otros", lengua)) %>%  summarise(pihogares = sum(pihogares)) %>% 
  ggplot(aes(reorder(lengua,pihogares),pihogares,fill = lengua))+
  geom_col()+
  geom_label(aes(label = comma(pihogares)),size = 10)+
  labs(x="", y= "Poblacion", title = paste0("10 Lenguas indigenas con mas presencia en el Estado de ",estado),
       subtitle = "Censado en el año 2020", caption = "Fuente: Instituto Nacional de los Pueblos Indigenas (INPI) - INEGI - 2020")+
  theme(plot.title = element_text(size = 30, color = "#9f2441", face = "bold"), 
        plot.subtitle = element_text(size = 22, face = "bold"), 
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 12,color = "black"),
        legend.position = "none")+
  scale_y_sqrt()

## Como proporcion de su poblacion ----

inpi_2020 %>% filter(entidad == estado) %>% group_by(entidad) %>%  summarise(total_indigenas = sum(pihogares)) %>% 
  left_join(poblacion_estatal %>% rename(entidad = nombre_entidad),by = "entidad") %>% 
  mutate(porcentaje = round(total_indigenas/poblacion*100,2)) %>% 
  pivot_longer(!entidad,names_to = "tipo",values_to = "poblacion") %>% mutate(tipo = case_when(tipo =="poblacion"~"Total",
                                                                                               tipo =="total_indigenas"~"Poblacion Indigena", 
                                                                                               T~tipo)) %>% 
  filter(tipo!="porcentaje") %>% arrange(desc(poblacion)) %>% 
  ggplot(aes(entidad,poblacion,group = tipo, fill = tipo))+
  geom_col(position = "identity", color = "black")+
  geom_label(aes(label = comma(poblacion)),size = 12)+
  labs (x="",y = "", title = str_wrap(paste0("Poblacion Indigena como proporcion de la poblacion total en el estado de ",estado),20),
        subtitle = "Censado en 2020", caption = str_wrap("Fuente: Instituto Nacional de Pueblos Indigenas (INPI) - INEGI - 2020",width = 50), fill = "")+
  theme(plot.title = element_text(size = 16, color = "#9f2441", face = "bold"), 
        plot.subtitle = element_text(size = 12, face = "bold"), 
        plot.caption = element_text(size = 9),
        axis.text.x = element_text(size = 16,color = "black", face = "bold"),
        legend.position = "bottom", 
        legend.text = element_text(size = 12, face = "bold"), legend.direction = "vertical")

# Porcentaje ----
inpi_2020 %>% filter(entidad == estado) %>% group_by(entidad) %>%  summarise(total_indigenas = sum(pihogares)) %>% 
  left_join(poblacion_estatal %>% rename(entidad = nombre_entidad),by = "entidad") %>% 
  mutate(porcentaje = round(total_indigenas/poblacion*100,2))
##

inpi_2020 %>% filter(entidad == estado) %>% group_by(entidad,pueblo) %>%  summarise(total = sum(pihogares)) %>% arrange(desc(total)) %>% 
  ungroup() %>% mutate(porcentaje = round(total/sum(total)*100,2))









