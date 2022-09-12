# Indicadores por Estado
# Indicadores de Padron Electoral

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales,cowplot)
rm(list=ls())
dev.off()
# sicuentas, Recursos SSA ----
padron_junio_22 <- read_csv("./padron/padron_junio_22.csv",
                             locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

poblacion_estatal <- read_csv("/Users/NSMJ01TM8X/Desktop/inegi/poblacion_censo2020.csv") %>% 
  clean_names() %>% select(!grupo_quinquenal_de_edad)  %>% mutate(nombre_entidad = toupper(entidad_federativa)) %>% select(!entidad_federativa)

poblacion_estatal$nombre_entidad <- poblacion_estatal$nombre_entidad %>% str_replace("Á","A") %>% 
  str_replace("É","E") %>% str_replace("Í","I") %>% str_replace("Ó","O") %>% str_replace("MICHOACAN DE OCAMPO","MICHOACAN") %>% 
  str_replace("COAHUILA DE ZARAGOZA","COAHUILA") %>% str_replace("VERACRUZ DE IGNACIO DE LA LLAVE","VERACRUZ")

poblacion_estatal$nombre_entidad[!poblacion_estatal$nombre_entidad %in% padron_junio_22$nombre_entidad]
padron_junio_22 %>% count(nombre_entidad) %>% as.data.frame()
poblacion_estatal %>% count(nombre_entidad) %>% as.data.frame()

# Analiss
padron_junio_22 %>% glimpse()
padron_junio_22 %>% count(nombre_entidad) %>% as.data.frame()
estado <- "GUANAJUATO" ############################################################################################################## Elegir Estado
# Porcentaje del padron nacional ----

padron_junio_22 %>% group_by(nombre_entidad) %>% summarise(padron = sum(padron_electoral)) %>% 
  ungroup() %>% mutate(porcentaje_nacional = round(padron/sum(padron)*100,2)) %>% left_join(poblacion_estatal,by = "nombre_entidad") %>%
  mutate(padron_poblacion = round(padron/poblacion*100,2)) %>% 
  mutate(lugar = rank(desc(porcentaje_nacional))) %>% filter(nombre_entidad == estado) %>% mutate(pob_padron = poblacion - padron) %>% 
  select(nombre_entidad,poblacion,padron,pob_padron,padron_poblacion,porcentaje_nacional,lugar)

# Distribucion por sexo del Estado ----

padron_junio_22 %>% filter(nombre_entidad == estado) %>% summarise(hombres = sum(padron_hombres), mujeres = sum(padron_mujeres))%>% 
  mutate(baja = "baja") %>% pivot_longer(!baja,names_to = "sexo", values_to = "poblacion") %>% 
  mutate(porcentaje = round(poblacion / sum(poblacion)*100,2)) %>% 
  ggplot(aes(sexo,porcentaje,fill = sexo))+
  geom_col()+
  geom_label(aes(label = comma(porcentaje,suffix = "%")),size = 12)+
  labs ( x="",y = "%",title = paste0("Porcentaje del Padron electoral de ",estado, "Por sexo"),subtitle = "A Junio de 2022")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), axis.text.x = element_text(size = 14,color ="black"),
        legend.text = element_text(size = 16,face = "bold"))


padron_junio_22 %>% filter(nombre_entidad == estado) %>% group_by(nombre_municipio) %>% 
  summarise(hombres = sum(padron_hombres), mujeres = sum(padron_mujeres))




















































































