# Indicadores por Estado
# Indicadores de Salud, Inversión SICUENTAS

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales,cowplot)
rm(list=ls())
dev.off()
# sicuentas, Recursos SSA ----
ssasicuentas2021 <- read_csv("./salud/Recursos Secretaria de Salud 2021.csv",
                             locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
ssasicuentas2021 %>% glimpse()
descriptores_tipologia <- read_csv("salud/descriptores_tipologia_ssa.csv") %>% clean_names()
recursos_descriptores <- read_csv("salud/recursos_descriptores.csv") %>% clean_names() %>% 
  mutate(clave = tolower(clave))


# ELEGIR UN NOMBRE ----

ssasicuentas2021 %>% count(entidad) %>% as.data.frame()
estado <- "QUINTANA ROO" ############################################################################# elegir estado
 
## SICUENTAS Recursos SSA E13, Medicos Generales, Especialistas y Odontólogos  ----

# ssasicuentas2021 %>% filter(entidad == estado) %>% View()

ssasicuentas2021 %>% filter(entidad == estado) %>% select(entidad,tipo_de_establecimiento,tipologia,e13) %>% 
  count(tipo_de_establecimiento,tipologia) %>% 
  mutate(tipo_de_establecimiento = case_when(tipo_de_establecimiento == "CE"~"Unidades de Consulta Externa",
                   tipo_de_establecimiento == "HO"~"Unidades Hospitalarias")) %>% 
  rename(clave = tipologia) %>% 
  left_join(descriptores_tipologia,by = "clave") %>% rename(clasificacion = descripcion) %>% 
  ggplot(aes(reorder(clasificacion,n),n,group = clasificacion,fill = clasificacion))+
  geom_col(position = position_dodge(width = 1),color = "black")+
  facet_wrap(~tipo_de_establecimiento,scales = "free_x")+
  geom_label(aes(label = comma(n)),size = 6)+
  labs(x="",y="Número de Unidades",title = str_wrap(paste0("Número de Unidades de Hospitalización y Consulta Externa en ", estado),width = 60),
       subtitle = "Al cierre de 2021, públicos miembros de SSA", caption = "Fuente: SSA - SICUENTAS")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(face = "bold",color = "black",angle = 90,size = 12),
        plot.caption = element_text(size = 14))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
  
  
## SICUENTAS Recursos SSA E14, Total de camas en área de hospitalización  ----

ssasicuentas2021 %>% filter(entidad == estado) %>%
  select(entidad,tipo_de_establecimiento,tipologia,e14) %>% 
  group_by(tipo_de_establecimiento,tipologia) %>% 
  summarise(camas = sum(e14)) %>% filter(camas>0) %>% 
  mutate(tipo_de_establecimiento = case_when(tipo_de_establecimiento == "CE"~"Unidades de Consulta Externa",
                                             tipo_de_establecimiento == "HO"~"Unidades Hospitalarias")) %>% 
  rename(clave = tipologia) %>% 
  left_join(descriptores_tipologia,by = "clave") %>% rename(clasificacion = descripcion) %>% 
  ggplot(aes(reorder(clasificacion,camas),camas,fill = clasificacion))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(camas)),size = 6)+
  labs(x="",y="Número de Camas",
       title = str_wrap(paste0("Número de camas en área de Hospitalización en ", estado),width = 60),
       subtitle = "Al cierre de 2021, en instituciones pertenecientes a la SSA",caption = "Fuente: SSA - SICUENTAS")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",
        axis.text.y = element_text(size=12,color="black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(color = "black", size = 14))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))


## SICUENTAS Recursos SSA E18, Total de MEDICOS GENERALES, ESPECIALISTAS Y ODONTÓLOGOS  ----

ssasicuentas2021 %>% filter(entidad == estado) %>%
  select(entidad,tipo_de_establecimiento,tipologia,e18) %>% 
  group_by(tipo_de_establecimiento,tipologia) %>% 
  summarise(medicos = sum(e18)) %>% filter(medicos>0) %>% 
  mutate(tipo_de_establecimiento = case_when(tipo_de_establecimiento == "CE"~"Unidades de Consulta Externa",
                                             tipo_de_establecimiento == "HO"~"Unidades Hospitalarias")) %>% 
  rename(clave = tipologia) %>% 
  left_join(descriptores_tipologia,by = "clave") %>% rename(clasificacion = descripcion) %>% 
  ggplot(aes(reorder(clasificacion,medicos),medicos,fill = clasificacion))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(medicos)),size = 6)+
  labs(x="",y="Número de medicos",
       title = str_wrap(paste0("Número de medicos en área de Hospitalización en ", estado),width = 60),
       subtitle = "Al cierre de 2021, en instituciones pertenecientes a la SSA",caption = "Fuente: SSA - SICUENTAS")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",
        axis.text.y = element_text(size=12,color="black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(color = "black", size = 14))+
  coord_flip()

## SICUENTAS Recursos SSA E21, personal de enefermería en contacto con pacientes  ----

ssasicuentas2021 %>% filter(entidad == estado) %>%
  select(entidad,tipo_de_establecimiento,tipologia,e21) %>% 
  group_by(tipo_de_establecimiento,tipologia) %>% 
  summarise(enfermeras = sum(e21)) %>% filter(enfermeras>0) %>% 
  mutate(tipo_de_establecimiento = case_when(tipo_de_establecimiento == "CE"~"Unidades de Consulta Externa",
                                             tipo_de_establecimiento == "HO"~"Unidades Hospitalarias")) %>% 
  rename(clave = tipologia) %>% 
  left_join(descriptores_tipologia,by = "clave") %>% rename(clasificacion = descripcion) %>% 
  ggplot(aes(reorder(clasificacion,enfermeras),enfermeras,fill = clasificacion))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(enfermeras)),size = 6)+
  labs(x="",y="Número de enfermeras",
       title = str_wrap(paste0("Número de enfermeras en área de Hospitalización en ", estado),width = 60),
       subtitle = "Al cierre de 2021, en instituciones pertenecientes a la SSA",caption = "Fuente: SSA - SICUENTAS")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",
        axis.text.y = element_text(size=12,color="black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(color = "black", size = 14))+
  coord_flip()


## SICUENTAS Recursos SSA C13.*, todos los medicos desglosados  ----

ssasicuentas2021 %>% filter(entidad == estado) %>%
  select(entidad,starts_with("c13")) %>% pivot_longer(!entidad,names_to = "clave",values_to = "medicos") %>% 
  group_by(clave) %>% summarise(medicos = sum(medicos,na.rm = T)) %>% 
   left_join(recursos_descriptores,by = "clave") %>% 
  ggplot(aes(reorder(descripcion,medicos),medicos,fill = descripcion))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(medicos)),size = 6)+
  labs (x="",y = "Número de Médicos", title = str_wrap(paste0("Médicos por especialización en ", estado),width = 70),
        subtitle = "Al cierre de 2021, en instituciones pertenecientes a la SSA",caption = "Fuente: SSA - SICUENTAS")+
  coord_flip()+
  scale_y_sqrt()+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",
        axis.text.y = element_text(size=15,color="black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(color = "black", size = 14))

## SICUENTAS Recursos SSA C14 y 15.*, Camas censables y no censables  ----

ssasicuentas2021 %>% filter(entidad == estado) %>%
  select(entidad,starts_with("c14"),starts_with("c15")) %>% pivot_longer(!entidad,names_to = "clave",values_to = "camas") %>% 
  group_by(clave) %>% summarise(camas = sum(camas,na.rm = T)) %>% 
  left_join(recursos_descriptores,by = "clave") %>% 
  ggplot(aes(reorder(descripcion,camas),camas,fill = descripcion))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(camas)),size = 6)+
  labs (x="",y = "Número de camas", title = str_wrap(paste0("Camas por área en ", estado),width = 70),
        subtitle = "Al cierre de 2021, en instituciones pertenecientes a la SSA",caption = "Fuente: SSA - SICUENTAS")+
  coord_flip()+
  scale_y_sqrt()+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",
        axis.text.y = element_text(size=15,color="black"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(color = "black", size = 14))



























