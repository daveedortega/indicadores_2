# Indicadores por Estado
# Indicadores Incidencia Delictiva 2015 - 2022: Series de tiempo

# Prepara Espacio ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly)

# Cargar Bases ----
prod_bienestar <- read_csv("/Users/NSMJ01TM8X/Desktop/indicadores/input/agricultura/padron_prodbien_2021cierre.csv") %>% clean_names()

prod_bienestar %>%count(nombre_de_la_delegacion) %>% as.data.frame()
estado <- "HIDALGO" ######################################################################################################## ELEGIR ESTADO
## Lugar nacional de Apoyos ----
estado_lugar <-prod_bienestar %>% group_by(nombre_de_la_delegacion) %>% 
  summarise(monto = sum(monto_apoyado)) %>% mutate(lugar = rank(monto)) %>% filter(nombre_de_la_delegacion == estado)

prod_bienestar %>% group_by(nombre_de_la_delegacion) %>% summarise(monto = sum(monto_apoyado)) %>% mutate(lugar = rank(desc(monto))) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,monto),monto,fill = nombre_de_la_delegacion))+
  geom_col()+
  geom_col(data = estado_lugar,color = "red",size = 3)+
  geom_label(aes(label = comma(monto,prefix = "$")))+
  geom_label(y=1,aes(label = comma(lugar,suffix = "°")))+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), axis.text.x = element_text(angle = 90))+
  labs (x="",y="",title = paste0("Distribución de Apoyos para Producción para el Bienestar"),
        subtitle = "Durante 2021",caption = "Fuente: Bienestar y SEGOB - PUB 2021")+
  coord_flip()+
  scale_y_sqrt()

  


# Monto en por municipio ----

prod_bienestar %>% filter(nombre_de_la_delegacion == estado) %>% group_by(nombre_del_municipio) %>% summarise(monto = sum(monto_apoyado)) %>% 
  ungroup() %>% mutate(porcentaje = round(100*monto / sum(monto),2)) %>% 
  slice_max(order_by = monto,n = 25) %>% 
  ggplot(aes(reorder(nombre_del_municipio,monto),monto,fill = nombre_del_municipio))+
  geom_col()+
  geom_label(aes(label = comma(monto,prefix = "$")))+
  geom_label(y=1000000,aes(label = comma(porcentaje,suffix = "% Estatal",accuracy = 0.1)),size=6)+
  coord_flip()+
  labs (x="",y="Pesos",title = paste0("Municipios de ",estado," más benefiados por Producción para el bienestar"),
        subtitle = "25 más beneficiados durante 2021",caption = "Fuente: Bienestar y SEGOB - PUB 2021")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"))
  
## Monto por Sexo ----

prod_bienestar %>% filter(nombre_de_la_delegacion == estado) %>% group_by(sexo) %>% summarise(monto = sum(monto_apoyado)) %>% 
  ungroup() %>% mutate(porcentaje = round(100*monto / sum(monto),2)) %>% 
  ggplot(aes(reorder(sexo,monto),monto,fill = sexo))+
  geom_col()+
  geom_label(aes(label = comma(monto,prefix = "$")),size=8)+
  geom_label(y=1000000,aes(label = comma(porcentaje,suffix = "% Estatal",accuracy = 0.1)),size=12)+
  labs (x="",y="Pesos",title = paste0("Produccion para el Bienestar: Apoyos entregados por Sexo del receptor en: ",estado),
        subtitle = "Durante 2021",caption = "Fuente: Bienestar y SEGOB - PUB 2021")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(size=20,color ="black"))

## Monto por Cultivo ----

prod_bienestar %>% filter(nombre_de_la_delegacion == estado) %>% group_by(cultivo) %>% summarise(monto = sum(monto_apoyado)) %>% 
  ungroup() %>% mutate(porcentaje = round(100*monto / sum(monto),2)) %>% 
  ggplot(aes(reorder(cultivo,monto),monto,fill = cultivo))+
  geom_col()+
  geom_label(y = 0, aes(label = comma(porcentaje,suffix = "% Estatal",accuracy = 0.1)))+
  geom_label(aes(label = comma(monto,prefix = "$")),size=5,vjust = -0.15)+
  labs (x="",y="Pesos",title = paste0("Produccion para el Bienestar: Apoyos entregados por cultivo en: ",estado),
        subtitle = "Durante 2021",caption = "Fuente: Bienestar y SEGOB - PUB 2021")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(size=10,color ="black",angle = 90))+
  scale_y_sqrt()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))





prod_bienestar %>% filter















