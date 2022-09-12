# Indicadores por Estado
# Indicadores Cierre Agrícola 2020

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales)
rm(list=ls())
dev.off()
# Cierre Agrícola SIAP ----
cierre_agricola <- read_csv("/Users/NSMJ01TM8X/Desktop/indicadores/input/agricultura/Cierre_agricola_mun_2021.csv",
                            locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
# ELEGIR UN NOMBRE ----
cierre_agricola %>% count(nomestado) %>% select(nomestado) %>% as.data.frame()
estado <- "Guanajuato" ############################################################################################################### elegir estado

# Principales Cultivos (Valor)----
cierre_agricola %>% filter(nomestado==estado) %>% group_by(nomcultivo) %>% summarise(valor = sum(valorproduccion)) %>% 
  slice_max(order_by=valor,n=10) %>% 
  ggplot(aes(reorder(nomcultivo,valor),valor,fill=nomcultivo))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(valor,prefix = "$")),size=7,hjust=0.8)+
  labs(x="",y="Pesos",title = paste0("Principales Cultivos del Estado de ",estado),subtitle = "Durante 2021, Niveles durante 2021",
       caption = "Fuente: Sistema de Informacion Agroalimentaria y Pesquera (SIAP) - 2021")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=22,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        plot.caption = element_text(size = 14))+
  coord_flip()
  

# Principales Cultivos (Volumen)----
cierre_agricola %>% filter(nomestado==estado,nomunidad =="Tonelada") %>% group_by(nomcultivo) %>% 
  summarise(volumen = sum(volumenproduccion)) %>% 
  slice_max(order_by=volumen,n=10) %>% 
  ggplot(aes(reorder(nomcultivo,volumen),volumen,fill=nomcultivo))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(volumen)),size=7,hjust=0.6)+
  labs(x="",y="Toneladas",title = paste0("Principales Cultivos del Estado de ",estado),subtitle = "Durante 2021, Niveles durante 2021",
       caption = "Fuente: SIAP 2021")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"))+
  coord_flip()+
  scale_y_sqrt()

# Principales cultivos, stacked por modalidad en valor ----

cierre_agricola %>% filter(nomestado==estado) %>% group_by(nomcultivo,nommodalidad) %>% summarise(valor = sum(valorproduccion)) %>% 
  ungroup() %>% 
  slice_max(order_by=valor,n=14) %>% 
  ggplot(aes(reorder(nomcultivo,valor),valor,fill=nommodalidad,group = nommodalidad))+
  geom_col(position = position_stack())+
  geom_label(aes(label = comma(valor,prefix = "$")),size=6,position = position_stack())+
  labs(x="",y="Pesos",title = paste0("Principales Cultivos del Estado de ",estado),subtitle = "Por modalidad de cultivo durante 2021, Niveles durante 2021",
       caption = "Fuente: SIAP 2021", fill = "Proceso: ")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="bottom",axis.text.y = element_text(size=12,color="black"), axis.text.x = element_text(size = 14,color ="black"),
        legend.text = element_text(size=15))+
  scale_y_sqrt()+
  scale_x_discrete(labels = wrap_format(10))

# Porcentaje de tierra de temporal vs. de riego ----

cierre_agricola %>% filter(nomestado==estado) %>% group_by(nommodalidad) %>% 
  summarise(superficie = sum(sembrada), valorproduccion = sum(valorproduccion)) %>% ungroup() %>% 
  mutate(porcentaje = round(superficie / sum(superficie)*100,2),porcentaje_v =  round(valorproduccion / sum(valorproduccion)*100,2)) %>% 
  ggplot(aes(x="",y = porcentaje,fill = nommodalidad))+
  geom_bar(stat = "identity",width = 1,color = "black")+
  coord_polar("y",start = 0)+
  geom_label(y=c(80,20),aes(label = comma(porcentaje,suffix = "%")),size = 18)+
  labs(x="",y="",title = paste0("Porcentaje de Tierras Agricolas en el Estado de ",estado),subtitle = "Anual total durante 2021, por modalidad de cultivo",
       caption = "Fuente: SIAP - 2021",fill = "Modalidad: ")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="bottom",axis.text.y = element_text(size=12,color="black"), axis.text.x = element_text(size = 14,color ="black"),
        legend.text = element_text(size = 16,face = "bold"))
# Porcentaje de valor de temporal vs. de riego ----

cierre_agricola %>% filter(nomestado==estado) %>% group_by(nommodalidad) %>% 
  summarise(superficie = sum(sembrada), valorproduccion = sum(valorproduccion)) %>% ungroup() %>% 
  mutate(porcentaje = round(superficie / sum(superficie)*100,2),porcentaje_v =  round(valorproduccion / sum(valorproduccion)*100,2)) %>% 
  ggplot(aes(x="",y = porcentaje_v,fill = nommodalidad))+
  geom_bar(stat = "identity",width = 1,color = "black")+
  coord_polar("y",start = 0)+
  geom_label(y=c(80,20),aes(label = comma(porcentaje_v,suffix = "%")),size = 18)+
  labs(x="",y="",title = paste0("Porcentaje de Valor Agricola en el Estado de ",estado),subtitle = "Anual total durante 2021, por modalidad de cultivo",
       caption = "Fuente: SIAP - 2021",fill = "Modalidad: ")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="bottom",axis.text.y = element_text(size=12,color="black"), axis.text.x = element_text(size = 14,color ="black"),
        legend.text = element_text(size = 16,face = "bold"))


# Principales cultivos de Riego ----

cierre_agricola %>% filter(nomestado==estado,nommodalidad=="Riego") %>% group_by(nomcultivo) %>% summarise(valor = sum(valorproduccion)) %>% 
  slice_max(order_by=valor,n=10) %>% 
  ggplot(aes(reorder(nomcultivo,valor),valor,fill=nomcultivo))+
  geom_col()+
  geom_label(aes(label = comma(valor,prefix = "$")),size=7,hjust=0.6)+
  labs(x="",y="Pesos",title = paste0("Principales Cultivos de Riego del Estado de ",estado),subtitle = "Durante 2021, Niveles durante 2021",
       caption = "Fuente: SIAP 2021")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"))+
  coord_flip()

# Principales cultivos de Temporal ----
cierre_agricola %>% filter(nomestado==estado,nommodalidad!="Riego") %>% group_by(nomcultivo) %>% summarise(valor = sum(valorproduccion)) %>% 
  slice_max(order_by=valor,n=10) %>% 
  ggplot(aes(reorder(nomcultivo,valor),valor,fill=nomcultivo))+
  geom_col()+
  geom_label(aes(label = comma(valor,prefix = "$")),size=7,hjust=0.6)+
  labs(x="",y="Pesos",title = paste0("Principales Cultivos de Temporal del Estado de ",estado),subtitle = "Durante 2021, Niveles durante 2021",
       caption = "Fuente: SIAP 2021")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"))+
  coord_flip()

# Principales cultivos por extensión Temporal ----

cierre_agricola %>% filter(nomestado==estado) %>% group_by(nomcultivo) %>% summarise(sembrada = sum(sembrada),cosechada = sum(cosechada)) %>% 
  slice_max(order_by=sembrada,n=10) %>% pivot_longer(!nomcultivo,names_to = "Proceso",values_to = "hectareas") %>%  
  filter (Proceso=="cosechada") %>% 
  ggplot(aes(reorder(nomcultivo,hectareas),hectareas,fill=Proceso))+
  geom_col(position = "identity")+
  geom_label(aes(label = comma(hectareas,prefix = "H: ")),size=7,hjust=0.6)+
  labs(x="",y="Hectareas",title = paste0("Cultuvos con mayor estensión de tierra en el Estado de ",estado),subtitle = "Durante 2021",
       caption = "Fuente: SIAP 2021")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"))+
  coord_flip()+
  scale_y_sqrt()









































































