# Indicadores por Estado
# Indicadores Incidencia Delictiva 2015 - 2022: Rankings estatales

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales)
dev.off()
rm(list=ls())
# Incidencia Estatal ----
incidencia_estatal <- read_csv("seguridad/IDEFC_NM_may22.csv",
                            locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

poblacion_estatal <- read_csv("/Users/NSMJ01TM8X/Desktop/inegi/poblacion_censo2020.csv") %>% 
  clean_names() %>% select(!grupo_quinquenal_de_edad)

incidencia_estatal %>% count(entidad) %>% select(entidad) %>% as.data.frame()
estado <- "Chiapas" ############################################################################################################### elegir estado

# Total por Tipo de Delito año corriente -----

incidencia_estatal %>% filter(entidad == estado,ano==2022) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),names_to = "mes",values_to = "incidencia") %>% 
  group_by(ano,tipo_de_delito) %>% summarise(incidencia = sum(incidencia,na.rm = T)) %>% filter(incidencia>0) %>%
  slice_max(incidencia,n=10) %>% 
  ggplot(aes(reorder(tipo_de_delito,incidencia),incidencia,fill=tipo_de_delito))+
  geom_col(color = "black")+
  scale_y_sqrt()+
  labs(x="",y="Carpetas de Investigación",
       title=paste0("Carpetas de Investigación en ",estado),subtitle = "Por tipo de Delito para los diez delitos mas denunciados entre enero y junio de 2022",
       caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Publica (SESNSP) - 2022")+
  theme(plot.title = element_text(size=28,face="bold",color="#9f2041"),
        plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",
        axis.text.y = element_text(size=22,color="black"),
        plot.caption = element_text(size = 15))+
  coord_flip()+
  geom_label(aes(label = comma(incidencia)),size=10)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))


incidencia_estatal %>% filter(entidad == estado,ano==2022) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),names_to = "mes",values_to = "incidencia") %>% 
   summarise(incidencia = sum(incidencia,na.rm = T)) 

incidencia_estatal %>% filter(entidad == estado,ano==2022) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),names_to = "mes",values_to = "incidencia") %>% 
  group_by(ano,tipo_de_delito) %>% summarise(incidencia = sum(incidencia,na.rm = T)) %>% filter(incidencia>0) %>% ungroup() %>%
  summarise(tipo_de_delito,porcentaje=round(incidencia/sum(incidencia)*100,2)) %>% arrange(desc(porcentaje)) %>% 
  head(8)

# Ranking delictivo Absoluto ----

incidencia_estatal %>% count(subtipo_de_delito) %>% as.data.frame()

seleccion_delitos <- c("Abuso sexual","Acoso sexual","Delitos cometidos por servidores públicos", "Feminicidio","Homicidio doloso",
                       "Secuestro","Trata de personas","Violación simple","Violación equiparada")

ranking_absoluto <- incidencia_estatal  %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
               names_to = "mes",values_to = "incidencia") %>% 
  group_by(ano,entidad,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia,na.rm = T)) %>% 
  filter(ano==2021,subtipo_de_delito %in% seleccion_delitos) %>% group_by(subtipo_de_delito) %>%
  mutate(ranking = rank(desc(incidencia))) 

ranking_absoluto %>% arrange(ranking)


# MANUAL; SISTEMATIZAR DESPUÉS ----

# "Abuso sexual"
i <- "Abuso sexual"
# "Acoso sexual"
i <- "Acoso sexual"
# "Delitos cometidos por servidores públicos"
i <- "Delitos cometidos por servidores públicos"
# "Feminicidio"
i <- "Feminicidio"
# "Homicidio doloso"
i <-  "Homicidio doloso"
# "Secuestro"
i <- "Secuestro"
# "Violación simple"
i <- "Violación simple"
# "Trata de personas"
i <- "Trata de personas"
# "Trata de personas"
i <- "Violación equiparada"

subset(ranking_absoluto,subtipo_de_delito == i) %>% 
  ggplot(aes(reorder(entidad,ranking),ranking,fill = entidad,group = subtipo_de_delito))+
  geom_col()+
  geom_col(data = subset(subset(ranking_absoluto,entidad ==estado),subtipo_de_delito ==i),aes(),color = "red",size=2)+
  geom_label(aes(label = comma(ranking,suffix = "°")),size=6)+
  labs(x="",y="Ranking delictivo Nacional",color = "Estado:", title=paste0("Ranking de hechos delictivos: ",estado," - ", i),
        subtitle = "Para delitos durante 2021", caption = "Fuente: SESNP")+
  theme(plot.title = element_text(size=22,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(face="bold",size=12,angle = 90, color = "black"))+
  scale_x_discrete(labels = wrap_format(10))



## Incidencia Pesada por Poblacion ----
# Estan todos igual!
poblacion_estatal <- poblacion_estatal %>% rename(entidad  = entidad_federativa)


seleccion_delitos <- c("Abuso sexual","Acoso sexual","Delitos cometidos por servidores públicos", "Feminicidio","Homicidio doloso",
                       "Secuestro","Trata de personas","Violación simple","Violación equiparada")

ranking_relativo <- ranking_absoluto %>% left_join(poblacion_estatal,by="entidad") %>%
  mutate(incidenciax1000 = round(incidencia /(poblacion/1000),3)) %>% 
  group_by(subtipo_de_delito) %>% mutate(ranking  = rank(desc(incidenciax1000)))


# MANUAL; SISTEMATIZAR DESPUÉS ----

# "Abuso sexual"
i <- "Abuso sexual"
# "Acoso sexual"
i <- "Acoso sexual"
# "Delitos cometidos por servidores públicos"
i <- "Delitos cometidos por servidores públicos"
# "Feminicidio"
i <- "Feminicidio"
# "Homicidio doloso"
i <-  "Homicidio doloso"
# "Secuestro"
i <- "Secuestro"
# "Violación simple"
i <- "Violación simple"
# "Trata de personas"
i <- "Trata de personas"
# "Trata de personas"
i <- "Violación equiparada"

subset(ranking_relativo,subtipo_de_delito == i) %>% 
  ggplot(aes(reorder(entidad,ranking),ranking,fill = entidad,group = subtipo_de_delito))+
  geom_col()+
  geom_col(data = subset(subset(ranking_relativo,entidad ==estado),subtipo_de_delito ==i),aes(),color = "red",size=2)+
  geom_label(aes(label = comma(ranking,suffix = "°")),size=6)+
  labs(x="",y="Ranking delictivo Nacional",color = "Estado:", title=paste0("Ranking de hechos delictivos: ",estado," - ", i),
       subtitle = "Para delitos durante 2021, incidencia por cada mil habitantes", caption = "Fuente: SESNP")+
  theme(plot.title = element_text(size=28,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(face="bold",size=12,angle = 90, color = "black"))+
  scale_x_discrete(labels = wrap_format(10))
