# Indicadores por Estado
# Indicadores Incidencia Delictiva 2015 - 2022: Series de tiempo

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales)
dev.off()
rm(list=ls())
# Incidencia Estatal ----
incidencia_estatal <- read_csv("seguridad/IDEFC_NM_jul22.csv",
                               locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
poblacion_estatal <- read_csv("/Users/NSMJ01TM8X/Desktop/inegi/poblacion_censo2020.csv") %>% clean_names() %>% select(!grupo_quinquenal_de_edad)
# ELEGIR UN NOMBRE ----
incidencia_estatal %>% count(entidad) %>% select(entidad) %>% as.data.frame()
estado <- "Guanajuato" ############################################################################################################### elegir estado

# Series de Tiempo SISTEMATIZAR DESPUÉS ----

# Homicidio doloso 
j <- "Homicidio doloso"
# Abuso sexual
j <- "Abuso sexual"
# Acoso sexual
j <- "Acoso sexual"
# "Delitos cometidos por servidores públicos"
j <- "Delitos cometidos por servidores públicos"
# "Feminicidio"
j <- "Feminicidio"
# "Violación Simple
j <- "Violación simple"
# "Violación equiparada"
j <- "Violación equiparada"
# "Secuestro"
j <- "Secuestro"
# "Trata de Personas"
j <- "Trata de personas"

labs <- incidencia_estatal %>% filter(entidad == estado) %>% select(!c(clave_ent,bien_juridico_afectado,modalidad)) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito,subtipo_de_delito),names_to = "mes",values_to = "incidencia") %>% 
  mutate(fecha = case_when(mes == "enero"~ as.Date(paste0(ano,"-01-01")),
                           mes == "febrero"~ as.Date(paste0(ano,"-02-01")),
                           mes == "marzo"~ as.Date(paste0(ano,"-03-01")),
                           mes == "abril"~ as.Date(paste0(ano,"-04-01")),
                           mes == "mayo"~ as.Date(paste0(ano,"-05-01")),
                           mes == "junio"~ as.Date(paste0(ano,"-06-01")),
                           mes == "julio"~ as.Date(paste0(ano,"-07-01")),
                           mes == "agosto"~ as.Date(paste0(ano,"-08-01")),
                           mes == "septiembre"~ as.Date(paste0(ano,"-09-01")),
                           mes == "octubre"~ as.Date(paste0(ano,"-10-01")),
                           mes == "noviembre"~ as.Date(paste0(ano,"-11-01")),
                           mes == "diciembre"~ as.Date(paste0(ano,"-12-01")))) %>% 
  group_by(fecha,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia)) %>% 
  filter(subtipo_de_delito == j , fecha %in% c(as.Date("2018-04-01"),as.Date("2021-12-01"),as.Date("2022-06-01"),as.Date("2016-08-01"),
                                               as.Date("2017-10-01"),as.Date("2017-05-01"),as.Date("2019-07-01"),as.Date("2020-08-01"),
                                               as.Date("2022-05-01"))) 

incidencia_estatal %>% filter(entidad == estado) %>% select(!c(clave_ent,bien_juridico_afectado,modalidad)) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito,subtipo_de_delito),names_to = "mes",values_to = "incidencia") %>% 
  mutate(fecha = case_when(mes == "enero"~ as.Date(paste0(ano,"-01-01")),
                           mes == "febrero"~ as.Date(paste0(ano,"-02-01")),
                           mes == "marzo"~ as.Date(paste0(ano,"-03-01")),
                           mes == "abril"~ as.Date(paste0(ano,"-04-01")),
                           mes == "mayo"~ as.Date(paste0(ano,"-05-01")),
                           mes == "junio"~ as.Date(paste0(ano,"-06-01")),
                           mes == "julio"~ as.Date(paste0(ano,"-07-01")),
                           mes == "agosto"~ as.Date(paste0(ano,"-08-01")),
                           mes == "septiembre"~ as.Date(paste0(ano,"-09-01")),
                           mes == "octubre"~ as.Date(paste0(ano,"-10-01")),
                           mes == "noviembre"~ as.Date(paste0(ano,"-11-01")),
                           mes == "diciembre"~ as.Date(paste0(ano,"-12-01")))) %>% 
  group_by(fecha,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia)) %>% 
  filter(subtipo_de_delito == j ) %>% 
  ggplot(aes(fecha,incidencia))+
  geom_line(size =1,color = "#9f2441")+
  geom_smooth()+
  labs (x="",y="Incidencia",title=paste0("Incidencia delictiva: ", j," en ", estado),subtitle = "Entre Enero 2015 y Julio 2022", 
        caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP) - 2022")+
  theme(plot.title = element_text(size=28,face="bold",color="#9f2241"),plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(face="bold",size=12,angle = 90, color = "black"), 
        plot.caption = element_text(size = 12, face = "bold"))+
  geom_label(data = labs,aes(label = comma(incidencia,prefix = paste0(format(fecha,formtat = "%Y-%m"),": "))),size=4)


# Robo Calificado (Con Violencia:) -----

incidencia_estatal %>% count(subtipo_de_delito,modalidad) %>% as.data.frame() %>% filter(str_detect(subtipo_de_delito,"Robo"),
                                                                                         str_detect(modalidad,"Con violencia"))
# Serie de Tiempo

incidencia_estatal %>% filter(entidad == estado) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  filter(str_detect(subtipo_de_delito,"Robo"),str_detect(modalidad,"Con violencia")) %>% select(!modalidad) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito,subtipo_de_delito),names_to = "mes",values_to = "incidencia") %>% 
  mutate(fecha = case_when(mes == "enero"~ as.Date(paste0(ano,"-01-01")),
                           mes == "febrero"~ as.Date(paste0(ano,"-02-01")),
                           mes == "marzo"~ as.Date(paste0(ano,"-03-01")),
                           mes == "abril"~ as.Date(paste0(ano,"-04-01")),
                           mes == "mayo"~ as.Date(paste0(ano,"-05-01")),
                           mes == "junio"~ as.Date(paste0(ano,"-06-01")),
                           mes == "julio"~ as.Date(paste0(ano,"-07-01")),
                           mes == "agosto"~ as.Date(paste0(ano,"-08-01")),
                           mes == "septiembre"~ as.Date(paste0(ano,"-09-01")),
                           mes == "octubre"~ as.Date(paste0(ano,"-10-01")),
                           mes == "noviembre"~ as.Date(paste0(ano,"-11-01")),
                           mes == "diciembre"~ as.Date(paste0(ano,"-12-01")))) %>% 
  group_by(fecha,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia)) %>% 
  filter(!subtipo_de_delito %in% c("Robo de ganado","Robo de maquinaria","Robo de autopartes", "Robo a transeúnte en espacio abierto al público")) %>% 
  ggplot(aes(fecha,incidencia,color = subtipo_de_delito))+
  geom_point()+
  geom_smooth(size = 1)+
  facet_wrap(~subtipo_de_delito,scales = "free_y")+
  labs(x="",y="Carpetas de Investigación",title = paste0("Robo Calificado en todas sus modalidades para el Estado de: ",estado), 
       subtitle = "Entre Enero 2015 y Junio de 2022",caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional Publica (SESNP) - 2022")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=22,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(face="bold",size=12,angle = 90, color = "black"),
        strip.text.x = element_text(size = 14,face="bold"),
        plot.caption = element_text(size = 14))

# Barra comparativa 

incidencia_estatal %>% filter(entidad == estado) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  filter(str_detect(subtipo_de_delito,"Robo"),str_detect(modalidad,"Con violencia")) %>% select(!modalidad) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito,subtipo_de_delito),names_to = "mes",values_to = "incidencia") %>% 
  mutate(fecha = case_when(mes == "enero"~ as.Date(paste0(ano,"-01-01")),
                           mes == "febrero"~ as.Date(paste0(ano,"-02-01")),
                           mes == "marzo"~ as.Date(paste0(ano,"-03-01")),
                           mes == "abril"~ as.Date(paste0(ano,"-04-01")),
                           mes == "mayo"~ as.Date(paste0(ano,"-05-01")),
                           mes == "junio"~ as.Date(paste0(ano,"-06-01")),
                           mes == "julio"~ as.Date(paste0(ano,"-07-01")),
                           mes == "agosto"~ as.Date(paste0(ano,"-08-01")),
                           mes == "septiembre"~ as.Date(paste0(ano,"-09-01")),
                           mes == "octubre"~ as.Date(paste0(ano,"-10-01")),
                           mes == "noviembre"~ as.Date(paste0(ano,"-11-01")),
                           mes == "diciembre"~ as.Date(paste0(ano,"-12-01")))) %>% 
  group_by(fecha,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia)) %>% 
  mutate(ano = format(fecha,format = "%Y")) %>% 
  filter(!subtipo_de_delito %in% c("Robo de ganado","Robo de maquinaria","Robo de autopartes", "Robo a transeúnte en espacio abierto al público")) %>% 
  group_by(ano,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia)) %>% filter(ano %in%c(2020,2021)) %>% filter(incidencia>0) %>% 
  ggplot(aes(reorder(subtipo_de_delito,incidencia),incidencia,group = ano,fill = ano))+
  geom_col(position = position_dodge(width = 1))+
  labs(x="",y="Carpetas de Investigación",title = paste0("Robo Calificado en todas sus modalidades* para el Estado de: ",estado), 
       subtitle = "Entre Enero 2015 y Junio de 2022",caption = "Fuente: SESNEP - 2022
       *Excluye: Robo de ganado, Robo de maquinaria, Robo de autopartes, Robo a transeúnte en espacio abierto al público",fill = "")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=20,face="bold"),
        legend.position ="bottom",axis.text.y = element_text(size=12,color="black"), 
        axis.text.x = element_text(face="bold",size=12,angle = 90, color = "black"),
        legend.text = element_text(size = 12,face="bold") )+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20),)+
  geom_label(aes(label = comma(incidencia)),position = position_dodge(width = 1),size = 8)



## Múltiples Delitos Serie D.t. ----

incidencia_estatal %>% count(subtipo_de_delito) %>% as.data.frame()

seleccion_de_delitos <- c("Robo de vehículo automotor","Acoso sexual","Delitos cometidos por servidores públicos", "Feminicidio","Homicidio doloso", 
                          "Narcomenudeo","Violación equiparada","Violación simple","Violencia familiar")
                          # "Violencia de género en todas sus modalidades distinta a la violencia familiar")

labs <- incidencia_estatal %>% filter(entidad == estado,subtipo_de_delito %in%seleccion_de_delitos) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),names_to = "mes",values_to = "incidencia") %>% 
  group_by(ano,tipo_de_delito) %>% summarise(incidencia = sum(incidencia,na.rm = T)) %>% filter(ano %in%c(2016,2018,2020,2021,2022))

incidencia_estatal %>% filter(entidad == estado,subtipo_de_delito %in% seleccion_de_delitos) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),names_to = "mes",values_to = "incidencia") %>% 
  group_by(ano,tipo_de_delito) %>% summarise(incidencia = sum(incidencia,na.rm = T)) %>% 
  ggplot(aes(ano,incidencia,group=tipo_de_delito,color = tipo_de_delito))+
  geom_point()+
  geom_smooth(se=F)+
  labs(x="",y="Carpetas de Investigación",color = "Delito:",
       title=paste0("Carpetas de Investigación en ",estado),
       subtitle = "Acumulado anual para delitos selectos entre enero 2015 y Julio 2022",
       caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Publica (SESNP) - 2022")+
  geom_label(data=labs,aes(label = comma(incidencia,prefix = paste0(ano,": "))),hjust=0.7,size=5)+
  facet_wrap(~tipo_de_delito,ncol=3,scales = "free_y")+
  theme(plot.title = element_text(size=30,face="bold",color="#9f2241"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position ="none",axis.text.y = element_text(size=12,color="black"), 
        strip.text.x = element_text(size = 15, color = "black", face = "bold"), 
        plot.caption = element_text(size = 15))+
  scale_y_sqrt()


