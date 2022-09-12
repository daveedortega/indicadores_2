# Indicadores por Estado
# Indicadores Incidencia Delictiva 2015 - 2022: comparativa interanual

# Prepara Espacio ----
pacman::p_load(tidyverse,janitor,scales)
dev.off()
rm(list=ls())
# Incidencia Estatal ----
incidencia_estatal <- read_csv("seguridad/IDEFC_NM_jul22.csv",
                               locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
poblacion_estatal <- read_csv("/Users/NSMJ01TM8X/Desktop/inegi/poblacion_censo2020.csv") %>% clean_names() %>% select(!grupo_quinquenal_de_edad)

# ELEGIR UN NOMBRE y Mes muerto----
incidencia_estatal %>% count(entidad) %>% select(entidad) %>% as.data.frame()
  estado <- "Guanajuato" #################################################################################################### elegir estado
mes_muerto <- 7
mm <- "Julio"

# Comparativa interanual hasta el mes corriente hasta el mes ----

incidencia_estatal_interanual <- incidencia_estatal %>%
  filter(entidad == estado) %>% select(!c(clave_ent,bien_juridico_afectado)) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
               names_to = "mes",values_to = "incidencia") %>% 
  mutate(mes = case_when(mes == "enero"~1,
                         mes == "febrero"~2,
                         mes == "marzo"~3,
                         mes == "abril"~4,
                         mes == "mayo"~5,
                         mes == "junio"~6,
                         mes == "julio"~7,
                         mes == "agosto"~8,
                         mes == "septiembre"~9,
                         mes == "octubre"~10,
                         mes == "noviembre"~11,
                         mes == "diciembre"~12)) %>% 
  filter(mes<mes_muerto) %>% group_by(entidad,ano,subtipo_de_delito) %>% summarise(incidencia = sum(incidencia)) %>% 
  mutate(sexenio = ifelse(ano<2019,"EPN","AMLO"))

## %j% Interanual ----
incidencia_estatal_interanual %>% ungroup() %>% count(subtipo_de_delito) %>% as.data.frame() 

j <-  "Homicidio doloso"
j <-  "Feminicidio"
j <-  "Narcomenudeo"
j <-  "Extorsión"

incidencia_estatal_interanual %>% filter(subtipo_de_delito == j) %>% 
  ggplot(aes(ano,incidencia, fill = sexenio))+
  geom_col(color = "black")+
  geom_label(aes(label = comma(incidencia)),size = 11)+
  labs(x="", y="Carpetas de Investigacion", title = paste0("Carpetas de investigacion por el delito de ",j, " en el estado de ",estado),
       subtitle = paste0("Comparativa para el Periodo Enero - ",mm, ", Entre 2015 - 2022"), 
       caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP) - 2022", fill = "Sexenio")+
  theme(plot.title = element_text(size = 30, color = "#9f2441", face = "bold"), 
        plot.subtitle = element_text(size = 22, face = "bold"), 
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 12,color = "black"), 
        legend.position = c(0.05,0.9))+
  scale_fill_manual(values = c(EPN = "#00A14E", AMLO = "#9F2441"))

































