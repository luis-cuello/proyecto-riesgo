install.packages("devtools")
library(devtools)
install_version("rmarkdown",version=1.8)

install.packages("tinytex")
tinytex::install_tinytex()
install.packages("xfun")
library(xfun)

# Modelo de Riesgo --------------------------------------------------------

library(tidyverse)
library(readxl)


bioetica = read_excel('Data/Bioetica_02.xlsx')
disciplinas = read_excel('Data/Cambios_disciplinas_03.xlsx')
cambios = read_excel('Data/carta_notas_04.xlsx')
data_gral = read_excel('Data/Data_gral_01.xlsx')
miembros_ge = read_excel('Data/GE_miembros_2006_05.xlsx')
etapa = read_excel('Data/Hist_etapa_2006_06.xlsx')
monto = read_excel('Data/Monto_solicitado_07.xlsx')
situaciones_esp = read_excel('Data/Situacion_especiales_08.xlsx')

etapa_data_gral = left_join(etapa, data_gral, by = 'cod_folio') %>% 
  select(- f_fin_proy.y, -duracion.y)


# Cuántos están en cumplimiento vs incumplimiento? ------------------------

glimpse(etapa_data_gral)

table(etapa_data_gral$gl_est_etapa)

aprobados <- c('APROBADA', 'APROB. RATIF.')
en_proceso <- c('I-T EN EVALUACI', 'I-T EN REEVALUA', 'INF.ADICI.RECIB', 'INF. TEC. RECIB') 
rechazados <- c('SOLIC.INF.ADIC', 'PENDIENTE CONSE')

## La categoría: EN EJECUCION, puede ser clasificado como rechazado 
## si se lo adjudicaron antes del 2020

data_prueba <- etapa_data_gral  %>% 
  filter(gl_est_etapa %in% aprobados | gl_est_etapa %in% en_proceso | gl_est_etapa %in% rechazados|
         gl_est_etapa == 'EN EJECUCION' & f_fin_proy.x <= '2021-03-31')  %>% 
  mutate(etapas_agrupadas = case_when(gl_est_etapa %in% aprobados ~ 'APROBADO', 
                                      gl_est_etapa %in% en_proceso ~ 'EN PROCESO', 
                                      gl_est_etapa %in% rechazados ~ 'RECHAZADO', 
                                      gl_est_etapa == 'EN EJECUCION' ~ 'RECHAZADO')) 

table(data_prueba$etapas_agrupadas)

## APROBADO EN PROCESO  RECHAZADO 
## 23423        174        643
## TOTAL 24240 filas 

## linea temporal: en proceso - aprobado

data_prueba %>% group_by(cod_folio) %>% count(cod_folio)
## tenemos 11139 filas unicas por cod_folio

# data_unica <- tibble()
# unicos <- unique(data_prueba$cod_folio)
# data_prueba_cod_fecha <- data_prueba %>% select(cod_folio, f_est_etapa)
# for (elem in data_prueba_cod_fecha){
#  if data_prueba_cod_fecha[elem,]$f_est_etapa 
# }


# graficar cantidad de etapas vs frecuencia -------------------------------

conteo_etapas_folio = etapa_data_gral %>% group_by(cod_folio)%>% count %>% arrange

ggplot(data = conteo_etapas_folio, mapping = aes(y = n)) +
  geom_point()

## tabla proyectos

reshape2::dcast(etapa_data_gral, cod_folio ~ gl_est_etapa)
