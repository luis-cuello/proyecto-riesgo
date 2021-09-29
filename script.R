knitr::opts_chunk$set(echo = TRUE)
options(tinytex.verbose = TRUE)

library(tidyverse)
library(readxl)
library(janitor)
library(knitr)

data_gral = read_excel('Data/Data_gral_01.xlsx')
etapa = read_excel('Data/Hist_etapa_2006_06.xlsx')
bioetica = read_excel('Data/Bioetica_02.xlsx')
disciplinas = read_excel('Data/Cambios_disciplinas_03.xlsx')
miembros_ge = read_excel('Data/GE_miembros_2006_05.xlsx')
monto = read_excel('Data/Monto_solicitado_07.xlsx')
situaciones_esp = read_excel('Data/Situacion_especiales_08.xlsx')
cambios_a = read_excel('Data/carta_notas_04.xlsx', sheet = "carat")
cambios_b = read_excel('Data/carta_notas_04.xlsx', sheet = "Nota")

### estandarizar nombres

data_gral <- janitor::clean_names(data_gral)
etapa <- janitor::clean_names(etapa)

### modificar formato de las variables

data_gral$cod_folio <- as.character(data_gral$cod_folio)
data_gral$c_t_relac <- as.character(data_gral$c_t_relac)
data_gral$c_region_del_ir <- as.character(data_gral$c_region_del_ir)
data_gral$cod_rut <- as.character(data_gral$cod_rut)
data_gral$gr_rel_disc <- as.character(data_gral$gr_rel_disc)
data_gral$c_disciplina <- as.character(data_gral$c_disciplina)
data_gral$c_region_ejecucion <- as.character(data_gral$c_region_ejecucion)
data_gral$c_region_institucion <- as.character(data_gral$c_region_institucion)

dplyr::glimpse(data_gral)
data_gral %>% naniar::vis_miss()

etapa$cod_folio <- as.character(etapa$cod_folio)
etapa$c_t_proyecto <- as.character(etapa$c_t_proyecto)

aprobados <- c('APROBADA',
               'APROB. RATIF.')
en_proceso <- c('I-T EN EVALUACI',
                'I-T EN REEVALUA',
                'INF.ADICI.RECIB',
                'INF. TEC. RECIB') 
rechazados <- c('SOLIC.INF.ADIC',
                'PENDIENTE CONSE')

etapa <- etapa %>% 
  mutate(tiempo = difftime(f_fin_proy, f_est_etapa, units = "day")) %>% 
  mutate(etapas_agrupadas = 
           case_when(gl_est_etapa %in% aprobados ~ 'APROBADO',
                     gl_est_etapa %in% en_proceso ~ 'EN PROCESO',
                     gl_est_etapa %in% rechazados ~ 'RECHAZADO', 
                     gl_est_etapa == 'EN EJECUCION' ~ 'RECHAZADO')) %>% 
  na.omit()

glimpse(etapa)
etapa %>% naniar::vis_miss()


etapa2 <- etapa %>% 
  group_by(cod_folio) %>% 
  summarise(agno_etapa = max(agno_etapa))

etapa_gral = inner_join(etapa2, etapa, by = c('cod_folio', 'agno_etapa'))

### re-clasificación etapa y eliminar proyectos después 31.03.2021


data_prueba <- etapa_gral  %>% 
  filter(gl_est_etapa %in% aprobados | 
           gl_est_etapa %in% en_proceso | 
           gl_est_etapa %in% rechazados| 
           gl_est_etapa == 'EN EJECUCION' 
         & f_fin_proy <= '2021-03-31')  %>% 
  mutate(etapas_agrupadas = 
           case_when(gl_est_etapa %in% aprobados ~ 'APROBADO',
                     gl_est_etapa %in% en_proceso ~ 'EN PROCESO',
                     gl_est_etapa %in% rechazados ~ 'RECHAZADO', 
                     gl_est_etapa == 'EN EJECUCION' ~ 'RECHAZADO')) 

### unión tablas data_prueba y data_gral (se dejan fuera columnas repetidas)
## data_gral_2 = etapa_gral
data_gral_2 = inner_join(data_prueba, data_gral[ c(-9, -10)], 
                         by = c('cod_folio'))

### agrupación de las regiones por  zona
data_gral_2 <- data_gral_2 %>% 
  mutate(zona = 
           case_when(c_region_ejecucion %in% c(1,2,3,15) ~ "norte",
                     c_region_ejecucion %in% c(4, 5, 13) ~ "centro",
                     c_region_ejecucion %in% c(6, 7, 8) ~ "centro-sur",
                     c_region_ejecucion %in% c(9, 14, 10, 11, 12)~ "sur"))%>% 
  mutate(zona = factor(zona, levels=c("norte", 
                                      "centro", 
                                      "centro-sur", 
                                      "sur")))


Nro_proy <- data_gral %>% 
  group_by(agno_concurso, gl_tiproy, duracion) %>% 
  summarise(Nro = n_distinct(cod_folio))

ggplot(Nro_proy, aes(x = agno_concurso, y = Nro, fill = duracion))+
  geom_bar(stat = "identity")+
  facet_wrap(~gl_tiproy)+
  labs(x = "",
       y = "Nro de proyectos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


knitr::kable(table(data_prueba$etapas_agrupadas))


# gráfica delta de tiempo desde última etapa vs fin del proyecto
data_gral_2$agno_etapa <- as.character(data_prueba$agno_etapa)
data_gral_2$tiempo <- as.numeric(data_prueba$tiempo)

ggplot(data_gral_2 %>% filter(tiempo>=0), aes(x= factor(etapas_agrupadas), y = tiempo)) +
  geom_boxplot() +
  facet_wrap(~gl_tiproy)+
  labs(x = "",
       y = "Delta de tiempo (días)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tabla <- data_gral_2 %>%
  filter(tiempo>=0) %>%
  group_by(gl_tiproy, etapas_agrupadas) %>%
  summarise(mean_tiempo = mean(tiempo),
            max_tiempo = max(tiempo),
            std_tiempo =sd(tiempo))

knitr::kable(tabla)  


knitr::kable(table(data_gral_2$etapas_agrupadas, data_gral_2$zona))


knitr::kable(round(prop.table(table(data_gral_2$agno_etapa, data_gral_2$etapas_agrupadas), margin = 2)*100, 1))


