#---------------------------------------------------------------------------------
#           PROBABILIDADES DE GANAR LA PREMIER LEAGUE 
#
# Autor: César Anderson Huamaní Ninahuanca.
#---------------------------------------------------------------------------------

library(dplyr)
library(rvest)
#devtools::install_github("CesarAHN/datametria")
library(datametria)
library(ggplot2)
library(tidyr)
library(gt)
#devtools::install_github("jthomasmock/gtExtras")
library(gtExtras)
library(ggridges)

#----------------------
# Tabla de posiciones. 
#----------------------
pw<-read_html("https://www.espn.com.pe/futbol/posiciones/_/liga/fifa.worldq.conmebol")

tab_pos<-pw %>% html_elements("table.Table.Table--align-right") %>% html_table() %>% as.data.frame() %>% as_tibble()
gsub("(.*)([A-Z].*)","\\2",tab_pos$X2023)->tab_pos$X2023
names(tab_pos)[1]<-"PAÍS"
tab_pos$PAÍS<-ifelse(tab_pos$PAÍS=="Bolívia","Bolivia",tab_pos$PAÍS)

tab_pos %>% as_tibble() %>% gt() %>%
  gt_theme_espn() %>% tab_header(title = "TABLA DE POSICIONES ELIMINATORIAS SUDAMERICANAS",
                                 subtitle = paste0("Actualizado al ", Sys.Date())) %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  gtsave("plots/tab_pos.png")


#-------------
# Calendario.
#-------------
pw<-read_html("https://es.wikipedia.org/wiki/Clasificaci%C3%B3n_de_Conmebol_para_la_Copa_Mundial_de_F%C3%BAtbol_de_2026")

fechas<-pw %>% html_elements("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(34)") %>%
  html_table() %>% as.data.frame()

fechas %>% filter(grepl("vs",Resultado)) %>% select(Var.3,Var.7) %>% rename(Local=Var.3,Visitante=Var.7)->tab_calendario

#-------------------------------------------------------------------------------
#------------------------
# Puntajes de desempeño.
#------------------------
# Puntaje de juego de local
# De 1 al 10, donde 1 es que le va muy mal jugando de visita y 10 que le va muy bien.
p_local<-tibble(PAÍS=sort(tab_pos$PAÍS), `PUNTAJE LOCAL`=c(9,7,7,5,7.5,7,7,5,7.5,6)/10)

# Puntaje de juego de visita 
# De 1 al 10, donde 1 es que le va muy mal jugando de local y 10 que le va muy bien.
p_visita<-tibble(PAÍS=sort(tab_pos$PAÍS), `PUNTAJE VISITA`=c(8,3.5,6,4,7,6,5,4,6.5,5.5)/10)

pp<-plyr::join_all(list(p_local,p_visita), by="PAÍS", type = "inner") %>% as_tibble()

pp %>% as_tibble() %>% gt() %>%
  gt_theme_espn() %>% 
  tab_header(title = "ÍNDICE DE DESEMPEÑO DE LOCAL Y VISITANTE PARA CADA UNA DE LAS SELECCIONES",
             subtitle = "Puntajes según juicios de expertos") %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  gtsave(r"(plots\desempeño_selecciones.png)")

#---------------------------------------------------------------------------------------
# creando la Función que simule los resultados de los partidos. 

simulador_partidos<-function(tab_pos,pp,tab_calendario){
  #---------------------
  # Partidos por jugar.
  resultados<-data.frame()
  for (i in 1:nrow(tab_calendario)) {
    d<-(1-(pp[pp$PAÍS %in% tab_calendario[i,1],2]-pp[pp$PAÍS %in% tab_calendario[i,2],3])) %>% pull()
    if(d<1){
      a1<-rbind(data.frame(PAÍS=tab_calendario[i,1], GF_=sample(0:4,1, prob = c(1*d/10,2*d/10,3*d/10,4*d/10,(1-d)))),
                data.frame(PAÍS=tab_calendario[i,2], GF_=sample(0:4,1, prob = c((1-d),5*d/14,4*d/14,3*d/14,2*d/14)))) 
    } else {
      d<-(1-(pp[pp$PAÍS %in% tab_calendario[i,2],2]-pp[pp$PAÍS %in% tab_calendario[i,1],3])) %>% pull()
      a1<-rbind(data.frame(PAÍS=tab_calendario[i,2], GF_=sample(0:4,1, prob = c(1*d/10,2*d/10,3*d/10,4*d/10,(1-d)))),
                data.frame(PAÍS=tab_calendario[i,1], GF_=sample(0:4,1, prob = c((1-d),5*d/14,4*d/14,3*d/14,2*d/14)))) 
    }
    a1$GC_<-rev(a1$GF_)
    a1$PTS_<-ifelse(a1$GF_>a1$GC_,3,
                    ifelse(a1$GF_==a1$GC_,1,0))
    a1$G_<-ifelse(a1$PTS_==3,1,0)
    a1$P_<-rev(a1$G_)
    a1$E_<-ifelse(a1$PTS_==1,1,0) 
    resultados<-rbind(resultados,a1)
  }
  resultados %>% group_by(PAÍS) %>% mutate(n=1) %>% summarise_all(~sum(.))->resultados
  
  tab_pos_final<-left_join(tab_pos,resultados, by="PAÍS")
  
  tab_pos_final<-tab_pos_final %>% mutate(J=J+n, G=G+G_, E=E+E_, P=P+P_, GF=GF+GF_, GC=GC+GC_,
                                          PTS=PTS+PTS_, DIF=GF-GC) %>% select(!matches("_|^n$")) %>% arrange(-PTS,-DIF,-G) %>% 
    mutate(PUESTO=1:n())
  
  return(tab_pos_final)
}

simulador_partidos(tab_pos,pp,tab_calendario)

# Para tomar menos tiempo - Código eficiente.
# asignando el número de repeticiones.
repeticiones<-50000
resul<-data.frame(PAÍS=vector("character",length = repeticiones*10),
                  PTS=vector("numeric",length = repeticiones*10),
                  PUESTO=vector("integer",length = repeticiones*10))

# Corriendo el número de repeticiones.
for (i in 1:repeticiones) {
  resul[(10*(i-1)+1):(10*(i-1)+10),]<-simulador_partidos(tab_pos,pp,tab_calendario)[,c(1,9,10)]
}

saveRDS(resul, "D:/simulaciones2.rds")
resul<-readRDS("D:/simulaciones2.rds")

# Save result

resul %>% arrange(PAÍS) %>% 
  ggplot(aes(x=PTS))+
  geom_density(fill="sienna2", alpha=.7)+
  facet_wrap(~PAÍS, scales = "free")+
  scale_x_continuous(breaks = seq(0,45,by=10), limits = c(9,50))+
  labs(title = "DISTRIBUCIÓN DE LOS POSIBLES PUNTAJES POR SELECCIONES",
       subtitle = "Eliminatorias sudamerica - Mundial 2026.", x="Puntos", y="Frecuencia",
       caption = "Resultados luego de 50 000 repeticiones.\nELABORACIÓN: https://github.com/CesarAHN")+
  theme_bw()+
  theme(plot.caption = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold"))

#----
# Puesto.
resul %>% arrange(PAÍS) %>% 
  ggplot(aes(x=PUESTO))+
  geom_histogram(fill="skyblue3", alpha=.7)+
  facet_wrap(~PAÍS, scales = "free_x")+
  scale_x_continuous(breaks = seq(1,20,by=1))+
  labs(title = "DISTRIBUCIÓN DE LOS POSIBLES PUESTOS POR SELECCIONES",
       subtitle = "Eliminatorias sudamerica - Mundial 2026.", x="Puestos", y="Frecuencia",
       caption = "Resultados luego de 50 000 repeticiones.\nELABORACIÓN: https://github.com/CesarAHN")+
  theme_bw()+
  theme(plot.caption = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold"))

#----
logos<-data.frame(PAÍS=sort(unique(tab_pos$PAÍS)),
                     LOGO=c("https://cdn-icons-png.flaticon.com/512/197/197573.png", # Argentina
                            "https://cdn-icons-png.flaticon.com/512/197/197504.png", # Bolivia.
                            "https://cdn-icons-png.flaticon.com/512/3909/3909370.png", # Brasil
                            "https://cdn-icons-png.flaticon.com/512/197/197586.png", # Chile.
                            "https://cdn-icons-png.flaticon.com/512/197/197575.png", # Colombia.
                            "https://cdn-icons-png.flaticon.com/512/197/197588.png", # Ecuador
                            "https://cdn-icons-png.flaticon.com/512/197/197376.png", # Paraguay.
                            "https://cdn-icons-png.flaticon.com/512/197/197563.png", # Peru
                            "https://cdn-icons-png.flaticon.com/512/197/197599.png", # Uruguay.
                            "https://cdn-icons-png.flaticon.com/512/197/197580.png")) # Venezuela
resul<-left_join(resul, logos)

resul %>% group_by(LOGO) %>% count(PUESTO) %>% mutate(p=n/50000) %>% select(-n) %>% 
  mutate(PUESTO=paste0("PUESTO ",PUESTO)) %>% spread(PUESTO,p) %>% 
  select(LOGO,paste0("PUESTO ",1:10)) %>% as_tibble() %>% gt() %>% 
  fmt_percent(columns = matches("^PUES")) %>% fmt_missing(columns = matches("^PUES"), missing_text = "-") %>% 
  tab_header(title = "PROBABLIDADES POR PUESTO AL CULMINAR LAS ELIMINATORIAS",
             subtitle = "Eliminatorias sudamerica - Mundial 2026.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(paste0("PUESTO ",1:10))))) %>% 
  gt_color_rows(`PUESTO 1`:`PUESTO 10`, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(plots\probabilidades_selecciones.png)", vwidth = 2000, vheight = 900)

resul %>% as_tibble() %>% mutate(clasificacion=case_when(PUESTO<=6~"SI",
                                         TRUE~"NO")) %>% group_by(LOGO) %>% 
  count(clasificacion) %>% mutate(p=n/50000) %>% select(-n) %>% 
  spread(clasificacion,p) %>% arrange(-SI) %>% as_tibble() %>% filter(!is.na(SI)) %>% gt() %>% 
  fmt_percent(column = c(NO,SI)) %>% fmt_missing(columns = c("NO","SI"), missing_text = "") %>% 
  tab_header(title = "PROBABLIDADES DE CLASIFICAR DIRECTO AL MUNDIAL",
             subtitle = "Eliminatorias sudamerica - Mundial 2026.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(NO,SI)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(NO,SI)))) %>% 
  gt_color_rows(NO:SI, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(plots\probabilidades_clasificar_6to.png)", vwidth = 300, vheight = 1000)

resul %>% as_tibble() %>% mutate(clasificacion=case_when(PUESTO<=7~"SI",
                                                         TRUE~"NO")) %>% group_by(LOGO) %>% 
  count(clasificacion) %>% mutate(p=n/50000) %>% select(-n) %>% 
  spread(clasificacion,p) %>% arrange(-SI) %>% as_tibble() %>% filter(!is.na(SI)) %>% gt() %>% 
  fmt_percent(column = c(NO,SI)) %>% fmt_missing(columns = c("NO","SI"), missing_text = "") %>% 
  tab_header(title = "PROBABLIDADES DE CLASIFICAR AL REPECHAJE",
             subtitle = "Eliminatorias sudamerica - Mundial 2026.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(NO,SI)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(NO,SI)))) %>% 
  gt_color_rows(NO:SI, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(plots\probabilidades_clasificar_7mo.png)", vwidth = 300, vheight = 1000)
