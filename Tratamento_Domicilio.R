library (pacman)
p_load(readr, readr, writexl, janitor, gt, writexl, tidyverse,ggthemes,dplyrAssist,dplyr)
library(readr)
library(readxl)
library(writexl)
library(janitor)
library(gt)
library(writexl)
library(tidyverse)
library(ggthemes)
library (dplyrAssist)
library(dplyr)
library(ggplot2)
library(tidyr)
rm(list=ls())
graphics.off()

Dados_dom <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Redcap_Dom.csv')

Dados_dom$tipo_dom3[Dados_dom$usodmrc =="5"] <-  "Residencial"
Dados_dom$tipo_dom3[Dados_dom$usodmrc =="6"] <-  "Residencial"
Dados_dom$tipo_dom3[Dados_dom$usodmrc =="1"] <-  "Nao residencial"
Dados_dom$tipo_dom3[Dados_dom$usodmrc =="2"] <-  "Nao residencial"
Dados_dom$tipo_dom3[Dados_dom$usodmrc =="3"] <-  "Nao residencial"
Dados_dom$tipo_dom3[Dados_dom$usodmrc =="4"] <-  "Nao residencial"

Dados_dom_tra2  <- dplyr:::rename(Dados_dom, "Uso do domicilio" = "usodmrc",
                                  "Seguimento" = "segmrc",
                                  "Data da entrevista" = "ent_dat",
                                  "Area do domicilio" = "dom_area",
                                  "Quarteirao" = "quart")
                                

Dados_dom_tra2$`Uso do domicilio` <-factor (Dados_dom_tra2$`Uso do domicilio`,labels = c("Vazio","Abandonada","Construcao","Comercial","Residencial","Comercial e Residencial","Sem Informacao"), levels = c(1,2,3,4,5,6,9))
Dados_dom_tra2$ `Seguimento` <-factor (Dados_dom_tra2$Seguimento,labels = c("Aceitou","Recusou","Nao encontrado","Acesso Impossibilitado","Sem Informacao"), levels = c(1,2,3,4,9))
Dados_dom_tra2$`Area do domicilio` <-factor (Dados_dom_tra2$`Area do domicilio`,labels = c("Area 2","Area 3","Area 4","Area 5","Area 6","Sem Informacao"), levels = c(2,3,4,5,6,9))


Dados_dom_tra2[is.na(Dados_dom_tra2)] <- "Sem Informacao"


write.csv(Dados_dom_tra2,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Analise_DOM.csv',row.names = F ,fileEncoding = "UTF-8")

####TRATAMENTO DE DADOS DOMICILIO VERIFICAR

Dados_dom <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Analise_DOM.csv')

head(casanova)

Dados_dom_tra2  <- dplyr:::rename(Dados_dom, "Uso do domicilio" = "usodmrc",
                                  "Seguimento" = "segmrc",
                                  "Data da entrevista" = "ent_dat",
                                  "Area do domicilio" = "dom_area",
                                  "Quarteirao" = "quart")

Dados_dom_tra2$`Uso do domicilio` <-factor (Dados_dom_tra2$`Uso do domicilio`,labels = c("Vazio","Abandonada","Construcao","Comercial","Residencial","Comercial e Residencial"), levels = c(1,2,3,4,5,6))
Dados_dom_tra2$ `Seguimento` <-factor (Dados_dom_tra2$Seguimento,labels = c("Aceitou","Recusou","Nao encontrado","Acesso Impossibilitado"), levels = c(1,2,3,4))
Dados_dom_tra2$`Area do domicilio` <-factor (Dados_dom_tra2$`Area do domicilio`,labels = c("Area 2","Area 3","Area 4","Area 5","Area 6"), levels = c(2,3,4,5,6))




write.table(Dados_dom_tra2, file = "C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/deashboardDOM5.csv", sep = ",")

