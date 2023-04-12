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


Dados_seg1 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Corte_GERAL_MRC/Dados_SEG1/SoroinqueritoMRC2-AnalisesIND_SEG1.csv')
Dados_seg2 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Corte_GERAL_MRC/Dados_SEG2/SoroinqueritoMRC2-AnalisesIND_SEG2.csv')
Dados_seg3 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Corte_GERAL_MRC/Dados_SEG3/SoroinqueritoMRC2-AnalisesIND_SEG3.csv')

Dados_seg1$idnova <- as.character(Dados_seg1$idnova)
Dados_seg2$idnova <- as.character(Dados_seg2$idnova)
Dados_seg3$idnova <- as.character(Dados_seg3$idnova)

Dados_seg1$motivo <-factor (Dados_seg1$motivo,labels = c("Nao encontrado","Recusou","Mudou-se","Faleceu","Mentalmente incapaz", "Sem informacao"), levels = c(0,1,2,3,4,9))
Dados_seg2$motivo <-factor (Dados_seg2$motivo,labels = c("Nao encontrado","Recusou","Mudou-se","Faleceu","Mentalmente incapaz", "Sem informacao"), levels = c(0,1,2,3,4,9))
Dados_seg3$motivo <-factor (Dados_seg3$motivo,labels = c("Nao encontrado","Recusou","Mudou-se","Faleceu","Mentalmente incapaz", "Sem informacao"), levels = c(0,1,2,3,4,9))

Dados_seg1$motivo[is.na(Dados_seg1$motivo)] <- "Sem informacao"
Dados_seg2$motivo[is.na(Dados_seg2$motivo)] <- "Sem informacao"
Dados_seg3$motivo[is.na(Dados_seg3$motivo)] <- "Sem informacao"

Dados_seg1$idnova <- as.character(Dados_seg1$idnova)
Dados_seg2$idnova <- as.character(Dados_seg2$idnova)
Dados_seg3$idnova <- as.character(Dados_seg3$idnova)

subdataseg1 <- Dados_seg1[, c("idnova", "nome", "dat_nas","ci_sexo", "consent", "data_consent", "coleta_ind", "dtcoleta", "motivo")]
head(subdataseg1)
colnames(subdataseg1) = c("idnova", "nome_seg1","dt_nasc_seg1","sexo_seg1", "consentseg1", "dt_con_seg1", "soro_seg1", "dt_col_seg1", "motivo_seg1")

subdataseg2 <- Dados_seg2[, c("idnova", "nome", "dat_nas","ci_sexo", "consent", "data_consent", "coleta_ind", "dtcoleta", "motivo")]
head(subdataseg2)
colnames(subdataseg2) = c("idnova", "nome_seg2","dt_nasc_seg2","sexo_seg2", "consent_seg2", "dt_con_seg2", "soro_seg2", "dt_col_seg2", "motivo_seg2")

subdataseg3 <- Dados_seg3[, c("idnova", "nome", "dat_nas","ci_sexo", "consent", "data_consent", "coleta_ind", "dtcoleta", "motivo")]
head(subdataseg3)
colnames(subdataseg3) = c("idnova", "nome_seg3","dt_nasc_seg3","sexo_seg3", "consentseg3", "dt_con_seg3", "soro_seg3", "dt_col_seg3", "motivo_seg3")


write.csv(subdataseg1,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Corte_GERAL_MRC/Corte geral/Dados_corte_geral_seg1.csv',fileEncoding = "UTF-8")
write.csv(subdataseg2,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Corte_GERAL_MRC/Corte geral/Dados_corte_geral_seg2.csv',fileEncoding = "UTF-8")
write.csv(subdataseg3,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Corte_GERAL_MRC/Corte geral/Dados_corte_geral_seg3.csv',fileEncoding = "UTF-8")


