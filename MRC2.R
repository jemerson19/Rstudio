library (pacman)
library(readr)
library(readxl)
library(writexl)
library(janitor)
library(gt)
library(writexl)
library(ggthemes)
library (dplyrAssist)
library(ggplot2)
library(tidyr)
library(plyr)
library(dbplyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(imputeTS)
library(table1)


rm(list=ls())
graphics.off()

#####################VERIFICAÇÃO DE RESULTADOS JAQUELINE ELISA IGG##################################
Resultados_Elisa_seg1 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Resultado_ELISA/Resultados_elisa_SEGUI1.csv')
Resultados_Elisa_seg2 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Resultado_ELISA/Resultados_elisa_SEGUI2.csv')
Resultados_Elisa_seg3 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Resultado_ELISA/Resultados_elisa_SEGUI3.csv')

Resultados_Elisa_seg1$idnova <- 
              as.character(Resultados_Elisa_seg1$idnova)
Resultados_Elisa_seg2$idnova <- 
              as.character(Resultados_Elisa_seg2$idnova)
Resultados_Elisa_seg3$idnova <- 
              as.character(Resultados_Elisa_seg3$idnova)

names(Resultados_Elisa_seg1)[9] <- "IgG_Dengue_SEG1"
names(Resultados_Elisa_seg1)[10] <- "IgG_Chikungunya_SEG1"
names(Resultados_Elisa_seg1)[11] <- "IgG_Covid_SEG1"
names(Resultados_Elisa_seg1)[12] <- "IgG_Toxoplasma_SEG1"

names(Resultados_Elisa_seg2)[6] <- "IgG_Dengue_SEG2"
names(Resultados_Elisa_seg2)[7] <- "IgG_Chikungunya_SEG2"
names(Resultados_Elisa_seg2)[8] <- "IgG_Covid_SEG2"
names(Resultados_Elisa_seg2)[9] <- "IgG_Toxoplasma_SEG2"

names(Resultados_Elisa_seg3)[5] <- "IgG_Dengue_SEG3"
names(Resultados_Elisa_seg3)[6] <- "IgG_Chikungunya_SEG3"
names(Resultados_Elisa_seg3)[7] <- "IgG_Covid_SEG3"


Resultados_Elisa_seg1 <- 
              Resultados_Elisa_seg1 %>% 
              select(casanova,idnova,dtcoleta,cens_idade,IgG_Dengue_SEG1,IgG_Chikungunya_SEG1,IgG_Covid_SEG1,IgG_Toxoplasma_SEG1)
Resultados_Elisa_seg2 <- 
              Resultados_Elisa_seg2 %>% 
              select(casanova,idnova,dtcoleta,cens_idade,IgG_Dengue_SEG2,IgG_Chikungunya_SEG2,IgG_Covid_SEG2,IgG_Toxoplasma_SEG2)
Resultados_Elisa_seg3 <- 
              Resultados_Elisa_seg3 %>% 
              select(casanova,idnova,dtcoleta,cens_idade,IgG_Dengue_SEG3,IgG_Chikungunya_SEG3,IgG_Covid_SEG3)

#SEGUIMENTO 1
Resultados_Elisa_seg1$IgG_Dengue_SEG1 <- 
          factor (Resultados_Elisa_seg1$IgG_Dengue_SEG1,
          labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), 
          levels = c(1,0,2,9))
Resultados_Elisa_seg1$IgG_Dengue_SEG1[is.na
          (Resultados_Elisa_seg1$IgG_Dengue_SEG1)] <- "Sem informacao"

Resultados_Elisa_seg1$IgG_Chikungunya_SEG1 <- 
          factor (Resultados_Elisa_seg1$IgG_Chikungunya_SEG1,
          labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"),
          levels = c(1,0,2,9))
Resultados_Elisa_seg1$IgG_Chikungunya_SEG1[is.na
          (Resultados_Elisa_seg1$IgG_Chikungunya_SEG1)] <- "Sem informacao"

Resultados_Elisa_seg1$IgG_Covid_SEG1 <-
          factor (Resultados_Elisa_seg1$IgG_Covid_SEG1,
          labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), 
          levels = c(1,0,2,9))
Resultados_Elisa_seg1$IgG_Covid_SEG1[is.na
          (Resultados_Elisa_seg1$IgG_Covid_SEG1)] <- "Sem informacao"

Resultados_Elisa_seg1$IgG_Toxoplasma_SEG1 <-
          factor (Resultados_Elisa_seg1$IgG_Toxoplasma_SEG1,
          labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"),
          levels = c(1,0,2,9))
Resultados_Elisa_seg1$IgG_Toxoplasma_SEG1[is.na
          (Resultados_Elisa_seg1$IgG_Toxoplasma_SEG1)]<- "Sem informacao"


#SEGUIMENTO 2
Resultados_Elisa_seg2$IgG_Dengue_SEG2 <-factor (Resultados_Elisa_seg2$IgG_Dengue_SEG2,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg2$IgG_Dengue_SEG2[is.na(Resultados_Elisa_seg2$IgG_Dengue_SEG2)] <- "Sem informacao"
Resultados_Elisa_seg2$IgG_Chikungunya_SEG2 <-factor (Resultados_Elisa_seg2$IgG_Chikungunya_SEG2,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg2$IgG_Chikungunya_SEG2[is.na(Resultados_Elisa_seg2$IgG_Chikungunya_SEG2)] <- "Sem informacao"
Resultados_Elisa_seg2$IgG_Covid_SEG2 <-factor (Resultados_Elisa_seg2$IgG_Covid_SEG2,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg2$IgG_Covid_SEG2[is.na(Resultados_Elisa_seg2$IgG_Covid_SEG2)] <- "Sem informacao"
Resultados_Elisa_seg2$IgG_Toxoplasma_SEG2 <-factor (Resultados_Elisa_seg2$IgG_Toxoplasma_SEG2,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg2$IgG_Toxoplasma_SEG2[is.na(Resultados_Elisa_seg2$IgG_Toxoplasma_SEG2)] <- "Sem informacao"

#SEGUIMENTO 3
Resultados_Elisa_seg3$IgG_Dengue_SEG3 <-factor (Resultados_Elisa_seg3$IgG_Dengue_SEG3,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg3$IgG_Dengue_SEG3[is.na(Resultados_Elisa_seg3$IgG_Dengue_SEG3)] <- "Sem informacao"
Resultados_Elisa_seg3$IgG_Chikungunya_SEG3 <-factor (Resultados_Elisa_seg3$IgG_Chikungunya_SEG3,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg3$IgG_Chikungunya_SEG3[is.na(Resultados_Elisa_seg3$IgG_Chikungunya_SEG3)] <- "Sem informacao"
Resultados_Elisa_seg3$IgG_Covid_SEG3 <-factor (Resultados_Elisa_seg3$IgG_Covid_SEG3,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Resultados_Elisa_seg3$IgG_Covid_SEG3[is.na(Resultados_Elisa_seg3$IgG_Covid_SEG3)] <- "Sem informacao"

#Fazendo o Merge dos meus dois bancos o de Resultados e do REDCAP
Resultados_Elisa_seg1_2 <- full_join(Resultados_Elisa_seg1, Resultados_Elisa_seg2,Resultados_Elisa_seg3, by = "idnova")

Resultados_Elisa_seg1_2_3 <- full_join(Resultados_Elisa_seg1_2,Resultados_Elisa_seg3, by = "idnova")


write.csv(Resultados_Elisa_seg1_2_3,  file = 'C:/MRC/Jaquelinecruz/Resultado_ELISA/Resultados_Elisa_IGG.csv',row.names = F ,fileEncoding = "UTF-8")



################################################## Codigo para subir o resultado para o banco ELISA DENGUE ############################################

#Subindo o banco para o R
Report_para_result <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/DENGUE/2023_05_23/Report_para_result.csv')
ELISA_DENGUE <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/DENGUE/2023_05_23/Resultado_ELISA_DENGUE.csv')

#Mudando a variavel cara nova para caracteres dos dois bancos (Motivo: O R esta comendo o numero final da IDNOVA)
Report_para_result$idnova <- as.character(Report_para_result$idnova)
ELISA_DENGUE$idnova <- as.character(ELISA_DENGUE$idnova)

#Fazendo um Select das variaveis que eu quero do BANCO DO SEGUIMENTO 1
Report_para_result <- Report_para_result[, c("casanova","idnova","redcap_event_name","lab_resultado_igg_dengue_complete")]

#Fazendo o Merge dos meus dois bancos o de Resultados e do REDCAP
Resultados_Redcap <- merge(ELISA_IGM, Report_para_result, by = "idnova")

#Modificando todo mundo que está como NA no para 2 (Para aparecer como complete no Redcap)
#Resultados_Redcap$lab_resultado_igg_dengue_complete[is.na(Resultados_Redcap$lab_resultado_igg_dengue_complete)] <- "2"
Resultados_Redcap$lab_resultado_igg_dengue_complete <- 
  factor (Resultados_Redcap$lab_resultado_igg_dengue_complete,
          labels = c("2"), 
          levels = c(0))

redcap_import <- 
  Resultados_Redcap[, c("casanova","redcap_event_name","idnova","iexiggdengv3e","diggdengv3e","odiggdengv3e",
                        "cofiggdengv3e","raziggdengv3e","kitiggdengv3e","loteiggdengv3e",
                        "rmaxiggdengv3e","cpiggdengv3e","rniggdengv3e","cniggdengv3e","uriggdengv3e",
                        "uraiggdengv3e","iggdengv3e","repiggdengv3e","lab_resultado_igg_dengue_complete")]

summary(as.factor(Resultados_Redcap$iggadengv3e))

#Exportando os dados
write.csv(Resultados_Redcap,  file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/DENGUE/2023_05_23/Import_redcap.csv',row.names = F ,fileEncoding = "UTF-8")

################################################## Codigo para subir o resultado para o banco ELISA chikungunya ############################################

#Subindo o banco para o R
Report_para_result <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/CHIKV/2023_05_23/Report_para_result.csv')
ELISA_CHIKUN <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/CHIKV/2023_05_23/Resultado_ELISA_CHIKV.csv')

#Mudando a variavel cara nova para caracteres dos dois bancos (Motivo: O R esta comendo o numero final da IDNOVA)
Report_para_result$idnova <- as.character(Report_para_result$idnova)
ELISA_CHIKUN$idnova <- as.character(ELISA_CHIKUN$idnova)

#Fazendo um Select das variaveis que eu quero do BANCO DO SEGUIMENTO 1
Report_para_result <- Report_para_result[, c("casanova","idnova","redcap_event_name","lab_resultado_igg_chik_complete")]

#Fazendo o Merge dos meus dois bancos o de Resultados e do REDCAP
Resultados_Redcap <- merge(ELISA_IGM, Report_para_result, by = "idnova")

#Modificando todo mundo que está como NA no para 2 (Para aparecer como complete no Redcap)
#Resultados_Redcap$lab_resultado_igg_dengue_complete[is.na(Resultados_Redcap$lab_resultado_igg_chik_complete)] <- "2"
Resultados_Redcap$lab_resultado_igg_chik_complete <- 
  factor (Resultados_Redcap$lab_resultado_igg_chik_complete,
          labels = c("2"), 
          levels = c(0))

redcap_import <- 
  Resultados_Redcap[, c("casanova","redcap_event_name","idnova","iexiggchikv3e","diggchikv3e","odiggchikv3e",
                        "cofiggchikv3e","raziggchikv3e","kitiggchikv3e","loteiggchikv3e",
                        "cpiggchikv3e","rmaxiggchikv3e","cniggchikv3e","rniggchikv3e",
                        "iggchikv3e","repiggchikv3e","lab_resultado_igg_chik_complete")]

summary(as.factor(Resultados_Redcap$iggadengv3e))

#Exportando os dados
write.csv(redcap_import,  file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/CHIKV/2023_05_23/Import_redcap.csv',row.names = F ,fileEncoding = "UTF-8")

################################################## Codigo para subir o resultado para o banco ELISA TOXOPLASMA###########################
Report_para_result <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/Toxoplasma/Report_para_result.csv')
ELISA_TOXOPLASMA <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/Toxoplasma/Resultado_ELISA_TOXO.csv')

#Mudando a variavel cara nova para caracteres dos dois bancos (Motivo: O R esta comendo o numero final da IDNOVA)
Report_para_result$idnova <- as.character(Report_para_result$idnova)
ELISA_TOXOPLASMA$idnova <- as.character(ELISA_TOXOPLASMA$idnova)

#Fazendo um Select das variaveis que eu quero do BANCO DO SEGUIMENTO 1
Report_para_result <- Report_para_result[, c("casanova","idnova","redcap_event_name","laboratorio_toxo_elisa_igg_complete")]

#Fazendo o Merge dos meus dois bancos o de Resultados e do REDCAP
Resultados_Redcap <- merge(ELISA_TOXOPLASMA, Report_para_result, by = "idnova")

#Modificando todo mundo que está como NA no para 2 (Para aparecer como complete no Redcap)
#Resultados_Redcap$lab_resultado_igg_dengue_complete[is.na(Resultados_Redcap$laboratorio_toxo_elisa_igg_complete)] <- "2"
Resultados_Redcap$laboratorio_toxo_elisa_igg_complete <- 
  factor (Resultados_Redcap$laboratorio_toxo_elisa_igg_complete,
          labels = c("2"), 
          levels = c(0))

redcap_import <- 
  Resultados_Redcap[, c("casanova","redcap_event_name","idnova","iexiggtoxo","diggtoxo","odiggtoxo",
                        "cofiggtoxo","uriggtoxo","uraiggtoxo","raziggtoxo",
                        "kitiggtoxo","loteiggtoxo","rmaxiggtoxo","rniggtoxo","cpiggtoxo",
                        "cniggtoxo","iggtoxo","uriggtoxores","repiggtoxo","laboratorio_toxo_elisa_igg_complete")]

summary(as.factor(Resultados_Redcap$iggadengv3e))

#Exportando os dados
write.csv(redcap_import,  file = 'C:/MRC/MRC2_SOROINQUERITO03/RESULTADOS/IGG/Toxoplasma/Import_redcap.csv',row.names = F ,fileEncoding = "UTF-8")


################################################## Codding para verification SE todo domiciliary tem chef de familia ############################################
Dados_dom <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Analises/Chefe de familia/2023_04_17/Redcap_Dom.csv')
chefe_familia <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Analises/Chefe de familia/2023_04_17/Chefe_familia_geral.csv')

Dados_dom <- filter(Dados_dom, segmrc== "1")
summary(as.factor(Dados_dom$segmrc))

Dados_chef <- filter(chefe_familia, chefe_fam == "1")
summary(as.factor(Dados_chef$chefe_fam))

Analise_chefe <- full_join(Dados_chef, Dados_dom, by = c("casanova" = "casanova"))

Analise_chefe$chefe_fam[is.na(Analise_chefe$chefe_fam)] <- 9
summary(as.factor(Analise_chefe$chefe_fam))

Analise_chefe$segmrc[is.na(Analise_chefe$segmrc)] <- 9
summary(as.factor(Analise_chefe$segmrc))

pendentes_chef_fam <- filter(Analise_chefe, segmrc == '9'| chefe_fam =='9')

subdata_ns2 <- pendentes_chef_fam[, c("casanova","idnova","redcap_event_name.x","chefe_fam", "segmrc" )]

view(subdata_ns2)

write.csv(subdata_ns2, file = 'C:/WT/Dashboard/exportações/chefe_fam_erros_ns2.csv', row.names = F)
################################################## Entrapment dos Dado Domiciliary ############################################

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
table1(~factor(sex) + Quarteirao)  

Dados_dom_tra2$`Uso do domicilio` <-factor (Dados_dom_tra2$`Uso do domicilio`,labels = c("Vazio","Abandonada","Construcao","Comercial","Residencial","Comercial e Residencial","Sem Informacao"), levels = c(1,2,3,4,5,6,9))
Dados_dom_tra2$ `Seguimento` <-factor (Dados_dom_tra2$Seguimento,labels = c("Aceitou","Recusou","Nao encontrado","Acesso Impossibilitado","Sem Informacao"), levels = c(1,2,3,4,9))
Dados_dom_tra2$`Area do domicilio` <-factor (Dados_dom_tra2$`Area do domicilio`,labels = c("Area 2","Area 3","Area 4","Area 5","Area 6","Sem Informacao"), levels = c(2,3,4,5,6,9))


Dados_dom_tra2[is.na(Dados_dom_tra2)] <- "Sem Informacao"


write.csv(Dados_dom_tra2,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Analise_DOM.csv',row.names = F ,fileEncoding = "UTF-8")

################################################## Entrapment dos Dado individuals ############################################

#Subindo o banco para o R
MRC2SEG1 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/MRC2SEG1.csv')
MRC2SEG2 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/MRC2SEG2.csv')
MRC2SEG3 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/MRC2SEG3.csv')

summary(as.factor(MRC2SEG1$consent))
summary(as.factor(MRC2SEG2$consent))
summary(as.factor(MRC2SEG3$consent))

#Modificando os 'NA' para '999'
MRC2SEG3$ `dupl_banc` <-factor (MRC2SEG3$`dupl_banc`,labels = c( "1","999"), levels = c(1,999))
MRC2SEG3$`dupl_banc`[is.na(MRC2SEG3$`dupl_banc`)] <- "999"
MRC2SEG2$ `dupl_banc` <-factor (MRC2SEG2$`dupl_banc`,labels = c( "1","999"), levels = c(1,999))
MRC2SEG2$`dupl_banc`[is.na(MRC2SEG2$`dupl_banc`)] <- "999"

#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
MRC2SEG1$idnova <- as.character(MRC2SEG1$idnova)
MRC2SEG2$idnova <- as.character(MRC2SEG2$idnova)
MRC2SEG3$idnova <- as.character(MRC2SEG3$idnova)

#subset(dupl_banc== "999" ) %>% dplyr::
#Filtrando o banco com a varivel desejada para o mergen
Dados_SEG1 <- MRC2SEG1 %>% select(casanova,idnova,consent, area_est, ci_sexo,data_consent, motivo, coleta_ind,dtcoleta,col_saliva,col_nasal,cens_idade)
Dados_SEG2 <- MRC2SEG2 %>% subset(dupl_banc== "999" ) %>% dplyr::select(casanova, idnova,consent, area_est, ci_sexo,data_consent, motivo, coleta_ind,col_saliva,col_nasal,cens_idade)
Dados_SEG3 <- MRC2SEG3 %>% subset(dupl_banc== "999" ) %>% dplyr::select(casanova, idnova,consent, area_est, ci_sexo,data_consent, motivo, coleta_ind,col_saliva,col_nasal,cens_idade )

summary(as.factor(Dados_SEG2$consent))

#mudando o nome das variveis de acordo a base de dados das variveis
names(Dados_SEG1)[3] <- "ConsentMRC1"
names(Dados_SEG1)[4] <- "Data do ConsentMRC1"
names(Dados_SEG2)[3] <- "ConsentMRC2"
names(Dados_SEG2)[4] <- "Data do ConsentMRC2"
names(Dados_SEG3)[3] <- "ConsentimentoMRC3"
names(Dados_SEG3)[6] <- "Data do ConsentimentoMRC3"

table1(~ factor(cens_idade)| motivo, data = Dados_SEG3)

#Fazendo merge do Banco do SEGUIMENTO 1 E 2 atravez da IDNOVA
dados_seg1seg2 <- merge(Dados_SEG1, Dados_SEG2, by = "idnova", all = T)
summary(as.factor(dados_seg1seg2$ConsentMRC1))
summary(as.factor(dados_seg1seg2$ConsentMRC2))

#Banco do SEGUIMENTO 2 E 3 atravez da IDNOVA
Dados_SEG3 <- merge(dados_seg1seg2, Dados_SEG3, by = "idnova", all = T)
summary(as.factor(Dados_SEG3$ConsentimentoMRC3))

#Fazendo merge do#Fazendo a comparação dos bancos utilizando o consent para analisar o seguimento dos estudos
Dados_SEG3$seguimento[Dados_SEG3$ConsentMRC1==1 & (Dados_SEG3$ConsentMRC2==0 | is.na(Dados_SEG3$ConsentMRC2)) & (Dados_SEG3$ConsentimentoMRC3==0 | is.na(Dados_SEG3$ConsentimentoMRC3))] <- "Seguimento 1"
Dados_SEG3$seguimento[Dados_SEG3$ConsentMRC2==1 & (Dados_SEG3$ConsentMRC1==0 | is.na(Dados_SEG3$ConsentMRC1)) & (Dados_SEG3$ConsentimentoMRC3==0 | is.na(Dados_SEG3$ConsentimentoMRC3))] <- "Seguimento 2"
Dados_SEG3$seguimento[Dados_SEG3$ConsentMRC1==1 & Dados_SEG3$ConsentMRC2==1 & (Dados_SEG3$ConsentimentoMRC3==0 | is.na(Dados_SEG3$ConsentimentoMRC3))] <- "Seguimento 1/2"
Dados_SEG3$seguimento[Dados_SEG3$ConsentMRC2==0 & (Dados_SEG3$ConsentimentoMRC3==1 & (Dados_SEG3$ConsentMRC1==1 | is.na(Dados_SEG3$ConsentMRC1)))] <- "Seguimento 1/3"
Dados_SEG3$seguimento[Dados_SEG3$ConsentMRC2==1 & (Dados_SEG3$ConsentimentoMRC3==1 & (Dados_SEG3$ConsentMRC1==0 | is.na(Dados_SEG3$ConsentMRC1)))] <- "Seguimento 2/3"
Dados_SEG3$seguimento[Dados_SEG3$ConsentimentoMRC3==1 & (Dados_SEG3$ConsentMRC2==1 & (Dados_SEG3$ConsentMRC1==1 ))] <- "Seguimento 1/2/3"
Dados_SEG3$seguimento[Dados_SEG3$ConsentimentoMRC3==1 & (Dados_SEG3$ConsentMRC1==0 | is.na(Dados_SEG3$ConsentMRC1)) & (Dados_SEG3$ConsentMRC2==0 | is.na(Dados_SEG3$ConsentMRC2))] <- "Seguimento 3"
Dados_SEG3$seguimento[is.na(Dados_SEG3$seguimento)] <-"Nunca participou"


#sumarizando os dados da comparação
summary(as.factor(Dados_SEG3$seguimento))
summary(as.factor(Dados_SEG3$ConsentimentoMRC3))

#Visualizando o BANCO
view(Dados_SEG3)

#alterando os nomes das variaveis do QUE VEM NO REDCAP
Dados_SEG3  <- dplyr:::rename(Dados_SEG3, "Area de estudo" = "area_est",
                              "Motivo do consente" = "motivo",
                              "Sexo" = "ci_sexo",
                              "Sangue Sim" = "coleta_ind",
                              "Saliva Sim" = "col_saliva",
                              "Swab Sim" = "col_nasal"
                              
)

table1(~ Sexo + "Sangue Sim" + "Saliva Sim" | treat, data=Dados_SEG3, topclass="Rtable1-grid Rtable1-shade Rtable1-times")



view(Dados_SEG3)
#Alterando os valores das varieveis que vem do redcap
Dados_SEG3$`Area de estudo`<-factor (Dados_SEG3$`Area de estudo`,labels = c("Area 02","Area 03","Area 04","Area 05","Area 06", "Sem informacao"), levels = c(2,3,4,5,6,99))                                                            
Dados_SEG3$ `ConsentimentoMRC3` <-factor (Dados_SEG3$ConsentimentoMRC3,labels = c("Recrutado","Nao Recrutado", "Sem informacao"), levels = c(1,0,9))
Dados_SEG3$`Motivo do consente` <-factor (Dados_SEG3$`Motivo do consente`,labels = c("Nao encontrado","Recusou","Mudou-se","Faleceu","Mentalmente incapaz", "Sem informacao"), levels = c(0,1,2,3,4,9))
Dados_SEG3$ `Sexo` <-factor (Dados_SEG3$`Sexo`,labels = c("Masculino","Feminino", "Sem informacao"), levels = c(1,0,9))
Dados_SEG3$ `Sangue Sim` <-factor (Dados_SEG3$`Sangue Sim`,labels = c("Sim","Nao", "Sem informacao"), levels = c(1,0,9))
Dados_SEG3$ `Saliva Sim` <-factor (Dados_SEG3$`Saliva Sim`,labels = c("Sim","Nao", "Sem informacao"), levels = c(1,0,9))
Dados_SEG3$ `Swab Sim` <-factor (Dados_SEG3$`Swab Sim`,labels = c("Sim","Nao", "Sem informacao"), levels = c(1,0,9))

#Modificando os 'NA' para 'sem informação'
Dados_SEG3$ConsentimentoMRC3[is.na(Dados_SEG3$ConsentimentoMRC3)] <- "Sem informacao"
Dados_SEG3$`Area de estudo`[is.na(Dados_SEG3$`Area de estudo`)] <- "Sem informacao"
Dados_SEG3$`Motivo do consente`[is.na(Dados_SEG3$`Motivo do consente`)] <- "Sem informacao"
Dados_SEG3$Sexo[is.na(Dados_SEG3$Sexo)] <- "Sem informacao"
Dados_SEG3$`Sangue Sim`[is.na(Dados_SEG3$`Sangue Sim`)] <- "Sem informacao"
Dados_SEG3$`Saliva Sim`[is.na(Dados_SEG3$`Saliva Sim`)] <- "Sem informacao"
Dados_SEG3$`Swab Sim`[is.na(Dados_SEG3$`Swab Sim`)] <- "Sem informacao"

#FAXETARIA DE IDADE

Dados_SEG3 %<>% 
  mutate(idade_cat=case_when(
    cens_idade %in% 2: 4 ~ "02 - 04",
    cens_idade %in% 5: 12 ~ "05 - 12",
    cens_idade %in% 13: 17 ~ "13 - 17",
    cens_idade %in% 18: 24 ~ "18 - 24",
    cens_idade %in% 25: 34 ~ "25 - 34",
    cens_idade %in% 35: 44 ~ "35 - 44",
    cens_idade %in% 45: 64 ~ "45 - 64",
    cens_idade %in% 65: 120 ~ "> 65",
    is.na(0) ~ "Sem informacao",
    .default = as.character('Sem informacao')
  ))
summary(as.factor(Dados_SEG3$ConsentMRC1))


Dados_SEG3$`idnova`[is.na(Dados_SEG3$`idnova`)] <- "999"
Dados_SEG3 <- Dados_SEG3 %>% subset(idnova!= "999" )


summary(as.factor(Dados_SEG3$ConsentMRC1))
summary(as.factor(Dados_SEG3$ConsentMRC2))
summary(as.factor(Dados_SEG3$ConsentimentoMRC3))


write.csv(Dados_SEG3,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Dados_IND.csv',row.names = F ,fileEncoding = "UTF-8")

################################################## MAT Serology ############################################

MRC2SEG1 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/MRC2SEG1.csv', header = TRUE, stringsAsFactors = FALSE)
MRC2SEG2 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/MRC2SEG2.csv', header = TRUE, stringsAsFactors = FALSE)

summary(as.factor(MRC2SEG1$consent))
summary(as.factor(MRC2SEG2$consent))

MRC2SEG1 <- MRC2SEG1 %>% filter(consent==1)
MRC2SEG2 <- MRC2SEG2 %>% filter(consent==1)

#MRC2SEG1[is.na(MRC2SEG1)] <- "Sem dados"
#MRC2SEG2[is.na(MRC2SEG2)] <- "Sem dados"

#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
MRC2SEG1$idnova <- as.character(MRC2SEG1$idnova)
MRC2SEG2$idnova <- as.character(MRC2SEG2$idnova)

#SUMARIZANDO QUANTOS "NA" tem em cada variavel
sapply(MRC2SEG1, function(x) sum(is.na(x)))
sapply(MRC2SEG2, function(x) sum(is.na(x)))

names(MRC2SEG1)[8] <- "Coleta_simMRC1"
names(MRC2SEG2)[8] <- "Coleta_simMRC2"


names(MRC2SEG1)[23] <- "sorovp_seg1"
names(MRC2SEG2)[21] <- "sorovp_seg2"

MRC2SEG1$reagente[MRC2SEG1$sorovp_seg1 !="0"] <-  "1"
MRC2SEG1$reagente[MRC2SEG1$sorovp_seg1 =="0"] <-  "0"
MRC2SEG2$reagente[MRC2SEG2$sorovp_seg2 !="0"] <-   "1"
MRC2SEG2$reagente[MRC2SEG2$sorovp_seg2 =="0"] <-  "0"

MRC2SEG2$`area_est`<-factor (MRC2SEG2$`area_est`,labels = c("Area 02","Area 03","Area 04","Area 05","Area 06", "Sem informacao"), levels = c(2,3,4,5,6,99))                                                            

MRC2SEG1$ci_sexo <-factor (MRC2SEG1$ci_sexo,labels = c("Masculino","Feminino", "Sem informacao"), levels = c(1,0,9))
MRC2SEG1$ci_sexo[is.na(MRC2SEG1$ci_sexo)] <- "Sem informacao"
MRC2SEG2$ci_sexo <-factor (MRC2SEG2$ci_sexo,labels = c("Masculino","Feminino", "Sem informacao"), levels = c(1,0,9))
MRC2SEG2$ci_sexo[is.na(MRC2SEG2$ci_sexo)] <- "Sem informacao"

#Linkando a base de dados do S1 E S2 Utilziando a IDNOVA como a variavel chave
dados_seg1seg2 <- merge(MRC2SEG1, MRC2SEG2, by= "idnova", all = T)

dados_seg1seg2$Celetasim[dados_seg1seg2$Coleta_simMRC1 =="1"] <-  "Aceitou"
dados_seg1seg2$Celetasim[dados_seg1seg2$Coleta_simMRC2 =="1"] <-  "Aceitou"

#dados_seg1seg2$reagente[dados_seg1seg2$reagente.x =="1"] <-  "REAGENT"
#dados_seg1seg2$reagente[dados_seg1seg2$reagente.x =="0"] <-  "NON-REAGENT"
#dados_seg1seg2$reagente[dados_seg1seg2$reagente.y =="1"] <-   "REAGENT"
#dados_seg1seg2$reagente[dados_seg1seg2$reagente.y =="0"] <-  "NON-REAGENT"

summary(as.factor(dados_seg1seg2$sorovp_seg1))
summary(as.factor(dados_seg1seg2$resultleptoseg1))
summary(as.factor(dados_seg1seg2$resultleptoseg2))

summary(as.factor(MRC2SEG1$sorovp_seg1))
summary(as.factor(MRC2SEG2$sorovp_seg2))



dados_seg1seg2$resultleptoseg1[dados_seg1seg2$sorovp_seg1 !="0"] <-  "REAGENT"
dados_seg1seg2$resultleptoseg1[dados_seg1seg2$sorovp_seg1 =="0"] <-  "NON-REAGENT"
dados_seg1seg2$resultleptoseg2[dados_seg1seg2$sorovp_seg2 !="0"] <-   "REAGENT"
dados_seg1seg2$resultleptoseg2[dados_seg1seg2$sorovp_seg2 =="0"] <-  "NON-REAGENT"

dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="0"] <-  "NON-REAGENT"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="1"] <-  "L1 130"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="11"] <-  "3522C"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="5"] <-  "Misto"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="2"] <-  "Other strains"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="6"] <-  "Other strains"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="8"] <-  "Other strains"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="9"] <-  "Other strains"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="14"] <-  "Other strains"
dados_seg1seg2$cepasSEG1[dados_seg1seg2$sorovp_seg1 =="17"] <-  "Other strains"

dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="0"] <-  "NON-REAGENT"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="1"] <-  "L1 130"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="11"] <-  "3522C"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="5"] <- "Misto"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="2"] <-  "Other strains"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="6"] <-  "Other strains"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="8"] <-  "Other strains"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="9"] <-  "Other strains"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="14"] <- "Other strains"
dados_seg1seg2$cepasSEG2[dados_seg1seg2$sorovp_seg2 =="17"] <- "Other strains"


#dados_seg1seg2$Coleta_simMRC1[is.na(dados_seg1seg2$Coleta_simMRC1)] <- "Sem Dados"
#dados_seg1seg2$Coleta_simMRC1[is.na(dados_seg1seg2$Coleta_simMRC1)] <- "Sem Dados"

#dados_seg1seg2$Coleta_simMRC2[is.na(dados_seg1seg2$Coleta_simMRC2)] <- "Sem Dados"
#dados_seg1seg2$Coleta_simMRC2[is.na(dados_seg1seg2$Coleta_simMRC2)] <- "Sem Dados"

dados_seg1seg2$seguimento[dados_seg1seg2$Coleta_simMRC1==1 & (dados_seg1seg2$Coleta_simMRC2==0 | is.na(dados_seg1seg2$Coleta_simMRC2))]  <- "Follow-up 1"
dados_seg1seg2$seguimento[dados_seg1seg2$Coleta_simMRC2==1 & (dados_seg1seg2$Coleta_simMRC1==0 | is.na(dados_seg1seg2$Coleta_simMRC1))]  <- "Follow-up 2"
dados_seg1seg2$seguimento[dados_seg1seg2$Coleta_simMRC1==1 & dados_seg1seg2$Coleta_simMRC2==1] <- "Follow-up 1/2"
dados_seg1seg2$seguimento[dados_seg1seg2$Coleta_simMRC1==0 & dados_seg1seg2$Coleta_simMRC2==0] <- "No follow-up"
dados_seg1seg2$seguimento[is.na(dados_seg1seg2$seguimento)] <-"No follow-up"

dados_seg1seg2$Coleta_simMRC1[is.na(dados_seg1seg2$Coleta_simMRC1)] <- "Sem Dados"
dados_seg1seg2$Coleta_simMRC1[is.na(dados_seg1seg2$Coleta_simMRC1)] <- "Sem Dados"
dados_seg1seg2$Coleta_simMRC2[is.na(dados_seg1seg2$Coleta_simMRC2)] <- "Sem Dados"
dados_seg1seg2$Coleta_simMRC2[is.na(dados_seg1seg2$Coleta_simMRC2)] <- "Sem Dados"

#sumarizando os dados da comparação
summary(as.factor(dados_seg1seg2$seguimento))


dados_seg1seg2 %<>% 
  mutate(idade_cat_seg1=case_when(
    cens_idade.x %in% 2: 4 ~ "02 - 04",
    cens_idade.x %in% 5: 12 ~ "05 - 12",
    cens_idade.x %in% 13: 17 ~ "13 - 17",
    cens_idade.x %in% 18: 24 ~ "18 - 24",
    cens_idade.x %in% 25: 34 ~ "25 - 34",
    cens_idade.x %in% 35: 44 ~ "35 - 44",
    cens_idade.x %in% 45: 64 ~ "45 - 64",
    cens_idade.x %in% 65: 120 ~ "> 65",
    cens_idade.y %in% 2: 4 ~ "02 - 04",
    cens_idade.y %in% 5: 12 ~ "05 - 12",
    cens_idade.y %in% 13: 17 ~ "13 - 17",
    cens_idade.y %in% 18: 24 ~ "18 - 24",
    cens_idade.y %in% 25: 34 ~ "25 - 34",
    cens_idade.y %in% 35: 44 ~ "35 - 44",
    cens_idade.y %in% 45: 64 ~ "45 - 64",
    cens_idade.y %in% 65: 120 ~ "> 65",
    is.na(0) ~ "No information",
    .default = as.character('No information')
  ))


#dados_seg1seg2 %<>% 
# mutate(idade_cat_seg2=case_when(
#  cens_idade.y %in% 2: 4 ~ "02 - 04",
# cens_idade.y %in% 5: 12 ~ "05 - 12",
#cens_idade.y %in% 13: 17 ~ "13 - 17",
#cens_idade.y %in% 18: 24 ~ "18 - 24",
#cens_idade.y %in% 25: 34 ~ "25 - 34",
#cens_idade.y %in% 35: 44 ~ "35 - 44",
#cens_idade.y %in% 45: 64 ~ "45 - 64",
#cens_idade.y %in% 65: 120 ~ "> 65",
#is.na(0) ~ "No information",
#.default = as.character('No information')
#  ))
dados_seg1seg2$`idnova`[is.na(dados_seg1seg2$`idnova`)] <- "999"
dados_seg1seg2 <- dados_seg1seg2 %>% subset(idnova!= "999" )



write.csv(dados_seg1seg2,  file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/Resultados_MAT2.csv',row.names = F ,fileEncoding = "UTF-8")


#Mudando idnova para texto
MRC2SEG2$idnova <- as.character(MRC2SEG2$idnova)

#atribuindo novo valor para variaveis idnova vazias
MRC2SEG2$idnova[is.na(MRC2SEG2$idnova)] <- 9

# Selecionando idnovas duplicadas
duplicados <- MRC2SEG2[duplicated(MRC2SEG2$idnova, incomparables = TRUE),]
duplicados <- duplicados[which(duplicados$idnova!=9),]


View(duplicados)




################################################## ELISA IgG Serology ############################################

Elisa_SEG1 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/Resultados_Elisa_seg1.csv', header = TRUE, stringsAsFactors = FALSE)
Elisa_SEG2 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/Resultados_Elisa_seg2.csv', header = TRUE, stringsAsFactors = FALSE)

summary(as.factor(Elisa_SEG1$iggdengv3e))
summary(as.factor(Elisa_SEG1$iggchikv3e))
summary(as.factor(Elisa_SEG1$iggsarsv3e))
summary(as.factor(Elisa_SEG1$iggtoxo))
summary(as.factor(Elisa_SEG2$iggdengv3e))
summary(as.factor(Elisa_SEG2$iggchikv3e))
summary(as.factor(Elisa_SEG2$iggsarsv3e))
summary(as.factor(Elisa_SEG2$iggtoxo))
#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
Elisa_SEG1$idnova <- as.character(Elisa_SEG1$idnova)
Elisa_SEG2$idnova <- as.character(Elisa_SEG2$idnova)
#MRC2SEG2$idnova <- as.character(MRC2SEG2$idnova)

#SUMARIZANDO QUANTOS "NA" tem em cada variavel
sapply(Elisa_SEG1, function(x) sum(is.na(x)))
sapply(Elisa_SEG2, function(x) sum(is.na(x)))

names(Elisa_SEG1)[7] <- "Coleta_simMRC1"
names(Elisa_SEG1)[9] <- "IgG_Dengue"
names(Elisa_SEG1)[10] <- "IgG_Chikungunya"
names(Elisa_SEG1)[11] <- "IgG_Covid"
names(Elisa_SEG1)[12] <- "IgG_Toxoplasma"

names(Elisa_SEG2)[7] <- "Coleta_simMRC2"
names(Elisa_SEG2)[9] <- "IgG_Dengue"
names(Elisa_SEG2)[10] <- "IgG_Chikungunya"
names(Elisa_SEG2)[11] <- "IgG_Covid"
names(Elisa_SEG2)[12] <- "IgG_Toxoplasma"

#SEGUIMENTO 1
Elisa_SEG1$IgG_Dengue <-factor (Elisa_SEG1$IgG_Dengue,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG1$IgG_Dengue[is.na(Elisa_SEG1$IgG_Dengue)] <- "Sem informacao"
Elisa_SEG1$IgG_Chikungunya <-factor (Elisa_SEG1$IgG_Chikungunya,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG1$IgG_Chikungunya[is.na(Elisa_SEG1$IgG_Chikungunya)] <- "Sem informacao"
Elisa_SEG1$IgG_Covid <-factor (Elisa_SEG1$IgG_Covid,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG1$IgG_Covid[is.na(Elisa_SEG1$IgG_Covid)] <- "Sem informacao"
Elisa_SEG1$IgG_Toxoplasma <-factor (Elisa_SEG1$IgG_Toxoplasma,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG1$IgG_Toxoplasma[is.na(Elisa_SEG1$IgG_Toxoplasma)] <- "Sem informacao"

#SEGUIMENTO 2
Elisa_SEG2$IgG_Dengue <-factor (Elisa_SEG2$IgG_Dengue,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG2$IgG_Dengue[is.na(Elisa_SEG2$IgG_Dengue)] <- "Sem informacao"
Elisa_SEG2$IgG_Chikungunya <-factor (Elisa_SEG2$IgG_Chikungunya,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG2$IgG_Chikungunya[is.na(Elisa_SEG2$IgG_Chikungunya)] <- "Sem informacao"
Elisa_SEG2$IgG_Covid <-factor (Elisa_SEG2$IgG_Covid,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG2$IgG_Covid[is.na(Elisa_SEG2$IgG_Covid)] <- "Sem informacao"
Elisa_SEG2$IgG_Toxoplasma <-factor (Elisa_SEG2$IgG_Toxoplasma,labels = c("REAGENTE","NON-REAGENTE", "UNDETERMINED", "Sem informacao"), levels = c(1,0,2,9))
Elisa_SEG2$IgG_Toxoplasma[is.na(Elisa_SEG2$IgG_Toxoplasma)] <- "Sem informacao"

#SEGUIMENTO 1
Elisa_SEG1$`area_est`<-factor (Elisa_SEG1$`area_est`,labels = c("Area 02","Area 03","Area 04","Area 05","Area 06", "Sem informacao"), levels = c(2,3,4,5,6,99))                                                            
#SEGUIMENTO 2
Elisa_SEG2$`area_est`<-factor (Elisa_SEG2$`area_est`,labels = c("Area 02","Area 03","Area 04","Area 05","Area 06", "Sem informacao"), levels = c(2,3,4,5,6,99))                                                            

#SEGUIMENTO 1
Elisa_SEG1$ci_sexo <-factor (Elisa_SEG1$ci_sexo,labels = c("Masculino","Feminino", "Sem informacao"), levels = c(1,0,9))
Elisa_SEG1$ci_sexo[is.na(Elisa_SEG1$ci_sexo)] <- "Sem informacao"
#SEGUIMENTO 2
Elisa_SEG2$ci_sexo <-factor (Elisa_SEG2$ci_sexo,labels = c("Masculino","Feminino", "Sem informacao"), levels = c(1,0,9))
Elisa_SEG2$ci_sexo[is.na(Elisa_SEG2$ci_sexo)] <- "Sem informacao"

#Linkando a base de dados do S1 E S2 Utilziando a IDNOVA como a variavel chave
Dados_elisaseg1_elisaseg2 <- merge(Elisa_SEG1, Elisa_SEG2, by= "idnova", all = T)

Dados_elisaseg1_elisaseg2$seguimento[Dados_elisaseg1_elisaseg2$Coleta_simMRC1==1 & (Dados_elisaseg1_elisaseg2$Coleta_simMRC2==0 | is.na(Dados_elisaseg1_elisaseg2$Coleta_simMRC2))]  <- "Follow-up 1"
Dados_elisaseg1_elisaseg2$seguimento[Dados_elisaseg1_elisaseg2$Coleta_simMRC2==1 & (Dados_elisaseg1_elisaseg2$Coleta_simMRC1==0 | is.na(Dados_elisaseg1_elisaseg2$Coleta_simMRC1))]  <- "Follow-up 2"
Dados_elisaseg1_elisaseg2$seguimento[Dados_elisaseg1_elisaseg2$Coleta_simMRC1==1 & Dados_elisaseg1_elisaseg2$Coleta_simMRC2==1] <- "Follow-up 1/2"
Dados_elisaseg1_elisaseg2$seguimento[Dados_elisaseg1_elisaseg2$Coleta_simMRC1==0 & Dados_elisaseg1_elisaseg2$Coleta_simMRC2==0] <- "No follow-up"
Dados_elisaseg1_elisaseg2$seguimento[is.na(Dados_elisaseg1_elisaseg2$seguimento)] <-"No follow-up"

Dados_elisaseg1_elisaseg2 %<>% 
  mutate(idade_cat_seg1=case_when(
    cens_idade.x %in% 2: 4 ~ "02 - 04",
    cens_idade.x %in% 5: 12 ~ "05 - 12",
    cens_idade.x %in% 18: 17 ~ "13 - 17",
    cens_idade.x %in% 18: 24 ~ "18 - 24",
    cens_idade.x %in% 25: 34 ~ "25 - 34",
    cens_idade.x %in% 35: 44 ~ "35 - 44",
    cens_idade.x %in% 45: 64 ~ "45 - 64",
    cens_idade.x %in% 65: 120 ~ "> 65",
    cens_idade.y %in% 2: 4 ~ "02 - 04",
    cens_idade.y %in% 5: 12 ~ "05 - 12",
    cens_idade.y %in% 13: 17 ~ "13 - 17",
    cens_idade.y %in% 18: 24 ~ "18 - 24",
    cens_idade.y %in% 25: 34 ~ "25 - 34",
    cens_idade.y %in% 35: 44 ~ "35 - 44",
    cens_idade.y %in% 45: 64 ~ "45 - 64",
    cens_idade.y %in% 65: 120 ~ "> 65",
    is.na(0) ~ "No information",
    .default = as.character('No information')
  ))



write.csv(Dados_elisaseg1_elisaseg2,  file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/Resultados_Elisa2.csv',row.names = F ,fileEncoding = "UTF-8")


################################################## CODIGO TESTE#################################
#Subindo a base de dados da soroteca
Soroteca_tratada <- read.csv(file = 'C:/MRC/Envio_yalle/2023_02_06/Soroteca_tratada_29_05_2023_SEG3.csv')
edit(Soroteca_tratada)
#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
Soroteca_tratada$REC <- as.character(Soroteca_tratada$REC)

#Verificando se tem aliquota em volume 0
Aliquota_Volume0 <- subset(Soroteca_tratada, Volume == "0") 
summary(as.factor(Aliquota_Volume0$Volume))

#Fazendo Filtro atraves do tipo de amostra da variavel "Type".
Soro_YALE <- subset(Soroteca_tratada, Freezer_Name == "Yale 5 (MRC2 S3)")
# distinct() é usada para remover as linhas duplicadas com base na coluna "ID". O argumento .keep_all = TRUE garante que todas as colunas do dataframe original sejam mantidas.
Soro_YALE_SEM_DUPLICATAS <- Soro_YALE %>% distinct(REC, .keep_all = TRUE)
Soro_IGM <- subset(Soroteca_tratada, Freezer_Name == "IGM 16(MRC2 S3)")
# distinct() é usada para remover as linhas duplicadas com base na coluna "ID". O argumento .keep_all = TRUE garante que todas as colunas do dataframe original sejam mantidas.
Soro_IGM_SEM_DUPLICATAS <- Soro_IGM %>% distinct(REC, .keep_all = TRUE)

teste_yale <- merge(Soro_IGM, Soro_YALE, by.x = "REC" , by.y = "REC", all = T)

write.csv(teste_yale,  file = 'C:/MRC/Envio_yalle/2023_02_06/TESTE.csv',row.names = F ,fileEncoding = "UTF-8")


################################################## ANALISE SOROTECA Verificar se tudo que tem no Redcap tem na soroteca e vice versa###############################

#Subindo a base de dados da soroteca
Soroteca_tratada <- read.csv(file = 'C:/MRC/Envio_yalle/MRC/soroteca_tratada_seg3.csv')
edit(Soroteca_tratada)
#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
Soroteca_tratada$REC <- as.character(Soroteca_tratada$REC)

#Verificando se tem aliquota em volume 0
Aliquota_Volume0 <- subset(Soroteca_tratada, Volume == "0") 
summary(as.factor(Aliquota_Volume0$Volume))

#Fazendo Filtro atraves do tipo de amostra da variavel "Type".
Saliva_soroteca <- subset(Soroteca_tratada, Type == "Saliva")
SwabN_Soroteca <- subset(Soroteca_tratada, Type == "SwabN")
Soro_Soroteca <- subset(Soroteca_tratada, Type == "Soro")

#Subindo a base de dados do Redcap
Redcap_coletas <- read.csv(file = 'C:/MRC/Envio_yalle/MRC/Coileta_geral.csv')

#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
Redcap_coletas$idnova <- as.character(Redcap_coletas$idnova)

#Fazendo Filtro atraves do tipo de amostra da variavel "Type".
Saliva_redcap <- subset(Redcap_coletas, col_saliva == "1") %>% dplyr::select(casanova, idnova,consent,col_saliva,dtcol_saliva)
SwabN_redcap <- subset(Redcap_coletas, col_nasal == "1") %>% dplyr::select(casanova, idnova,consent,col_nasal,col_nasal)
Soro_redcap <- subset(Redcap_coletas, coleta_ind == "1") %>% dplyr::select(casanova, idnova,consent,coleta_ind,dtcoleta)

#Fazendo Merge para verificar se tudo que tem na soroteca tem no Redcap (Saliva)
Saliva_redcapXredcap <- merge(Saliva_redcap, Saliva_soroteca, by.x = "idnova" , by.y = "REC", all = T)
summary(as.factor(Saliva_redcapXredcap$Project_ID))

#Fazendo Merge para verificar se tudo que tem na soroteca tem no Redcap(SwabN)
SwabN_redcapXredcap <- merge(SwabN_redcap, SwabN_Soroteca, by.x = "idnova" , by.y = "REC", all = T)
summary(as.factor(SwabN_redcapXredcap$Project_ID))

#Fazendo Merge para verificar se tudo que tem na soroteca tem no Redcap(Soro) 
Soro_redcapXredcap <- merge(Soro_redcap, Soro_Soroteca, by.x = "idnova" , by.y = "REC", all = T)
summary(as.factor(Soro_redcapXredcap$Project_ID))



################################################## Frequencia de Aliquitas#########################################
data=read.csv(file = 'C:/MRC/Envio_yalle/2023_02_06/soroteca_MRC.csv')

#data$Project_ID <- as.character(data$Project_ID)
#wt1_seg2 <- filter(data, Project_ID=='60')

guardados <- filter(data, Position!="")
guardados_sem_volume <- filter(guardados, Volume=="" | Volume=='0')

frequência_caixas <-table(guardados$BOX3, guardados$Freezer_Name)


frequency(guardados, SampleID$REC)
write.table(frequência_caixas,  file = 'C:/MRC/Envio_yalle/2023_02_06/frequência_caixas.csv', sep = ",", row.names = T)

con <- odbcConnectAccess('C:/MRC/Envio_yalle/2023_02_06/Soroteca_tratada_29_05_2023_SEG3.mdb')

# Filtrar o dataframe para IDs maiores que 2 e calcular a média do Valor
media <- data %>%
  filter(ID > 2) %>%
  summarize(media = mean(Valor))