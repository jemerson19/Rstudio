MRC2SEG1 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/MRC2SEG1.csv', header = TRUE, stringsAsFactors = FALSE)
MRC2SEG2 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/MRC2SEG2.csv', header = TRUE, stringsAsFactors = FALSE)

summary(as.factor(MRC2SEG1$consent))
summary(as.factor(MRC2SEG2$consent))

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


dados_seg1seg2$reagente[dados_seg1seg2$reagente.x =="1"] <-  "REAGENT"
dados_seg1seg2$reagente[dados_seg1seg2$reagente.x =="0"] <-  "NON-REAGENT"
dados_seg1seg2$reagente[dados_seg1seg2$reagente.y =="1"] <-   "REAGENT"
dados_seg1seg2$reagente[dados_seg1seg2$reagente.y =="0"] <-  "NON-REAGENT"


#dados_seg1seg2$reagente[dados_seg1seg2$sorovp_seg1 !="0"] <-  "REAGENT"
#dados_seg1seg2$reagente[dados_seg1seg2$sorovp_seg1 =="0"] <-  "NON-REAGENT"
#dados_seg1seg2$reagente[dados_seg1seg2$sorovp_seg2 !="0"] <-   "REAGENT"
#dados_seg1seg2$reagente[dados_seg1seg2$sorovp_seg2 =="0"] <-  "NON-REAGENT"

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
    cens_idade.x %in% 18: 17 ~ "12 - 18",
    cens_idade.x %in% 18: 24 ~ "18 - 24",
    cens_idade.x %in% 25: 34 ~ "25 - 34",
    cens_idade.x %in% 35: 44 ~ "35 - 44",
    cens_idade.x %in% 45: 64 ~ "45 - 64",
    cens_idade.x %in% 65: 120 ~ "> 65",
    is.na(0) ~ "No information",
    .default = as.character('No information')
  ))


dados_seg1seg2 %<>% 
  mutate(idade_cat_seg2=case_when(
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
dados_seg1seg2$`idnova`[is.na(dados_seg1seg2$`idnova`)] <- "999"
dados_seg1seg2 <- dados_seg1seg2 %>% subset(idnova!= "999" )



write.csv(dados_seg1seg2,  file = 'C:/MRC/Jaquelinecruz/Dashboard_jaque/Resultados_MAT2.csv',row.names = F ,fileEncoding = "UTF-8")

