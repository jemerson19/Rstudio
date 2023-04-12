#Subindo o banco para o R
Resultados_sim <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Analises/Redcap_x_Soroteca/2023_03_28/soroteca_mrc2.csv')
Resultados_sim$REC <- as.character(Resultados_sim$REC)
summary(as.factor(Resultados_sim$Volume))

#Criando o sub banco para cada freezer
Banco_envioIGM11 <- Resultados_sim[which(Resultados_sim$Freezer_Name == "IGM 11(MRC2 S2)"),]
Banco_envioIGM25 <- Resultados_sim[which(Resultados_sim$Freezer_Name == "IGM 25 (MRC2 S2)"),]
Banco_envioYALE11 <- Resultados_sim[which(Resultados_sim$Freezer_Name == "YALE 25 (MRC2 S2)"),]
Banco_envioYALE5 <- Resultados_sim[which(Resultados_sim$Freezer_Name == "Yale 5 (MRC2 S2)"),]

#Verificando se tem alguma amostra com volume 0
summary(as.factor(Banco_envioIGM11$Volume))
summary(as.factor(Banco_envioIGM25$Volume))
summary(as.factor(Banco_envioYALE11$Volume))
summary(as.factor(Banco_envioYALE5$Volume))

#Verificando as contagem de aliquiotas de cada freezer atraves do REC
YALE5 <- Banco_envioYALE5 %>% group_by(Sample_ID2) %>% dplyr::summarise(count=n())
YALE11 <- Banco_envioYALE11 %>% group_by(Sample_ID2) %>% dplyr::summarise(count=n())
IGM25 <- Banco_envioIGM25 %>% group_by(Sample_ID2) %>% dplyr::summarise(count=n())
IGM11 <- Banco_envioIGM11 %>% group_by(Sample_ID2) %>% dplyr::summarise(count=n())
summary(as.factor(YALE11$count))

write.csv(YALE11,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/YALE11.csv',row.names = F ,fileEncoding = "UTF-8")


# Selecionando idnovas duplicadas
#Banco_envioIGM11 <- Banco_envioIGM11[duplicated(Banco_envioIGM11$REC, incomparables = TRUE),]
#Banco_envioIGM12 <- Banco_envioIGM11[which(Banco_envioIGM11$REC!=999),]


