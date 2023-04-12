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
Dados_SEG1 <- MRC2SEG1 %>% select(idnova,consent, area_est, ci_sexo,data_consent, motivo, coleta_ind,dtcoleta,col_saliva,col_nasal,cens_idade)
Dados_SEG2 <- MRC2SEG2 %>% subset(dupl_banc== "999" ) %>% dplyr::select(casanova, idnova,consent, area_est, ci_sexo,data_consent, motivo, coleta_ind,col_saliva,col_nasal,cens_idade)
Dados_SEG3 <- MRC2SEG3 %>% subset(dupl_banc== "999" ) %>% dplyr::select(casanova, idnova,consent, area_est, ci_sexo,data_consent, motivo, coleta_ind,col_saliva,col_nasal,cens_idade )



#mudando o nome das variveis de acordo a base de dados das variveis
names(Dados_SEG1)[2] <- "ConsentMRC1"
names(Dados_SEG1)[3] <- "Data do ConsentMRC1"
names(Dados_SEG2)[3] <- "ConsentMRC2"
names(Dados_SEG2)[4] <- "Data do ConsentMRC2"
names(Dados_SEG3)[3] <- "ConsentimentoMRC3"
names(Dados_SEG3)[6] <- "Data do ConsentimentoMRC3"


#Fazendo merge do Banco do SEGUIMENTO 1 E 2 atravez da IDNOVA
dados_seg1seg2 <- merge(Dados_SEG1, Dados_SEG2, by = "idnova", all = T)
summary(as.factor(Dados_SEG3$ConsentimentoMRC3))

Dados_SEG3 <- merge(dados_seg1seg2, Dados_SEG3, by = "idnova", all = T)
summary(as.factor(Dados_SEG3$ConsentimentoMRC3))


#Visualizando o BANCO
view(Dados_SEG3)
#Fazendo a comparação dos bancos utilizando o consent para analisar o seguimento dos estudos
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

view(Dados_SEG3)
#alterando os nomes das variaveis do QUE VEM NO REDCAP
Dados_SEG3  <- dplyr:::rename(Dados_SEG3, "Area de estudo" = "area_est",
                                  "Motivo do consente" = "motivo",
                                  "Sexo" = "ci_sexo",
                                  "Sangue Sim" = "coleta_ind",
                                  "Saliva Sim" = "col_saliva",
                                  "Swab Sim" = "col_nasal"
                               
)
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



Dados_SEG3$`idnova`[is.na(Dados_SEG3$`idnova`)] <- "999"
Dados_SEG3 <- Dados_SEG3 %>% subset(idnova!= "999" )

write.csv(Dados_SEG3,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Dados_IND.csv',row.names = F ,fileEncoding = "UTF-8")

#summary(as.factor(Dados_SEG3$idade_cat))

#consentiu_sem_idade <- filter(Dados_SEG3, cens_idade!= "")
#View(consentiu_sem_idade)
#faixa_etaria <- filter(consentiu_sem_idade, ConsentimentoMRC3== "Recrutado")
#View(faixa_etaria)

#dados_MRC2SEG1 <- Dados_indMRC2SEG3 %>% filter(dupl_banc== 0) %>% dplyr::select(idnova,consent ,ci_sexo,data_consent,coleta_ind,dtcoleta )
#dados_MRC2SEG1 <- Dados_indMRC2SEG3 %>% filter(consent== 1 & coleta_ind==1) %>% dplyr::select(idnova,consent ,ci_sexo,data_consent,coleta_ind,dtcoleta )
#dados_MRC2SEG2 <- Dados_indMRC2SEG2 %>% filter(consent== 1 & coleta_ind==1) %>% dplyr::select(idnova,consent ,ci_sexo,data_consent,coleta_ind,dtcoleta )
#dados_MRC2SEG3 <- Dados_indMRC2SEG3 %>% filter(consent== 1 & coleta_ind==1) %>% dplyr::select(idnova,consent ,ci_sexo,data_consent,coleta_ind,dtcoleta )
#names(dados_MRC2SEG1)[2] <- "ConsentMRC1"
#names(dados_MRC2SEG1)[4] <- "Data do ConsentMRC1"
#names(dados_MRC2SEG2)[2] <- "ConsentMRC2"
#names(dados_MRC2SEG2)[4] <- "Data do ConsentMRC2"
#names(dados_MRC2SEG3)[2] <- "ConsentimentoMRC3"
#names(dados_MRC2SEG3)[4] <- "Data do ConsentimentoMRC3"

#dados1 <- dados_MRC2SEG3 %>% filter(!is.na(idnova))
#dados1$idnova <- as.character(dados1$idnova)

#dados_seg1_seg2 <- merge(dados_MRC2SEG1, dados_MRC2SEG2, by = "idnova", all = T)

#dados_seg1_seg2_seg3 <- merge(dados_seg1_seg2, dados_MRC2SEG3,by = "idnova", all = T)




##Filtrando o banco com a varivel desejada para a checagem da duplicagem
#Dados_SEG3 <- Dados_SEG3[, c(idnova, ConsentimentoMRC3,ci_sexo,data_consent,coleta_ind )]
#dadosind22 <- filter(Dados_SEG3, consent == "1")

#duplicidade_idnova <- Dados_SEG3 %>% group_by(idnova ) %>% dplyr::summarise(count=n())
#duplicidade_idnova <- duplicidade_idnova %>% filter(count>1)
#dadosind22 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/MRC2SEG3.csv')
#dadosind22 <- dadosind22[,c("idnova", "consent","dat_nas","ci_sexo","data_consent","coleta_ind" )]
#Mudando idnova para texto
#dadosind$idnova <- as.character(dadosind$idnova)
#atribuindo novo valor para variaveis idnova vazias
#dadosind$idnova[is.na(dadosind$idnova)] <- 999

# Selecionando idnovas duplicadas
#duplicados <- dadosind[duplicated(dadosind$idnova, incomparables = TRUE),]
#duplicados <- duplicados[which(duplicados$idnova!=999),]


#View(duplicados)
#Dados_SEG3 <- Dados_SEG3 %>% filter(dupl_banc!=1) %>% dplyr::select(casanova,idnova,consent ,ci_sexo,data_consent,coleta_ind,dtcoleta,dupl_banc )

#Mudando idnova para texto
#Dados_SEG3$idnova <- as.character(Dados_SEG3$idnova)


#atribuindo novo valor para variaveis idnova vazias
#Dados_SEG3$idnova[is.na(Dados_SEG3$idnova)] <- 9


# Selecionando idnovas duplicadas
#Dados_SEG3 <- Dados_SEG3[duplicated(Dados_SEG3$idnova, incomparables = TRUE),]
#Dados_SEG3 <- Dados_SEG3[which(Dados_SEG3$dupl_banc!=1),]


#View(Dados_SEG3)

#transformando os NA das variveis consent em 0
#Dados_SEG1$`ConsentMRC1`[is.na(Dados_SEG1$`ConsentMRC1`)] <- "0"
#Dados_SEG2$`ConsentMRC2`[is.na(Dados_SEG2$`ConsentMRC2`)] <- "0"
#Dados_SEG3$`ConsentimentoMRC3`[is.na(Dados_SEG3$`ConsentimentoMRC3`)] <- "0"
