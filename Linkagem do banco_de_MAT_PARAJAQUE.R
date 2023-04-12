#Subindo o banco 
Resultdos_MAT_SEG1 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Resultdos_MAT_SEG1.csv')
Resultdos_MAT_SEG2 <- read.csv(file = 'C:/MRC/Jaquelinecruz/Resultdos_MAT_SEG2.csv')

#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da IDNOVA)
Resultdos_MAT_SEG1$idnova <- as.character(Resultdos_MAT_SEG1$idnova)
Resultdos_MAT_SEG2$idnova <- as.character(Resultdos_MAT_SEG2$idnova)

#Mudando todos NA do banco para SEM DADOS
Resultdos_MAT_SEG1[is.na(Resultdos_MAT_SEG1)] <- "Sem dados"
Resultdos_MAT_SEG2[is.na(Resultdos_MAT_SEG2)] <- "Sem dados"

#SUMARIZANDO QUANTOS "NA" tem em cada variavel
sapply(Resultdos_MAT_SEG1, function(x) sum(is.na(x)))
sapply(Resultdos_MAT_SEG2, function(x) sum(is.na(x)))



#Filtrando o banco com a varivel desejada para o mergen
Resultdos_MAT_SEG1 <- Resultdos_MAT_SEG1 %>% dplyr::select(casanova,idnova,coleta_ind, dtcoleta,sorovp,dmat,copl1,cani,misto,akiy,ballum,grippo,c3522c,lv3954,rga,maxtv)
Resultdos_MAT_SEG2 <- Resultdos_MAT_SEG2 %>% dplyr::select(casanova,idnova,coleta_ind,dtcoleta,sorovp,dmat,copl1,cani,misto,akiy,ballum,grippo,c3522c,lv3954,rga,maxtv)


Resultdos_MAT_SEG1  <- dplyr:::rename(Resultdos_MAT_SEG1, "coleta_ind" = "Coleta_sim",
                              "dtcoleta_SEG1" = "dtcoleta",
                              "sorovp_SEG1" = "sorovp",
                              "copl1_SEG1" = "copl1",
                              "cani_SEG1" = "cani",
                              "misto_SEG1" = "misto",
                              "akiy_SEG1" = "akiy",
                              "ballum_SEG1" = "ballum",
                              "grippo_SEG1" = "grippo",
                              "lv3954_SEG1" = "lv3954",
                              "rga_SEG1" = "rga",
                              "maxtv_SEG1" = "maxtv",  
                              "dmat_SEG1" = "dmat"  
)
Resultdos_MAT_SEG2  <- dplyr:::rename(Resultdos_MAT_SEG2,"coleta_ind" = "Coleta_sim",
                              "dtcoleta_SEG2" = "dtcoleta",
                              "sorovp_SEG2" = "sorovp",
                              "copl1_SEG2" = "copl1",
                              "cani_SEG2" = "cani",
                              "misto_SEG2" = "misto",
                              "akiy_SEG2" = "akiy",
                              "ballum_SEG2" = "ballum",
                              "grippo_SEG2" = "grippo",
                              "lv3954_SEG2" = "lv3954" , 
                              "rga_SEG2" = "rga" , 
                              "maxtv_SEG2" = "maxtv" , 
                              "dmat_SEG2" = "dmat"  
)


#Linkando a base de dados do S1 E S2 Utilziando a IDNOVA como a variavel chave
dados_seg1seg2 <- merge(Resultdos_MAT_SEG1, Resultdos_MAT_SEG2, by= "idnova", all = T)


dados_seg1seg2[is.na(dados_seg1seg2)] <- "Sem dados"

write.csv(dados_seg1seg2,  file = 'C:/MRC/Jaquelinecruz/Resultados_MAT.csv',row.names = F ,fileEncoding = "UTF-8")

