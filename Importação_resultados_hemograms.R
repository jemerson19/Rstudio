#Subindo o banco para o R
Resultados_sim <- read.csv(file = 'C:/MRC/Animais_Domestico_PL/Resultados/HEMOGRAMAS/Resultados_sim.csv')
soro_sim <- read.csv(file = 'C:/MRC/Animais_Domestico_PL/Resultados/HEMOGRAMAS/soro_sim.csv')

names(Resultados_sim)[1] <- "id.animal"
names(soro_sim)[3] <- "id.animal"

#Fazendo merge do Banco do Redcap e da planilha do resultado atravez do id.animal
Import_redcap <- merge(Resultados_sim, soro_sim, by = "id.animal", all = T)

#Mudando a variavel cara nova para caracteres (Motivo: O R esta comendo o numero final da casanova_ad)
Import_redcap$casanova_ad <- as.character(Import_redcap$casanova_ad)

#Modificando os 'NA' para '999'
Import_redcap$`incexec`[is.na(Import_redcap$`incexec`)] <- "999"

#Selecionando idnovas  duplicadas
sem_result <- Import_redcap[duplicated(Import_redcap$incexec, incomparables = TRUE),]
sem_result <- sem_result[which(sem_result$incexec == 999),]
#Slecionando so quem teve coleta de EDTA
sem_result <- sem_result[which(sem_result$edta == 1),]

#Selecionando idnovas  não duplicadas
Import_redcap <- Import_redcap[duplicated(Import_redcap$incexec, incomparables = TRUE),]
Import_redcap <- Import_redcap[which(Import_redcap$incexec != 999),]


write.csv(Import_redcap,  file = 'C:/MRC/Animais_Domestico_PL/Resultados/HEMOGRAMAS/Import_redcap.csv',row.names = F )
write.csv(sem_result,  file = 'C:/MRC/Animais_Domestico_PL/Resultados/HEMOGRAMAS/sem_result.csv',row.names = F ,fileEncoding = "UTF-8")


