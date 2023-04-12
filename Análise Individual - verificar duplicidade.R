# Verifica??o e instala??o de pacotes necess?rios
if (!require(pacman))
  install.packages("pacman")

if (!require(readr))
  install.packages("readr")

if (!require(plyr))
  install.packages("plyr")

if (!require(dplyr))
  install.packages("dplyr")

if (!require(RCurl))
  install.packages("RCurl")

if (!require(rlang))
  install.packages("rlang")

if (!require(car))
  install.packages("car")


# Chamando bibliotecas necess?rias

library(pacman)
library(readr)
library(plyr)
library(dplyr)
library(RCurl)
library(rlang)
library(car)


# Selecionando Diret?rio de Trabalho

#setwd("C:/Analises/Semanal/L48")

# Carregando arquivo de Dados de Entrevista Individual


dadosind22 <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/MRC2SEG3.csv')


dadosind22 <- dadosind22[,c("idnova", "consent","dat_nas","ci_sexo","data_consent","coleta_ind" )]


#Mudando idnova para texto
dadosind22$idnova <- as.character(dadosind22$idnova)


#atribuindo novo valor para variaveis idnova vazias
dadosind22$idnova[is.na(dadosind22$idnova)] <- 9


# Selecionando idnovas duplicadas
duplicados <- dadosind22[duplicated(dadosind22$idnova, incomparables = TRUE),]
duplicados <- duplicados[which(duplicados$idnova!=9),]


View(duplicados)

write.csv(duplicados,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/duplicados.csv',row.names = F ,fileEncoding = "UTF-8")





###################################################################



# Renomeando variavel ?..casanova
# Sintaxe: dados <- rename(dados,new_name = old=name)
names(dadosind)[1] <- "casanova"

# Outro comando para renomear Vari?vel (espec?fica)
#  dadosind <- rename(dadosind, casanova = ?..casanova)


# Visualizando o banco de dados

View(dadosind)


# Visualizando Vari?veis repetidas

#dadosind_rep <- dadosind[which(dadosind$duplicidade==1),]


# Visualizando vari?veis repetidas no banco

View(dadosind_rep)



# Visualizando Vari?veis n?o repetidas

# Atribuindo 9 para variaveis no momento, n?o repetidas

#criando c?pia do banco
dadosind_crep <- dadosind

#atribuindo novo valor para variaveis n?o duplicadas
dadosind_crep$duplicidade[is.na(dadosind_crep$duplicidade)] <- 999

#visualizando dados ap?s a inser??o do valor 999
View(dadosind_crep)

#criando novo banco apenas com vari?veis com indiv?duo sem marca??o de duplicidade
dadosind_nrep <- dadosind_crep[which(dadosind_crep$duplicidade==999),]

#visualizando banco apenas com vari?veis com indiv?duo sem marca??o de duplicidade
View(dadosind_nrep)


#######################testar######################################

# Visualizando vari?veis repetidas no banco

#View(dadosind_nrep)


# contando caracters
#nchar(dadosind$casanova)+nchar(dadosind$duplicidade)

#View(dadosind_nrep)

#dados_tes <- dadosind[View()]

#glimpse(dadosind)
#View((dadosind$casanova, na.rm=FALSE))

#summary(dadosind$renda)

#barplot(dadosind_rep$duplicidade)




#frequency(dadosind_nrep$idnova)

#summarise(dadosind_nrep, dadosind_nrep$idnova)




#Excluindo vari?veis
#Podemos excluir uma vari?vel (desalocar) da mem?ria usando a fun??o rm(). Por exemplo, se quisermos excluir uma vari?vel de nome soma basta executar a instru??o a seguir:

#  rm(soma)
#Caso seja necess?rio desalocar todas as vari?veis de uma vez, podemos combinar as fun??es ls() e rm():

#  rm(list = ls())


#Mudando de tipo
#dom_wt1 <- dom_wt1%>%
#  mutate_all(as.character)

#Converter texto em número
#indwt1$calcidadin <- as.numeric(indwt1$calcidadin)

#naoindwt1 <- naoindwt1%>%
#  mutate_all(as.character)







