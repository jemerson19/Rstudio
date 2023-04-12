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


Dados_dom <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Analises/Chefe de familia/2023_03_31/Redcap_Dom.csv')
chefe_familia <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Analises/Chefe de familia/2023_03_31/Chefe_familia_geral.csv')

names(Dados_dom)[4] <- "Seguimento"
Dados_dom <- filter(Dados_dom, Seguimento== "1")
Dados_dom <- Dados_dom[, c("casanova", "Seguimento")]

View(Dados_dom)

subdatas_chef <- filter(chefe_familia, chefe_fam == "1")
subdatas_chef1 <- subdatas_chef[, c("casanova", "chefe_fam")]

View(subdatas_chef1)

full.join.dplyr <- full_join(subdatas_chef1, Dados_dom, by = c("casanova" = "casanova"))


write.csv(full.join.dplyr,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Analises/Chefe de familia/2023_03_31/Dom_Chef_geral.csv',fileEncoding = "UTF-8")
