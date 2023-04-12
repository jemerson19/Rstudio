### no script base tenho todos estos pacotes, mas acreidito que nao precisa de todos eles
install.packages("terra")
library(terra)
library(sf)
library(rlang)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(binom)
library(tidyverse)
library(lubridate)
library(grid)
library(ggthemes)
library(ggpubr)
library(rgdal)
library(lattice)
library(leafpop)
library(mapview)
library(viridis)
library(RColorBrewer)

## Leitura de shapfile para ter o mapa de base
Quarterones_MR <- readOGR('C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Quarterones_MR_AdC.shp')
summary(Quarterones_MR)
plot(Quarterones_MR)
## Leitura da tabela criada do reporte redcap

df <- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/Analise_DOM.csv')
df$segmrc<-factor (df$segmrc,labels = c("Recrutado","Nao Recrutado ","Nao encontrado","Sem informacao"), levels = c(1,2,3,9))
df$segmrc[is.na(df$segmrc)] <- "Sem informacao"

summary(df)
df2 <-na.omit(df)
df2$dom_area<-as.factor(df2$dom_area)
df2$quart<-as.factor(df2$quart)

### Criacao da tabela sumarizada por quarterao para mapa, eu no reporte criei a variavel "Aceitou"
## porque a funcáo para sumarizar e fazer o contagem tenho para uma variavel numerica de 0 e 1
## mas acredito voces podam adaptar
summary(df2)
##Water
prop.seguimento<-df2%>% group_by(quart)%>%
  summarize(x=sum(1), n=n())%>%
  Propor_aceitaram=round(binom.confint(x, n, conf.level = 0.95, methods = "exact")[[4]]*100,3))
  mutate(Seguimento=rep("Recrutado",1),
prop.seguimento

## merge dos dados sumarizados com o shapefile
map8<-merge(Quarterones_MR, prop.seguimento, by = "quart")
map8

### visualizaçao do mapa
mapview(map8,zcol = "Propor_aceitaram", col.regions=brewer.pal(9,"Reds"),legend = FALSE)

