mrc2seg1 <- read_csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/mrc2s1.csv')
mrc2seg1$consent <- "1"

str(mrc2seg1)

spec(mrc2seg1)

mrc2seg2 <- read_csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/mrc2s2.csv')
mrc2seg2$consent <- "1"
str(mrc2seg2)
spec(mrc2seg2)

mrc2seg3 <- read_csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/mrc2s3.csv')
mrc2seg3$consent <- "1"
str(mrc2seg3)
spec(mrc2seg3)




dados_gerais_ind <- rbind(mrc2seg1, mrc2seg2, mrc2seg3)


write.csv(dados_gerais_ind,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/dados_gerais_ind.csv',fileEncoding = "UTF-8")


### LINKAGEM DE BANCO VERTICAL

mrc2s1=read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/mrc2s1.csv')

mrc2s2=read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/mrc2s2.csv')

mrc2s3=read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/mrc2s3.csv')

(full.join.dplyr <- full_join(mrc2s1, mrc2s2, by = c("idnova" = "idnova")))

(full.join.dplyr <- full_join(full.join.dplyr, mrc2s3, by = c("idnova" = "idnova")))

write.csv(full.join.dplyr,  file = 'C:/MRC/MRC2_SOROINQUERITO03/Apresentação/dashboard/full.join.dplyr.csv')

