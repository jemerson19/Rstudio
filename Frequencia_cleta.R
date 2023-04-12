
Freq_colet<- read.csv(file = 'C:/MRC/MRC2_SOROINQUERITO03/Soroinquerito3MRC2-FrequenciaDeEntrevis_DATA_2023-03-20_1321.csv')

frequency(Freq_colet$entr_ind)

summarise(Freq_colet, Freq_colet$entr_ind)

table(Freq_colet$entr_ind)
tab1(Freq_colet$entr_ind,
    sort.group = "decreasing", cum.percent = TRUE, main = " Individuos", ylab = "Frequência")
