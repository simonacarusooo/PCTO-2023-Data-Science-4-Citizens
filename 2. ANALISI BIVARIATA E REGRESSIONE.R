#ANALISI BIVARIATA
fumo <- read.csv("~/smoking_update.csv")
fumo <- fumo[,-1]
colnames(fumo)
str(fumo)
fumo[c(6,7,16,21,22,23)] <- lapply(fumo[c(6,7,16,21,22,23)],factor)
table(fumo$fumatore)

colnames(fumo)
attach(fumo)
library(ggplot2)

#1. CARATTERISTICHE FISICHE
#a. peso
ggplot(fumo, aes(x = fumatore, y = peso_kg, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot dei pesi per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Peso in kg")+
  coord_flip()

ggplot(fumo, aes(x = peso_kg, fill = fumatore)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Peso in kg", y = "Frequenza")

#b. altezza
ggplot(fumo, aes(x = fumatore, y = altezza_cm, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot dell'altezza per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Altezza in cm")+
  coord_flip()


#c.girovita
ggplot(fumo, aes(x = fumatore, y = girovita_cm, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot del girovita per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Girovita in cm")+
  coord_flip()

library(corrplot)
cor1 <- fumo[,c(1,2,3)]
matr_cor1 <- cor(cor1)
corrplot(matr_cor1, method="number", number.cex=1, tl.col="black",tl.cex=1)

fumo <- fumo[,-2]
attach(fumo)

str(fumo)

#d. Fumatore - carie
tab1 <- table(fumatore,carie)
prop.table(tab1)
prop.table(tab1, margin=1)


#2. VISTA E UDITO 
#a. vista sinistra
tab2 <- table(fumatore,vista_sinistra)
prop.table(tab2)
prop.table(tab2, margin=1)

tab3 <- table(fumatore,vista_destra)
prop.table(tab3)
prop.table(tab3, margin=1)

tab4 <- table(fumatore,udito_sinistra)
prop.table(tab4)
prop.table(tab4, margin=1)

tab5 <- table(fumatore,udito_destra)
prop.table(tab5)
prop.table(tab5, margin=1)


#3. VALORI DEL SANGUE
#a. Pressione sistolica e diastolica
ggplot(fumo, aes(x = fumatore, y = sistolica, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot pressione sistolica per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Pressione sistolica")+
  coord_flip()

ggplot(fumo, aes(x = fumatore, y = diastolica, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot pressione diastolica per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Pressione diastolica")+
  coord_flip()

#b. Glicemia
ggplot(fumo, aes(x = fumatore, y = glicemia_digiuno, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot glicemia per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Glicemia")+
  coord_flip()

#c. Colesterolo
ggplot(fumo, aes(x = fumatore, y = colesterolo, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot colesterolo per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Colesterolo")+
  coord_flip()

ggplot(fumo, aes(x = fumatore, y = HDL, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot HDL per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("HDL")+
  coord_flip()

ggplot(fumo, aes(x = fumatore, y = LDL, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot LDL per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("LDL")+
  coord_flip()

colnames(fumo)
cor2 <- fumo[,c(10,12,13)]
matr_cor2 <- cor(cor2)
corrplot(matr_cor2, method="number", number.cex=1, tl.col="black",tl.cex=1)

fumo <- fumo[,-10]
attach(fumo)


#d. trigliceridi
ggplot(fumo, aes(x = fumatore, y = trigliceridi, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot trigliceridi per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Trigliceridi")+
  coord_flip()

colnames(fumo)
cor3 <- fumo[,c(10,11,12)]
matr_cor3 <- cor(cor3)
corrplot(matr_cor3, method="number", number.cex=1, tl.col="black",tl.cex=1)

#e. creatinina
ggplot(fumo, aes(x = fumatore, y = creatinina, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot creatinina per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Creatinina")+
  coord_flip()

#f. transaminasi
ggplot(fumo, aes(x = fumatore, y = transaminasi_ALT, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot transaminasi ALT per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Transaminasi ALT")+
  coord_flip()

ggplot(fumo, aes(x = fumatore, y = transaminasi_AST, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot transaminasi AST per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Transaminasi AST")+
  coord_flip()

colnames(fumo)
cor4 <- fumo[,c(16,17)]
matr_cor4 <- cor(cor4)
corrplot(matr_cor4, method="number", number.cex=1, tl.col="black",tl.cex=1)


#g. Gamma GT
ggplot(fumo, aes(x = fumatore, y = gamma_GT, fill = fumatore)) +
  geom_boxplot() +
  ggtitle("Boxplot Gamma GT per fumatori e non fumatori") +
  xlab("Fumatore") +
  ylab("Gamma GT")+
  coord_flip()


#4.PROTEINE URINE
ggplot(fumo, aes(x = proteine_urine, fill = fumatore)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Proteine nelle urine", y = "Frequenza")

tab6 <- table(fumatore,proteine_urine)
prop.table(tab6)
prop.table(tab6, margin=1)


cor5 <- fumo[,c(1,2,3,4,7,8,9,10,11,12,13,15,16,17,18)]
matr_cor5 <- cor(cor5)
corrplot(matr_cor5, method="number", number.cex=0.5, tl.col="black",tl.cex=0.5)
fumo<-fumo[,-17]
colnames(fumo)
write.csv(fumo, "smoking_update2")

