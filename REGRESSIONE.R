#REGRESSIONE

#1. Creazione del modello e risultati
smoking_update2 <- read.csv("~/smoking_update2.csv", encoding="UTF-8")
smoking_update2<-smoking_update2[,-1]
modello <- glm(fumatore ~ ., data=smoking_update2,family=binomial)
summary(modello)


#RISULTATI DEL QUESTIONARIO
quest <- read.csv("C:/Users/39334/AppData/Local/Temp/Indagine_sulla_diffusione_dell_abitudine_di_fumo_di_sigaretta_tra_i_ragazzi_del_liceo_Respighi_SYEGBz", encoding="UTF-8", stringsAsFactors=FALSE)
colnames(quest)
quest <- quest[,-1]

#analisi bivariata 
#ci concentriamo sulle domande in cui possiamo distinguere i fumatori dai non fumatori
attach(quest)
library(ggplot2)

#famiglia
quest$C.è.qualcuno.nella.tua.famiglia.che.fuma.abitualmente. <- ifelse(quest$C.è.qualcuno.nella.tua.famiglia.che.fuma.abitualmente.=="Nessuno", "No", "Sì")
attach(quest)
tab1 <- table(Fumi., C.è.qualcuno.nella.tua.famiglia.che.fuma.abitualmente.)
prop.table(tab1, margin=1)

ggplot(quest, aes(x = C.è.qualcuno.nella.tua.famiglia.che.fuma.abitualmente.,y=..prop..,group=Fumi., fill = Fumi.)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Fumatori in famiglia", y = "Frequenza", fill="Fumatore")

#amici
colnames(quest)
View(quest)
ggplot(quest, aes(x = Nella.tua.compagnia.abituale.c.è.qualcuno.che.fuma.,y=..prop..,group=Fumi., fill = Fumi.)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Fumatori tra amici", y = "Frequenza", fill="Fumatore")
tab2 <- table(Fumi., Nella.tua.compagnia.abituale.c.è.qualcuno.che.fuma.)
prop.table(tab2, margin=1)


#genere
quest2<-quest[!quest$Qual.è.il.tuo.genere.=="Preferisco non rispondere" ,]
head(quest2)
ggplot(quest2, aes(x = Qual.è.il.tuo.genere.,y=..prop..,group=Fumi., fill = Fumi.)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Genere", y = "Frequenza", fill="Fumatore")
attach(quest2)
tab3 <- table(Fumi., Qual.è.il.tuo.genere.)
prop.table(tab3, margin=2)

#classe
attach(quest)
tab4 <- table(Fumi., Che.classe.frequenti.)
prop.table(tab4, margin=2)
table(Che.classe.frequenti.)

#sport
tab5 <- table(Fumi., Quante.volte.alla.settimana.pratichi.sport.)
prop.table(tab5, margin=1)
ggplot(quest, aes(x = Quante.volte.alla.settimana.pratichi.sport.,y=..prop..,group=Fumi., fill = Fumi.)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Sport", y = "Frequenza", fill="Fumatore")

#musica
install.packages("wordcloud2")
library(wordcloud2)

text <- strsplit(as.character(quest$Che.genere.di.musica.ascolti.abitualmente.), ";")
vettore_parole <- unlist(text)
tab_parole <- table(vettore_parole)
df_parole <- data.frame(word = names(tab_parole), freq = as.numeric(tab_parole))
wordcloud2(df_parole, color = "random-dark", size=20)

#percezione del rischio
tab6 <- table(Fumi., Ti.preoccupano.gli.effetti.che.il.fumo.provoca.sull.organismo.)
prop.table(tab6, margin=1)
ggplot(quest, aes(x = Ti.preoccupano.gli.effetti.che.il.fumo.provoca.sull.organismo.,y=..prop..,group=Fumi., fill = Fumi.)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Preoccupazione effetti fumo", y = "Frequenza", fill="Fumatore")


