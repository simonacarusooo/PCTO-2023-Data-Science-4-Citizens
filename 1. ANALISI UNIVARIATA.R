#######################################
###UPLOADING E STRUTTURA DEL DATASET###
#######################################

#Facciamo l'uploading del dataset in R e visualizziamone le caratteristiche
fumo <-read.csv("C:/Users/39334/Downloads/smoking.csv")
View(fumo)
head(fumo) #R mostra di default le prime 6 osservazioni
head(fumo,3) #selezioniamo le prime tre osservazioni
dim(fumo) #vediamo il numero di osservazioni e il numero di variabili

colnames(fumo)
nomi_ita <- c("eta","altezza_cm","peso_kg", "girovita_cm", "vista_sinistra","vista_destra", "udito_sinistra", "udito_destra", "sistolica", "diastolica", "glicemia_digiuno", "colesterolo", "trigliceridi","HDL", "LDL", "emoglobina", "proteine_urine","creatinina", "transaminasi_AST", "transaminasi_ALT", "gamma_GT", "carie", "fumatore")
colnames(fumo) <- nomi_ita
 
#Tipologia variabili
str(fumo)#Abbiamo capito che in realtà queste sono variabili categoriche e non numeriche
colnames(fumo)
fumo[c(7,8,17,22,23)] <- lapply(fumo[c(7,8,17,22,23)],factor)
str(fumo)
attach(fumo)



########################
###ANALISI UNIVARIATA###
########################

install.packages("ggplot2")
library(ggplot2)


#Caratteristiche fisiche ed anagrafiche
View(fumo) #età non è numerica, ma categorica

#ETA'
fumo$eta_ <- ifelse(fumo$eta %in% c("20", "25"), "20-29",
                     ifelse(fumo$eta %in% c("30", "35"), "30-49",
                            ifelse(fumo$eta %in% c("40"), "40-44",
                                   ifelse(fumo$eta %in% c("45"), "45-49",
                                          ifelse(fumo$eta %in% c("50", "55"), "50-60",
                                                 ifelse(fumo$eta %in% c("60","65"), "60-65",
                                                        ifelse(fumo$eta %in% c("70", "75", "80", "85"), "70+", NA)))))))

fumo$eta_ <- factor(fumo$eta_)
View(fumo)
fumo <- fumo[,-1]
attach(fumo)


  
#ALTEZZA_CM
min(altezza_cm)
max(altezza_cm)
mean(altezza_cm)
median(altezza_cm)
quantile(altezza_cm)
sqrt(var(altezza_cm))

ggplot(fumo, aes(x=altezza_cm)) + 
  geom_bar(color="blue", fill="blue")+
  labs(x="Altezza in cm")

#PESO_KG
min(peso_kg)
max(peso_kg)
median(peso_kg)
mean(peso_kg)
quantile(peso_kg)
sqrt(var(peso_kg))

ggplot(fumo, aes(x=peso_kg))+ 
  geom_bar(color="blue", fill="blue")+
  labs(x="peso in kg")
  
#GIROVITA_CM
min(girovita_cm)
max(girovita_cm)
median(girovita_cm)
quantile(girovita_cm)
mean(girovita_cm)
sqrt(var(girovita_cm))

ggplot(fumo, aes(x=girovita_cm)) + 
  geom_histogram(color="blue", fill="blue")+
  labs(x="girovita in cm")



#Vista, udito, carie e proteine nelle urine

#VISTA_SINISTRA
attach(fumo)
summary(vista_sinistra) #notiamo il valore max che è tanto distante dal 75 percentile

ggplot(fumo, aes(x=vista_sinistra)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, (fumo$vista_sinistra < 1.3 & fumo$vista_sinistra > 0.2))
attach(fumo)
summary(vista_sinistra)
dim(fumo)

ggplot(fumo, aes(x=vista_sinistra))+
  geom_bar(color="blue", fill="blue")+
  labs("vista occhio sinistro")


#VISTA_DESTRA
summary(vista_destra)
table(vista_destra)

ggplot(fumo, aes(x=vista_destra)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, (fumo$vista_destra < 1.3 & fumo$vista_destra > 0.2))

attach(fumo)
summary(vista_destra)

ggplot(fumo, aes(x=vista_destra))+
  geom_bar(color="blue", fill="blue")+
  labs(x="vista occhio destro")

#UDITO_SINISTRA 
summary(udito_sinistra)

ggplot(fumo, aes(x=udito_sinistra))+
  geom_bar(color="blue", fill="blue")+
  labs(x="udito orecchio sinistro")

#UDITO_DESTRA
summary(udito_destra)

ggplot(fumo, aes(x=udito_destra))+
  geom_bar(color="blue", fill="blue")+
  labs(x="udito orecchio destro")

#PROTEINE URINE
#"negativo";
#"poche" (corrispondente approssimativamente 10-20 mg/dL);
#"proteine 1+" (circa 30 mg/dL);
#"proteine 2+" (circa 100 mg/dL);
#"proteine 3+" (circa 300 mg/dL);
#"proteine 4+" (circa 1000 mg/dL).
summary(proteine_urine)

ggplot(fumo, aes(x=proteine_urine))+
  geom_bar(color="blue", fill="blue")+
  labs(x="proteine nelle urine")


#CARIE
summary(carie)

ggplot(fumo, aes(x=carie))+
  geom_bar(color="blue", fill="blue")+


#Pressione

#SISTOLICA (alta)
summary(sistolica)

ggplot(fumo, aes(x=sistolica)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, (fumo$sistolica > 80 & fumo$sistolica < 200))

attach(fumo)
summary(sistolica)

ggplot(fumo, aes(x=sistolica))+
  geom_histogram(color="blue", fill="blue")+
  labs(x="pressione sistolica")


#DIASTOLICA
summary(diastolica)

ggplot(fumo, aes(x=diastolica)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

ggplot(fumo, aes(x=diastolica))+
  geom_histogram(color="blue", fill="blue")+
  labs(x="pressione diastolica")


#valori del sangue

#GLICEMIA DIGIUNO
summary(glicemia_digiuno)

ggplot(fumo, aes(x=glicemia_digiuno)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo,(fumo$glicemia_digiuno>60 & fumo$glicemia_digiuno<300))

attach(fumo)

ggplot(fumo, aes(x=glicemia_digiuno))+
  geom_histogram(color="blue", fill="blue")+
  labs(x="glicemia")

#COLESTEROLO
summary(colesterolo)
quantile(colesterolo)

ggplot(fumo, aes(x=colesterolo)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

attach(fumo)

ggplot(fumo, aes(x=colesterolo))+
  geom_histogram(color="blue", fill="blue")

#TRIGLICERIDI
summary(trigliceridi)
quantile(trigliceridi)

ggplot(fumo, aes(x=trigliceridi)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

summary(trigliceridi)
fumo <- subset(fumo, (fumo$trigliceridi < 500))

ggplot(fumo, aes(x=trigliceridi))+
  geom_histogram(color="blue", fill="blue")

#HDL
summary(HDL)
quantile(HDL)

ggplot(fumo, aes(x=HDL)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

ggplot(fumo, aes(x=HDL))+
  geom_histogram(color="blue", fill="blue")

#LDL 
summary(LDL)
quantile(LDL)

ggplot(fumo, aes(x=LDL)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo,fumo$LDL < 500)
attach(fumo)
summary(LDL)

ggplot(fumo, aes(x=LDL))+
  geom_histogram(color="blue", fill="blue")

#EMOGLOBINA
summary(emoglobina)
quantile(emoglobina)

ggplot(fumo, aes(x=emoglobina)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

ggplot(fumo, aes(x=emoglobina))+
  geom_histogram(color="blue", fill="blue")


#CREATININA
summary(creatinina)
quantile(creatinina)

ggplot(fumo, aes(x=creatinina)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, fumo$creatinina <2.5 )
attach(fumo)
summary(creatinina)

ggplot(fumo, aes(x=creatinina))+
  geom_histogram(color="blue", fill="blue")

#TRANSAMINASI_AST
summary(transaminasi_AST)
quantile(transaminasi_AST)

ggplot(fumo, aes(x=transaminasi_AST)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, (fumo$transaminasi_AST < 300 ))
attach(fumo)
summary(transaminasi_AST)

ggplot(fumo, aes(x=transaminasi_AST))+
  geom_histogram(color="blue", fill="blue")+
  labs(x="transaminasi AST")

#TRANSAMINASI_ALT
summary(transaminasi_ALT)
quantile(transaminasi_ALT)

ggplot(fumo, aes(x=transaminasi_ALT)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, fumo$transaminasi_ALT < 500)

attach(fumo)

summary(transaminasi_ALT)

ggplot(fumo,aes(x=transaminasi_ALT))+
  geom_histogram(color="blue", fill="blue")+
  labs(x="transaminasi ALT")


#GAMMA_GT
summary(gamma_GT)
quantile(gamma_GT)

ggplot(fumo, aes(x=gamma_GT)) + 
  geom_boxplot(color="black", fill="blue", outlier.colour = "red")

fumo <- subset(fumo, fumo$gamma_GT < 500 )

attach(fumo)
summary(gamma_GT)

ggplot(fumo, aes(x=gamma_GT))+
  geom_histogram(color="blue", fill="blue")+
  labs(x="gamma GT")


#Dimensioni dataset modificato
dim(fumo)
write.csv(fumo, "smoking_update")



