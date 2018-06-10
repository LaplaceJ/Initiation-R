# exercice 1

  #1.1 dans le rapport


  #1.2

dataW = c(48.80 , 7.30 , 27.30 , 53.30 , 25.70 , 59.40 , 42.90 , 63.70 , 68.40 , 19.80)



#Fonction qui estime bêta par la méthode du maximum de vraisemblance
mleWeibull <- function(vect , X) {
  k = vect[1]
  beta  = vect[2]
  n = length(X)
 
  lv = sum(dweibull(X, k , beta ,log=TRUE ))
 
  return (-lv)
 
}

vecteur = c(1,1)
estiWeibull1 = optim( vecteur , mleWeibull, X = dataW , lower=c(-Inf,1E-10) , method = "L-BFGS-B" )


#Même méthode via une librairie

install.packages("fitdistrplus")
library("fitdistrplus")
resW = fitdist(data = dataW, distr = "weibull", method = "mle")

#Nous obtenons les mêmes résultats par ces deux méthodes
estiWeibull1

# shape = k
# scale  = beta
summary(resW)

#Exercice 2


  #2.1 normalité

#Mise en forme des données
dataEffectifs = c(1 , 2 ,  6 ,  13 ,  20 , 21, 18, 11, 5, 1, 1 )
dataPoids = c(500.50 , 501, 501.50, 502, 502.50, 503 , 503.50 ,504 , 504.50, 505 , 505.50)
dataEx2 = rep( dataPoids , dataEffectifs)



#shapiro test  H0 : normalité des échantillons  
qqnorm(dataEx2)
qqline(dataEx2)
# p-value > alpha = 5%, nous gardons l'hypothèse H0
shapiro.test(dataEx2)

# Annalyse à "l'oeil"
hist(dataEx2 , xlab = "Poids (en grammes)" , ylab = "Effectifs" , main = "Histogramme des poids de pots de chocolats "
     ,col = "darkred")


  #1.2 IC



#IC de la moyenne avec variance inconnue
statM  <-  function(X , alpha) {
  #moyenne empirique
  m = mean(X)
 
  #S variance empirique sans biais
  S = var(X)
 
 
  n = length(X)
 
  quantille = qt(1 - alpha/ 2 ,df = n - 1 )
 
  l2_min <- m - sqrt(S/n )  * quantille
  l2_max <- m + sqrt(S/n ) * quantille
 
  l2 <- list(min = l2_min ,max = l2_max )
 
  return (l2)
}

IcMean = statM( dataEx2, 0.05)


#IC de la variance avec moyenne inconnue
statVar  <-  function(X , alpha) {
  #moyenne empirique
  m = mean(X)
 
  #S variance empirique sans biais
  S = var(X)
 
 
  n = length(X)
 
  #V variance empirique biaisée
  Vn = (n - 1 ) * S
 
 
  l2_min <-   Vn  / qchisq(1 - alpha/ 2 ,df = n - 1 )
  l2_max <-   Vn / qchisq( alpha/ 2 ,df = n - 1 )
 
  l2 <- list(min = l2_min ,max = l2_max )
 
  return (l2)
}

IcVar = statVar( dataEx2, 0.05)


#exercice 3

  #3.1

data = data.frame(oui=c(128213,647,359,42),non=c(65963,4000,2642,303),row.names = c("aucune","minimales","legere","grave"))
data = data.frame(aucune=c(128213,65963),minimale=c(647,4000),legere=c(359,2642),grave=c(42,303),row.names = c("Oui","Non"))



#gravité aucune pie chart
piepercent<- paste(round(100*data$aucune/sum(data$aucune), 2) , "%" )

pie(data$aucune, labels = piepercent, main = "Accidents avec aucune gravité",col = rainbow(length(data$aucune)))

legend("topright", c("Avec ceinture","Sans ceinture"), cex = 0.8,
       fill = rainbow(length(data$aucune)))





# gravité minimal  pie chart
piepercent<- paste(round(100*data$minimale/sum(data$minimale), 2)  , "%" )

pie(data$minimale, labels = piepercent, main = "Accidents avec gravité minimal",col = rainbow(length(data$minimale)))
legend("topright", c("Avec ceinture","Sans ceinture"), cex = 0.8,
       fill = rainbow(length(data$minimale)))



# gravité legere  pie chart
piepercent<- paste(round(100*data$legere/sum(data$legere), 2)  , "%" )

pie(data$legere, labels = piepercent, main = "Accidents avec gravité legere",col = rainbow(length(data$legere)))
legend("topright", c("Avec ceinture","Sans ceinture"), cex = 0.8,
       fill = rainbow(length(data$legere)))


# gravité grave  pie chart
piepercent<- paste(round(100*data$grave/sum(data$grave), 2)  , "%" )

pie(data$grave, labels = piepercent, main = "Accidents avec gravité grave",col = rainbow(length(data$grave)))
legend("topright", c("Avec ceinture","Sans ceinture"), cex = 0.8,
       fill = rainbow(length(data$grave)))




  #3.2

#H0 indep vs H1 pas indep ; p = 2.2e-16 => d'indep
chisq.test(data)


#Exercice 4


  # 4.1

#mise en forme des données
incident <- read.csv("/home/laplace_jordan/Vidéos/R/incident.csv", header = TRUE, dec=",",sep=";" )

#pas besoins de la colonne ID
typeCLient = summary(incident$TYPE_CLIENT)
duree_indicent = summary(incident$duree_indicent)

# pie chart des types de clients  
piepercent<- paste(round(100*typeCLient/sum(typeCLient), 1) , "%" )

pie(typeCLient, labels = piepercent, main = "Type clients",col = rainbow(length(typeCLient)))
legend("topright", c("Restructure","Standard" , "Surendette"), cex = 0.8,
       fill = rainbow(3))

hist(incident$duree_indicent,main="Histogramme des incidents apres qu'un client est été jugé fragile", prob = TRUE , xlab = "temps avant un incident " , ylab = "proportion")

  # 4.2


#IC avec ho = m ; var inconnue


#supérieure à 2 mois
t.test(x = incident$duree_indicent , mu = 2 , alternative = "greater" , conf.level = 0.95)

#inférieure à 4 mois
t.test(x = incident$duree_indicent , mu = 4 , alternative = "less" , conf.level = 0.95)

#Intervalle de confiance de niveau 95% de la durée moyenne avant la survenance du premier
# incident de paiement
IcMeanIncindent  = statM( incident$duree_indicent, 0.05)  

  #4.3

statEcar  <-  function(X , alpha) {
  #moyenne empirique
  m = mean(X)
 
  #S variance empirique sans biais
  S = var(X)
 
 
  n = length(X)
 
  #V variance empirique biaisée
  Vn = (n - 1 ) * S
 
 
  l2_min <-   Vn  / qchisq(1 - alpha/ 2 ,df = n - 1 )
  l2_max <-   Vn / qchisq( alpha/ 2 ,df = n - 1 )
 
  l2 <- list(min = sqrt(l2_min) ,max = sqrt(l2_max) )
 
  return (l2)
}

# IC sur les “restructurés”

IcMeanRestruc  = statM( incident$duree_indicent[incident$TYPE_CLIENT == "RESTRUCTURE"], 0.01)  
IcEcRestruc = statEcar ( incident$duree_indicent[incident$TYPE_CLIENT == "RESTRUCTURE"], 0.01)

# IC sur les “STANDARD”
IcMeanSTANDARD  = statM( incident$duree_indicent[incident$TYPE_CLIENT == "STANDARD"], 0.01)  
IcEcSTANDARD = statEcar ( incident$duree_indicent[incident$TYPE_CLIENT == "STANDARD"], 0.01)

# IC sur les “SURENDETTE”
IcMeanSURENDETTE = statM( incident$duree_indicent[incident$TYPE_CLIENT == "SURENDETTE"], 0.01)  
IcEcSURENDETTE = statEcar ( incident$duree_indicent[incident$TYPE_CLIENT == "SURENDETTE"], 0.01)

IcMeanRestruc
IcMeanSTANDARD
IcMeanSURENDETTE

IcEcRestruc
IcEcSTANDARD
IcEcSURENDETTE



  # 4.4
var.test(incident$duree_indicent[incident$TYPE_CLIENT == "SURENDETTE"] , incident$duree_indicent[incident$TYPE_CLIENT == "STANDARD"],  conf.level = 0.99)
var.test(incident$duree_indicent[incident$TYPE_CLIENT == "RESTRUCTURE"] , incident$duree_indicent[incident$TYPE_CLIENT == "STANDARD"],  conf.level = 0.99)
var.test(incident$duree_indicent[incident$TYPE_CLIENT == "SURENDETTE"] , incident$duree_indicent[incident$TYPE_CLIENT == "RESTRUCTURE"],  conf.level = 0.99)

  # 4.5
t.test(incident$duree_indicent[incident$TYPE_CLIENT == "SURENDETTE"] , incident$duree_indicent[incident$TYPE_CLIENT == "STANDARD"],  conf.level = 0.99)
t.test(incident$duree_indicent[incident$TYPE_CLIENT == "RESTRUCTURE"] , incident$duree_indicent[incident$TYPE_CLIENT == "STANDARD"],  conf.level = 0.99)
t.test(incident$duree_indicent[incident$TYPE_CLIENT == "SURENDETTE"] , incident$duree_indicent[incident$TYPE_CLIENT == "RESTRUCTURE"],  conf.level = 0.99)

#Exercice 5
  #5.1

prematures <- read.csv("/home/laplace_jordan/Vidéos/R/prematures.csv", header = TRUE, dec=".",sep="," )
summary(prematures)



#CONSIS : la consistance du col (1=mou, 2=moyen, 3=ferme)

prematures_consis = summary(factor(prematures$CONSIS))

piepercent<- paste(round(100*prematures_consis/sum(prematures_consis), 2) , "%")

pie(prematures_consis, labels = piepercent, main = "Consistance du col ",col = rainbow(length(prematures_consis)))
legend("topright", c("Mou","Moyen" , "Ferme"), cex = 0.8,
       fill = rainbow(3))



#CONTR : la présence ou nonde contraction

# on enleve le 3, surrement une erreur
prematures_contr = summary(factor(prematures$CONTR))
prematures_contr = prematures_contr[-3]

piepercent<- paste(round(100*prematures_contr/sum(prematures_contr), 2) , "%")

pie(prematures_contr, labels = piepercent, main = "Présence ou non de contraction",col = rainbow(length(prematures_contr)))
legend("topright", c("Avec contraction","Sans contraction" ), cex = 0.8,
       fill = rainbow(2))


#MEMBRAN : les membranes ruptures (=1) ou non (=2) ou incertain (=3)


prematures_membran = summary(factor(prematures$MEMBRAN))

piepercent<- paste(round(100*prematures_membran/sum(prematures_membran), 2) , "%")

pie(prematures_membran, labels = piepercent, main = "État de la menbrane ",col = rainbow(length(prematures_membran)))
legend("topright", c("Ruptures","Sans ruptures", "Incertain" ), cex = 0.8,
       fill = rainbow(length(prematures_membran)))


#DIAB : la présence (=1) ou non (=2) d’un problème de diabète, ou valeur manquante (=3).

prematures_diab = summary(factor(prematures$DIAB))

piepercent<- paste(round(100*prematures_diab/sum(prematures_diab), 2) , "%")

pie(prematures_diab, labels = piepercent, main = "Présence de diabète",col = rainbow(length(prematures_diab)))
legend("topright", c("Diabète","Sans diabète", "Valeur manquante" ), cex = 0.8,
       fill = rainbow(length(prematures_diab)))


#TRANSF : transfert (1) ou non (2) dans un hôpital de soins spécialises

prematures_transfert = summary(factor(prematures$TRANSF))

piepercent<- paste(round(100*prematures_transfert/sum(prematures_transfert), 2) , "%")

pie(prematures_transfert, labels = piepercent, main = "Transfert dans un hôpital de soins spécialises",col = rainbow(length(prematures_diab)))
legend("topright", c("Oui","Non" ), cex = 0.8,
       fill = rainbow(length(prematures_transfert)))


#GEMEL : grossesse simple (=1) ou multiple (2)

prematures_gemel = summary(factor(prematures$GEMEL))

piepercent<- paste(round(100*prematures_gemel/sum(prematures_gemel), 2) , "%")

pie(prematures_gemel, labels = piepercent, main = "Grossesse",col = rainbow(length(prematures_diab)))
legend("topright", c("Simple","Multiple" ), cex = 0.8,
       fill = rainbow(length(prematures_gemel)))



#PREMATURE : accouchement prématuré (positif ou négatif)

prematures_premature = summary(factor(prematures$PREMATURE))

piepercent<- paste(round(100*prematures_premature/sum(prematures_premature), 2) , "%")

pie(prematures_premature, labels = piepercent, main = "Accouchement prématuré",col = rainbow(length(prematures_premature)))
legend("topright", c("Positif","Négatif" ), cex = 0.8,
       fill = rainbow(length(prematures_premature)))



#GEST : l’âge gestationnel en semaines à l’entrée dans l’étude

hist(prematures$GEST,main="L’âge gestationnel à l’entrée dans l’étude", prob = TRUE , xlab = "Semaine" , ylab = "Effectifs",col = "darkred")


#DILATE : la dilatation du col en cm

hist(prematures$DILATE,main="Dilatation du col", prob = TRUE , xlab = "Centimètre" , ylab = "Effectifs",col = "darkred")

#EFFACE : l’effacement de col (en pourcentages)

hist(prematures$EFFACE,main="L’effacement de col", prob = TRUE , xlab = "Pourcentages" , ylab = "Effectifs",col = "darkred")

#AGE : âge de la patiente
hist(prematures$AGE,main=" ge de la patiente", prob = TRUE , xlab = " ge" , ylab = "Effectifs",col = "darkred")

#GRAVID : la gestite (nombre de grossesses antérieures y compris celle en cours
hist(prematures$GRAVID,main="Gestite", prob = TRUE , xlab = "Nombre de grossesses" , ylab = "Effectifs",col = "darkred")





  #5.2 2. Estimer à partir des données brutes la proportion de femmes ayant une période de gestation de
  # moins de 31 semaines.

prematures_gInf31 =  summary(factor(prematures$GEST[prematures$GEST < 31 ] ) )
prematures_gSup31 =  summary(factor(prematures$GEST[prematures$GEST >= 31 ] ) )

length(prematures$PREMATURE[prematures$GEST < 31 ])

prematures_gTotal =  c(sum(prematures_gInf31) , sum(prematures_gSup31)   )
names(prematures_gTotal) <- c("Inf31", "Sub31")

piepercent<- paste(round(100*prematures_gTotal/sum(prematures_gTotal), 2) , "%")
pie(prematures_gTotal, labels = piepercent, main = "Proportion du temps de gestation",col = rainbow(length(prematures_gTotal)))
legend("topright", c("Inf 31 semaines","Sup 31 semaines" ), cex = 0.8,
       fill = rainbow(length(prematures_gTotal)))


  #5.3 Peut –on considérer que la moitié des femmes ayant déjà connues un accouchement prématuré
  #ont une période de gestation inférieure à 31 semaines ? Donner la région critique du test, sa
  #p-valeur.


premature_Inf31V2 = table(prematures$GEST[prematures$PREMATURE=="positif"]<31)

#p-value = 0.9237
prop.test(premature_Inf31V2  ,  p = 0.5 , alternative = "less" )

qnorm
(1-(0.05/2))


  #4  La durée moyenne de gestation est-elle différente entre les femmes ayant déjà connues un accouchement
  #prématuré et celle qui n’en ont jamais eu ?


mean(prematures$GEST[prematures$PREMATURE == "positif"])
mean(prematures$GEST [prematures$PREMATURE == "negatif"])


t.test(prematures$GEST[prematures$PREMATURE == "positif"] ,prematures$GEST [prematures$PREMATURE == "negatif"] ,  alternative = "two.sided" )


  # 5.5.1

#H0: “Grossesse” et “accouchement prématuré” sont indépendantes Contre H1:“Grossesse” et “accouchement
#prématuré” dépendantes

chisq.test(prematures$PREMATURE,prematures$GEMEL)
table(prematures$PREMATURE,prematures$GEMEL)
# pvalue < 0.05, on rejette l’hypothèse H0 (indépendance)
# Donc dépendance

chisq.test(prematures$MEMBRAN,prematures$CONSIS)
table(prematures$MEMBRAN,prematures$CONSIS)

# Necessaire d'analyser la variable INCERTAINE ? Pourrait fausser le resultats
chisq.test(prematures$MEMBRAN[prematures$MEMBRAN!="3"],prematures$CONSIS[prematures$MEMBRAN!="3"])
table(prematures$MEMBRAN[prematures$MEMBRAN!="3"],prematures$CONSIS[prematures$MEMBRAN!="3"])










