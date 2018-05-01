#Lecture des données 

library(readr)

library(dplyr)

data <- read.csv("~/Linux/SI/TP2/poids.csv", " ", header = FALSE, dec=",",sep=" ")

colnames(data)[1]="poids"
summary(data)



#1 moyenne et variance empirique 


moyennepoids = mean(data$poids)
tailleData = length(data$poids)
variancepoids =  var(data$poids)* ( ( tailleData - 1 )  / tailleData  )

ecartTypepoids = sqrt(variancepoids)

#2 fonction d'échantillonnage 


getEchantille  <-  function(n ,X) {
   l_ech <-  X[sample(1:nrow(X), n), "poids"]
   
   l_mean <-mean(l_ech)
   tailleData = length(l_ech)
   l_var <-  var(l_ech)* ( ( tailleData - 1 )  / tailleData  )  
   
   l <- list(echantillon = l_ech ,moyenne = l_mean ,variance = l_var  )
   return (l)
}

test30 = getEchantille(30 ,data )
test60 = getEchantille(60 ,data )
test80 = getEchantille(80 ,data )



#3 intervalle de confiance pour var connue

icSigmaConnue  <-  function(l , alpha) {
  
  #fixer à var = 438,9²
  var = 438.9 * 438.9
  quantille = qnorm(1 - alpha/ 2 , mean = 0, sd = 1)
  
  l2_min <- l$moyenne - 438.9/sqrt(length(l$echantillon)) * quantille
  l2_max <- l$moyenne + 438.9/sqrt(length(l$echantillon)) * quantille
  
  l2 <- list(min = l2_min ,max = l2_max )
  
  return (l2)
}


listeTotal = list(echantillon = data$poids ,moyenne = moyennepoids ,variance = variancepoids  )

q3T30 = icSigmaConnue(test30, 0.05)
q3T60 = icSigmaConnue(test60, 0.05)
q3T80 = icSigmaConnue(test80, 0.05)
q3Total = icSigmaConnue(listeTotal, 0.05)




#4 intervalle de confiance pour var inconnue


icSigmaInconnue  <-  function(l , alpha) {
  
  var = 438.9 * 438.9
  quantille = qt( 1 - alpha / 2 , df = length(l$echantillon) - 1)
  
  l3_min <- l$moyenne - sd(l$echantillon) / sqrt( length(l$echantillon)) * quantille
  l3_max <- l$moyenne + sd(l$echantillon) / sqrt( length(l$echantillon)) * quantille
  
  l3 <- list(min = l3_min ,max = l3_max )
  
  return (l3)
}


listeTotal = list(echantillon = data$poids ,moyenne = moyennePoids ,variance = variancePoids  )

q4T30 = icSigmaInconnue(test30, 0.05)
q4T60 = icSigmaInconnue(test60, 0.05)
q4T80 = icSigmaInconnue(test80, 0.05)
q4Total = icSigmaInconnue(listeTotal, 0.05)



  #utilisation de t.test

t.test(test30$echantillon )
q4T30
t.test(test60$echantillon)
q4T60
t.test(test80$echantillon)
q4T80
t.test(listeTotal$echantillon)
q4Total







#partie 5 IC d'une proportion 


quantille = qnorm(1 - 0.05/ 2 , mean = 0, sd = 1)
proportion = 20/197
l3_min <- proportion - sqrt (proportion * (1-proportion)/ 197 )   * quantille
l3_max <- proportion + sqrt (proportion * (1-proportion)/ 197 )   * quantille

l3 <- list(min = l3_min ,max = l3_max )  


binom.test(20,197, alternative = "two.sided" , conf.level = 0.95)




