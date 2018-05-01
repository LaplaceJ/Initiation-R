
############################### partie 1

N=30
install.packages(c("readr", "dplyr"))
library(readr)

library(dplyr)

data <- read.csv("~/Linux/SI/TP1/fiabilites.csv", " ", header = FALSE, dec=",",sep=" ")

data

colnames(data)[1]="Duree"

summary(data)
#str(data)

hist(data$Duree)

#data[1:3,]
data[c(1,3,5),]


N = 30 

#échantillon des données obptenue dans fiabilites.csv (resp 30, 100 , 200 ) 
test1 = data[sample(1:nrow(data), 30), "Duree"]
test2 = data[sample(1:nrow(data), 100),"Duree" ]
test3 = data[sample(1:nrow(data), 500), "Duree"]


par(mfrow = c(2,2))
hist(data$Duree,main="Histogramme de la population", prob = TRUE)
hist(test1,main="Histogramme avec un échantillon de 30", prob = TRUE)
hist(data$Duree,add = TRUE,col="yellow" , prob = TRUE)
hist(test2,main="Histogramme avec un échantillon de 100", prob = TRUE)
hist(data$Duree,add = TRUE , col="yellow" , prob = TRUE)
hist(test3,main="Histogramme avec un échantillon de 500" , prob = TRUE)
hist(data$Duree,add = TRUE, col="yellow" , prob = TRUE)


############################### partie 2 


LVv1 <- function(vect , X) {
  mu = vect[1]
  sigma  = vect[2] 
  #print(paste("mu = " , mu ," sigma = ", sigma  ))
  
  #la log-vraissemblance {X1,...,Xn}
  lv = sum(dlnorm(X,mu,sigma,log=TRUE))
  #print(-lv)
  return (-lv)
  
}


vecteur = c(1,1)
lv = LVv1(vecteur, data$Duree)

############################### partie 3
#optim testais des sigma négatif. Nous utilisons la méthode L-BFGS-B qui permet de ne pas tester des sigmas négatif. 
estiR1 = optim( vecteur,LVv1, X = test1 , lower=c(-Inf,1E-10) , method = "L-BFGS-B" )
estiR2 = optim( vecteur,LVv1, X = test2 , lower=c(-Inf,1E-10) , method = "L-BFGS-B" )
estiR3 = optim( vecteur,LVv1, X = test3 , lower=c(-Inf,1E-10) , method = "L-BFGS-B" )
estiRAll = optim( vecteur,LVv1, X = data$Duree , lower=c(-Inf,1E-10) , method = "L-BFGS-B" )



############################### partie 4 

#moyenne
muTest1 = mean(log(test1))
muTest2 = mean(log(test2))
muTest3 = mean(log(test3))
muTestAll = mean(log(data$Duree))



#sigma

tailleT1 = length(test1)
tailleT2 = length(test2)
tailleT3 = length(test3)
tailleAll = length(data$Duree)

sigmaTest1 = sqrt(var(log(test1))* ( ( tailleT1 - 1 )  / tailleT1  ))
sigmaTest2 = sqrt(var(log(test2))* ( ( tailleT2 - 1 ) / tailleT2  ))
sigmaTest3 = sqrt(var(log(test3))* (( tailleT3 - 1 )/ tailleT3  ))
sigmaTestAll = sqrt(var(log(data$Duree))* ( ( tailleAll - 1 ) /tailleAll ))


############################### partie 5 

Empirique <-  function(E_X ,V_X) {
  var = log( 1 + V_X /E_X^2 )
  mu = log (E_X) - (var/2)
  
  return (c(mu,sqrt(var)))
  
}

#nouveau estimateur
Empiriquetest1 = Empirique(mean(test1),var(test1) )
Empiriquetest2 = Empirique(mean(test2),var(test2) )
Empiriquetest3 = Empirique(mean(test3),var(test3) )
EmpiriquetestAll = Empirique(mean(data$Duree),var(data$Duree) )


