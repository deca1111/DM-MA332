rm(list = ls())

duree_max <- 3600 #duree en secondes
lambda <- 1/3 #lambda optimal
duree <- 0
nb_voiture <- 0
temps_vector <- c()
voiture_vector <- c()
inter_vector <- c()

while (duree <= duree_max) {
  inter_vector[(length(inter_vector) + 1)] <- rexp(1,lambda)
  duree <- duree + inter_vector[length(inter_vector)]
  nb_voiture <- nb_voiture +1
  temps_vector[(length(temps_vector) + 1)] <- duree
  voiture_vector[(length(voiture_vector) + 1)] <- nb_voiture
}

#affichage de la simulation
plot.new() 
par(mar=c(4,4,3,5)) 
plot(temps_vector,voiture_vector,"s",col="blue",axes=F,xlab="",ylab="")
axis(4, ylim=c(0,10),col="blue",col.axis="blue",at=seq(0,((max(voiture_vector)/100)+1)*100,100))
mtext("Nombre de voitures",side=4,line=2.5,col="blue") 
axis( 1 , ylim=c(20,40),col="black",col.axis="black",at=seq(0,3600, by=200)) 
mtext("Temps [s]",side=1,line=2.5,col="black") 

#calcul et affichage de l'estimation du lambda (lambda experimental)
N_T <- nb_voiture
T <- duree_max
lambda_exp <- N_T / T
print(lambda_exp)

#fonction calculant le nombre de voiture arrivant en t minutes
nb_voiture_fct <- function(t,x_v,y_v){
  temp <- x_v[1]
  index <- 0
  if(t <= 3600){
    while(temp <= t){
      index <- index + 1
      temp <- x_v[index]
    }
    y_v[index]
  }else{
    0
  }
  
}

#calcul et affichage du graphe de la fonction precedente
x_vect <- 1 : 3600
y_vect <- c()
for (i in x_vect) {
  y_vect[i] = nb_voiture_fct(i, temps_vector, voiture_vector)
}

par(new = T)
plot(x_vect,y_vect,"s",col="red",axes=F,xlab="",ylab="")
axis( 2 ,col="red",col.axis="red",at=seq(20, 40, by=5)) 
mtext("Axe de la courbe rouge",side=2,line=2.5,col="red")


#calcul et affichage de l'histogramme
histo <- hist(inter_vector, breaks = max(inter_vector), plot =FALSE)