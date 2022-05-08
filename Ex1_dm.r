rm(list = ls())

duree_max <- 3600 #duree en secondes
lambda <- 1/3 #lambda optimal

#Question i

#simulation d'arrivé des voitures en t secondes
nb_voiture_fct <- function(t, type_return) { 
  duree <- 0
  index <- 1
  tableau <- list()
  inter <- list()
  inter_ = rexp(1,lambda)
  duree <- duree + inter_
  while(duree < t){
    tableau[index] <- duree
    inter[index] <- inter_
    index <- index + 1
    inter_ = rexp(1,lambda)
    duree <- duree + inter_
  }
  if(type_return == "duree"){
    return(tableau)
  }
  if(type_return == "inter"){
    return(inter)
  }
  
}

Dessine_N <- function (t){
  tableau = nb_voiture_fct(t,"duree")
  list = 1:length(tableau)
  plot(tableau, list
       ,type ="s"
       ,xlab="Temps [s]"
       ,ylab="Nombre de voitures"
       ,main= paste("Arrivée des voitures en", t, "secondes\n"))
  #calcul et affichage de l'estimation du lambda (lambda experimental)
  cat("Lambda experimental = ", length(tableau)/t, "\n\n")
}

#Dessine_N(duree_max)

#calcul et affichage de l'histogramme, verification de la distribution exponentielle
Dessine_Histo <- function(){
  inter = unlist(nb_voiture_fct(duree_max,"inter"))
  histo <- hist(inter, breaks = max(inter), plot = T)
}

#Dessine_Histo(3600)

#Question ii

#calcule le nombre moyen d'arrive pour un temps en seconde (moyenne calculée en nb_repet répétitions)
moyenne_arrive <- function (temps, nb_repet){
  nb_arrive <- 0
  for (i in (1:nb_repet)) {
    nb_arrive <- nb_arrive + length(nb_voiture_fct(temps,"duree"))
  }
  estimation = nb_arrive / nb_repet
  return(estimation)
}

Compare_moyenne <- function(t, n){
  m_e = moyenne_arrive(t,n)
  m_t = lambda*t
  diff = abs((m_e - m_t)*100/m_t)
  #cat("Moyenne experimentale d'arrivé sur", t, "secondes (",n, "répétitions):\t", m_e ,"\n")
  #cat("Moyenne théorique d'arrivé sur", t, "secondes :\t\t\t\t", m_t,"\n")
  #cat("Différence de",diff,"%\n\n")
  #L’espérance dune variable de Poisson est λ
  return(diff)
}

test_quest_ii <- function(n){
  t1 <- 0
  t2 <- 0
  t3 <- 0
  for (i in (1:n)) {
    t1 <- t1 + Compare_moyenne(3600,100)
    t2 <- t2 + Compare_moyenne(3600,5)
    t3 <- t3 + Compare_moyenne(72000,5)
    print(i)
  }
  t1 = t1 / n
  cat("Différence pour t = 3600s et n = 100 :\t",t1,"%\n")
  t2 = t2 / n
  cat("Différence pour t = 3600s et n = 5 :\t",t2,"%\n")
  t3 = t3 / n
  cat("Différence pour t = 72000s et n = 5 :\t",t3,"%\n\n")
}


#Question iii

#a)

#on prend les arrivée 5,50 500 et 1000 et on compare leur densité de probabilité à une loi gamma censée correspondre
compare_loi_gamma <- function(){
  A2 <- c()
  A5 <- c()
  A50 <- c()
  A500 <- c()
  for (i in 1:1000) {
    tab = nb_voiture_fct(3600,"duree")
    A2[i] = tab[2]
    A5[i] = tab[5]
    A50[i] = tab[50]
    A500[i] = tab[500]
  }
  
  densite_A2 = density(unlist(A2))
  plot(densite_A2
       ,xlab="Temps [s]"
       ,ylab="Densité"
       ,main= "Densité de probabilité de A2")
  x_vect = seq(0,30,by = 0.2)
  lines(x_vect,dgamma(x_vect, 2, lambda), col = "red")
  legend("topright", inset = .05, legend=c("Pratique", "Théorie"),
         col=c("black", "red"), lty = 1)
  
  
  densite_A5 = density(unlist(A5))
  plot(densite_A5
       ,xlab="Temps [s]"
       ,ylab="Densité"
       ,main= "Densité de probabilité de A5")
  x_vect = seq(0,50,by = 0.5)
  lines(x_vect,dgamma(x_vect, 5, lambda), col = "red")
  legend("topright", inset = .05, legend=c("Pratique", "Théorie"),
         col=c("black", "red"), lty = 1)
  
  
  densite_A50 = density(unlist(A50))
  plot(densite_A50
       ,xlab="Temps [s]"
       ,ylab="Densité"
       ,main= "Densité de probabilité de A50")
  x_vect = seq(80,220,by = 1)
  lines(x_vect,dgamma(x_vect, 50, lambda), col = "red")
  legend("topright", inset = .05, legend=c("Pratique", "Théorie"),
         col=c("black", "red"), lty = 1)
  
  
  densite_A500 = density(unlist(A500))
  plot(densite_A500
       ,xlab="Temps [s]"
       ,ylab="Densité"
       ,main= "Densité de probabilité de A500")
  x_vect = seq(1300,1700)
  lines(x_vect,dgamma(x_vect, 500, lambda), col = "red")
  legend("topright", inset = .05, legend=c("Pratique", "Théorie"),
         col=c("black", "red"), lty = 1)
}

#b)

#verifie que les interval inter-arrivée vérifie une loi uniforme quand on connait le nombre totale d'arrivée
verif_uniforme <- function(t){
  arrivee <- c()
  index <- 0
  for (i in 1:t) {
    tab = unlist(nb_voiture_fct(60,"duree"))
    if (length(tab)==20){
      arrivee <- c(arrivee,tab)
      index <- index +1
    }
  }
  densite_arrivee = density(arrivee)
  plot(densite_arrivee
       ,xlab="Temps [s]"
       ,ylab="Densité"
       ,main= "Densité de probabilité de arrivee")
  x_vect = seq(-5,65)
  lines(x_vect,dunif(x_vect, min = 0, max =60 ), col = "red")
  legend("center", inset = .05, legend=c("Pratique", "Théorie"),
         col=c("black", "red"), lty = 1)
}
