rm(list = ls())

lambda = 2
mu = 3


#1 
#On commence par remplir 2 tableaux:
#Le premier compte les arrivées et le deuxièmes les temps de traitement
#On se base sur le premier exercice

fct_arrive <- function(temps,l, type_return){
  duree <- 0
  index <- 1
  tableau <- c()
  inter <- c()
  
  inter_ = rexp(1,l)
  
  while(duree < temps){
    duree <- duree + inter_
    
    tableau[index] <- duree
    inter[index] <- inter_
    
    index <- index + 1
    inter_ = rexp(1,l)
  }
  if(type_return == "duree"){
    return(tableau)
  }
  if(type_return == "inter"){
    return(inter)
  }
}

fct_service <- function(temps,m, type_return){
  duree <- 0
  index <- 1
  tableau <- c()
  inter <- c()
  
  inter_ = rexp(1,m)
  
  while(duree < temps){
    duree <- duree + inter_
    
    tableau[index] <- duree
    inter[index] <- inter_
    
    index <- index + 1
    inter_ = rexp(1,m)
  }
  if(type_return == "duree"){
    return(tableau)
  }
  if(type_return == "inter"){
    return(inter)
  }
}

#prend en entree un temps max et 2 tableaux contenant les temps d'arrivé et les temps de service
fct_depart <- function(temps,arrivees, services, type_return){
  
  duree <- 0
  index <- 1
  depart_duree <- c()
  depart_inter <- c()
  
  interval = arrivees[1]+services[1]
  
  while ((duree < temps) && (index<length(arrivees))) {
    duree <- duree + interval
    depart_duree[index] <- duree
    depart_inter[index] <- interval
    
    index <- index +1
    
    if (arrivees[index] < depart_duree[index-1]){
      interval = services[index]
    }else{
      interval = (arrivees[index] - depart_duree[index-1]) + services[index]
    }
  }
  if(type_return == "duree"){
    return(depart_duree)
  }
  if(type_return == "inter"){
    return(depart_inter)
  }
}
  
N <- function(t, arrivees, departs){
  temps <- c()
  nb_client <-c()
  
  index_arr <- 1
  index_dep <- 1
  index_temp <- 1
  
  temps[1] <- 0
  nb_client[1] <- 0
  
  while ((index_dep <= length(departs)) && (temps[index_temp]<t)) {
    
    if(arrivees[index_arr] < departs[index_dep]){
      temps[index_temp+1] = arrivees[index_arr]
      nb_client[index_temp+1] <- nb_client[index_temp] +1
      index_arr <- index_arr + 1
    }else{
      temps[index_temp+1] = departs[index_dep]
      nb_client[index_temp+1] <- nb_client[index_temp] - 1
      index_dep <- index_dep + 1
    }
    index_temp <- index_temp + 1
  }
  
  my_list <- list("x" = temps, "y" = nb_client)
  
  return(my_list)
}

Dessine_arrive <- function (t){
  tableau = fct_arrive(t,lambda,"duree")
  list = 1:length(tableau)
  plot(tableau, list
       ,type ="s"
       ,xlab ="Temps [min]"
       ,ylab ="Nombre d'arrivée"
       ,main = paste("Arrivée des clients en", t, "minutes\n"))
}

Dessine_Q1 <- function (t){
  #par(mfrow=c(2,2)) #Pour afficher sur la meme image
  layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  
  arrivees <- fct_arrive(t,lambda,"duree")
  services <- fct_service(t,mu,"inter")
  
  depart = fct_depart(t,arrivees,services,"duree")
  
  nbclient = N(t,arrivees,depart)
  
  list_arr = 1:length(arrivees)
  plot(arrivees, list_arr
       ,type ="s"
       ,xlab ="Temps [min]"
       ,ylab ="Nombre d'arrivée"
       ,main = paste("Arrivée des clients en", t, "minutes\n"))
  
  list_dep = 1:length(depart)
  plot(depart, list_dep
       ,type ="s"
       ,xlab ="Temps [min]"
       ,ylab ="Nombre de depart"
       ,main = paste("Départ des clients en", t, "minutes\n"))
  
  plot(nbclient$x, nbclient$y
       ,type ="s"
       ,xlab ="Temps [min]"
       ,ylab ="Nombre de clients"
       ,main = paste("Nombre de client présent dans la boutique durant", t, "minutes\n"))
}


#2
nb_moy_client <- function(t,lambda_,mu_){
  
  arrivees <- fct_arrive(t,lambda_,"duree")
  services <- fct_service(t,mu_,"inter")
  depart = fct_depart(t,arrivees,services,"duree")
  
  nbclient = N(t,arrivees,depart)
  
  moy <- 0
  for (i in 2:(length(nbclient$x))) {
    moy <- moy + ((nbclient$x[i]-nbclient$x[i-1])*nbclient$y[i-1])
  }
  moy = moy / t
  return(moy)
}

#affiche le nombre moyen de client avec T qui varie
#retourne le paramètre operationnel, c-a-d l'integrale de 0 à T du nombre moyen de client divisé par T
q2 <- function(t_min,t_max,nb_rep,afficher,lambda_,mu_){
  nb_point = 125
  delta = (t_max - t_min)/nb_point
  temps <- seq(t_min,t_max,delta)
  nb_client <- c()
  
  param_ope <- 0
  
  for (i in (1:nb_point+1)) {
    temp<- 0
    for (j in 1:nb_rep) {
      temp<- temp + nb_moy_client(temps[i],lambda_,mu_)
    }
    nb_client[i] <- temp / nb_rep
    param_ope <- param_ope + (delta*nb_client[i])
  }
  
  param_ope <- param_ope / t_max
  
  if(afficher){
    layout(matrix(c(1), 1,1, byrow = TRUE))
    plot(temps, nb_client
         ,xlab ="Durée de T [min]"
         ,ylab ="Nombre moyen de client"
         ,main = paste("Nombre moyen de client pendant une durée T\n"))
    lines(temps, nb_client, col = 'blue', lwd = 2)
    abline(h=2)
  }
  
  return(param_ope)
}

evol_para_stocha <- function(nb_rep){
  nb_point = 100
  mu_ = 1
  delta = (0.99 - 0.01)/nb_point
  lambda_vect <- seq(0.01,0.99,delta)
  Q_vect <- c()
  
  for (i in (1:nb_point+1)) {
    lambda_ <- lambda_vect[i]
    temp<- 0
    for (j in 1:nb_rep) {
      temp<- temp + nb_moy_client(2000,lambda_,mu_)
    }
    Q_vect[i] <- temp / nb_rep
  }
  
  layout(matrix(c(1), 1,1, byrow = TRUE))
  plot(lambda_vect, Q_vect
       ,xlab ="Valeur de rho [min]"
       ,ylab ="Q(2000)"
       ,main = paste("Nombre moyen de client pendant une durée T\n"))
  lines(lambda_vect, Q_vect, col = 'blue', lwd = 2)
}

evol_para_ope <- function(nb_rep){
  nb_point = 50
  mu_ = 1
  delta = (0.99 - 0.01)/nb_point
  lambda_vect <- seq(0.01,0.99,delta)
  Q_vect <- c()
  
  for (i in (1:nb_point+1)) {
    lambda_ <- lambda_vect[i]
    Q_vect[i] <- q2(1,2000,nb_rep,FALSE,lambda_,mu_)
    cat("Avancement :",(i/(nb_point+1))*100,"%\n" )
  }
  
  layout(matrix(c(1), 1,1, byrow = TRUE))
  plot(lambda_vect, Q_vect
       ,xlab ="Valeur de rho [min]"
       ,ylab ="q¯(2000)"
       ,main = paste("Paramètre opérationnel selon rho (pour T=2000)\n"))
  lines(lambda_vect, Q_vect, col = 'blue', lwd = 2)
}

