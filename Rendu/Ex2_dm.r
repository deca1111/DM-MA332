"Exercice 2"
# fonction pour calculer un etat a partir d'un autre
nouvel_etat <- function(etat){
  proba <- runif(1,min = 0, max = 1)
  etat_temp <- 0
  if(etat == 0){ #si on est sain et pas vacciné
    if(proba <= 0.65){
      etat_temp <- etat
    }else if(proba > 0.65 && proba <= 0.85){
      etat_temp <- 1
    }else{
      etat_temp <- 2
    }
  }else if(etat == 1){#Si on est vacciné ou immunisé
    etat_temp <- 1
  }else if(etat == 2){#Si on est infécté
    if(proba <= 0.60){
      etat_temp <- etat
    }else if(proba > 0.60 && proba <= 0.90){
      etat_temp <- 1
    }else{
      etat_temp <- 3
    }
  }else{#si on est mort
    etat_temp <- etat
  }
  return (etat_temp)
}
simulation <- function(nb_semaine, nb_population){
  population <- rep.int(0, nb_population)
  for(i in seq(1:nb_semaine)){
    population <- changement_etat_population(population)
  }
  return(bilan_population(population = population))
}
changement_etat_population <- function(population){
  for(i in seq(1:length(population))){
    population[i] <- nouvel_etat(population[i])
  }
  return(population)
}
bilan_population <- function(population){
  sains <- 0
  vaccine <- 0
  infecte <- 0
  morts <- 0
  for(i in population){
    if(i == 0){
      sains <- sains + 1
    }else if(i == 1){
      vaccine <- vaccine + 1
    }else if(i == 2){
      infecte <- infecte + 1
    }else{
      morts <- morts + 1
    }
  }
  sains <- sains / length(population)
  vaccine <- vaccine / length(population)
  infecte <- infecte / length(population)
  morts <- morts / length(population)
  cat(sprintf("Sain : %s Vaccine : %s Malade : %s Mort: %s \n", sains, vaccine, infecte, morts))
}

# lancer la simulation sur 5 semaines et 100000 personnes
# simulation(5, 100000)

# calculer le temps qu'il faut avant qu'une personne ne soit morte ou vaccinée:
nb_semaine <- function(){
  etat <- 0
  compteur <- 0
  while ((etat != 1) && (etat != 3)){
    etat <- nouvel_etat(etat)
    compteur <- compteur + 1
  }
  return (compteur)
}
simulation_nb_semaine <- function(taille_population){
  somme <- 0
  for(i in seq(1:taille_population)){
    somme <- somme + nb_semaine()
  }
  return (somme / taille_population)
}

# lancer la simulation pour 10000 personnes
# simulation_nb_semaine(10000)


robert <- function(nb_test){
  nb_mort <- 0
  for(i in seq(1:nb_test)){
    etat <- 0
    while(etat != 1 && etat != 3){
      etat <- nouvel_etat(etat)
    }
    if(etat == 3){
      nb_mort <- nb_mort + 1
    }
  }
  return ((nb_mort / nb_test) * 100)
}
# simulation pour 100000 fois sur notre Robert
# robert(100000)
