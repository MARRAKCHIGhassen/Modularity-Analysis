
#######################################
#### Projet ARS
####
#### P2 : Approche de sélection de la modularité locale pour le calcul de communauté égo-centré
####
#### Auteurs : 
#### - Ghassen MARRAKCHI
#### - Abdelmounaim BOUZERIRA
####
#######################################






#######################################
#### 2-Matrice_des_modularités
#### Calcul des modularités des noeuds et des labels
#######################################






# --------------------------




###############
# Configuration
###############
library(stringr)
library(igraph)
set.seed(123)
setwd("P:/Projets/4-CS-Modularity-Analysis")




# --------------------------




###############
# Implémentation des fonctions
###############



# --- calcul de la modularité R
#' mod_R
#'
#' @param graphe 
#' @param C 
#' @param B 
#' @param S 
#'
#' @return number : modularité R
#' @export
#'
#' @examples
mod_R <- function(graphe, C, B, S) {
  b_in <- length(E(graphe)[B%--%B])
  b_out <- length(E(graphe)[B%--%S])
  return(b_in / (b_in + b_out))
}



# --- calcul de la modularité M
#' mod_M
#'
#' @param graphe 
#' @param C 
#' @param B 
#' @param S 
#'
#' @return number : modularité M
#' @export
#'
#' @examples
mod_M <- function(graphe, C, B, S) {
  D <- union(C, B)
  d_in <- length(E(graphe)[D%--%D])
  d_out <- length(E(graphe)[B%--%S])
  return(d_in / d_out)
}



# --- calcul de nombre de voisins appartenant à la communauté
#' voisins_appart
#'
#' @param noeud 
#' @param graphe 
#' @param ens 
#'
#' @return number
#' @export
#'
#' @examples
voisins_appart <- function(noeud, graphe, ens) {
  return(length(intersect(neighbors(graphe, noeud), ens)))
}



# --- calcul de la modularité L
#' mod_L
#'
#' @param graphe 
#' @param C 
#' @param B 
#' @param S 
#'
#' @return number : modularité L
#' @export
#'
#' @examples
mod_L <- function(graphe, C, B, S) {
  D <- union(C, B)
  d_in <- sum(sapply(D, voisins_appart, graphe, D))/length(D)
  d_out <- sum(sapply(B, voisins_appart, graphe, S))/length(B)
  return(d_in / d_out)
}



# --- mettre à jour les ensembles C, B et S
#' update
#'
#' @param noeud 
#' @param graphe 
#' @param C 
#' @param B 
#' @param S 
#'
#' @return list de vecteurs
#' @export
#'
#' @examples
update <- function(noeud, graphe, C, B, S) {
  
  # Enlever S
  S<- S[S != noeud]
  
  # Ensemble de toute la communauté
  D <- union(C, B)
  
  # Vérification des voisins du noeud
  if(all(neighbors(graphe, noeud) %in% D)){
    ## Tous les coisins appartiennent à la communauté
    C <- union(C, noeud)
    
  } else {
    ## Au moins un des voisins n'appartient pas
    B<- union(B, noeud)
    S_inter <- setdiff(neighbors(graphe, noeud), union(D, S))
    if(length(S_inter) > 0){
      S <- union(S, S_inter)
    }
    for(noeud_b in B){
      if(all(neighbors(graphe, noeud_b) %in% D)){
        B<- B[B != noeud_b]
        C <- union(C, noeud_b)
      }
    }
  }
  
  return(list(C=C, B=B, S=S))
}



# --- calcul de la qualité du noeud
#' calcul_qualite
#'
#' @param noeud 
#' @param graphe 
#' @param C 
#' @param B 
#' @param S 
#' @param mod 
#'
#' @return number : modularité calculée
#' @export
#'
#' @examples
calcul_qualite <- function(noeud, graphe, C, B, S, mod) {
  
  # Mettre à jour 
  mise_a_jour <- update(noeud, graphe, C, B, S)
  
  # Récupération des ensembles
  C <- mise_a_jour$C
  B <- mise_a_jour$B
  S <- mise_a_jour$S
  
  return(mod(graphe, C, B, S))
}



# --- détection de la communauté locale
#' local_com
#'
#' @param target 
#' @param graphe 
#' @param mod 
#'
#' @return vecteur de la communauté : C + B
#' @export
#'
#' @examples
local_com <- function(target, graphe, mod) {
  
  # Initialisation
  C <- c()
  B <- c(target)
  S <- c(V(graphe)[neighbors(graphe, target)]$id)
  Qualite <- 0
  Qualite_calcul <- 0

  # Boucle principale
  while((length(S) > 0) && (Qualite <= Qualite_calcul)){
    ## Caclul la modularité de chaque noeud de S
    Qualite_S <- sapply(S, calcul_qualite, graphe, C, B, S, mod)
    Qualite_calcul <- max(Qualite_S)
    
    ## Mettre à jour si ça s'améliore
    if(Qualite <= Qualite_calcul){
      noeud_S <- S[which.max(Qualite_S)]
      mise_a_jour <- update(noeud_S, graphe, C, B, S)
      C <- mise_a_jour$C
      B <- mise_a_jour$B
      S <- mise_a_jour$S
      Qualite <- Qualite_calcul
    }
  }
  
  return(union(C, B))
}



# --- transformer en bi-partition
#' bi_part
#'
#' @param target 
#' @param graphe 
#' @param mod 
#'
#' @return list de vecteurs : partition entre communauté et non-communauté
#' @export
#'
#' @examples
bi_part <- function(target, graphe, mod) {
  
  # Communauté
  com <- local_com(target, graphe, mod)

  # Non-communauté
  ids_inter <- V(graphe)$id
  non_com <- ids_inter[!(ids_inter %in% com)]

  return(list(com=com, non_com=non_com))
}



# --- comparer avec la vérité terrain
#' comp_verite_terr
#'
#' @param graphe 
#' @param bipartition 
#' @param methode 
#'
#' @return number : ratio de comparaison avec la vérité terrain
#' @export
#'
#' @examples
comp_verite_terr <- function(graphe, bipartition, methode) {
  
  detec_val <- V(graphe)$value
  
  # Attribution de la valeur suivant la détection
  for(i in 1:length(detec_val)){
    if(V(graphe)$id[i] %in% bipartition$com){
      detec_val[i] <- 0
    } else {
      detec_val[i] <- 1
    }
  }
  
  return(compare(V(graphe)$value, detec_val, methode))
}



# --- calculer la qualité pour tous les noeuds
#' calcul_modularite
#'
#' @param graphe 
#' @param modularites 
#' @param modularite_df 
#'
#' @return dataframe : dataframe contenant les modularités 
#' @export
#'
#' @examples
calcul_modularite <- function(graphe, modularites, modularite_df) {
  
  # Boucle Principale
  for(id in V(graphe)$id){
    
    print(paste("---- ", as.character(id)))
    
    ## Variation de la modularité
    for(mod_str in modularites){
      
      print(paste("-- ", mod_str))
      
      ### Calcul de la bipartition
      if(mod_str == "mod_R"){
        bipartition <- bi_part(id, graphe, mod_R)
      } else if (mod_str == "mod_M") {
        bipartition <- bi_part(id, graphe, mod_M)
      } else if (mod_str == "mod_L") {
        bipartition <- bi_part(id, graphe, mod_L)
      }
      
      ### Comparaison à la vérité terrain
      comparaison <- comp_verite_terr(graphe, bipartition, "nmi")
      
      ### Mettre à jour le dataframe
      modularite_df[id, mod_str] = comparaison
    }
    
  }
  
  return(modularite_df)
}



# --- calculer les labels
#' calcul_label
#'
#' @param graphe 
#' @param labels 
#' @param modularite_df 
#'
#' @return dataframe : contenant les labels des différents ordres d'importance
#' @export
#'
#' @examples
calcul_label <- function(graphe, labels, modularite_df) {
  
  # Boucle Principale
  for(label in labels){
    
    print(paste("---- ", label))
    
    ## Expliciter les modularité
    mod_str_list <- substring(label, first=8, last=nchar(label))
    mod_str_list <- str_split_1(mod_str_list, "_")
    for(i in 1:length(mod_str_list)){
      mod_str_list[i] <- paste("mod_", mod_str_list[i], sep="")
    }
    
    ## Boucle sur les lignes
    for(i_ligne in 1:nrow(modularite_df)){
      
      print(paste("-- ", as.character(i_ligne)))
      
      ### Récupération du max
      ligne <- modularite_df[i_ligne, c("mod_R", "mod_M", "mod_L")]
      max_ligne <- max(ligne)
      
      ### Vérification suivant l'ordre de force
      for(mod_str in mod_str_list){
        
        #### Récupération de la valeur du mod
        val_mod <- modularite_df[i_ligne, mod_str]
        
        #### Vérification
        if(val_mod == max_ligne){
          mod_label <- substring(mod_str, first=5, last=nchar(mod_str))
          modularite_df[i_ligne, label] <- mod_label
          break
        }
      }
    }
  }
  
  return(modularite_df)
}



# --- calculer les labels
#' enreg_modularite_df
#'
#' @param modularite_df 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
enreg_modularite_df <- function(modularite_df, name) {
  
  # Enregistrer en CSV
  ### Nom
  str <- "matrices/"
  str <- paste(str, name, "-MODULARITES.csv", sep="")
  write.csv(modularite_df, str, row.names=FALSE)
}




# --------------------------




###############
# Exécution
###############



# --- general
modularites <- c("mod_R", "mod_M", "mod_L")
labels <- c("labels_R_M_L", "labels_R_L_M", 
            "labels_M_R_L", "labels_M_L_R", 
            "labels_L_R_M", "labels_L_M_R")
columns <- union(modularites, labels)




# --- dolphins
print("--------------------------")
print("----------------- dolphins")
print("--------------------------")

# --- --- chargement
graphe <- read.graph("graphes/generated/dolphins.gml" , format="gml")

# --- --- création du dataframe
print("-------- Création modularite_df")
modularite_df <- data.frame(matrix(ncol = length(columns), nrow = vcount(graphe)))
colnames(modularite_df) <- columns
head(modularite_df, 3)
print("-------- ")

# --- --- calcul des modularités
print("-------- Création modularite_df")
modularite_df <- calcul_modularite(graphe, modularites, modularite_df)
print("-------- ")

# --- --- calcul du label
print("-------- Calcul des labels")
modularite_df <- calcul_label(graphe, labels, modularite_df)
print("-------- ")

# --- --- Enregistrement des résultats
print("-------- Enregistrement des résultats")
enreg_modularite_df(modularite_df, "dolphins")
print("-------- ")

head(modularite_df, 3)




# --- karate
print("--------------------------")
print("------------------- karate")
print("--------------------------")

# --- --- chargement
graphe <- read.graph("graphes/generated/karate.gml" , format="gml")

# --- --- création du dataframe
print("-------- Création modularite_df")
modularite_df <- data.frame(matrix(ncol = length(columns), nrow = vcount(graphe)))
colnames(modularite_df) <- columns
head(modularite_df, 3)
print("-------- ")

# --- --- calcul des modularités
print("-------- Création modularite_df")
modularite_df <- calcul_modularite(graphe, modularites, modularite_df)
print("-------- ")

# --- --- calcul du label
print("-------- Calcul des labels")
modularite_df <- calcul_label(graphe, labels, modularite_df)
print("-------- ")

# --- --- Enregistrement des résultats
print("-------- Enregistrement des résultats")
enreg_modularite_df(modularite_df, "karate")
print("-------- ")

head(modularite_df, 3)




# --- football
print("--------------------------")
print("----------------- football")
print("--------------------------")

# --- --- chargement
graphe <- read.graph("graphes/generated/football.gml" , format="gml")

# --- --- création du dataframe
print("-------- Création modularite_df")
modularite_df <- data.frame(matrix(ncol = length(columns), nrow = vcount(graphe)))
colnames(modularite_df) <- columns
head(modularite_df, 3)
print("-------- ")

# --- --- calcul des modularités
print("-------- Création modularite_df")
modularite_df <- calcul_modularite(graphe, modularites, modularite_df)
print("-------- ")

# --- --- calcul du label
print("-------- Calcul des labels")
modularite_df <- calcul_label(graphe, labels, modularite_df)
print("-------- ")

# --- --- Enregistrement des résultats
print("-------- Enregistrement des résultats")
enreg_modularite_df(modularite_df, "football")
print("-------- ")

head(modularite_df, 3)




# --- polbooks
print("--------------------------")
print("----------------- polbooks")
print("--------------------------")

# --- --- chargement
graphe <- read.graph("graphes/generated/polbooks.gml" , format="gml")

# --- --- création du dataframe
print("-------- Création modularite_df")
modularite_df <- data.frame(matrix(ncol = length(columns), nrow = vcount(graphe)))
colnames(modularite_df) <- columns
head(modularite_df, 3)
print("-------- ")

# --- --- calcul des modularités
print("-------- Création modularite_df")
modularite_df <- calcul_modularite(graphe, modularites, modularite_df)
print("-------- ")

# --- --- calcul du label
print("-------- Calcul des labels")
modularite_df <- calcul_label(graphe, labels, modularite_df)
print("-------- ")

# --- --- Enregistrement des résultats
print("-------- Enregistrement des résultats")
enreg_modularite_df(modularite_df, "polbooks")
print("-------- ")

head(modularite_df, 3)





# --------------------------