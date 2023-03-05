
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
#### 3-Pre_processing
#### Pré-traitement des données
#######################################






# --------------------------




###############
# Configuration
###############
library(igraph)
library(caTools)
set.seed(123)
setwd("P:/Projets/4-CS-Modularity-Analysis")




# --------------------------




###############
# Implémentation des fonctions
###############



# --- calcul de la modularité R
#' joindre_matrices
#'
#' @param name 
#'
#' @return matrix : concatenation de toutes les matrices
#' @export
#'
#' @examples
joindre_matrices <- function(name) {
  
  # matrice ALPHA
  str <- "matrices/"
  str <- paste(str, name, "-ALPHA.csv", sep="")
  df_ALPHA <- read.csv(str)
  
  # matrice ALPHA
  str <- "matrices/"
  str <- paste(str, name, "-RESTANTES.csv", sep="")
  df_RESTANTES <- read.csv(str)
  
  # matrice ALPHA
  str <- "matrices/"
  str <- paste(str, name, "-MODULARITES.csv", sep="")
  df_MODULARITES <- read.csv(str)

  # Joindre
  df_brut = cbind(df_ALPHA, df_RESTANTES, df_MODULARITES)
  
  return(df_brut)
}



# --- Filtration des colonnes
#' filtrations_colonnes
#'
#' @param df 
#'
#' @return dataframe : contenant que les colonnes nécessaire à la version finale
#' @export
#'
#' @examples
filtrations_colonnes <- function(df) {
  
  # Supression des modularité
  df <- subset(df, select=-c(mod_R, mod_L, mod_M))
  
  return(df)
}



# --- Régularisation des types du dataframe
#' fixation_des_types
#'
#' @param df 
#'
#' @return dataframe : avec les types correctes
#' @export
#'
#' @examples
fixation_des_types <- function(df) {
  
  # Features
  df$alpha        <- as.double (as.character(df$alpha)       )
  df$degree       <- as.integer(as.character(df$degree)      )
  df$transitivity <- as.double (as.character(df$transitivity))
  df$betweenness  <- as.double (as.character(df$betweenness) )
  df$closeness    <- as.double (as.character(df$closeness)   )
  df$coreness     <- as.integer(as.character(df$coreness)    )
  df$eigenvector  <- as.double (as.character(df$eigenvector) )
  df$pagerank     <- as.double (as.character(df$pagerank)    )
  df$harmonic     <- as.double (as.character(df$harmonic)    )
  df$constraint   <- as.double (as.character(df$constraint)  )
  df$authority    <- as.double (as.character(df$authority)   )
  df$hub          <- as.double (as.character(df$hub)         )
  
  return(df)
}



# --- Traitement des valeurs manquantes
#' val_manquantes
#'
#' @param df 
#'
#' @return dataframe : sans valeurs manquantes
#' @export
#'
#' @examples
val_manquantes <- function(df) {
  
  # Transitivity
  df$transitivity <- ifelse(is.na(df$transitivity), 
                            ave(df$transitivity, 
                                FUN = function(x) mean(x, na.rm=TRUE)), 
                            df$transitivity)
  
  return(df)
}



# --- Encodage des label
#' encodage_label
#'
#' @param df 
#' @param labels 
#' @param mo 
#'
#' @return dataframe : avec les labels encodés
#' @export
#'
#' @examples
encodage_label <- function(df, labels) {
  
  # Boucle des labels
  for(label in labels){
    
    print(paste("- ", label))
    
    ## Encodage
    df[, label] <- factor(df[, label],
                          levels=c("R", "M", "L"), 
                          labels=c(1, 2, 3))
  }
  
  return(df)
}



# --- Création des différents jeux de données
#' split_diff_datasets
#'
#' @param df 
#' @param labels 
#'
#' @return
#' @export
#'
#' @examples
split_diff_datasets <- function(df, name, labels) {
  
  # Récupération des features
  features <- names(df)
  features <- features[!(features %in% labels)]
  
  # Boucle des labels
  for(label in labels){
    
    print(paste("- ", label))
    
    ## Récupération du dataframe des feautures
    df_inter <- subset(df, select=features)
    
    ## Ajout des labels en question
    df_inter$label <- as.integer(as.character(df[, label]))
    
    print(paste("- Nombre de lignes   = ", as.character(nrow(df_inter))))
    print(paste("- Nombre de colonnes = ", as.character(ncol(df_inter))))
    
    ## Enregistrer en CSV
    ### Nom
    str <- "datasets/bruts/"
    str <- paste(str, name, "-", label, ".CSV", sep="")
    ### Enregistrement
    write.csv(df_inter, str, row.names=FALSE)
  }
}



# --- Jointure suivant le label
#' jointure_dataset
#'
#' @param label 
#' @param graphes_nom 
#'
#' @return dataframe du label en question
#' @export
#'
#' @examples
jointure_dataset <- function(label, graphes_nom) {

  # Initialisation
  df <- NULL
  
  # Pour chaque graphe
  for(nom in graphes_nom){
    
    ## Lexture du CSV
    ### Nom
    str <- "datasets/bruts/"
    str <- paste(str, nom, "-", label, ".CSV", sep="")
    ### Lecture
    df_inter <- read.csv(str)
    
    ## Jointure
    if(is.null(df)){
      df <- df_inter
    } else {
      df <- rbind(df, df_inter)
    }
  }
  
  return(df)
}



# --- Normalise les données 
#' normalisation
#'
#' @param df 
#'
#' @return dataframe : normalisé
#' @export
#'
#' @examples
normalisation <- function(df) {
  
  # Récupération des features
  features <- names(df)
  features <- features[!(features %in% c("degree", "coreness", "label"))]
  print(features)
  # Normalisation
  df[, features] <- scale(df[, features])
  
  return(df)
}



# --- Enregistre le dataset 
#' enreg_dataset
#'
#' @param df 
#' @param PART 
#'
#' @return
#' @export
#'
#' @examples
enreg_dataset <- function(df, PART) {
  
  # Nom
  str <- "datasets/finaux/"
  str <- paste(str, label, "-", PART, ".CSV", sep="")
  
  # Enregistrement
  write.csv(df, str, row.names=FALSE)
}




# --------------------------




###############
# Exécution
###############



# --- general
labels <- c("labels_R_M_L", "labels_R_L_M", 
            "labels_M_R_L", "labels_M_L_R", 
            "labels_L_R_M", "labels_L_M_R")
graphes_nom <- c("dolphins", "karate", "polbooks", "football")




# --- dolphins
print("--------------------------")
print("----------------- dolphins")
print("--------------------------")

# --- --- Joindre les matrices : ALPHA-RESTANTES-MODULARITES
print("-------- Joindre les matrices : ALPHA-RESTANTES-MODULARITES")
df <- joindre_matrices("dolphins")
print("--- matrice_brut")
print(paste("--- Nombre de lignes = "  , nrow(df)))
print(paste("--- Nombre de colonnes = ", ncol(df)))
print("-------- ")

# --- --- Filtration des colonnes
print("-------- Filtration des colonnes")
df <- filtrations_colonnes(df)
print("-------- ")

# --- --- Fixation des types
print("-------- Fixation des types")
df <- fixation_des_types(df)
print("-------- ")

# --- --- Normalisation
# --- --- --- Valeurs manquantes
print("--- NORMALISATION : Valeurs manquantes")
df <- val_manquantes(df)
print("--- ")
# --- --- --- Encodage des label
print("--- NORMALISATION : Encodage des labels")
df <- encodage_label(df, labels)
print("--- ")

# --- --- Création des différents jeux de données
print("-------- Création des différents jeux de données")
split_diff_datasets(df, "dolphins", labels)
print("-------- ")




# --- karate
print("--------------------------")
print("------------------- karate")
print("--------------------------")

# --- --- Joindre les matrices : ALPHA-RESTANTES-MODULARITES
print("-------- Joindre les matrices : ALPHA-RESTANTES-MODULARITES")
df <- joindre_matrices("karate")
print("--- matrice_brut")
print(paste("--- Nombre de lignes = "  , nrow(df)))
print(paste("--- Nombre de colonnes = ", ncol(df)))
print("-------- ")

# --- --- Filtration des colonnes
print("-------- Filtration des colonnes")
df <- filtrations_colonnes(df)
print("-------- ")

# --- --- Fixation des types
print("-------- Fixation des types")
df <- fixation_des_types(df)
print("-------- ")

# --- --- Normalisation
# --- --- --- Valeurs manquantes
print("--- NORMALISATION : Valeurs manquantes")
df <- val_manquantes(df)
print("--- ")
# --- --- --- Encodage des label
print("--- NORMALISATION : Encodage des labels")
df <- encodage_label(df, labels)
print("--- ")

# --- --- Création des différents jeux de données
print("-------- Création des différents jeux de données")
split_diff_datasets(df, "karate", labels)
print("-------- ")




# --- polbooks
print("--------------------------")
print("----------------- polbooks")
print("--------------------------")

# --- --- Joindre les matrices : ALPHA-RESTANTES-MODULARITES
print("-------- Joindre les matrices : ALPHA-RESTANTES-MODULARITES")
df <- joindre_matrices("polbooks")
print("--- matrice_brut")
print(paste("--- Nombre de lignes = "  , nrow(df)))
print(paste("--- Nombre de colonnes = ", ncol(df)))
print("-------- ")

# --- --- Filtration des colonnes
print("-------- Filtration des colonnes")
df <- filtrations_colonnes(df)
print("-------- ")

# --- --- Fixation des types
print("-------- Fixation des types")
df <- fixation_des_types(df)
print("-------- ")

# --- --- Normalisation
# --- --- --- Valeurs manquantes
print("--- NORMALISATION : Valeurs manquantes")
df <- val_manquantes(df)
print("--- ")
# --- --- --- Encodage des label
print("--- NORMALISATION : Encodage des labels")
df <- encodage_label(df, labels)
print("--- ")

# --- --- Création des différents jeux de données
print("-------- Création des différents jeux de données")
split_diff_datasets(df, "polbooks", labels)
print("-------- ")




# --- football
print("--------------------------")
print("----------------- football")
print("--------------------------")

# --- --- Joindre les matrices : ALPHA-RESTANTES-MODULARITES
print("-------- Joindre les matrices : ALPHA-RESTANTES-MODULARITES")
df <- joindre_matrices("football")
print("--- matrice_brut")
print(paste("--- Nombre de lignes = "  , nrow(df)))
print(paste("--- Nombre de colonnes = ", ncol(df)))
print("-------- ")

# --- --- Filtration des colonnes
print("-------- Filtration des colonnes")
df <- filtrations_colonnes(df)
print("-------- ")

# --- --- Fixation des types
print("-------- Fixation des types")
df <- fixation_des_types(df)
print("-------- ")

# --- --- Normalisation
# --- --- --- Valeurs manquantes
print("--- NORMALISATION : Valeurs manquantes")
df <- val_manquantes(df)
print("--- ")
# --- --- --- Encodage des label
print("--- NORMALISATION : Encodage des labels")
df <- encodage_label(df, labels)
print("--- ")

# --- --- Création des différents jeux de données
print("-------- Création des différents jeux de données")
split_diff_datasets(df, "football", labels)
print("-------- ")




# --------------------------




# --- FINALISATION
print("--------------------------")
print("------------- FINALISATION")
print("--------------------------")

label <- "labels_M_L_R"




# --- --- Pour caque labels faire
for(label in labels){
  
  print(label)
  
  # --- --- --- Jointure suivant le label
  print("-------- Jointure suivant le label")
  df <- jointure_dataset(label, graphes_nom)
  print("-------- ")
  
  # --- --- --- Split des données en deux ensembles
  print("-------- Split des données en deux ensembles")
  # --- --- --- --- Mixer : Shuffle
  df <- df[sample(1:nrow(df)), ]
  # --- --- --- --- Train/Test
  split <- sample.split(df$label, SplitRatio=0.8)
  training_df <- subset(df, split==TRUE)
  test_df <- subset(df, split==FALSE)
  # --- --- --- --- ETAT DES LIEUX
  print(paste("- Dimensions totales = (", 
              as.character(nrow(df)), ",", as.character(ncol(df)), ")", 
              sep=""))
  print(paste("- Dimensions TRAINING = (", 
              as.character(nrow(training_df)), ",", as.character(ncol(training_df)), ")", 
              sep=""))
  print(paste("- Dimensions TEST = (", 
              as.character(nrow(test_df)), ",", as.character(ncol(test_df)), ")", 
              sep=""))
  print("-------- ")
  
  # --- --- --- Normalise les données
  print("-------- Normalise les données")
  print("--- TRAINING")
  training_df <- normalisation(training_df)
  print("--- TEST")
  test_df <- normalisation(test_df)
  print("-------- ")
  
  # --- --- --- Enregistre le dataset 
  print("-------- Enregistre le dataset ")
  print("--- TRAINING")
  enreg_dataset(training_df, "TRAIN")
  print("--- TEST")
  enreg_dataset(test_df, "TEST")
  print("-------- ")
}




# --------------------------