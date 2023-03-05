
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
#### 1-Matrices_des_centralités
#### Calcul des centralités de chaque graphe
#######################################






# --------------------------




###############
# Configuration
###############
library(igraph)
set.seed(123)
setwd("P:/Projets/4-CS-Modularity-Analysis")




# --------------------------




###############
# Implémentation des fonctions
###############



# --- Vérification d'une matrice générée par rapport au graphe
#' verif_mat_graphe
#'
#' @param graphe 
#' @param graphe_name 
#' @param mat 
#' @param mat_name 
#'
#' @return
#' @export
#'
#' @examples
verif_mat_graphe <- function(graphe, graphe_name, mat, mat_name){
  
  ## matrice en question
  str <- "-------------------------"
  str <- paste(str, mat_name)
  print(str)
  
  
  ## graphe en question
  str <- "----------"
  str <- paste(str, graphe_name)
  print(str)
  
  
  ## nombre de noeuds du graphe en question
  nbre_noeuds = vcount(graphe)
  str <- "-- Nombre de noeud du graphe = "
  str <- paste(str, as.character(nbre_noeuds))
  print(str)
  
  
  ## nombre de lignes de la matrice en question
  nbre_lignes = nrow(mat)
  str <- "-- Nombre de lignes de la matrice = "
  str <- paste(str, as.character(nbre_lignes))
  print(str)
  
  
  ## Verification de la correspondance lignes-noeuds
  str <- "-- Correspondance = "
  str <- paste(str, as.character(nbre_lignes == nbre_noeuds))
  print(str)
}



# --- Matrice des IDs
#' IDs_mat
#'
#' @param graphe 
#' @param name 
#'
#' @return matrice contenant les IDs des noeuds d'un graphe
#' @export
#'
#' @examples
IDs_mat <- function(graphe, name){
  
  ## IDs
  IDs <- V(graphe)$id
  
  
  ## Création de la matrice résultante
  IDs_df <- data.frame(name, IDs)
  
  
  ## Enregistrer en CSV
  ### Nom
  str <- "matrices/"
  str <- paste(str, name, "-IDs.csv", sep="")
  write.csv(IDs_df, str, row.names=FALSE)
  
  
  ## Retour de résultat
  return(as.matrix(IDs_df))
}



# --- Matrices de Katz
#' katz_mat
#'
#' @param graphe 
#' @param name 
#'
#' @return matrice contenant les mesures katz des noeuds d'un graphe
#' @export
#'
#' @examples
katz_mat <- function(graphe, name){
  
  ## Calcul des centralités
  ### Katz
  #katz <- alpha_centrality(graphe, loops=FALSE)
  
  
  ## Création de la matrice résultante
  katz_df <- data.frame(katz)
  
  
  ## Enregistrer en CSV
  ### Nom
  str <- "matrices/"
  str <- paste(str, name, "-KATZ.csv", sep="")
  write.csv(katz_df, str, row.names=FALSE)
  
  
  ## Retour de résultat
  return(as.matrix(katz_df))
}



# --- Matrices de alpha
#' alpha_mat
#'
#' @param graphe 
#' @param name 
#'
#' @return matrice contenant les mesures alpha des noeuds d'un graphe
#' @export
#'
#' @examples
alpha_mat <- function(graphe, name){
  
  ## Calcul des centralités
  ### Alpha
  alpha <- alpha_centrality(graphe, loops=FALSE)

  
  ## Création de la matrice résultante
  alpha_centrality_df <- data.frame(alpha)
  
  
  ## Enregistrer en CSV
  ### Nom
  str <- "matrices/"
  str <- paste(str, name, "-ALPHA.csv", sep="")
  write.csv(alpha_centrality_df, str, row.names=FALSE)
  
  
  ## Retour de résultat
  return(as.matrix(alpha_centrality_df))
}



# --- Matrice des centralités réstantes
#' restantes_mat
#'
#' @param graphe 
#' @param name 
#'
#' @return matrice contenant les mesures de centralités restantes des noeuds d'un graphe
#' @export
#'
#' @examples
restantes_mat <- function(graphe, name){
  
  ## Calcul des centralités
  ### Degré
  degree <- degree(graphe, loops=FALSE)
  
  ### Transitivity
  transitivity <- transitivity(graphe, type="local")
  
  ### Intermédiarité
  betweenness <- betweenness(graphe, directed=FALSE)
  
  ### Proximité
  closeness <- closeness(graphe)
  
  ### Centralité
  coreness <- coreness(graphe)
  
  ### Vecteur propre
  eigenvector <- eigen_centrality(graphe, directed=FALSE)$vector
  
  ### Page Rank
  pagerank <- page.rank(graphe, directed=FALSE)$vector
  
  ### Harmonique
  harmonic <- harmonic_centrality(graphe)
  
  ### Contrainte
  constraint <- constraint(graphe)
  
  
  ### Autorité
  authority <- authority_score(graphe)$vector
  
  ### Hub
  hub <- hub_score(graphe)$vector
  
  
  ## Création de la matrice résultante
  centrality_df <- data.frame(degree, 
                              transitivity, 
                              betweenness, 
                              closeness, 
                              coreness, 
                              eigenvector, 
                              pagerank, 
                              harmonic, 
                              constraint,
                              authority,
                              hub)
  
  
  ## Enregistrer en CSV
  ### Nom
  str <- "matrices/"
  str <- paste(str, name, "-RESTANTES.csv", sep="")
  write.csv(centrality_df, str, row.names=FALSE)
  
  
  ## Retour de résultat
  return(as.matrix(centrality_df))
}




# --------------------------




###############
# Exécution
###############



# --- dolphins
# --- --- chargement
graphe <- read.graph("graphes/generated/dolphins.gml" , format="gml")
# --- --- Matrice des IDs
mat <- IDs_mat(graphe, "dolphins")
verif_mat_graphe(graphe, "dolphins", mat, "IDs")
# --- --- Matrice des KATZ
#mat <- katz_mat(g, "dolphins")
#verif_mat_graphe(g, "dolphins", mat, "KATZ")
# --- --- Matrice des ALPHA
mat <- alpha_mat(graphe, "dolphins")
verif_mat_graphe(graphe, "dolphins", mat, "ALPHA")
# --- --- Matrice des RESTANTES
mat <- restantes_mat(graphe, "dolphins")
verif_mat_graphe(graphe, "dolphins", mat, "RESTANTES")




# --- karate
# --- --- chargement
graphe <- read.graph("graphes/generated/karate.gml" , format="gml")
# --- --- Matrice des IDs
mat <- IDs_mat(graphe, "karate")
verif_mat_graphe(graphe, "karate", mat, "IDs")
# --- --- Matrice des KATZ
#mat <- katz_mat(graphe, "karate")
#verif_mat_graphe(graphe, "karate", mat, "KATZ")
# --- --- Matrice des ALPHA
mat <- alpha_mat(graphe, "karate")
verif_mat_graphe(graphe, "karate", mat, "ALPHA")
# --- --- Matrice des RESTANTES
mat <- restantes_mat(graphe, "karate")
verif_mat_graphe(graphe, "karate", mat, "RESTANTES")




# --- football
# --- --- chargement
graphe <- read.graph("graphes/generated/football.gml" , format="gml")
# --- --- Matrice des IDs
mat <- IDs_mat(graphe, "football")
verif_mat_graphe(graphe, "football", mat, "IDs")
# --- --- Matrice des KATZ
#mat <- katz_mat(graphe, "football")
#verif_mat_graphe(graphe, "football", mat, "KATZ")
# --- --- Matrice des ALPHA
mat <- alpha_mat(graphe, "football")
verif_mat_graphe(graphe, "football", mat, "ALPHA")
# --- --- Matrice des RESTANTES
mat <- restantes_mat(graphe, "football")
verif_mat_graphe(graphe, "football", mat, "RESTANTES")




# --- polbooks
# --- --- chargement
graphe <- read.graph("graphes/generated/polbooks.gml" , format="gml")
# --- --- Matrice des IDs
mat <- IDs_mat(graphe, "polbooks")
verif_mat_graphe(graphe, "polbooks", mat, "IDs")
# --- --- Matrice des KATZ
#mat <- katz_mat(graphe, "polbooks")
#verif_mat_graphe(graphe, "polbooks", mat, "KATZ")
# --- --- Matrice des ALPHA
mat <- alpha_mat(graphe, "polbooks")
verif_mat_graphe(graphe, "polbooks", mat, "ALPHA")
# --- --- Matrice des RESTANTES
mat <- restantes_mat(graphe, "polbooks")
verif_mat_graphe(graphe, "polbooks", mat, "RESTANTES")





# --------------------------