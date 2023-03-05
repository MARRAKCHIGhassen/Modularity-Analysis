
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
#### 0-Graphes
#### Récupération des graphes et de leurs caractéristiques principales
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



# --- Extraction de la composante connexe maximale
#' comp_con_max
#'
#' @param graphe 
#'
#' @return composante connexe maximale
#' @export
#'
#' @examples
comp_con_max <- function(graphe){
  graph_clusters <- clusters(graphe)
  clust_nodes <- which(graph_clusters$membership == which.max(graph_clusters$csize))
  return(induced.subgraph(graphe, clust_nodes))
}



# --- Affichage des caractéristiques globales d'un graphe
#' carac_glob
#'
#' @param graphe 
#' @param name 
#'
#' @return 
#' @export
#'
#' @examples
carac_glob <- function(graphe, name){
  
  print("------------------------")
  
  ## Nom du graphe
  str <- "--------------- "
  str <- paste(str, name)
  print(str)
  
  print("------------------------")
  
  ## Nombre de noeuds
  str <- "-- Nombre de noeuds = "
  str <- paste(str, as.character(vcount(graphe)))
  print(str)
  
  ## Nombre arête
  str <- "-- Nombre arêtes    = "
  str <- paste(str, as.character(ecount(graphe)))
  print(str)
  
  ## Densité
  str <- "-- Densité          = "
  str <- paste(str, as.character(graph.density(graphe)))
  print(str)
  
  ## Diamètre
  str <- "-- Diamètre         = "
  str <- paste(str, as.character(diameter(graphe)))
  print(str)
  
  ## Connexité
  str <- "-- Connexité        = "
  if(is.connected(graphe)){
    str <- paste(str, "Connexe")
  }
  else{
    str <- paste(str, "Non Connexe")
  }
  
  
  print("------------------------")
}




# --- Plot d'un graphe
#' plot_graph
#'
#' @param graphe 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
plot_graph <- function(graphe, name){
  
  ## Nom
  str <- "plots/"
  str <- paste(str, name, "-GRAPH", sep="")
  
  
  ## JPG
  ### Ouvrir le fichier
  jpeg(file=paste(str, ".jpg", sep=""), width=720, height=720)
  ### Plot
  plot(graphe)
  ### Fermer le fichier
  dev.off() 
  
  
  ## PNG
  ### Ouvrir le fichier
  png(file=paste(str, ".png", sep=""), width=720, height=720)
  ### Plot
  plot(graphe)
  ### Fermer le fichier
  dev.off() 
  
  
  ## PDF
  ### Ouvrir le fichier
  pdf(file=paste(str, ".pdf", sep=""))
  ### Plot
  plot(graphe)
  ### Fermer le fichier
  dev.off() 
}




# --- plot la distribution ddes degrés d'un graphe
#' plot_degree_hist
#'
#' @param graphe 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
plot_degree_hist <- function(graphe, name){
  
  ## Nom
  str <- "plots/"
  str <- paste(str, name, "-DEGREE_HIST", sep="")
  
  
  ## JPG
  ### Ouvrir le fichier
  jpeg(file=paste(str, ".jpg", sep=""), width=720, height=720)
  ### Plot
  hist(degree(graphe))
  ### Fermer le fichier
  dev.off() 
  
  
  ## PNG
  ### Ouvrir le fichier
  png(file=paste(str, ".png", sep=""), width=720, height=720)
  ### Plot
  hist(degree(graphe))
  ### Fermer le fichier
  dev.off() 
  
  
  ## PDF
  ### Ouvrir le fichier
  pdf(file=paste(str, ".pdf", sep=""))
  ### Plot
  hist(degree(graphe))
  ### Fermer le fichier
  dev.off() 
}




# --------------------------
###############
# Exécution
###############



# --- dolphins
graphe <- read.graph("graphes/bruts/dolphins.gml" , format="gml")
carac_glob(graphe, "dolphins")
plot_graph(graphe, "dolphins")
plot_degree_hist(graphe, "dolphins")
if(V(graphe)$id[1] == 0){
  noeuveau_ids <- c()
  
  for(id in 1:vcount(graphe)){
    noeuveau_ids[id] <- id
  }
  
  V(graphe)$id <- noeuveau_ids
}
write_graph(graphe, "graphes/generated/dolphins.gml", "gml")



# --- karate
graphe   <- read.graph("graphes/bruts/karate.gml"   , format="gml")
carac_glob(graphe, "karate")
plot_graph(graphe, "karate")
plot_degree_hist(graphe, "karate")
if(V(graphe)$id[1] == 0){
  noeuveau_ids <- c()
  
  for(id in 1:vcount(graphe)){
    noeuveau_ids[id] <- id
  }
  
  V(graphe)$id <- noeuveau_ids
}
write_graph(graphe, "graphes/generated/karate.gml", "gml")



# --- football
graphe <- read.graph("graphes/bruts/football.gml" , format="gml")
carac_glob(graphe, "football")
plot_graph(graphe, "football")
plot_degree_hist(graphe, "football")
if(V(graphe)$id[1] == 0){
  noeuveau_ids <- c()
  
  for(id in 1:vcount(graphe)){
    noeuveau_ids[id] <- id
  }
  
  V(graphe)$id <- noeuveau_ids
}
write_graph(graphe, "graphes/generated/football.gml", "gml")



# --- polbooks
graphe <- read.graph("graphes/bruts/polbooks.gml" , format="gml")
carac_glob(graphe, "polbooks")
plot_graph(graphe, "polbooks")
plot_degree_hist(graphe, "polbooks")
if(V(graphe)$id[1] == 0){
  noeuveau_ids <- c()
  
  for(id in 1:vcount(graphe)){
    noeuveau_ids[id] <- id
  }
  
  V(graphe)$id <- noeuveau_ids
}
write_graph(graphe, "graphes/generated/polbooks.gml", "gml")





# --------------------------