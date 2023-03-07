
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
#### 4-Data_Viz
#### Visualisation des données
#######################################






# --------------------------




###############
# Configuration
###############
library(ggplot2)
library(corrplot)
set.seed(123)
setwd("P:/Projets/4-CS-Modularity-Analysis")




# --------------------------




###############
# Implémentation des fonctions
###############




# --------------------------




###############
# Exécution
###############




# --- Chargement du dataset
datasets <- c("labels_R_M_L", "labels_R_L_M", 
              "labels_M_R_L", "labels_M_L_R", 
              "labels_L_R_M", "labels_L_M_R")



# --- --- DISTRIBUTION DES MODULARITES
plot_df <- data.frame(matrix(ncol = 4, nrow = 0))
total_df <- data.frame(matrix(ncol = 14, nrow = 0))

for(actual_dataset in datasets){
  
  print(actual_dataset)
  
  # --- --- --- Training
  str <- "datasets/finaux/"
  str <- paste(str, actual_dataset, "-TRAIN.csv", sep="")
  training_df <- read.csv(str)
  
  # --- --- --- Test
  str <- "datasets/finaux/"
  str <- paste(str, actual_dataset, "-TEST.csv", sep="")
  test_df <- read.csv(str)
  
  # --- --- --- join
  df <- rbind(training_df, test_df)
  
  # --- --- --- plot df
  modul_hist_inter  <- c("R", "M", "L")
  labels_hist_inter <- as.integer(df$label)
  count_modul_hist_inter <- c(length(labels_hist_inter[labels_hist_inter == 1]), 
                              length(labels_hist_inter[labels_hist_inter == 2]), 
                              length(labels_hist_inter[labels_hist_inter == 3]))
  dataset_hist_inter  <- c(actual_dataset, actual_dataset, actual_dataset)
  hist_inter <- data.frame(modul_hist_inter, count_modul_hist_inter, dataset_hist_inter)
  plot_df <- rbind(plot_df, hist_inter)
  
  # --- --- --- TOTAL DF
  dataset_hist_inter  <- c(actual_dataset)
  total_inter <- data.frame(df, dataset_hist_inter)
  total_df <- rbind(total_df, total_inter)
}

# --- --- --- Enregistrement plot df
str <- "plots/DIST-MOD"
# JPG
jpeg(file=paste(str, ".jpg", sep=""), width=1080, height=720)
ggplot(plot_df, aes(dataset_hist_inter, count_modul_hist_inter, fill = modul_hist_inter)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Distribution des modularités suivant le dataset")
dev.off() 
# PNG
png(file=paste(str, ".png", sep=""), width=1080, height=720)
ggplot(plot_df, aes(dataset_hist_inter, count_modul_hist_inter, fill = modul_hist_inter)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Distribution des modularités suivant le dataset")
dev.off() 
# PDF
pdf(file=paste(str, ".pdf", sep=""))
ggplot(plot_df, aes(dataset_hist_inter, count_modul_hist_inter, fill = modul_hist_inter)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Distribution des modularités suivant le dataset")
dev.off() 

# --- --- --- Enregistrement TOTAL df
data <- df[, -1] # Numerical variables
groups <- df[, "label"] # Factor variable (groups)
str <- "plots/COVARIABILITE"
# JPG
jpeg(file=paste(str, ".jpg", sep=""), width=1080, height=720)
pairs(data,                   
      labels = colnames(df),  
      pch = 21,                
      bg = rainbow(3)[groups], 
      col = rainbow(3)[groups], 
      main = "Visualisation de tous les datasets", 
      row1attop = TRUE,        
      gap = 1,                  
      cex.labels = NULL,        
      font.labels = 1) 
dev.off() 
# PNG
png(file=paste(str, ".png", sep=""), width=1080, height=720)
pairs(data,                    
      labels = colnames(df), 
      pch = 21,               
      bg = rainbow(3)[groups],  
      col = rainbow(3)[groups],
      main = "Visualisation de tous les datasets",   
      row1attop = TRUE,         
      gap = 1,                 
      cex.labels = NULL,        
      font.labels = 1) 
dev.off() 
# PDF
pdf(file=paste(str, ".pdf", sep=""))
pairs(data,                 
      labels = colnames(df),  
      pch = 21,             
      bg = rainbow(3)[groups],  
      col = rainbow(3)[groups],
      main = "Visualisation de tous les datasets",   
      row1attop = TRUE,        
      gap = 1,           
      cex.labels = NULL,
      font.labels = 1) 
dev.off()



# --- --- --- CORRELATIONS
str <- "plots/CORRELATION"
# JPG
jpeg(file=paste(str, ".jpg", sep=""), width=1080, height=720)
corrplot(cor(df),
         method = "shade",
         type = "full",
         diag = TRUE, 
         tl.col = "black", 
         bg = "white",   
         title = "",  
         col = NULL) 
dev.off() 
# PNG
png(file=paste(str, ".png", sep=""), width=1080, height=720)
corrplot(cor(df),
         method = "shade",
         type = "full",
         diag = TRUE, 
         tl.col = "black", 
         bg = "white",   
         title = "",  
         col = NULL) 
dev.off() 
# PDF
pdf(file=paste(str, ".pdf", sep=""))
corrplot(cor(df),
         method = "shade",
         type = "full",
         diag = TRUE, 
         tl.col = "black", 
         bg = "white",   
         title = "",  
         col = NULL) 
dev.off() 




# --------------------------