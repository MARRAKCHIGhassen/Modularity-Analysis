
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
#### 5-Model_Training
#### Entrainement des modèles
#######################################






# --------------------------




###############
# Configuration
###############
library(caTools)

library(nnet)
library(class)
library(rpart)

library(MLmetrics)
library(pROC)

set.seed(123)
setwd("P:/Projets/4-CS-Modularity-Analysis")




# --------------------------




###############
# Implémentation des fonctions
###############



# --- calcul le nombre de True Positive
#' true_positive_FCT
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
true_positive_FCT <- function(y_pred, y_true) {
  
  # Initialisation
  tp <- 0
  
  # Boucle
  for (i in 1:length(y_true)) {
    if (y_true[i] == 1 && y_pred[i] == 1) {
      tp <- tp + 1
    }
  }
  
  return(tp)
}



# --- calcul le nombre de True Negative
#' true_negative_FCT
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
true_negative_FCT <- function(y_pred, y_true) {
  
  # Initialisation
  tn <- 0
  
  # Boucle
  for (i in 1:length(y_true)) {
    if (y_true[i] == 0 && y_pred[i] == 0) {
      tn <- tn + 1
    }
  }
  
  return(tn)
}



# --- calcul le nombre de False positive
#' false_positive_FCT
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
false_positive_FCT <- function(y_pred, y_true) {
  
  # Initialisation
  fp <- 0
  
  # Boucle
  for (i in 1:length(y_true)) {
    if (y_true[i] == 0 && y_pred[i] == 1) {
      fp <- fp + 1
    }
  }
  
  return(fp)
}



# --- calcul le nombre de False Negative
#' false_negative_FCT
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
false_negative_FCT <- function(y_pred, y_true) {
  
  # Initialisation
  fn <- 0
  
  # Boucle
  for (i in 1:length(y_true)) {
    if (y_true[i] == 1 && y_pred[i] == 0) {
      fn <- fn + 1
    }
  }
  
  return(fn)
}



# --- calcul de la précision micro moyenne
#' Micro_Avg_Precision
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
Micro_Avg_Precision <- function(y_pred, y_true) {
  
  # Récupération de nombre de classe
  num_classes <- length(unique(y_true))
  
  # Initialisation
  tp <- 0
  fp <- 0
  
  # Pour chaque classe faire
  for (classe_ in unique(y_true)) {
    
    # Toutes les classes sont négatives 
    # SAUF LA PRESENTE
    temp_true <- ifelse(y_true == classe_, 1, 0)
    temp_pred <- ifelse(y_pred == classe_, 1, 0)
    
    # Calcul des true positives de la classe en cours
    tp <- tp + true_positive_FCT(temp_pred, temp_true)
    
    # Calcul des false positives de la classe en cours
    fp <- fp + false_positive_FCT(temp_pred, temp_true)
  }
  
  # calculer la précision pour toutes les classes
  precision_calcul <- tp / (tp + fp)
  
  return(precision_calcul)
}



# --- calcul de la précision macro moyenne
#' Macro_Avg_Precision
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
Macro_Avg_Precision <- function(y_pred, y_true) {
  
  # Récupération de nombre de classe
  num_classes <- length(unique(y_true))
  
  # Initialisation
  precision_calcul <- 0
  
  # Pour chaque classe faire
  for (classe_ in unique(y_true)) {
    
    # Toutes les classes sont négatives 
    # SAUF LA PRESENTE
    temp_true <- ifelse(y_true == classe_, 1, 0)
    temp_pred <- ifelse(y_pred == classe_, 1, 0)
    
    # Calcul des true positives de la classe en cours
    tp <- true_positive_FCT(temp_pred, temp_true)
    
    # Calcul des false positives de la classe en cours
    fp <- false_positive_FCT(temp_pred, temp_true)
    
    # Calcul de la précision de la classe en cours
    temp_precision <- tp / (tp + fp + 1e-6)
    
    # ajouter la précision à celle de toutes les classes 
    precision_calcul <- precision_calcul + temp_precision
  }
  
  # calculer la précision pour toutes les classes
  precision_calcul <- precision_calcul/num_classes
  
  return(precision_calcul)
}



# --- calcul du rappel micro moyenne
#' Micro_Avg_Recall
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
Micro_Avg_Recall <- function(y_pred, y_true) {
  
  # Récupération de nombre de classe
  num_classes <- length(unique(y_true))
  
  # Initialisation
  tp <- 0
  fn <- 0
  
  # Pour chaque classe faire
  for (classe_ in unique(y_true)) {
    
    # Toutes les classes sont négatives 
    # SAUF LA PRESENTE
    temp_true <- ifelse(y_true == classe_, 1, 0)
    temp_pred <- ifelse(y_pred == classe_, 1, 0)
    
    # Calcul des true positives de la classe en cours
    tp <- tp + true_positive_FCT(temp_pred, temp_true)
    
    # Calcul des false negatives de la classe en cours
    fn <- fn + false_negative_FCT(temp_pred, temp_true)
  }
  
  # calculer du rappel pour toutes les classes
  recall_calcul <- tp / (tp + fn)
  
  return(recall_calcul)
}



# --- calcul du rappel macro moyenne
#' Macro_Avg_Recall
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
Macro_Avg_Recall <- function(y_pred, y_true) {
  
  # Récupération de nombre de classe
  num_classes <- length(unique(y_true))
  
  # Initialisation
  recall_calcul <- 0
  
  # Pour chaque classe faire
  for (classe_ in unique(y_true)) {
    
    # Toutes les classes sont négatives 
    # SAUF LA PRESENTE
    temp_true <- ifelse(y_true == classe_, 1, 0)
    temp_pred <- ifelse(y_pred == classe_, 1, 0)
    
    # Calcul des true positives de la classe en cours
    tp <- true_positive_FCT(temp_pred, temp_true)
    
    # Calcul des false negatives de la classe en cours
    fn <- false_negative_FCT(temp_pred, temp_true)
    
    # Calcul du rappel de la classe en cours
    temp_recall <- tp / (tp + fn + 1e-6)
    
    # ajouter du rappel à celle de toutes les classes 
    recall_calcul <- recall_calcul + temp_recall
  }
  
  # calculer du rappel pour toutes les classes
  recall_calcul <- recall_calcul/num_classes
  
  return(recall_calcul)
}



# --- calcul du score F1 micro moyenne
#' Micro_Avg_F1_Score
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
Micro_Avg_F1_Score <- function(y_pred, y_true) {
  
  # micro-averaged precision score
  P = Micro_Avg_Precision(y_pred, y_true)
  
  # micro-averaged recall score
  R = Micro_Avg_Recall(y_pred, y_true)
  
  #micro averaged f1 score
  f1 = 2*P*R / (P + R)    
  
  return(f1)
}



# --- calcul du score F1 macro moyenne
#' Macro_Avg_F1_Score
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
Macro_Avg_F1_Score <- function(y_pred, y_true) {
  
  # Récupération de nombre de classe
  num_classes <- length(unique(y_true))
  
  # Initialisation
  f1_calcul <- 0
  
  # Pour chaque classe faire
  for (classe_ in unique(y_true)) {
    
    # Toutes les classes sont négatives 
    # SAUF LA PRESENTE
    temp_true <- ifelse(y_true == classe_, 1, 0)
    temp_pred <- ifelse(y_pred == classe_, 1, 0)
    
    # Calcul des true positives de la classe en cours
    tp <- true_positive_FCT(temp_pred, temp_true)
    
    # Calcul des false negatives de la classe en cours
    fn <- false_negative_FCT(temp_pred, temp_true)
    
    # Calcul des false positives de la classe en cours
    fp <- false_positive_FCT(temp_pred, temp_true)
    
    # Calcul du rappel de la classe en cours
    temp_recall <- tp / (tp + fn + 1e-6)
    
    # Calcul de la précision de la classe en cours
    temp_precision <- tp / (tp + fp + 1e-6)
    
    # Calcul du F1 de la classe en cours
    temp_f1 <- 2 * temp_precision * temp_recall
    temp_f1 <- temp_f1/(temp_precision + temp_recall + 1e-6)
    
    # ajouter du F1 à celle de toutes les classes 
    f1_calcul <- f1_calcul + temp_f1
  }
  
  # calculer du F1 pour toutes les classes
  f1_calcul <- f1_calcul/num_classes
  
  return(f1_calcul)
}



# --- calcul ROC et AUC Multiclass
#' roc_auc_score_multiclass
#'
#' @param y_pred 
#' @param y_true 
#'
#' @return
#' @export
#'
#' @examples
roc_auc_score_multiclass <- function(actual_classe, pred_classe) {
  
  # Creation d'un ensemble des classes uniques
  # en utilisant la classe actuelle
  unique_classes <- unique(actual_classe)
  roc_auc_dict <- list()
  
  # Pour chaque classe faire
  for (per_classe in unique_classes) {
    
    # Création d'une liste des classes 
    # SAUF POUR LA PRESENTE CLASSE 
    other_classes <- unique_classes[unique_classes != per_classe]
    
    # Marquer LA PRESENTE CLASSE COMME 1
    # et toutes les autres classes comme 0
    new_actual_classe <- ifelse(actual_classe %in% other_classes, 0, 1)
    new_pred_classe <- ifelse(pred_classe %in% other_classes, 0, 1)
    
    # En utilisant pROC 
    # on calcule le roc_auc_score
    roc_auc <- pROC::auc(roc(new_actual_classe, new_pred_classe, levels=c(0, 1), direction = "<"))
    roc_auc_dict[[per_classe]] <- roc_auc
  }
  
  return(roc_auc_dict)
}



# --- calcul des métriques
#' eval_metriques
#'
#' @param y_pred 
#' @param y_true 
#' @param algo_name 
#' @param metriques_df 
#'
#' @return dataframe des métriques
#' @export
#'
#' @examples
eval_metriques <- function(y_pred, y_true, algo_name, metriques_df) {
  
  # Accuracy
  accuracy_calcul <- Accuracy(y_pred, y_true)
  
  # Precision
  precision_calcul <- Precision(y_pred, y_true)
  micro_avg_precision_calcul <- Micro_Avg_Precision(y_pred, y_true)
  macro_avg_precision_calcul <- Macro_Avg_Precision(y_pred, y_true)
  
  # Recall
  recall_calcul <- Recall(y_pred, y_true)
  micro_avg_recall_calcul <- Micro_Avg_Recall(y_pred, y_true)
  macro_avg_recall_calcul <- Macro_Avg_Recall(y_pred, y_true)
  
  # F1
  f1_calcul <- F1_Score(y_pred, y_true)
  micro_avg_f1_calcul <- Micro_Avg_F1_Score(y_pred, y_true)
  macro_avg_f1_calcul <- Macro_Avg_F1_Score(y_pred, y_true)
  
  # Ajout des calculs
  metriques_df[algo_name, ] <- c(accuracy_calcul, 
                                 precision_calcul, micro_avg_precision_calcul, 
                                 macro_avg_precision_calcul, 
                                 recall_calcul, micro_avg_recall_calcul,
                                 macro_avg_recall_calcul,
                                 f1_calcul, micro_avg_f1_calcul, 
                                 macro_avg_f1_calcul)
  
  return(metriques_df)
}



# --- calcul des scores ROC/AUC
#' eval_roc_auc
#'
#' @param y_pred 
#' @param y_true 
#' @param algo_name 
#' @param roc_auc_df 
#'
#' @return dataframe des scores ROC/AUC
#' @export
#'
#' @examples
eval_roc_auc <- function(y_pred, y_true, algo_name, roc_auc_df) {
  
  # AUC
  roc_auc_score_calcul <- roc_auc_score_multiclass(y_pred, y_true)

  # Vérification de valeur manquante
  if(!("1" %in% names(roc_auc_score_calcul))){roc_auc_score_calcul$"1" <- 0.0}
  if(!("2" %in% names(roc_auc_score_calcul))){roc_auc_score_calcul$"2" <- 0.0}
  if(!("3" %in% names(roc_auc_score_calcul))){roc_auc_score_calcul$"3" <- 0.0}
  
  # Ajout des calculs
  roc_auc_df[algo_name, ] <- c(roc_auc_score_calcul$"1", 
                               roc_auc_score_calcul$"2", 
                               roc_auc_score_calcul$"3")
  
  return(roc_auc_df)
}



# --- calcul la matrice de confusion
#' eval_conf_matrix
#'
#' @param y_pred 
#' @param y_true 
#' @param algo_name 
#'
#' @return dataframe de la matrice de confusion
#' @export
#'
#' @examples
eval_conf_matrix <- function(y_pred, y_true, algo_name) {
  
  # Matrice de confusion
  conf_matrix_df <- ConfusionMatrix(y_pred, y_true)
  
  # Enregistrement
  str <- "results/"
  str <- paste(str, actual_dataset, algo_name, "-EVAL_CONF_MAT.csv", sep="")
  write.csv(conf_matrix_df, str, row.names=TRUE)
  
  return(conf_matrix_df)
}




# --------------------------




###############
# Exécution
###############




# --- Chargement du dataset
datasets <- c("labels_R_M_L", "labels_R_L_M", 
              "labels_M_R_L", "labels_M_L_R", 
              "labels_L_R_M", "labels_L_M_R")




for(actual_dataset in datasets){
  
  print(actual_dataset)
  
  # --- --- Training
  str <- "datasets/finaux/"
  str <- paste(str, actual_dataset, "-TRAIN.csv", sep="")
  training_df <- read.csv(str)
  
  # --- --- Test
  str <- "datasets/finaux/"
  str <- paste(str, actual_dataset, "-TEST.csv", sep="")
  test_df <- read.csv(str)
  
  # --- --- features
  features <- names(training_df)
  features <- features[!(features %in% c("label"))]
  
  
  
  
  # --- Dataframe des Métriques
  colonnes_metriques_df <- c("Accuracy", 
                             "Precision", "Micro Avg Precision", "Macro Avg Precision", 
                             "Recall", "Micro Avg Recall", "Macro Avg Recall", 
                             "F1 Score", "Micro Avg F1 Score", "Macro Avg F1 Score")
  metriques_df <- data.frame(matrix(ncol=length(colonnes_metriques_df), nrow=0))
  colnames(metriques_df) <- colonnes_metriques_df
  metriques_df
  
  
  
  
  # --- Dataframe des courbe ROC et AUC
  colonnes_roc_auc_df <- c("1", "2", "3")
  roc_auc_df <- data.frame(matrix(ncol=length(colonnes_roc_auc_df), nrow=0))
  colnames(roc_auc_df) <- colonnes_roc_auc_df
  roc_auc_df
  
  
  
  
  # --- Logistic regression
  print("--------------------------")
  print("------ Logistic regression")
  print("--------------------------")
  
  
  # --- --- model
  model <- nnet::multinom(label ~ ., data=training_df)
  summary(model)
  
  
  # --- --- training (Pseudo)
  
  
  # --- --- Evaluation
  pred_res <- predict(model, test_df)
  
  metriques_df <- eval_metriques(pred_res, test_df$label, "Regression_Logistique", metriques_df)
  metriques_df
  
  #roc_auc_df <- eval_roc_auc(pred_res, test_df$label, "Regression_Logistique", roc_auc_df)
  #roc_auc_df
  
  conf_matrix_df <- eval_conf_matrix(pred_res, test_df$label, "Regression_Logistique")
  conf_matrix_df
  
  
  
  
  # --- KNN
  print("--------------------------")
  print("---------------------- KNN")
  print("--------------------------")
  
  
  for(k_val in 1:5){
    
    # --- --- model
    model_name <- paste("KNN-K=", as.character(k_val))
    pred_res <- knn(train=training_df[, features], 
                    test=test_df[, features], 
                    cl=training_df$label, 
                    k=k_val)
    
    # --- --- training (Pseudo)
    
    # --- --- Evaluation
    metriques_df <- eval_metriques(pred_res, test_df$label, model_name, metriques_df)
    metriques_df
    
    #roc_auc_df <- eval_roc_auc(pred_res, test_df$label, model_name, roc_auc_df)
    #roc_auc_df
    
    conf_matrix_df <- eval_conf_matrix(pred_res, test_df$label, model_name)
    conf_matrix_df
  }
  
  
  
  
  # --- Decision Tree
  print("--------------------------")
  print("------------ Decision Tree")
  print("--------------------------")
  
  
  # --- --- model
  model <- rpart(label ~ ., data=training_df, method="class")
  summary(model)
  
  
  # --- --- training (Pseudo)
  
  
  # --- --- Evaluation
  pred_res <- predict(model, test_df, type="class")
  
  metriques_df <- eval_metriques(pred_res, test_df$label, "Decision_Tree", metriques_df)
  metriques_df
  
  #roc_auc_df <- eval_roc_auc(pred_res, test_df$label, "Decision_Tree", roc_auc_df)
  #roc_auc_df
  
  conf_matrix_df <- eval_conf_matrix(pred_res, test_df$label, "Decision_Tree")
  conf_matrix_df
  
  
  
  
  # --- Enregistrement des métriques d'évaluations
  # --- --- Métriques
  str <- "results/"
  str <- paste(str, actual_dataset, "-EVAL_METRICS.csv", sep="")
  write.csv(metriques_df, str, row.names=TRUE)
  # --- --- ROC et AUC
  str <- "results/"
  str <- paste(str, actual_dataset, "-EVAL_ROC_AUC.csv", sep="")
  write.csv(roc_auc_df, str, row.names=TRUE)
  
}






# --------------------------