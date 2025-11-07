# Application Shiny pour générer N splits train/validation personnalisables
# avec export Excel incluant statistiques descriptives

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("shiny")
usePackage("shinycssloaders")
usePackage("shinydashboard")
usePackage("DT")
usePackage("openxlsx")
usePackage("dplyr")
usePackage("ggplot2")
usePackage("gtsummary")

library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(gtsummary)

# transformdata<-function(toto,transpose){
#   if(transpose){
#     toto<-t(toto)
#     toto<-as.data.frame(toto[,c(colnames(toto)[1],sort(colnames(toto)[-1]))], stringsAsFactors = F)
#     colnames(toto)<-toto[1,]
#     toto <- toto[-1,]
#     
#   }
#   return(toto)
# }

# 
# transformdata <- function(toto, transpose) {
#   if(transpose) {
#     # Transposer les données
#     toto <- t(toto)
#     
#     # Convertir en data frame
#     toto <- as.data.frame(toto[, c(colnames(toto)[1], sort(colnames(toto)[-1]))], 
#                           stringsAsFactors = FALSE)
#     
#     # Utiliser la première ligne comme noms de colonnes
#     colnames(toto) <- toto[1, ]
#     toto <- toto[-1, ]
#     
#     # *** NOUVELLE PARTIE : Convertir les colonnes numériques ***
#     # Pour chaque colonne, essayer de la convertir en numérique
#     for(col in names(toto)) {
#       # Tenter la conversion
#       converted <- suppressWarnings(as.numeric(toto[[col]]))
#       
#       # Si la conversion réussit (pas que des NA), utiliser la version numérique
#       # On vérifie qu'au moins 80% des valeurs non-NA originales sont convertibles
#       original_non_na <- sum(!is.na(toto[[col]]))
#       converted_non_na <- sum(!is.na(converted))
#       
#       if(original_non_na > 0 && converted_non_na / original_non_na > 0.8) {
#         toto[[col]] <- converted
#       }
#     }
#   }
#   return(toto)
# }

transformdata <- function(toto, transpose) {
  if(transpose) {
    toto <- t(toto)
    toto <- as.data.frame(toto[, c(colnames(toto)[1], sort(colnames(toto)[-1]))], 
                          stringsAsFactors = FALSE)
    
    colnames(toto) <- toto[1, ]
    toto <- toto[-1, ]
    
    is_numeric_column <- function(x) {
      x_clean <- x[!is.na(x) & x != ""]
      
      if(length(x_clean) == 0) return(FALSE)
      
      x_num <- suppressWarnings(as.numeric(x_clean))
      
      return(sum(!is.na(x_num)) / length(x_clean) > 0.8)
    }
    
    for(col in names(toto)) {
      if(is_numeric_column(toto[[col]])) {
        toto[[col]] <- as.numeric(toto[[col]])
      }
    }
  }
  return(toto)
}

#' Calculer les statistiques descriptives pour les variables cliniques
#' #' Retourne un format "mean +/- sd" pour chaque variable
calculate_descriptive_stats <- function(data, target_col, clinic_vars = NULL) {
  if (is.null(clinic_vars)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
    
    numeric_vars <- setdiff(numeric_vars, target_col)
    categorical_vars <- setdiff(categorical_vars, target_col)
    
    numeric_vars <- numeric_vars[!grepl("\\d+$", numeric_vars)]
  }else{
    numeric_vars = clinic_vars
    categorical_vars =  setdiff(names(data)[sapply(data, function(x) is.factor(x) | is.character(x))], target_col)
  }
  
  
  stats_list <- list()
  
  # Statistiques pour variables numériques - FORMAT "mean +/- sd"
  if (length(numeric_vars) > 0) {
    numeric_stats_formatted <- data.frame(
      Variable = numeric_vars,
      Stats = sapply(numeric_vars, function(var) {
        mean_val <- mean(data[[var]], na.rm = TRUE)
        sd_val <- sd(data[[var]], na.rm = TRUE)
        # Format: "mean +/- sd"
        sprintf("%.3f ±  %.3f", mean_val, sd_val)
      }),
      stringsAsFactors = FALSE
    )
    
    stats_list[["Numeriques"]] <- numeric_stats_formatted
  }
  
  # Statistiques pour variables catégorielles
  if (length(categorical_vars) > 0) {
    cat_stats_list <- lapply(categorical_vars, function(var) {
      table_var <- table(data[[var]], useNA = "ifany")
      data.frame(
        Variable = var,
        Modalite = names(table_var),
        Frequence = as.numeric(table_var),
        Pourcentage = round(as.numeric(table_var) / sum(table_var) * 100, 2)
      )
    })
    cat_stats <- do.call(rbind, cat_stats_list)
    stats_list[["Categorielles"]] <- cat_stats
  }
  
  return(stats_list)
}
# calculate_descriptive_stats <- function(data, target_col) {
#   
#   # Séparer les variables numériques et catégorielles
#   numeric_vars <- names(data)[sapply(data, is.numeric)]
#   categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
#   
#   # Exclure la variable cible des statistiques si elle est numérique
#   numeric_vars <- setdiff(numeric_vars, target_col)
#   categorical_vars <- setdiff(categorical_vars, target_col)
#   
#   # exclure les variables commencant par 999 ou finissant par des chiffres
#   numeric_vars <- numeric_vars[!grepl("\\d+$", numeric_vars)]
#   
#   stats_list <- list()
#   
#   # Statistiques pour variables numériques - FORMAT TABLEAU CROISÉ
#   if (length(numeric_vars) > 0) {
#     # Créer un tableau croisé : variables en lignes, statistiques en colonnes
#     numeric_stats_wide <- data.frame(
#       Variable = numeric_vars,
#       Mean = sapply(numeric_vars, function(var) {
#         mean(data[[var]], na.rm = TRUE)
#       }),
#       SD = sapply(numeric_vars, function(var) {
#         sd(data[[var]], na.rm = TRUE)
#       }),
#       stringsAsFactors = FALSE
#     )
#     
#     # Arrondir les valeurs à 3 décimales pour une meilleure lisibilité
#     numeric_stats_wide$Mean <- round(numeric_stats_wide$Mean, 3)
#     numeric_stats_wide$SD <- round(numeric_stats_wide$SD, 3)
#     
#     stats_list[["Numeriques"]] <- numeric_stats_wide
#   }
#   
#   # Statistiques pour variables catégorielles
#   if (length(categorical_vars) > 0) {
#     cat_stats_list <- lapply(categorical_vars, function(var) {
#       table_var <- table(data[[var]], useNA = "ifany")
#       data.frame(
#         Variable = var,
#         Modalite = names(table_var),
#         Frequence = as.numeric(table_var),
#         Pourcentage = round(as.numeric(table_var) / sum(table_var) * 100, 2)
#       )
#     })
#     cat_stats <- do.call(rbind, cat_stats_list)
#     stats_list[["Categorielles"]] <- cat_stats
#   }
#   
#   return(stats_list)
# }

#' Créer un split train/validation
create_split <- function(data, 
                         train_ratio = 0.7,
                         target_col,
                         sex_col = NULL,
                         age_col = NULL,
                         gfr_col = NULL,
                         n_age_bins = 4,
                         n_gfr_bins = 4,
                         seed = NULL) {
  
  # --------------------------------------------------------------------------
  # 1. VALIDATION DES ENTRÉES
  # --------------------------------------------------------------------------
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Vérifier que target_col existe
  if (!target_col %in% names(data)) {
    stop(paste("La colonne", target_col, "n'existe pas dans les données"))
  }
  
  # Vérifier les autres colonnes si spécifiées
  cols_to_check <- c(sex_col, age_col, gfr_col)
  cols_to_check <- cols_to_check[!is.null(cols_to_check)]
  
  missing_cols <- setdiff(cols_to_check, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Colonnes manquantes:", paste(missing_cols, collapse = ", ")))
  }
  
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("train_ratio doit être entre 0 et 1")
  }
  
  data_work <- data
  
  # Discrétiser age si spécifié et si c'est numérique
  if (!is.null(age_col) && is.numeric(data_work[[age_col]])) {
    data_work$age_bin_temp <- cut(
      data_work[[age_col]], 
      breaks = quantile(data_work[[age_col]], 
                        probs = seq(0, 1, length.out = n_age_bins + 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("age_Q", 1:n_age_bins)
    )
  } else if (!is.null(age_col)) {
    # Si age est déjà catégoriel, l'utiliser directement
    data_work$age_bin_temp <- data_work[[age_col]]
  }
  
  # Discrétiser gfr si spécifié et si c'est numérique
  if (!is.null(gfr_col) && is.numeric(data_work[[gfr_col]])) {
    data_work$gfr_bin_temp <- cut(
      data_work[[gfr_col]], 
      breaks = quantile(data_work[[gfr_col]], 
                        probs = seq(0, 1, length.out = n_gfr_bins + 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("gfr_Q", 1:n_gfr_bins)
    )
  } else if (!is.null(gfr_col)) {
    # Si gfr est déjà catégoriel, l'utiliser directement
    data_work$gfr_bin_temp <- data_work[[gfr_col]]
  }
  
  # --------------------------------------------------------------------------
  # 4. CRÉER LA VARIABLE DE STRATIFICATION COMBINÉE
  # --------------------------------------------------------------------------
  
  strata_components <- list(data_work[[target_col]])
  
  if (!is.null(sex_col)) {
    strata_components <- c(strata_components, list(data_work[[sex_col]]))
  }
  
  if (!is.null(age_col)) {
    strata_components <- c(strata_components, list(data_work$age_bin_temp))
  }
  
  if (!is.null(gfr_col)) {
    strata_components <- c(strata_components, list(data_work$gfr_bin_temp))
  }
  
  # Combiner toutes les variables en une seule strate
  strata <- do.call(paste, c(strata_components, sep = "_"))
  
  # --------------------------------------------------------------------------
  # 5. CRÉER LES INDICES TRAIN/VALIDATION PAR STRATE
  # --------------------------------------------------------------------------
  
  train_indices <- c()
  unique_strata <- unique(strata)
  
  for (stratum in unique_strata) {
    stratum_indices <- which(strata == stratum)
    n_stratum <- length(stratum_indices)
    
    # Calculer le nombre d'observations pour train
    n_train_stratum <- floor(n_stratum * train_ratio)
    
    # S'assurer d'avoir au moins 1 observation en train si la strate n'est pas vide
    if (n_train_stratum == 0 && n_stratum > 0) {
      n_train_stratum <- 1
    }
    
    # S'assurer d'avoir au moins 1 observation en validation si possible
    if (n_train_stratum >= n_stratum && n_stratum > 1) {
      n_train_stratum <- n_stratum - 1
    }
    
    # Échantillonner aléatoirement dans la strate
    if (n_train_stratum > 0 && n_train_stratum < n_stratum) {
      train_stratum_indices <- sample(stratum_indices, size = n_train_stratum)
      train_indices <- c(train_indices, train_stratum_indices)
    } else if (n_train_stratum > 0) {
      # Si tous les indices vont en train (strate trop petite)
      train_indices <- c(train_indices, stratum_indices)
    }
  }
  
  # Créer les indices de validation
  validation_indices <- setdiff(1:nrow(data), train_indices)
  
  # --------------------------------------------------------------------------
  # 6. RETOURNER LES SPLITS
  # --------------------------------------------------------------------------
  
  list(
    train = data[train_indices, ],
    validation = data[validation_indices, ]
  )
}


# create_split <- function(data, train_ratio = 0.7, target_col, seed = NULL) {
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#   
#   if (!target_col %in% names(data)) {
#     stop(paste("the column ", target_col, " does not exist in the data frame"))
#   }
#   
#   target_values <- data[[target_col]]
#   unique_classes <- unique(target_values)
#   
#   
#   train_indices <- c()
#   for (class in unique_classes) {
#     class_indices <- which(target_values == class)
#     n_class <- length(class_indices)
#     
#     n_train_class <- floor(n_class * train_ratio)
#     if (n_train_class == 0 && n_class > 0) {
#       n_train_class <- 1
#     }
#     
#     if (n_train_class > 0 && n_train_class < n_class) {
#       train_class_indices <- sample(class_indices, size = n_train_class)
#       train_indices <- c(train_indices, train_class_indices)
#     } else if (n_train_class >= n_class) {
#       if (n_class > 1) {
#         train_class_indices <- sample(class_indices, size = n_class - 1)
#         train_indices <- c(train_indices, train_class_indices)
#       }
#     }
#   }
#   
#   validation_indices <- setdiff(1:nrow(data), train_indices)
#   
#   list(
#     train = data[train_indices, ],
#     validation = data[validation_indices, ]
#   )
# }

# create_split <- function(data, train_ratio = 0.7, seed = NULL) {
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#   
#   n <- nrow(data)
#   train_indices <- sample(1:n, size = floor(n * train_ratio))
#   
#   list(
#     train = data[train_indices, ],
#     validation = data[-train_indices, ]
#   )
# }

#' Exporter un split vers Excel avec 3 feuilles
#' Exporter un split vers Excel avec 4 feuilles (ajout gtsummary)
export_split_to_excel <- function(split_data, file_path, target_col, clinic_vars = NULL) {
  wb <- openxlsx::createWorkbook()
  
  # ===== FEUILLE 1 : Train =====
  addWorksheet(wb, "Train")
  writeData(wb, "Train", split_data$train)
  
  # ===== FEUILLE 2 : Validation =====
  addWorksheet(wb, "Validation")
  writeData(wb, "Validation", split_data$validation)
  
  # # ===== FEUILLE 3 : Statistiques (format actuel) =====
  # addWorksheet(wb, "Statistiques")
  # 
  # stats_train <- calculate_descriptive_stats(split_data$train, target_col, clinic_vars = clinic_vars)
  # stats_validation <- calculate_descriptive_stats(split_data$validation, target_col, clinic_vars = clinic_vars)
  # 
  # current_row <- 1
  # 
  # # Variables numériques combinées
  # if (!is.null(stats_train$Numeriques) && !is.null(stats_validation$Numeriques)) {
  #   writeData(wb, "Statistiques", "=== VARIABLES NUMÉRIQUES ===",
  #             startCol = 1, startRow = current_row)
  #   current_row <- current_row + 2
  #   
  #   combined_stats <- data.frame(
  #     Variable = stats_train$Numeriques$Variable,
  #     Train = stats_train$Numeriques$Stats,
  #     Validation = stats_validation$Numeriques$Stats,
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   writeData(wb, "Statistiques", combined_stats,
  #             startCol = 1, startRow = current_row)
  #   
  #   headerStyle <- createStyle(
  #     textDecoration = "bold",
  #     border = "TopBottom",
  #     borderColour = "#000000"
  #   )
  #   addStyle(wb, "Statistiques", headerStyle, 
  #            rows = current_row, cols = 1:3, gridExpand = TRUE)
  #   
  #   current_row <- current_row + nrow(combined_stats) + 3
  # }
  # 
  # # Variables catégorielles
  # if (!is.null(stats_train$Categorielles)) {
  #   writeData(wb, "Statistiques", "=== VARIABLES CATÉGORIELLES - TRAIN ===",
  #             startCol = 1, startRow = current_row)
  #   current_row <- current_row + 1
  #   writeData(wb, "Statistiques", stats_train$Categorielles,
  #             startCol = 1, startRow = current_row)
  #   current_row <- current_row + nrow(stats_train$Categorielles) + 2
  # }
  # 
  # if (!is.null(stats_validation$Categorielles)) {
  #   writeData(wb, "Statistiques", "=== VARIABLES CATÉGORIELLES - VALIDATION ===",
  #             startCol = 1, startRow = current_row)
  #   current_row <- current_row + 1
  #   writeData(wb, "Statistiques", stats_validation$Categorielles,
  #             startCol = 1, startRow = current_row)
  # }
  
  # ===== FEUILLE 4 : Tableau gtsummary =====
  addWorksheet(wb, "Summary_Table")
  
  tryCatch({
    # Charger gtsummary si pas déjà fait
    if (!require(gtsummary)) {
      install.packages("gtsummary")
      library(gtsummary)
    }
    
    # Déterminer les variables à inclure
    if (!is.null(clinic_vars) && length(clinic_vars) > 0) {
      vars_to_include <- c(clinic_vars)
    } else {
      # Exclure les variables qui ressemblent à des features omiques (finissant par des chiffres)
      vars_to_include <- names(split_data$train)[!grepl("\\d+$", names(split_data$train))]
    }
    
    # S'assurer que target_col est inclus
    if (!target_col %in% vars_to_include) {
      vars_to_include <- c(vars_to_include, target_col)
    }
    
    # Créer les données pour gtsummary
    train_data <- split_data$train[, vars_to_include, drop = FALSE]
    train_data$Dataset <- "Train"
    
    validation_data <- split_data$validation[, vars_to_include, drop = FALSE]
    validation_data$Dataset <- "Validation"
    
    combined_data <- rbind(train_data, validation_data)
    
    # Créer le tableau gtsummary
    tbl_summary_result <- combined_data %>%
      gtsummary::tbl_summary(
        by = Dataset,
        include = -Dataset,
        statistic = list(
          all_continuous() ~ "{mean} ± ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = all_continuous() ~ 2
      ) %>%
      gtsummary::modify_header(
        label = "**Variable**"
      ) %>%
      gtsummary::bold_labels()
    
    # Convertir en data.frame
    summary_df <- as.data.frame(tbl_summary_result)
    
    # Écrire dans Excel
    writeData(wb, "Summary_Table", summary_df, startRow = 1, startCol = 1)
    
    # Formater les en-têtes
    headerStyle2 <- createStyle(
      textDecoration = "bold",
      fgFill = "#4F81BD",
      fontColour = "#FFFFFF",
      border = "TopBottom",
      borderColour = "#000000",
      halign = "center"
    )
    
    addStyle(wb, "Summary_Table", headerStyle2, 
             rows = 1, cols = 1:ncol(summary_df), gridExpand = TRUE)
    
    # Ajuster la largeur des colonnes
    setColWidths(wb, "Summary_Table", cols = 1:ncol(summary_df), widths = "auto")
    
    # Dans la fonction export_split_to_excel, après la feuille 3 :
    
    # # ===== FEUILLE 5 : Tableau comparatif simplifié =====
    # addWorksheet(wb, "Comparison_Table")
    # 
    # # Créer un tableau comparatif simple
    # vars_to_compare <- if (!is.null(clinic_vars)) clinic_vars else 
    #   names(split_data$train)[sapply(split_data$train, is.numeric) & 
    #                             !grepl("\\d+$", names(split_data$train))]
    # 
    # comparison_table <- data.frame(Variable = vars_to_compare)
    # comparison_table$Train <- sapply(vars_to_compare, function(var) {
    #   sprintf("%.2f ± %.2f", 
    #           mean(split_data$train[[var]], na.rm = TRUE),
    #           sd(split_data$train[[var]], na.rm = TRUE))
    # })
    # comparison_table$Validation <- sapply(vars_to_compare, function(var) {
    #   sprintf("%.2f ± %.2f", 
    #           mean(split_data$validation[[var]], na.rm = TRUE),
    #           sd(split_data$validation[[var]], na.rm = TRUE))
    # })
    # 
    # writeData(wb, "Comparison_Table", comparison_table, startRow = 1)
    # 
    # # Style des en-têtes
    # headerStyle3 <- createStyle(
    #   textDecoration = "bold",
    #   fgFill = "#4F81BD",
    #   fontColour = "#FFFFFF",
    #   border = "TopBottom"
    # )
    # addStyle(wb, "Comparison_Table", headerStyle3, rows = 1, cols = 1:3, gridExpand = TRUE)
    
  }, error = function(e) {
    # En cas d'erreur, écrire un message dans la feuille
    writeData(wb, "Summary_Table", 
              paste("Erreur lors de la création du tableau gtsummary:", e$message),
              startRow = 1, startCol = 1)
  })
  
  # Sauvegarder le workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}
# export_split_to_excel <- function(split_data, file_path, target_col, clinic_vars = NULL) {
#   wb <- openxlsx::createWorkbook()
#   
#   addWorksheet(wb, "Train")
#   writeData(wb, "Train", split_data$train)
#   
#   addWorksheet(wb, "Validation")
#   writeData(wb, "Validation", split_data$validation)
#   
#   addWorksheet(wb, "Statistiques")
#   
#   # Calculer les statistiques pour Train et Validation
#   stats_train <- calculate_descriptive_stats(split_data$train, target_col, clinic_vars = clinic_vars)
#   stats_validation <- calculate_descriptive_stats(split_data$validation, target_col, clinic_vars = clinic_vars)
#   
#   current_row <- 1
#   
#   # ===== TABLEAU COMBINÉ POUR VARIABLES NUMÉRIQUES =====
#   if (!is.null(stats_train$Numeriques) && !is.null(stats_validation$Numeriques)) {
#     
#     writeData(wb, "Statistiques", "=== VARIABLES NUMÉRIQUES ===",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 2
#     
#     # Créer le tableau combiné
#     combined_stats <- data.frame(
#       Variable = stats_train$Numeriques$Variable,
#       Train = stats_train$Numeriques$Stats,
#       Validation = stats_validation$Numeriques$Stats,
#       stringsAsFactors = FALSE
#     )
#     
#     writeData(wb, "Statistiques", combined_stats,
#               startCol = 1, startRow = current_row)
#     
#     # Ajouter un style aux en-têtes
#     headerStyle <- createStyle(
#       textDecoration = "bold",
#       border = "TopBottom",
#       borderColour = "#000000"
#     )
#     addStyle(wb, "Statistiques", headerStyle, 
#              rows = current_row, cols = 1:3, gridExpand = TRUE)
#     
#     current_row <- current_row + nrow(combined_stats) + 3
#   }
#   
#   # ===== VARIABLES CATÉGORIELLES (Train) =====
#   if (!is.null(stats_train$Categorielles)) {
#     writeData(wb, "Statistiques", "=== VARIABLES CATÉGORIELLES - TRAIN ===",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 1
#     writeData(wb, "Statistiques", stats_train$Categorielles,
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + nrow(stats_train$Categorielles) + 2
#   }
#   
#   # ===== VARIABLES CATÉGORIELLES (Validation) =====
#   if (!is.null(stats_validation$Categorielles)) {
#     writeData(wb, "Statistiques", "=== VARIABLES CATÉGORIELLES - VALIDATION ===",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 1
#     writeData(wb, "Statistiques", stats_validation$Categorielles,
#               startCol = 1, startRow = current_row)
#   }
#   
#   # Sauvegarder
#   openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
# }
# export_split_to_excel <- function(split_data, file_path, target_col) {
#   wb <- openxlsx::createWorkbook()
#   
#   addWorksheet(wb, "Train")
#   writeData(wb, "Train", split_data$train)
#   
#   addWorksheet(wb, "Validation")
#   writeData(wb, "Validation", split_data$validation)
#   
#   addWorksheet(wb, "Statistiques")
#   
#   # Statistiques pour Train
#   stats_train <- calculate_descriptive_stats(split_data$train, target_col)
#   current_row <- 1
#   
#   writeData(wb, "Statistiques", "=== ENSEMBLE TRAIN ===",
#             startCol = 1, startRow = current_row)
#   current_row <- current_row + 2
#   
#   if (!is.null(stats_train$Numeriques)) {
#     writeData(wb, "Statistiques", "Variables Numériques",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 1
#     writeData(wb, "Statistiques", stats_train$Numeriques,
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + nrow(stats_train$Numeriques) + 2
#   }
#   
#   if (!is.null(stats_train$Categorielles)) {
#     writeData(wb, "Statistiques", "Variables Catégorielles",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 1
#     writeData(wb, "Statistiques", stats_train$Categorielles,
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + nrow(stats_train$Categorielles) + 2
#   }
#   
#   # Statistiques pour Validation
#   stats_validation <- calculate_descriptive_stats(split_data$validation, target_col)
#   
#   writeData(wb, "Statistiques", "=== ENSEMBLE VALIDATION ===",
#             startCol = 1, startRow = current_row)
#   current_row <- current_row + 2
#   
#   if (!is.null(stats_validation$Numeriques)) {
#     writeData(wb, "Statistiques", "Variables Numériques",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 1
#     writeData(wb, "Statistiques", stats_validation$Numeriques,
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + nrow(stats_validation$Numeriques) + 2
#   }
#   
#   if (!is.null(stats_validation$Categorielles)) {
#     writeData(wb, "Statistiques", "Variables Catégorielles",
#               startCol = 1, startRow = current_row)
#     current_row <- current_row + 1
#     writeData(wb, "Statistiques", stats_validation$Categorielles,
#               startCol = 1, startRow = current_row)
#   }
#   
#   # Sauvegarder
#   openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
# }

library(shiny)
ui <- fluidPage(
  titlePanel(
    div(
      img(src = 'cut-out.png', height = "40px", width = "40px"),
      "Data Spliter"
    )   
  ),
  ## image icon 
  # tags$head(
  #   tags$img(height = "100px" , width = "100px", src = "pictures/cut-out.jpg")
  # ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      wellPanel(
        fluidRow(
          box(
            title = "Data upload",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            background = "purple",
            radioButtons("file_type",label =  "Type of file :",
                         choices = c("CSV" = "csv", "Excel" = "excel"),
                         selected = "csv",
                         inline = TRUE),
            fileInput("file", "upload a file",
                      accept = c(".csv", ".txt", ".xlsx", ".xls")),
            helpText("Maximum size = 5Mb"),
            # actionButton("load_btn", "Charger les données",
            #              icon = icon("upload"),
            #              class = "btn-info"),
            hr()#,
            # helpText("1. Sélectionnez le type de fichier et configurez les paramètres"),
            # helpText("2. Choisissez votre fichier"),
            # helpText("3. Cliquez sur 'Charger les données'")
          ),
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            conditionalPanel(
              condition = "input.file_type == 'csv'",
              fluidRow(
                column(5,
                       selectInput("csv_sep", "Séparateur :",
                                   choices = c("Virgule (,)" = ",",
                                               "Point-virgule (;)" = ";",
                                               "Tabulation" = "\t",
                                               "Espace" = " "),
                                   selected = ",")
                ),
                column(
                  width = 5, 
                  selectInput("csv_dec", "Caractère décimal :",
                              choices = c("Point (.)" = ".",
                                          "Virgule (,)" = ","),
                              selected = ".")
                )
              )
              ,
              textInput("csv_na", "Chaîne pour valeurs manquantes :",
                        value = "NA",
                        placeholder = "NA, vide, NULL, etc."),
              helpText("
                           
                           ")
              #Séparez plusieurs chaînes par des virgules (ex: NA,vide,NULL)
            )
            ,checkboxInput("transpose_data", "Transpose",value = FALSE)
            # ,
            # helpText("Check this box if your variables are in rows instead of columns.")
          )
        )
        
        # radioButtons("filetype", "Select file type:",
        #              choices = c("CSV" = "csv",
        #                          "Excel" = "excel"),
        #              inline =T),
        # fileInput("datafiles", 
        #           label = h4("data File"),
        #           accept =  c("text/csv",
        #                       "application/vnd.ms-excel",
        #                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        #                       ".xls",".xlsx")
        #   ),
        # # fluidRow pour la gestion des format 
        # fluidRow(
        #   conditionalPanel("input.filetype == 'csv'",
        #     column(6,
        #            textInput("dec", 
        #                      'character for decimal point', 
        #                      value = ".")
        #            ),
        #     column(6,
        #       textInput("NAstring", 
        #                 label = "character for missing value", 
        #                 value = "NA")
        #     )
        #   )
        # ),
        # 
        # fluidRow(
        #   conditionalPanel(
        #     condition = "input.filetype == 'csv'",
        #     radioButtons('sep', 'Separator',
        #                  c(Comma=',', Semicolon=';', Tab='\t'),
        #                  inline = TRUE ) 
        #   ),
        #   fluidRow(
        #     conditionalPanel(
        #       condition = "input.filetype == 'excel'",
        #       column(6,
        #              numericInput("skipn",label = "number of lines to skip",value = 0)
        #       ),
        #       column(6,
        #              numericInput("sheetn",label = "sheet",value = 1)
        #       )
        #     )
        #   )
        # ),
        # 
        # hr(),
        # br(nrow = 2),
        # checkboxInput("transpose","Transpose the table",FALSE),
        # checkboxInput("zeroegalNA","consider 0 as NA",FALSE) ,
        # actionButton("confirmdatabutton","Confirm data", 
        #              style = "background-color: blue;
        #                         color: white;
        #                         border-color: blue;")
        
      )# 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel( condition = "!output.Uploadfile",
                        h3("The purpose of this application is to provide a user-friendly tool for generating multiple train cuts/validations from omics data.",
                           align="center"),
                        fluidRow(column(6,imageOutput("image1")),
                                 column(2,imageOutput("image2"))),
                        br(),
                        h4("This application is developped in the 12th team of I2MC for internal used.",align="center")
      ), 
      conditionalPanel(condition = "output.Uploadfile", 
                       tabsetPanel(id = 'tab_split', 
                                   tabPanel( "DATA",
                                             icon = icon("table"),
                                             fluidRow(
                                               box(
                                                 title = "Data overview",
                                                 status = "info",
                                                 solidHeader = TRUE,
                                                 width = 12,
                                                 DTOutput("data_preview") %>% withSpinner(color="#0dc5c1",
                                                                                          type = 1)
                                               )
                                             )
                                   ),
                                   tabPanel( "SPLITS",
                                             icon = icon("scissors"),
                                             # fluidPage(
                                             #   tags$head(
                                             #     tags$style(
                                             #       HTML("
                                             #         #generation_log {
                                             #           color : gray;
                                             #           background-color : #F0F8FF;
                                             #           font-family : 'Courier New', Courier, monospace;
                                             #           font-size : 12px;
                                             #         }
                                             #       ")
                                             #     )
                                             #   )
                                             # ), 
                                             fluidRow(
                                               box(
                                                 title = "Parametrs of split",
                                                 status = "warning",
                                                 solidHeader = TRUE,
                                                 width = 3,
                                                 selectInput("target_col", "Classification variable (target/label) :",
                                                             choices = NULL),
                                                 sliderInput("train_ratio", " Train size (%) :",
                                                             min = 50, max = 95, value = 70, step = 1),
                                                 numericInput("n_splits", "Number of splits :",
                                                              value = 15, min = 1, max = 100, step = 1),
                                                 
                                                 hr(),
                                                 #br(nrow =2),
                                                 uiOutput('get_clinivars')
                                                
                                                 # numericInput("base_seed", "Base random seed :",
                                                 #                             value = 123, min = 1, max = 10000)
                                                 #helpText("N seeds will be generated: base_seed, base_seed+1, ..., base_seed+N-1")
                                               ),
                                               box(
                                                 width = 3,
                                                 title = "startifcation Varaibles",
                                                 status = "warning",
                                                 solidHeader = TRUE,
                                                 uiOutput("stratified_vars_ui")
                                               ),
                                               box(
                                                 title = "Data information",
                                                 status = "success",
                                                 solidHeader = TRUE,
                                                 height = 6,
                                                 width = 6,
                                                 verbatimTextOutput("data_info")
                                               )
                                             ),
                                             fluidRow(
                                               box(
                                                 title = "Generate splits",
                                                 status = "primary",
                                                 solidHeader = TRUE,
                                                 width = 6,
                                                 actionButton("generate_btn", "Générer les splits",
                                                              icon = icon("play"),
                                                              class = "btn-success btn-lg"),
                                                 hr(),
                                                 verbatimTextOutput("generation_log")
                                               )
                                             )
                                             
                                   ),
                                   tabPanel("DOWNLOAD",
                                            icon = icon("download"),
                                            fluidRow(
                                              box(
                                                title = "Downloading files",
                                                status = "success",
                                                solidHeader = TRUE,
                                                width = 12,
                                                uiOutput("download_buttons"),
                                                hr(),
                                                helpText("Each Excel file contains three sheets: Train, Validation, and Statistics.")
                                              )
                                            )
                                   )
                       )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$Uploadfile = reactive({
    return (!is.null(input$file))
  })
  
  outputOptions(output, 'Uploadfile', suspendWhenHidden=FALSE)
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           width=300,
                                           height=200,
                                           alt="I2MC logo"))},deleteFile = F)
  output$image2<-renderImage({return (list(src="pictures/rflabxx.png", 
                                           contentType="image/png",
                                           width=600,
                                           height=200,
                                           alt="RFlab logo"))},deleteFile = F)
  output$image3<-renderImage({return (list(src="pictures/structurdata2.jpg", 
                                           contentType="image/jpeg",
                                           width=600,
                                           height=300,
                                           alt="structure data"))},deleteFile = F)
  
  # Variable réactive pour stocker les données
  data_loaded <- reactiveVal(NULL)
  splits_generated <- reactiveVal(NULL)
  
  # Charger les données avec le bouton
  observeEvent({input$file
    input$csv_sep
    input$csv_dec
    input$csv_na
    input$transpose_data
  }, {
    #input$load_btn, {
    req(input$file)
    req(input$csv_dec, input$csv_sep)
    
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      data <- NULL
      
      # Charger selon le type sélectionné
      if (input$file_type == "csv") {
        # Préparer les chaînes NA
        na_strings <- trimws(unlist(strsplit(input$csv_na, ",")))
        if (length(na_strings) == 0 || na_strings[1] == "") {
          na_strings <- "NA"
        }
        cat("NA strings:", paste(na_strings, collapse = ";"), "\n")
        cat('Dec char:', input$csv_dec, '\n')
        cat('Sep char:', input$csv_sep, '\n')
        
        # Lire le CSV avec les paramètres personnalisés
        data <- read.table(
          input$file$datapath,
          header = TRUE,
          sep = input$csv_sep,
          dec = input$csv_dec,
          na.strings = na_strings,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
      } else if (input$file_type == "excel") {
        if (ext %in% c("xlsx", "xls")) {
          #data <- read.xlsx(input$file$datapath, sheet = 1)
          data <- openxlsx::read.xlsx(
            xlsxFile = input$file$datapath, 
            sheet = 1,
            colNames = TRUE,
            check.names = FALSE
          )
        } else {
          showNotification("For Excel, select an .xlsx or .xls file", type = "error")
          return()
        }
      }
      
      # Vérifier que les données ont été chargées
      if (is.null(data)) {
        showNotification("Error loading data", type = "error")
        return()
      }
      
      # Transposer the data
      if (input$transpose_data) {
        # Sauvegarder les noms de colonnes originaux (qui deviendront des ID/labels)
        original_colnames <- colnames(data)
        
        # Transposer les données
        data_t = transformdata(data,transpose = input$transpose_data )
        # data_t <- as.data.frame(t(data), stringsAsFactors = FALSE)
        # 
        # if (nrow(data_t) > 0) {
        #   data_t <- cbind(Variable = original_colnames, data_t)
        # 
        #   #colnames(data_t) <- c("Variable", paste0("Sample_", 1:(ncol(data_t)-1)))
        # }
        
        data <- data_t
      }
      # if (input$transpose_data) {
      #   # Sauvegarder les noms de colonnes comme première colonne
      #   first_col <- colnames(data)
      #   data_t <- as.data.frame(t(data), stringsAsFactors = FALSE)
      #   
      #   # Utiliser la première ligne comme noms de colonnes
      #   if (nrow(data_t) > 0) {
      #     colnames(data_t) <- first_col
      #     data <- data_t
      #   }
      # }
      
      # Stocker les données
      data_loaded(data)
      
      # Mettre à jour les choix de la variable cible
      updateSelectInput(session, "target_col",
                        choices = names(data),
                        selected = names(data)[1])
      
      showNotification("Data successfully loaded!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$get_clinivars =  renderUI({
    req(data_loaded())
    cols_to_remove <- colnames(data_loaded())[grepl('^\\d+$', colnames(data_loaded()))]
    
    filtered_data <- data_loaded()[, !colnames(data_loaded()) %in% cols_to_remove]
    
    selectInput("clinicalvars", label = "Choose clinical variables (statistics) : ", 
                choices = names(filtered_data)[sapply(filtered_data, function(c) is.numeric(x) | is.factor(x) | is.character(x) )],
                multiple = TRUE
    )
  })
  
  # Aperçu des données
  output$data_preview <- renderDT({
    req(data_loaded())
    datatable(head(data_loaded(), 100),
              options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Informations sur les données
  output$data_info <- renderPrint({
    req(data_loaded())
    data <- data_loaded()
    
    cat("Number of lines:", nrow(data), "\n")
    cat("Number of columns:", ncol(data), "\n\n")
    
    cat("data sctruture: \n")
    str(data)
  })
  
  # Générer les splits
  observeEvent(input$generate_btn, {
    req(data_loaded())
    
    withProgress(message = 'Generating...', value = 0, {
      req(input$target_col, input$n_splits)
      
      data <- data_loaded()
      train_ratio <- input$train_ratio / 100
      base_seed <- 123 # input$base_seed
      target_col <- input$target_col
      n_splits <- input$n_splits
      
      # Validation
      if (n_splits < 1 || n_splits > 100) {
        showNotification("The number of splits must be between 1 and 100.", type = "error")
        return()
      }
      
      # Créer un dossier temporaire pour les fichiers
      temp_dir <- tempdir()
      output_dir <- file.path(temp_dir, "splits_output")
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      splits_list <- list()
      log_messages <- c()
      
      # Générer les N splits
      for (i in 1:n_splits) {
        incProgress(1/n_splits, detail = paste("Split", i, "sur", n_splits))
        
        seed <- base_seed + i - 1
        #split_data <- create_split(data, train_ratio, seed)
        #split_data <- create_split(data, train_ratio, target_col, seed) 
        sex_col <- if (input$use_sex) input$sex_col else NULL
        age_col <- if (input$use_age) input$age_col else NULL
        gfr_col <- if (input$use_gfr) input$gfr_col else NULL
        split_data <-  create_split(
                    data = data,
                    train_ratio = train_ratio,
                    target_col = input$target_col,
                    sex_col = sex_col,
                    age_col = age_col,
                    gfr_col = gfr_col,
                    n_age_bins = 4, #input$n_age_bins,
                    n_gfr_bins = 4, #input$n_gfr_bins,
                    seed = seed
                  )
        
        # Nom du fichier
        file_name <- sprintf("split_%02d.xlsx", i)
        file_path <- file.path(output_dir, file_name)
        
        # Exporter vers Excel
        export_split_to_excel(split_data, file_path, target_col, clinic_vars = input$clinicalvars)
        
        splits_list[[i]] <- list(
          number = i,
          seed = seed,
          file_path = file_path,
          file_name = file_name,
          n_train = nrow(split_data$train),
          n_validation = nrow(split_data$validation)
        )
        
        log_messages <- c(log_messages,
                          sprintf("Split %02d: Train=%d, Validation=%d, Seed=%d",
                                  i, nrow(split_data$train),
                                  nrow(split_data$validation), seed))
      }
      
      splits_generated(splits_list)
      
      output$generation_log <- renderPrint({
        cat(sprintf("Generation completed successfully! (%d splits)\n\n", n_splits))
        cat("Split details:\n")
        cat(paste(log_messages, collapse = "\n"))
        cat("\nThe files are ready to be downloaded in the ‘Download’ tab.")
      })
      
      showNotification(sprintf("%d splits successfully generated!", n_splits),
                       type = "message", duration = 5)
    })
  })
  
  # Boutons de téléchargement
  output$download_buttons <- renderUI({
    req(splits_generated())
    
    splits <- splits_generated()
    n_splits <- length(splits)
    
    buttons <- lapply(1:n_splits, function(i) {
      downloadButton(
        outputId = paste0("download_", i),
        label = sprintf("download Split %02d", i),
        class = "btn-primary",
        style = "margin: 5px;"
      )
    })
    
    # Ajouter un bouton pour tout télécharger
    all_button <- downloadButton(
      outputId = "download_all",
      label = sprintf("Download all splits (ZIP) - %d files", n_splits),
      class = "btn-success btn-lg",
      style = "margin: 15px 5px;"
    )
    
    tagList(
      all_button,
      hr(),
      h4(sprintf("%d available files :", n_splits)),
      buttons
    )
  })
  
  # les handlers de téléchargement individuels
  observe({
    req(splits_generated())
    splits <- splits_generated()
    n_splits <- length(splits)
    
    lapply(1:n_splits, function(i) {
      local({
        ii <- i
        output[[paste0("download_", ii)]] <- downloadHandler(
          filename = function() {
            splits[[ii]]$file_name
          },
          content = function(file) {
            file.copy(splits[[ii]]$file_path, file)
          }
        )
      })
    })
  })
  
  # Télécharger tous les fichiers en ZIP
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("splits_train_validation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(splits_generated())
      
      # créeation fichier ZIP avec tous les splits
      splits <- splits_generated()
      files_to_zip <- sapply(splits, function(x) x$file_path)
      
      zip(file, files_to_zip, flags = "-j")
    }
  )
  
  output$stratified_vars_ui <- renderUI({
    req(data_loaded())
    data <- data_loaded()
    cols_to_consider = names(data)[sapply(data, function(x) is.factor(x)  | is.character(x) | is.numeric(x))]
    final_cols = cols_to_consider[!grepl(pattern = "\\d$", cols_to_consider)]
     
    # Exclure la variable cible des options
    choices <- setdiff(names(data), input$target_col)
    
    fluidRow(
      
      # Sex (optionnel)
      checkboxInput("use_sex","Stratifier sur le Sexe", value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s']", "use_sex"),
        selectInput(
          "sex_col",
          "Colonne Sexe:",
          choices = final_cols
        )
      ),
      
      # Age (optionnel)
      checkboxInput("use_age","Stratifier sur l'Âge", value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s']", "use_age"),
        selectInput("age_col", "Colonne Âge:",
          choices = names(data[final_cols])[sapply(data[final_cols], is.numeric)]
        )
        ,
        # sliderInput( "n_age_bins", "Nombre de groupes d'âge:",
        #   min = 2,
        #   max = 10,
        #   value = 4,
        #   step = 1
        # )
      ),
      
      # GFR (optionnel)
      checkboxInput("use_gfr", "Stratifier sur le GFR", value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s']", "use_gfr"),
        selectInput( "gfr_col","Colonne GFR:", choices = names(data[final_cols])[sapply(data[final_cols], is.numeric)]
        )
        # ,
        # sliderInput("n_gfr_bins", "Nombre de groupes de GFR:",
        #   min = 2,
        #   max = 10,
        #   value = 4,
        #   step = 1
        # )
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)