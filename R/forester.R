#' Function for training models
#'
#' Function \code{train} automates the whole ML pipelines ....
#'
#' @param metric string containing name of mertic based on which the model will be selected.
#' Metric shoud be written in small letters
#'
#'
#' @return An object of the class \code{forester_model}.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @importFrom stats median weighted.mean
#' @export
#' @examples
#' \donttest{
#' library(DALEX)
#' data(apartments, package="DALEX")
#'
#' best_model <- forester(apartments, "m2.price", "regression")
#'}
##


forester <- function(data, target, type, metric = NULL, data_test = NULL, threshold_na = 0.5, remove_outliers = FALSE, fill_na = FALSE, scaling = NULL, num_features = NULL){
  data <- check_conditions(data, target, type)
  
  ### If data_test is blank, it is needed to split data into data_train and data_test
  if (is.null(data_test)){
    if (type == "regression"){
      # Split data in ratio 4:1
      sample_size <- floor(0.8 * nrow(data))
      set.seed(123)
      train_index <- sample(seq_len(nrow(data)), size = sample_size)
      
      data_train <- data[train_index,]
      data_test  <- data[-train_index,]
    }
    
    if (type=="classification"){
      # Split data by ratio 4:1, while stratification is needed.
      uniq <- unique(data[[target]])
      data_negative <- data[data[[target]] == uniq[1],]
      data_positive <- data[data[[target]] == uniq[2],]
      
      set.seed(123)
      sample_size_pos <- floor(0.8 * nrow(data_positive))
      sample_size_neg <- floor(0.8 * nrow(data_negative))
      
      train_index_pos <- sample(seq_len(nrow(data_positive)),size = sample_size_pos)
      train_index_neg <- sample(seq_len(nrow(data_negative)),size = sample_size_neg)
      
      data_train <- rbind(data_positive[train_index_pos,], data_negative[train_index_neg,])
      data_test  <- rbind(data_positive[-train_index_pos,], data_negative[-train_index_neg,])
    }
    
    ### Shuffering rows in data_train and data_test:
    rows_train <- sample(nrow(data_train))
    rows_test  <- sample(nrow(data_test))
    
    data_train <- data_train[rows_train,]
    data_test  <- data_test[rows_test,]
  } else {
    data_test <- check_conditions(data_test, target, type)
    
    # Check structure of data_test:
    if (!(setequal(colnames(data_train),colnames(data_test)))){
      stop("Column names in train data set and test data set are not identical.")
    }
  }

  
  # Feature engineering step 
  processed_data <- feature_engineering_function(data_train, data_test, target,
                                                 type, threshold_na = threshold_na,
                                                 remove_outliers = remove_outliers,
                                                 fill_na = fill_na,
                                                 scaling = scaling, 
                                                 num_features = num_features)
  data_train <- processed_data[[1]]
  data_test <- processed_data[[2]]
  
  ### Creating models:
  ranger_exp   <- make_ranger(data_train, target, type)
  suppressMessages(catboost_exp <- make_catboost(data_train, target, type))
  suppressMessages(xgboost_exp  <- make_xgboost(data_train, target, type))
  suppressMessages(lightgbm_exp <- make_lightgbm(data_train, target, type))
  
  models <- list(catboost_exp, xgboost_exp, ranger_exp, lightgbm_exp)
  
  ### Processing target column in data_test
  if (type == "classification"){
      # Unquote quotation marks
      processed_label <- catboost_exp$y
      uniq_label <- unique(processed_label)
      data_test[[target]] <- ifelse(data_test[[target]] == data_train[[target]][1],
                                 uniq_label[1],
                                 uniq_label[2])
  }
  
  best_model <- compare_models(models, data_test, target, metric)
  
}
