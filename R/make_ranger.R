#' Automated function for explaining ranger model
#'
#' Function \code{make_ranger} creates ranger model with default parameters using ranger library and
#' wraps it with the explainer.
#'
#' @param data training set for ranger model. It can be data frame, matrix, data.table or dgCMatrix. Note: data should contain target column
#' @param target name of a target value. Should be character. Has to be in data frame
#' @param type specify weather it is classification task or regression. Should be one of these characters: "classif", "regr"
#'
#'
#' @return An object of the class \code{explainer}.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @importFrom stats median weighted.mean
#' @export
#' @examples
#' \donttest{
#' # regression
#' library(DALEX)
#' data(apartments, package="DALEX")
#'
#' exp <- make_ranger(apartments, "m2.price", "regression")
#' # plot(model_parts(exp))
#'
#' # binary classification
#' library(DALEX)
#' data(titanic_imputed, package="DALEX")
#' exp <- make_ranger(titanic_imputed, "survived", "classification")
#' # plot(model_parts(exp))
#'}
##

make_ranger <- function(data, target, type, tune = FALSE, metric = NULL, iter = 20) {
  
  ### Conditions
  data <- check_conditions(data, target, type)
  
  ### Data processing level 1/2 (remove NAs, split label and training frame,...)
  # Remove rows with NA values (I will write in the documentation of function):
  data <- na.omit(data)
  
  # Creating formula
  # try to convert column names to names without znaki
  test_colnames <- lapply(colnames(data), function(x) gsub("_", "", x))
  test_colnames <- lapply(colnames(data), function(x) gsub("[.]", "", x))
  if (any(grepl('[[:punct:]]', test_colnames))) {
    colnames(data) <- lapply(colnames(data), function(x) gsub("[[:punct:]]", "_", x))
    target <- gsub("[[:punct:]]", "_", target)
    message("Column names are wrong for creating a formula. Replacing special signs with '_'")
  }
  form <- stats::as.formula(paste(target , "~."))
  
  
  ### Model
  # First binary classification
  if (type == "classification") {
    # Checking if theres right number of classes in target column
    if (length(unique(data[, target])) < 2) {
      stop("Too few classes for binary classification")
    } else if (length(unique(data[, target])) > 2) {
      stop("Too many classes for binary classification")
    }
    
    if (class(data[[target]]) != "numeric") {
      # Converting target column to numeric for Dalex
      uniq <- unique(data[, target])
      data[, target] <- ifelse(data[, target] == uniq[1], 0, 1)
      message(paste("Wrong type of target column. Changed to numeric: ",
          uniq[1], " -> 1 and ", uniq[2], " -> 0 ", sep = "")
      )
    }
  } else {
    # Checking if target column is numeric
    if (class(data[[target]]) != 'numeric' &
        class(data[[target]]) != 'integer') {
      stop("Program is stopped. The class of target column is factor, not appropriate for regression problem")
    }
  }
  
  is_classif <- type == "classification"
  
  if (tune){
    #### Tuning part
    # Validation set 
    # data_val <-
    # y_val <- 
    
    ranger_tune_fun <- function(num.trees, sample.fraction, mtry, min.node.size){
      ranger_tune <- ranger::ranger(form, data,
                                    num.trees = num.trees,
                                    mtry = ceiling(mtry * (ncol(data) - 1)),
                                    min.node.size = ceiling(nrow(data) ^ min.node.size),
                                    sample.fraction = sample.fraction)
      
      predicted <- predict(ranger_tune, data_val)
      score <- calculate_metric(metric, predicted, y_val)
      
      list(Score = score, Pred = predicted)
    }
    
    ### Tuning process
    tuned_ranger <- BayesianOptimization(ranger_tune_fun,
                                         bounds = list(num.trees = c(1L, 2000L),
                                                       mtry = c(0, 1),
                                                       min.node.size = c(0, 1),
                                                       sample.fraction = c(0.1, 1)),
                                         init_grid_dt = NULL,
                                         init_points = 10,
                                         n_iter = iter,
                                         acq = "ucb",
                                         kappa = 2.576,
                                         eps = 0.0,
                                         verbose = TRUE)
    
    best_params <- tuned_ranger$Best_Par
    
    rg <- ranger::ranger(form, data = data,
                         num.trees = best_params["num.trees"],
                         mtry = best_params["mtry"],
                         min.node.size = best_params["min.node.size"],
                         sample.fraction = best_params["sample.fraction"]
                         classification = is_classif)
    
  } else {
    rg <- ranger::ranger(form, data = data, classification = is_classif)
  }
  
  ### Explainer
  # Deleting target column from data frame
  df_exp <- data[,-which(names(data) == target)]
  
  ### Creating an explainer
  # Custom predict function for regression
  ranger_predict <- function(object, newdata) {
    return( predict(object, na.omit(newdata))$predictions )
  }
  
  ranger_explained <- DALEX::explain(rg,
                                     data = df_exp,
                                     y = data[, target],
                                     label = "Ranger",
                                     predict_function = ranger_predict,
                                     verbose = 0)
  
  class(ranger_explained) <- c("forester_model", "explainer")
  return(ranger_explained)
}
