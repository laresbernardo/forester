#' Automatic Function for Feature Engineering
#'
#' Function \code{feature_engineering} covers fundamental techniques in ML Feature Engineering process.
#'
#' With adjustable arguments, users can independently decide how function deals with outliers, NAs value,
#' feature scaling and important features selection. Furthermore, options for sampling methods will pop up in
#' case of classification problem.
#'
#'
#'
#' @param data_train dataset, used for training models. Class of data_train is one of those classes: data.frame, matrix, data.table or dgCMatrix. NOTE: data_train includes target column.
#' @param data_test  dataset having target column, used for evaluating models' performance, having same structure with data_train.
#' @param target character, indicating name of the target column in data_train.
#' @param type character, defining the task. Option is "regression" or "classification", namely binary classification.
#' @param threshold_na number, ranged from 0 to 1. Function removes columns having percentage of NAs higher than value of threshold_na.
#' @param remove_outliers logical, default is FALSE. If TRUE, all outliers in numeric columns will be removed, except for target column.
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param scaling character, default is NULL. Parameter is used for scaling features. Options are "standardize", "minmax" and NULL.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#'
#' @return A list contains 2 objects: processed data_train and process data_test.
#'
#'
#' @references forester library \url{https://modeloriented.github.io/forester/}


feature_engineering_function  <- function(data_train, data_test, target, type, threshold_na = 0.5, remove_outliers = FALSE, fill_na = FALSE, scaling = NULL, num_features = NULL){
  
  ### Conditions 
  data_train <- check_conditions(data_train, target, type)
  data_test <- check_conditions(data_test, target, type)
  
  if (threshold_na < 0 | threshold_na > 1){
    stop("Treshold for NA values should be a value between 0 and 1")
  }
  
  if (remove_outliers != TRUE & remove_outliers != FALSE){
    stop("Argument remove_outliers should be a logical variable: TRUE or FALSE")
  }
  
  if (fill_na != TRUE & fill_na != FALSE){
    stop("Argument fill_na should be a logical variable: TRUE or FALSE")
  }
  
  if (!is.null(num_features) && !is.numeric(num_features)){
    stop("Argument num_features should be a numerical.")
  }
  
  if (!is.null(scaling) && scaling != "standardize" && scaling != "minmax"){
    stop("Choose another option for scaling: standardize, minmax or NULL")
  }
  
  num_rows <- nrow(data_train)
  num_cols <- ncol(data_train)
  message("Original shape of train data frame: ", num_rows, " rows, ", num_cols, " columns")
  message("Original shape of test data frame: ", nrow(data_test), " rows, ", ncol(data_test), " columns")
  data_train <- unique(data_train)
  
  if (nrow(data_train) != num_rows){
    message("_____________")
    message("Duplications")
    message("Shape of data train frame after deleting duplicates: ",
            nrow(data_train), " rows, ", ncol(data_train), " columns")
  }

  message("_____________")
  message("NA values")
  ### Imputation:
  # Messages informing percentage of NAs in data_train and data_test:
  if (any(is.na(data_train)) | any(is.na(data_test))){
    message("percentage of NAs in data_train: ",sum(is.na(data_train))/prod(dim(data_train)) * 100, "%")
    message("percentage of NAs in data_test: ",sum(is.na(data_test))/prod(dim(data_test))* 100, "%")
  } else {
    message("There is no NA values in your data.")
  }

  # Extract names of columns in train & test having percentage of NAs greater than threshold_na:
  drop_train <- colnames(data_train)[colSums(is.na(data_train))/nrow(data_train) >= threshold_na]
  drop_test  <- colnames(data_test)[colSums(is.na(data_test))/nrow(data_test) >= threshold_na]
  drops_col  <- unique(c(drop_train,drop_test))
  
  if (length(drops_col) != 0){
    # Remove those columns dissatisfying threshold condition:
    data_train <- data_train[ , !(names(data_train) %in% drops_col), drop = FALSE]
    data_test  <- data_test[ , !(names(data_test) %in% drops_col), drop = FALSE]
    
    message("Deleting columns with precentage of NA values grater than treshold = ", threshold_na)
    message("Deleted columns are: ", drops_col)
  }
  
  #### Filling NAs:

  if (fill_na & any(is.na(data_train))){
    # deleting na values from target column 
    data_train <- data_train[!is.na(data_train[[target]]),]
    
    ## for categorical data (what to do with categorical data but numerics!!!)
    na_cat <- colnames(data_train)[(apply(data_train, 2, anyNA) & 
                                      unlist(lapply(data_train, is.factor)))]
    if (length(na_cat) > 0){
      mode <- function(x){
        ux <- na.omit(unique(x))
        tab <- tabulate(match(x, ux))
        ux[tab == max(tab)][1]
      }
      
      mode_missing <- apply(data_train[,colnames(data_train) %in% na_cat, drop = FALSE], 2, mode)
      
      for (i in 1:length(na_cat)){
        data_train[is.na(data_train[[na_cat[i]]]), na_cat[i]] <- mode_missing[i]
      }
    }
    
    ### for numerical data
    # names of numerical columns with na values 
    na_num <- colnames(data_train)[(apply(data_train, 2, anyNA) & unlist(lapply(data_train, is.numeric)))]
    if (length(na_num) > 0){
      
      # Calculating  median for those columns 
      median_missing <- apply(data_train[,colnames(data_train) %in% na_num, drop = FALSE],
                              2, median, na.rm =  TRUE)

      # changing na values
      for (i in 1:length(na_num)){
        is_integer <- all(floor(na.omit(data_train[,na_num[i]])) == na.omit(data_train[,na_num[i]]))
        if (is_integer){
          data_train[is.na(data_train[[na_num[i]]]), na_num[i]] <- floor(median_missing[i])
        } else {
          data_train[is.na(data_train[[na_num[i]]]), na_num[i]] <- median_missing[i]
        }
      }
    }
    message("NA values has been filled.")
  } else if (any(is.na(data_train))){
    message("Deleting rows with NA values in train set. If you want to fill NA values use fill_na = TRUE")
    data_train <- na.omit(data_train)
    message("Shape of data train frame after deleting NA values: ",
            nrow(data_train), " rows, ", ncol(data_train), " columns")
  }
  
  ### Removing na values from train set
  if (any(is.na(data_test))){
    message("Deleting rows with NA values in test set.")
    data_test <- na.omit(data_test)
    message("Shape of data train frame after deleting NA values: ",
            nrow(data_train), " rows, ", ncol(data_train), " columns")
  }

  ### Removing outliers
  if (remove_outliers){
    message("_____________")
    message("Outliers")
    # Remove outliers from a vector
    remove_out <- function(x, na.rm = TRUE) {
      qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm)
      H <- 3 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      return (y)
    }
    # Removes all outliers from a data set
    remove_all_outliers <- function(df, tar = target){
      df_num <- df[ ,sapply(df, is.numeric), drop = FALSE]
      
      # Avoid considering outliers on target column:
      df_num <- df_num[ , !(names(df_num) %in% tar), drop = FALSE]
      df_nonnum <- df[ , sapply(df, Negate(is.numeric)), drop = FALSE]
      df_target <- df[ , tar, drop = FALSE]
      
      # Marking outliers in df_num as NAs:
      df_num <- apply(df_num[,!(colnames(df_num) %in% tar),drop = FALSE], MARGIN = 2, remove_out)
      
      # Combining again:
      df_marked <- cbind(df_num, df_nonnum, df_target)
      
      return (na.omit(df_marked))
    }
    remove_all_outliers(data_train, target)
    message("Outliers have been removed.")
    message("Shape of data train frame after deleting outliers: ",
            nrow(data_train), " rows, ", ncol(data_train), " columns")
  }
  
  ### Over or under sampling 
  if (type == "classification"){
    uniq <- unique(data_train[[target]])
    nrow_class_1 <- nrow(data_train[data_train[[target]] == uniq[1] , , drop = FALSE])
    nrow_class_2 <- nrow(data_train) - nrow_class_1
    percent_class_1 <- round(nrow_class_1/nrow(data_train) * 100, 4) 
    percent_class_2 <- round((100 - percent_class_1), 4)
    
    if (percent_class_1 < 10 | percent_class_1 > 90){
      message("_____________")
      message("Imbalanced data")
      message("Your training set has: ",nrow(data_train),"rows in total.")
      message("Class ", uniq[1], " accounts for ", percent_class_1, "%")
      message("Class ", uniq[2], " accounts for ", percent_class_2, "%")
      message("Your data might be umbalanced. Do you want to use oversampling or undersampling method?. Press 0, 1 or 2 to decide.")
      message("0 - nothing")
      message("1 - undersampling")
      message("2 - oversampling")
      message("What is your choice?")
      
      choice <- readLines(con = getOption("mypkg.connection"), n = 1)
      
      while (choice != 0 & choice != 1 & choice != 2){
        message("Wrong option. Choose option: ")
        choice <- readLines(con = getOption("mypkg.connection"), n = 1)
      }
      
      # Starting sampling method:
      class_1_ind <- which(data_train[[target]] == uniq[1])
      class_2_ind <- which(data_train[[target]] == uniq[2])
      
      # Undersampling:
      if (choice == 1){
        n_samp <- min(length(class_1_ind), length(class_2_ind))
        ind1   <- sample(class_1_ind, n_samp)
        ind2   <- sample(class_2_ind, n_samp)
        data_train <- data_train[c(ind1,ind2),]
        message("Performing undersampling")
        message("Shape of data train frame after undersampling: ",
                nrow(data_train), " rows, ", ncol(data_train), " columns")
        
      } else if (choice == 2){
        # Oversampling
        n_samp <- max(length(class_1_ind), length(class_2_ind))
        ind1 <- sample(class_1_ind, n_samp, replace = !(length(class_1_ind) == n_samp))
        ind2 <- sample(class_2_ind, n_samp, replace = !(length(class_2_ind) == n_samp))
        data_train <- data_train[c(ind1,ind2), ]
        message("Performing oversampling")
        message("Shape of data train frame after oversampling: ",
                nrow(data_train), " rows, ", ncol(data_train), " columns")
      }
    }
  }
  
  if (!is.null(scaling)){
    message("_____________")
    message("Scaling")
    ### Standardize/ Normalize numerical features:
    # take dataframe with numeric columns only except target:
    num_cols_train <- unlist(lapply(data_train, is.numeric)) & colnames(data_train) != target
    num_data_train <- data_train[, num_cols_train, drop = FALSE]
    
    num_cols_test  <- unlist(lapply(data_test, is.numeric)) & colnames(data_test) != target
    num_data_test  <- data_test[, num_cols_test, drop = FALSE]
    
    if (scaling == "standardize"){
      # Standard Scaling: mean value of zero and an unit variance value.
      data_train[num_cols_train] <- as.data.frame(scale(num_data_train))
      data_test[num_cols_test]  <- as.data.frame(scale(num_data_test))
      message("Data has been standardized")
    } else {
      # Min-Max Scaling: the effects of outliers increase. Handle NAs and outliers first!
      normalize <- function(x){
        return ((x - min(x))/(max(x) - min(x)))
      }
      
      data_train[num_cols_train] <- as.data.frame(apply(num_data_train, 2, normalize))
      data_test[num_cols_test]  <- as.data.frame(apply(num_data_test, 2, normalize))
      message("Data has been normalized")
    }
  }

  ### Feature selection 
  if (!is.null(num_features)){
    message("_____________")
    message("Feature selection")
    tryCatch(
      {
        feat_imp <- Boruta::Boruta(data_train[,-which(names(data_train) == target)], data_train[[target]])
        imps <- Boruta::attStats(feat_imp)
        imps2 = imps[, c('meanImp'), drop = FALSE]
        top_features <- row.names(head(imps2, num_features))
        
        data_train <- data_train[, (names(data_train) %in% c(top_features, target)), drop = FALSE]
        data_test  <- data_test[, (names(data_test) %in% c(top_features, target)), drop = FALSE]
      },
      warning = function(cond) {
        if(cond$message == "getImp result contains NA(s) or NaN(s); replacing with 0(s), yet this is suspicious."){
          message("Data set is probably to small for feature selection. If you're using undersampling try oversampling instead.")
        } else {
          message(cond$message)
        }
      }
    )
  }

  return(list(data_train, data_test))
}

