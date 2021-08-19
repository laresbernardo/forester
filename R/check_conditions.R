#' Checking conditions for dataset
#'
#' \code{check_conditions} verifies whether the input data with the defined target and defined type of problem
#' is ready for further steps in creating Machine Learning models.
#'
#' Function transforms different data structures into dataframe, checks the presence of defined target column in data and the validity
#' of type.
#'
#'
#' @param data dataset, used for training models. Class of data_train is one of those classes: data.frame, matrix, data.table or dgCMatrix. NOTE: data_train includes target column.
#' @param target character, indicating name of the target column in data_train, placed in quotation marks "...".
#' @param type character, defining the task, placed in quotation marks "...". Option is "regression" or "classification", namely binary classification.
#'
#' @return A verified dataframe, which is ready for creating ML models.
#'
#'
#' @references forester library \url{https://modeloriented.github.io/forester/}

check_conditions <- function(data, target, type){
  ### Conditions:
  # Check data class
  if (!any(class(data) %in% c("data.frame", "dgCMatrix", "matrix", "data.table")))
    stop("Object is not one of the types: 'data.frame', 'dgCMatrix', 'matrix', 'data.table")
  
  # Unify data class to data frame
  if (any(class(data) == "matrix"))
  {
    data <- as.data.frame(data)
  }
  if (any(class(data) == "data.table"))
  {
    data <- as.data.frame(data)
  }
  if (any(class(data) == "dgCMatrix"))
  {
    data <- as.data.frame(as.matrix(data))
  }
  
  # Checking if data frame is empty
  if (nrow(data) == 0 | ncol(data) < 2) {
    stop("The data frame is empty or has too little columns.")
  }
  
  # Check if target is one of colnames of data frame
  if (!is.character(target) | !(target %in% colnames(data)))
    stop(
      "Either 'target' input is not a character or data does not have column named similar to 'target'"
    )

  # Check condition for type between "classification" and "regression":
  ## Classification type
  if ((type != "classification") & (type != "regression"))
    stop("Type of problem is invalid.")
  
  return(data)
}
