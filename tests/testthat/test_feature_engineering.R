context("Tests for compare function")

source("objects_for_tests.R")

test_that("Running function with default parameters", {
  data <- feature_engineering_function(titanic[1:100, ], titanic[101:130, ], "survived", "classification")
  expect_equal(class(data), "list")
  data <- feature_engineering_function(apartments[1:100, ], apartments[101:130, ], "m2.price", "regression")
  expect_equal(class(data), "list")
})

test_that("Running function with deleting outliers", {
  data <- feature_engineering_function(titanic[1:100, ], titanic[101:130, ], "survived", "classification", remove_outliers = TRUE)
  expect_equal(class(data), "list")
})

test_that("Running function with filling NA", {
  data <- feature_engineering_function(titanic[1:100, ], titanic[101:130, ], "survived", "classification", fill_na = TRUE)
  expect_equal(class(data), "list")
  
  apart_na <- apartments[1:130,]
  apart_na[1,"floor"] <- NA
  data <- feature_engineering_function(apart_na[1:100, ], apart_na[101:130, ], "m2.price", "regression", fill_na = TRUE)
  expect_equal(class(data), "list")
})

test_that("Running function with scaling", {
  data <- feature_engineering_function(titanic[1:100, ], titanic[101:130, ], "survived", "classification", scaling = "minmax")
  expect_equal(class(data), "list")
  data <- feature_engineering_function(titanic[1:100, ], titanic[101:130, ], "survived", "classification", scaling = "standardize")
  expect_equal(class(data), "list")
})

test_that("Running function with feature selection", {
  data <- feature_engineering_function(titanic[1:100, ], titanic[101:130, ], "survived", "classification", num_features = 3)
  expect_equal(class(data), "list")
})

test_that("Imbalanced data", {
  options(mypkg.connection = stdin())
  
  imb_titanic <- rbind(titanic[titanic$survived == "yes",], head(titanic, 8))
  
  f <- file()
  options("mypkg.connection" = f)
  ans <- paste(c("0", "1", "2", "3", "0"), collapse = "\n") # set this to the number of tests you want to run
  write(ans, f)
  
  # Do nothing
  data <- feature_engineering_function(imb_titanic, titanic[101:130,],
                                       "survived", "classification")
  expect_equal(class(data), "list")
  
  # Do undersampling
  data <- feature_engineering_function(imb_titanic, titanic[101:130,],
                                       "survived", "classification", num_features = 4)
  expect_equal(class(data), "list")
  
  # Do oversampling
  data <- feature_engineering_function(imb_titanic, titanic[101:130,],
                                       "survived", "classification")
  expect_equal(class(data), "list")
  
  # Wrong choice
  data <- feature_engineering_function(imb_titanic, titanic[101:130,],
                                       "survived", "classification")
  expect_equal(class(data), "list")
  
  
  # reset connection
  options(mypkg.connection = stdin())
  # close the file
  close(f)
})

test_that("Wrong target and type ", {
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survd",
                                            "classification"))
  
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survived",
                                            "claation"))
})

test_that("Wrong threshold", { 
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survived",
                                            "classification",
                                            threshold_na = 1.3))
})

test_that("Wrong remove_outliers or fill_na", { 
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survived",
                                            "classification",
                                            remove_outliers = 5))
  
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survived",
                                            "classification",
                                            fill_na = 123))
})

test_that("Wrong num_features or scaling", { 
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survived",
                                            "classification",
                                            num_features = "abcd"))
  expect_error(feature_engineering_function(titanic[1:100, ],
                                            titanic[101:130, ],
                                            "survived",
                                            "classification",
                                            scaling = TRUE))
})







