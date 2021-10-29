#'
#' postpi_covariate_calibrate function fits a model relating a true binary variable to its predicted probability, and other variables to be used in a downstream analysis.
#'
#' This function takes in a labeled dataset for calibration containing:
#' (1) true measurements of a binary variable that is being predicted using ML
#' (2) predicted probabilities for this binary variable to be calibrated
#' (3) additional covariates to be used in a downstream analytic model
#' (4) the outcome of interest in the downstream analytic model
#' These variable names are specified as character or character vector inputs to the function, which are then combined into a model specification using standard R forumla syntax.
#' Note that in the case of a survival outcome, we recommend accounting for both the observed time and censoring indictor by specifying \code{time * status}.
#'
#' The calibration model is fit as an l2-penalized logistic regression model, using the glmnet package.
#' The penalty parameter is set using cross-validation to minimize logistic deviance.
#'
#' @import tidyverse
#' @import survival
#' @import glmnet
#'
#' @param calib_data dataframe that contains the below variables needed for post-prediction inference calibration
#' @param true_var character string; column name of the true binary variable of interest in \code{calib_data}
#' @param pred_prob_var character string; column name of the predicted probability variable for the binary variable of interest in \\code{calib_data}
#' @param covariates character vector; column names of the covariates for the analysis of interest in \code{calib_data}
#' @param outcome character string; column name (or names in formula syntax) of the outcome variable for the analysis of interest in \code{calib_data}
#'
#' @return list containing glmnet object, optimal lambda parameter, and calibration formula.
#'
#'
#' @export
#'
#' @examples
#'
#' data(EHRdata, package="postpi")
#' library(tidyverse)
#' library(survival)
#'
#' labeled_data <- EHRdata[[1]]
#' postpi_calib <- postpi_covariate_calibrate(calib_data = labeled_data,
#'                                            true_var = "met_true",
#'                                            pred_prob_var = "met_prob",
#'                                            covariates = c("age_dx", "sex"),
#'                                            outcome = "time * status")
#'

postpi_covariate_calibrate <- function(calib_data,
                                       true_var,
                                       pred_prob_var,
                                       covariates,
                                       outcome) {

  calib_features <- c(pred_prob_var, covariates, outcome)
  calib_formula <- as.formula(paste0(true_var, " ~ ", paste(calib_features, collapse = " + ")))

  xtrain <- model.matrix(calib_formula, data = calib_data)[,-1]
  calib_m <- glmnet(x = xtrain, y = calib_data[[true_var]], family = "binomial", alpha = 0)
  lambda_cv <- cv.glmnet(x = xtrain, y = calib_data[[true_var]], family = "binomial", alpha = 0)$lambda.min

  postpi_calib <- list("model" = calib_m,
                       "lambda" = lambda_cv,
                       "formula" = calib_formula)
  return(postpi_calib)
}


