#'
#' postpi_covariate_impute function imputes a binary variable given its predicted probability and other variables to be used in a downstream analysis.
#' The analysis is then implemented on multiply imputed datasets with the results combined using Rubin's rules.
#'
#' This function takes in a labeled dataset for training the imputation model containing:
#' (1) true measurements of a binary variable that is being predicted using ML
#' (2) predicted probabilities for this binary variable to be calibrated
#' (3) additional covariates to be used in a downstream analytic model
#' (4) the outcome of interest in the downstream analytic model
#'
#' This function also takes in an unlabeled dataset containing all of the same columns, with the exception of the true measurements of the binary variable
#' The imputation model is fit as a logistic regression model, using the mice package.
#'
#' @import tidyverse
#' @import survival
#' @import mice
#'
#' @param labeled_data dataframe that contains labeled dataset for training imputation model
#' @param true_var character string; column name of the true binary variable of interest in \code{labeled_data}
#' @param unlabeled_data dataframe that contains all the columns in \code{labeled_data} except for \code{true_var}
#' @param inf_formula formula describing Cox model of interest; passed to \code{coxph()}, should contain \code{true_var} on the right side
#'
#' @return tibble containing analysis results; point estimates and standard errors on log hazard ratio scale
#'
#'
#' @export
#'
#' @examples
#'
#' data(EHRdata, package="postpi")
#' library(tidyverse)
#' library(survival)
#' library(mice)
#'
#' labeled_data <- EHRdata[[1]]
#' unlabeled_data <- EHRdata[[2]]
#' inf_formula <- as.formula("Surv(time, status) ~ met_true + age_dx + sex")
#'
#' res_impute <- postpi_covariate_impute(labeled_data = labeled_data,
#'                                       true_var = "met_true",
#'                                       unlabeled_data = unlabeled_data,
#'                                       inf_formula = inf_formula)
#'
#'

postpi_covariate_impute <- function(labeled_data,
                                    true_var,
                                    unlabeled_data,
                                    inf_formula) {

  labeled_data[[true_var]] <- as.factor(labeled_data[[true_var]])
  unlabeled_data[[true_var]] <- NA
  mice_data <- bind_rows(labeled_data, unlabeled_data)

  imp_results <- mice(mice_data, method = "logreg", m = 100, maxit = 1, printFlag = FALSE)

  pooled_fit <- complete(imp_results, "all") %>%
    lapply(function(dd) coxph(inf_formula, data = dd)) %>%
    pool() %>%
    summary() %>%
    as_tibble()

  return(pooled_fit)
}


