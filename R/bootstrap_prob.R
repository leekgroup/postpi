#'
#' bootstrap_prob function fits a Cox proportional hazards analysis model given predicted probabilities for a binary covariate.
#'
#' This function takes in an analysis dataset containing a column of predicted probabilities.
#' The specified Cox model is fit over B nonparametric bootstrap iterations.
#' For each bootstrapped dataset, a binary variable vector is drawn from the predicted probabilities.
#'
#' @import tidyverse
#' @import survival
#'
#' @param analysis_data dataframe that contains the predicted probability variable, and the other relevant variables for analysis
#' @param inf_formula formula describing Cox model of interest; passed to \code{coxph()}, should contain \code{pred_var_col} on the right side
#' @param pred_prob_var character string; column name of the predicted probability variable in \code{analysis_data} for the binary variable of interest
#' @param pred_var_col character string; column name assigned to the binary variable of interest used in analysis
#' @param B integer; number of bootstrap iterations, defaults to 500
#' @param alpha numeric; significance level for inference, defaults to 0.05
#'
#' @return tibble of inferential results; point estimates and 100(1 - alpha)% confidence intervals on the hazard ratio scale
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
#' analysis_data <- EHRdata[[2]]
#' postpi_calib <- postpi_covariate_calibrate(calib_data = labeled_data,
#'                                            true_var = "met_true",
#'                                            pred_prob_var = "met_prob",
#'                                            covariates = c("age_dx", "sex"),
#'                                            outcome = "time * status")
#'
#' xtrain_analysis <- analysis_data %>%
#'  select(met_prob, age_dx, sex, time, status) %>%
#'  mutate(`time:status` = time * status) %>%
#'  as.matrix
#'
#' analysis_data[["met_calib_prob"]] <- as.numeric(predict(postpi_calib$model,
#'                                                         newx = xtrain_analysis,
#'                                                         type = "response",
#'                                                         s = postpi_calib$lambda,
#'                                                         alpha = 0))
#' inf_formula <- as.formula("Surv(time, status) ~ met_calib + age_dx + sex")
#' res_calib <- bootstrap_prob(analysis_data, inf_formula, "met_calib_prob", "met_calib")
#'

bootstrap_prob <- function(analysis_data,
                           inf_formula,
                           pred_prob_col,
                           pred_var_col,
                           B = 500,
                           alpha = 0.05) {

  boot_dist <- NULL
  for (i in 1:B) {
    fit_ind <- sample(1:nrow(analysis_data), size = nrow(analysis_data), replace = TRUE)
    data_fit <- analysis_data[fit_ind,]

    data_fit[[pred_var_col]] <- rbinom(nrow(data_fit), 1, data_fit[[pred_prob_col]])

    fit_i <- coxph(inf_formula, data = data_fit, control = coxph.control(iter.max = 100))
    boot_dist <- rbind(boot_dist, coef(fit_i))
  }

  out <- data.frame(variable = colnames(boot_dist)) %>%
    mutate(estimate = apply(boot_dist, 2, function(x) exp(median(x))),
           ci_lower = apply(boot_dist, 2, function(x) exp(quantile(x, alpha / 2))),
           ci_upper = apply(boot_dist, 2, function(x) exp(quantile(x, 1 - (alpha / 2))))) %>%
    as_tibble()

  return(out)
}
