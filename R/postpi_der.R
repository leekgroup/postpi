#'
#' postpi_der function provides the corrected inference result table using a derivation approach for continuous outcomes
#'
#' This function is required to take in two data sets (i.e. testing set and validation set), names for observed and predicted continuous outcomes, and an inference formula.
#' The function relates continuous observed and predicted outcomes in the testing set and quantify biases
#' and then corrects the inference results in the validation set based on the input inference formula
#'
#'
#' @param test_dat testing set that contains observed outcomes and predicted outcomes (continuous data) or probabilities of predicted outcomes (categorical data)
#' @param yobs name of the continuous observed outcome
#' @param ypred name of the continuous predicted outcome
#' @param valid_dat validation set that contains predicted outcomes and covariates
#' @param inf_formula inference formula, eg. y ~ x1
#'
#'
#' @return tidytable a tidy table for inference results. It contains conlumns: term, estimate, std.error, statistic, p.value
#'
#'
#' @export
postpi_der <- function(test_dat, yobs, ypred, valid_dat, inf_formula){

  obs  <- deparse(substitute(yobs))
  pred <- deparse(substitute(ypred))

  if (is.numeric(test_dat[ , obs])){

    covariates      <- all.vars(inf_formula)[-1]

    ## inference formula for testing data with observed outcome
    inf_formula_obs <- as.formula(paste(obs, "~", paste(covariates, collapse ="+")))

    ## calculate bias on testing set
    bias <- tidy(lm(inf_formula, test_dat))$estimate[-1] - tidy(lm(inf_formula_obs, test_dat))$estimate[-1]

    yp_y <- as.formula(paste0(obs, " ~ ", pred))

    ## calculate conditional variance of yp on testing set
    gamma1   <- tidy(lm(yp_y, test_dat))$estimate[-1]

    sigma_p  <- glance(lm(yp_y, test_dat))$sigma

    sigma_y  <- glance(lm(inf_formula, test_dat))$sigma

    cond_var <- sigma_p + gamma1^2 * sigma_y


    ## derivation correction of iap estimate on validation set
    estimate <- tidy(lm(inf_formula, valid_dat))$estimate[-1] - bias

    ## derivation correction of iap standard error on validation set
    design_matrix <- cbind(rep(1, nrow(valid_dat)), valid_dat[, covariates])  %>% as.matrix()

    var_matrix    <- solve(t(design_matrix) %*% design_matrix) * cond_var

    std.error     <- sqrt(diag(var_matrix)[-1])

    ## calcute t-statistic and p-value
    statistic  <- estimate/std.error

    p.value    <- 2*pt(-abs(statistic), df = nrow(valid_dat) - 1 - length(covariates))

    tidytable  <- data.frame(term = covariates,
                             estimate = estimate,
                             std.error = std.error,
                             statistic = statistic,
                             p.value = p.value,
                             row.names = NULL)
  }


  tidytable

}
