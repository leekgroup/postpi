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
#' @param inf_formula inference formula for fitting predicted outcomes ~ covariates, eg. yp ~ x1
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

    y_x <- as.formula(paste(obs, "~", paste(covariates, collapse ="+")))

    yp_x <- inf_formula

    y_yp <- as.formula(paste0(obs, " ~ ", pred))

    ## calculate conditional variance of yp on testing set
    gamma1   <- tidy(lm(y_yp, test_dat))$estimate[-1]

    sigma_rel  <- sigma(lm(y_yp, test_dat))

    sigma_yp_x  <- sigma(lm(yp_x, valid_dat))


    inf_factor <- sigma_rel^2 + gamma1^2 * (sigma_yp_x^2)

    mod_matrix <- cbind(rep(1, nrow(valid_dat)), valid_dat[, covariates])  %>% as.matrix()

    ## derived adjusted std.error
    der_se <- sqrt(diag(solve(t(mod_matrix) %*% mod_matrix)*inf_factor))[-1]


    ## derived adjusted beta
    beta_yp_x <- tidy(lm(yp_x, valid_dat)) %>% pull(estimate)
    beta_yp_x <- beta_yp_x[-1]

    der_beta <- gamma1 * beta_yp_x

    ## derived t-statistic
    der_t <- der_beta / der_se

    ## derived p-val
    der_p <- 2*pt(-abs(der_t), df = nrow(valid_dat) - 1 - length(covariates))



    tidytable  <- data.frame(term = covariates,
                             estimate = der_beta,
                             std.error = der_se,
                             statistic = der_t,
                             p.value = der_p,
                             row.names = NULL)
  }


  tidytable

}
