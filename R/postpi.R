#' postpi function provides the corrected inference result table using a bootstrap approach for continuous / catigorical outcomes
#'
#' This function is required to take in the relationship model from `postpi_relate()`, a validation set, and the inference formula.
#' Through a bootstrap approach, the function quantifies biases using the relationship model
#' and then corrects the inference results in the validation set based on the input inference formula.
#' The default number of bootstrapping is 100 and the seed is 1234. Both parameters can be defined by users.
#'
#' @import broom
#' @import matrixStats
#'
#' @param valid_dat validation set that contains predicted outcomes and covariates
#' @param rel_model model object created by `postpi_relate()`
#' @param inf_formula inference formula, eg. y ~ x1
#' @param bs number of bootstrap times. The default value is 100 times.
#' @param seed seed number
#'
#' @return tidytable a tidy table for inference results. It contains conlumns: term, estimate, std.error, statistic, p.value
#'
#'
#' @export
postpi <- function(valid_dat, rel_model, inf_formula, bs = 100, seed = 1234){

  set.seed(seed)

  ## ss: sample size
  ss <- nrow(valid_dat)


  ## find outcome and covariate names from the inference formula
  ypred      <- all.vars(inf_formula)[1]
  covariates <- all.vars(inf_formula)[-1]

  ## bootstrap data, simulate y outcomes from the relationship model, and fit inference model on simulated data
  table_list <- lapply(1:bs, function(i){

    bs_idx <- sample(1:ss,ss,replace = TRUE)

    bs_data <- valid_dat[bs_idx,]

    if (is.numeric(valid_dat[ , ypred])){

      sim_y           <- rnorm(ss, mean = predict(rel_model, bs_data), sd = sigma(rel_model))

      bs_data$sim_y   <- sim_y

      sim_inf_formula <- update(inf_formula, sim_y ~ . )

      sim_inf_table   <- tidy(lm(sim_inf_formula, bs_data))

    }else{

      #sim_y = rbinom(ss, 1, prob=predict(rel_model, bs_data, type = "prob")[,2])

      prob  <- predict(rel_model, bs_data, type = "prob")

      sim_y <- unlist(lapply(1:nrow(prob), function(x) {

        cat_matrix <- rmultinom(1, 1, prob[x,])
        result     <- rownames(cat_matrix)[which(cat_matrix == 1)]

        }))


      bs_data$sim_y   <- as.factor(sim_y)

      sim_inf_formula <- update(inf_formula, sim_y ~ .)

      sim_inf_table   <- tidy(suppressWarnings(glm(sim_inf_formula, bs_data, family = binomial(link = "logit"))))
    }



    term      <- sim_inf_table$term[-1]

    estimate  <- sim_inf_table$estimate[-1]

    std.error <- sim_inf_table$std.error[-1]

    ## check whether any covariate contains all 0s and don't have the inference results when fitting the inference model
    match_cov <- match(covariates,sim_inf_table$term[-1])

    if (anyNA(match_cov)){

      na_cov   <- covariates[which(is.na(match_cov))]

      term     <- c(term, na_cov)

      estimate <- c(estimate, rep(NA, length(na_cov)))

      std.error<- c(std.error, rep(NA, length(na_cov)))

    }

    sim_table <- data.frame(term, estimate, std.error)

    sim_table <- sim_table[match(covariates, sim_table$term),]


  }) ## end lapply


  ## calculate medians to each covariate's estimate, standeard error over bootstrap procedures
  table_temp <- lapply(1:length(covariates), function(cov){

    rowMedians(sapply(table_list, function(x) as.numeric(x[cov,-1])),
               na.rm = TRUE)

  })

  ## create a tidy table similar to broom output with term, estimate, std.error, p.value
  tidytable           <- data.frame(cbind(covariates, do.call(rbind,table_temp)))

  colnames(tidytable) <- c("term", "estimate", "std.error")

  tidytable$estimate  <- as.numeric(as.character(tidytable$estimate))

  tidytable$std.error <- as.numeric(as.character(tidytable$std.error))

  tidytable           <- na.omit(tidytable)

  tidytable$statistic <- tidytable$estimate / tidytable$std.error

  tidytable$p.value   <- 2*pt(-abs(tidytable$statistic),df = ss -1 - length(covariates))



  tidytable

}


