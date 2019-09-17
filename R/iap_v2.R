
library(dplyr)
library(broom)
library(gam)
library(caret)

iap_relate <- function(test_dat, outcome_name, prediction_name, method = "knn"){
  
  
  if (is.numeric(test_dat[ , outcome_name])){
    
    ## gam ?
    rel_model <- lm(as.formula(paste0(outcome_name, " ~ ", prediction_name)), test_dat)
    
  }else{
    
    rel_model <- train(as.formula(paste0(outcome_name, " ~ ", prediction_name)), test_dat, method = method)
    
  }
  
  
  rel_model
  
}


iap <- function(inf_formula, valid_dat, rel_model, bs = 100, seed = 1234){
  
  set.seed(seed)
  
  ## ss: sample size
  ss <- nrow(valid_dat)
  
  
  ## find outcome and covariate names from the inference formula
  outcome    <- all.vars(inf_formula)[1]
  covariates <- all.vars(inf_formula)[-1]
  
  ## bootstrap data, simulate y outcomes from the relationship model, and fit inference model on simulated data
  table_list <- lapply(1:bs, function(i){
    
    bs_idx <- sample(1:ss,ss,replace = TRUE)
    
    bs_data <- valid_dat[bs_idx,]
    
    if (is.numeric(valid_dat[ , outcome])){
      
      sim_y           <- rnorm(ss, mean = predict(rel_model, bs_data), sd = glance(rel_model)$sigma)
      
      bs_data$sim_y   <- sim_y
      
      sim_inf_formula <- update(inf_formula, sim_y ~ . )
      
      sim_inf_table   <- tidy(lm(sim_inf_formula, bs_data))
      
    }else{
      
      #sim_y = rbinom(ss, 1, prob=predict(rel_model, bs_data, type = "prob")[,2])
      
      prob <- predict(rel_model, bs_data, type = "prob")
      
      sim_y <- unlist(lapply(1:nrow(prob), function(x) {
        
        cat_matrix <- rmultinom(1, 1, prob[x,])
        result     <- rownames(cat_matrix)[which(cat_matrix == 1)]
          
        }))
      
      
      bs_data$sim_y <- as.factor(sim_y)
      
      sim_inf_formula = update(inf_formula, sim_y ~ .)
      
      sim_inf_table = tidy(suppressWarnings(glm(sim_inf_formula, bs_data, family = binomial(link = "logit"))))
    }
  
    
    
    term = sim_inf_table$term[-1]
    
    estimate = sim_inf_table$estimate[-1]
    
    std.error = sim_inf_table$std.error[-1]
    
    ## check whether any covariate contains all 0s and don't have the inference results when fitting the inference model
    match_cov = match(covariates,sim_inf_table$term[-1])
    
    if (anyNA(match_cov)){
      
      na_cov = covariates[which(is.na(match_cov))]
      
      term = c(term, na_cov)
      
      estimate = c(estimate, rep(NA, length(na_cov)))
      
      std.error = c(std.error, rep(NA, length(na_cov)))
      
    }
    
    sim_table = data.frame(term, estimate, std.error)
    
    sim_table = sim_table[match(covariates, sim_table$term),]
    
    
  }) ## end lapply
  
  
  ## calculate medians to each covariate's estimate, standeard error over bootstrap procedures
  table_temp = lapply(1:length(covariates), function(cov){
    
    rowMedians(sapply(table_list, function(x) as.numeric(x[cov,-1])),
               na.rm = TRUE)
    
  })
  
  ## create a tidy table similar to broom output with term, estimate, std.error, p.value
  tidytable = data.frame(cbind(covariates, do.call(rbind,table_temp)))
  
  colnames(tidytable) = c("term", "estimate", "std.error")
  
  tidytable$estimate = as.numeric(as.character(tidytable$estimate))
  
  tidytable$std.error = as.numeric(as.character(tidytable$std.error))
  
  tidytable = na.omit(tidytable)
  
  tidytable$statistic = tidytable$estimate / tidytable$std.error
  
  tidytable$p.value = 2*pt(-abs(tidytable$statistic),df = ss -1 - length(covariates))
  
  
  
  return(tidytable)
  
}


iap_der <- function(inf_formula, outcome_name, prediction_name, valid_dat, test_dat){
  
  if (is.numeric(test_dat[ , outcome_name])){
           
       covariates = all.vars(inf_formula)[-1]
       
       ## inference formula for testing data with observed outcome
       inf_formula_obs = as.formula(paste(outcome_name, "~", paste(covariates, collapse ="+")))
       
       ## calculate bias on testing set
       bias = tidy(lm(inf_formula, test_dat))$estimate[-1] - tidy(lm(inf_formula_obs, test_dat))$estimate[-1]
       
       yp_y = as.formula(paste0(outcome_name, " ~ ", prediction_name))
       
       ## calculate conditional variance of yp on testing set
       gamma1 = tidy(lm(yp_y, test_dat))$estimate[-1]
       
       sigma_p = glance(lm(yp_y, test_dat))$sigma
       
       sigma_y = glance(lm(inf_formula, test_dat))$sigma
       
       cond_var = sigma_p + gamma1^2 * sigma_y
       
       
       ## derivation correction of iap estimate on validation set
       estimate = tidy(lm(inf_formula, valid_dat))$estimate[-1] - bias
       
       ## derivation correction of iap standard error on validation set
       design_matrix = cbind(rep(1, nrow(valid_dat)), valid_dat[, covariates])  %>% as.matrix()
       
       var_matrix = solve(t(design_matrix) %*% design_matrix) * cond_var
       
       std.error = sqrt(diag(var_matrix)[-1])
       
       ## calcute t-statistic and p-value
       statistic = estimate/std.error
       
       p.value = 2*pt(-abs(statistic), df = nrow(valid_dat) - 1 - length(covariates))
       
       tidytable = data.frame(term = covariates, 
                              estimate = estimate,
                              std.error = std.error, 
                              statistic = statistic,
                              p.value = p.value,
                              row.names = NULL)
     }
  
  
  return(tidytable)
  
}

##### real data examples


library(rsample) 
library(matrixStats) 

####
####
#### Continuous: RIN DATA demo -- inference for one region

load("RINdata.rda")
data = RIN

## split the data into testing and validation sets using rsample package
set.seed(2019)
data_split <- initial_split(data, prop = 1/2)
testing    <- training(data_split)
validation <- testing(data_split)


## fit the relationship model on testing set
rel_model <- iap_relate(testing, "actual", "predictions")

## set up the inferance formula
formula <- predictions ~ region_100 

## fit the inference model on validation set and make iap corrections using bootstrap approach
results_iap <- iap(formula, validation, rel_model)
results_iap

## fit the inference model on validation set and make iap corrections using derivation approach
results_der <- iap_der(formula, "actual", "predictions", validation, testing)
results_der


## show the inference results on validation set without corrections
tidy(lm(formula, validation))

## show the inference results on validation set without corrections using observed outcomes 
## This output is only for comparing results. In practice we do not have observed outcomes on validation set.
tidy(lm(update(formula, actual ~ .), validation))

####
####
#### Categorical: Tissue Data demo

## data cleaning
TISSUE_data <- readRDS("breast_TISSUE_results.rds")
colnames(TISSUE_data)[colnames(TISSUE_data) == "Adipose Tissue"] <- "Adipose_predicted_prob"
colnames(TISSUE_data)[colnames(TISSUE_data) == "Breast"]         <- "Breast_predicted_prob"

TISSUE_data$predictions <- as.character(TISSUE_data$predictions)
TISSUE_data$actual      <- as.character(TISSUE_data$actual)
TISSUE_data[TISSUE_data == "Breast"]         <- 1
TISSUE_data[TISSUE_data == "Adipose Tissue"] <- 0

TISSUE_data$actual      <- as.factor(TISSUE_data$actual)
TISSUE_data$predictions <- as.factor(TISSUE_data$predictions)

## split the data into testing and validation sets using rsample package
set.seed(2019)
data_split <- initial_split(TISSUE_data, prop = 1/2)
testing    <- training(data_split)
validation <- testing(data_split)



## fit the relationship model on testing set
rel_model <- iap_relate(testing, "actual", "Breast_predicted_prob")

## set up the inferance formula
formula = predictions ~  region_100

## fit the inference model on validation set and make iap corrections using bootstrap approach
results_iap <- iap(formula, validation, rel_model)
results_iap

## show the inference results on validation set without corrections
tidy(glm(formula, validation, family = binomial(link = "logit")))

## show the inference results on validation set without corrections using observed outcomes 
## This output is only for comparing results. In practice we do not have observed outcomes on validation set.
tidy(glm(update(formula, actual ~ .), validation, family = binomial(link = "logit")))


