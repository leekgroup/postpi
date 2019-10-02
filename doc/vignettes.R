## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

devtools::load_all()

## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(kableExtra)

## ---- message=FALSE------------------------------------------------------
data("RINdata")
data <- RINdata

## split the data into testing and validation sets using rsample package
set.seed(2019)
data_split <- rsample::initial_split(data, prop = 1/2)
testing    <- rsample::training(data_split)
validation <- rsample::testing(data_split)

## ------------------------------------------------------------------------
## fit the relationship model on testing set
rel_model <- testing %>%
  select(actual, predictions) %>%
  postpi_relate(actual)

## ------------------------------------------------------------------------
inf_formula <- predictions ~ region_10

## fit the inference model on validation set and make iap corrections using bootstrap approach
results_postpi <- validation %>%
  postpi(rel_model, inf_formula)


## ------------------------------------------------------------------------
## fit the inference model on validation set and make iap corrections using derivation approach
results_der <- testing %>%
  postpi_der(actual, predictions, validation, inf_formula)

## ------------------------------------------------------------------------
## show the inference results on validation set using observed outcomes
broom::tidy(lm(update(inf_formula, actual ~ .), validation))[-1,] %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## ------------------------------------------------------------------------
kable(results_postpi) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## ------------------------------------------------------------------------
kable(results_der) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## ------------------------------------------------------------------------
## show the inference results on validation set without corrections
broom::tidy(lm(inf_formula, validation))[-1,] %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## ------------------------------------------------------------------------
## fit the relationship model on testing set
rel_model <- testing %>%
  select(actual, predictions) %>%
  postpi_relate(actual)

inf_formula <- predictions ~ region_10 + region_20 + region_50 

## fit the inference model on validation set and make iap corrections using bootstrap approach
results_postpi <- validation %>%
  postpi(rel_model, inf_formula)

results_der <- testing %>%
  postpi_der(actual, predictions, validation, inf_formula)


## ------------------------------------------------------------------------
## gold standard
broom::tidy(lm(update(inf_formula, actual ~ .), validation))[-1,] %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## postpi
kable(results_postpi) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## postpi_der
kable(results_der) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## no correction
broom::tidy(lm(inf_formula, validation))[-1,] %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")


## ---- echo = TRUE--------------------------------------------------------
data("TISSUEdata")
TISSUE_data <- TISSUEdata


colnames(TISSUE_data)[colnames(TISSUE_data) == "Adipose Tissue"] <- "Adipose_Tissue"

TISSUE_data$predictions <- as.character(TISSUE_data$predictions)
TISSUE_data$actual      <- as.character(TISSUE_data$actual)

TISSUE_data[TISSUE_data == "Adipose Tissue"] <- "Adipose_Tissue"

TISSUE_data$actual      <- as.factor(TISSUE_data$actual)
TISSUE_data$predictions <- as.factor(TISSUE_data$predictions)

## split the data into testing and validation sets using rsample package
set.seed(2019)
data_split <- rsample::initial_split(TISSUE_data, prop = 1/2)
testing    <- rsample::training(data_split)
validation <- rsample::testing(data_split)

## ------------------------------------------------------------------------
# fit the relationship model on testing set
rel_model <- testing %>%
  select(actual, Adipose_Tissue, Breast) %>%
  postpi_relate(actual)

## ------------------------------------------------------------------------
inf_formula <- predictions ~ region_200

## fit the inference model on validation set and make iap corrections using bootstrap approach
results_postpi <- validation %>%
  postpi(rel_model, inf_formula)


## ------------------------------------------------------------------------
## show the inference results on validation set using observed outcomes
broom::tidy(glm(update(inf_formula, actual ~ .), validation, family = binomial(link = "logit")))[-1,] %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## ------------------------------------------------------------------------
kable(results_postpi) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

## ------------------------------------------------------------------------
## show the inference results on validation set without corrections
broom::tidy(glm(inf_formula, validation, family = binomial(link = "logit")))[-1,] %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(3, bold = T, color = "red") %>%
  column_spec(4, bold = T, color = "blue")

