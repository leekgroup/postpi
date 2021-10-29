#' Gene expression data for RNA quality prediction example
#'
#' A dataset containing data for 4769 samples with
#' gene expression information, predictions, and actual
#' values for RIN (a measure of RNA-quality)
#'
#' @format A data frame with 4,769 rows and 202 variables:
#' \describe{
#'   \item{predictions}{RNA quality predictions}
#'   \item{actual}{RNA quality actual values}
#'   \item{region_x}{Gene expression data for region x}
#'   ...
#' }
#' @source \url{https://academic.oup.com/nar/article/46/9/e54/4920847}
"RINdata"


#' Gene expression data for tissue prediction example
#'
#' A dataset containing data for 288 samples with
#' gene expression information, predictions, and actual
#' values for tissue type.
#'
#' @format A data frame with 288 rows and 2,285 variables:
#' \describe{
#'   \item{predictions}{Tissue predictions}
#'   \item{actual}{Actual tissue types}
#'   \item{region_x}{Gene expression data for region x}
#'   ...
#' }
#' @source \url{https://academic.oup.com/nar/article/46/9/e54/4920847}
"TISSUEdata"


#' Simulated EHR-derived data for covariate post-pi in survival analysis
#'
#' A dataset containing data for 2,000 patients with fully observed age at diagnosis,
#' sex, survival time, and event indicator. Also contains predicted probability of
#' metastatic disease, with true metastatic status available for first 1,000 patients.
#'
#' @format A list of two data frames (labeled and unlabeled) each with 1,000 rows and 6 or 7 variables:
#' \describe{
#'   \item{patientid}{Patient ID, integer}
#'   \item{age_dx}{Age at diagnosis, numeric}
#'   \item{sex}{Sex, binary}
#'   \item{met_true}{True metastatic status, available for labeled data only}
#'   \item{met_prob}{ML-predicted probability of metastatic status}
#'   \item{time}{Observed event time, numeric}
#'   \item{status}{Event indicator, 1 if event observed, 0 if censored}
#' }
#' 
"EHRdata"


