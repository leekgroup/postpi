#'
#' postpi_relate function models the the relationship between the observed and predicted outcomes.
#'
#' This function is required to take in a data set (i.e. testing set) containing only observed and predicted outcomes and name for observed outcomes.
#' It relates observed and predicted continuous outcomes through a gamma function.
#' For categorical outcomes, user inputs observed outcomes and the probabilities of predicted outcomes (i.e. probabilities for each predicted category).
#' It related categorical data through a user defined maching learning method from the caret package. The default method is k-nearest neighbours.
#'
#' @import gam
#' @import caret
#'
#' @param relation_dat testing set that contains observed outcomes and predicted outcomes (continuous data) or probabilities of predicted outcomes (categorical data)
#' @param yobs name of the observed outcome in the testing set
#' @param method_categorical Method to be passed to caret train function for categorical data
#'
#' @return rel_model relationship model between observed outcomes and predicted outcomes/probabilities
#'
#'
#' @export
#' @examples
#' data(RINdata,package="postpi")
#'
#' testing    <- RINdata[1:2000,]
#' relation_dat   <- data.frame(actual = testing$actual, pred = testing$predictions)
#' relation_model <- postpi_relate(relation_dat,actual)
#'
postpi_relate <- function(relation_dat, yobs, method_categorical = "knn"){

  yobs  <- deparse(substitute(yobs))

  if (is.numeric(relation_dat[[yobs]])){

    ypred  <- colnames(relation_dat)[-which(colnames(relation_dat) == yobs)]

    rel_model <- gam(as.formula(paste0(yobs, " ~ ", paste("s(", ypred, ")", collapse = " + "))), data = relation_dat)

  }else{

    rel_model <- train(as.formula(paste0(yobs, "~ .")), relation_dat, method= method_categorical)

  }

  rel_model

}
