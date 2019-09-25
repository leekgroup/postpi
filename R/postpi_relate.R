#'
#' postpi_relate function models the the relationship between the observed and predicted outcomes.
#'
#' This function takes in a data set (i.e. testing set) containing only observed and predicted outcomes.
#' It relates observed and predicted continuous outcomes through a gamma function.
#' For categorical outcomes, user inputs observed outcomes and the probabilities of predicted outcomes (i.e. probabilities for each predicted category).
#' It related categorical data through a user defined maching learning method from the caret package. The default method is k-nearest neighbours.
#'
#' @param yobs name of the observed outcome in the testing set
#' @param test_dat testing set that contains observed outcomes and predicted outcomes (continuous data) or probabilities of predicted outcomes (categorical data)
#' @param yobs name of the observed outcome in the testing set
#'
#' @return rel_model relationship model between observed outcomes and predicted outcomes/probabilities
#'
#' @import gam
#' @import caret
#'
#' @export
#'
postpi_relate <- function(test_dat, yobs, method = "knn"){

  yobs  <- deparse(substitute(yobs))

  if (is.numeric(test_dat[,yobs])){

    ypred  <- colnames(test_dat)[-which(colnames(test_dat) == yobs)]

    rel_model <- gam(as.formula(paste0(yobs, " ~ ", paste("s(", ypred, ")", collapse = " + "))), data = test_dat)

  }else{

    rel_model <- train(as.formula(paste0(yobs, "~ .")), test_dat, method = method)

  }

  rel_model

}
