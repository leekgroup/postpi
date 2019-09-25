#' postpi_relate function
#'
#' This function models the relationship between the observed and predicted outcomes.
#' @import gam
#' @import caret
#'
#' @param test_dat testing set that contains observed outcomes and predicted outcomes (continuous data) or probabilities of predicted outcomes (categorical data)
#' @param yobs name of the observed outcome in the testing set
#' @export
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
