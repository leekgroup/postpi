
data(RINdata,package="postpi")
data(TISSUEdata,package="postpi")
testing    <- RINdata[1:2000,]
testing_tiss <- TISSUEdata[1:144,]

validation <- RINdata[2001:nrow(RINdata),]
validation_tiss<- TISSUEdata[144:288,]

relation_dat   <- data.frame(actual = testing$actual, pred = testing$predictions)
relation_dat_tiss   <- data.frame(actual = testing_tiss$actual, pred = testing_tiss$predictions)

test_that("check that numeric response is ok", {

  inf_results = postpi_der(relation_dat, actual, pred, validation, predictions ~ region_1)
   expect_s3_class(inf_results,"data.frame")
})

test_that("check that categorical response throws error", {
  expect_error({
    postpi_der(relation_dat_tiss, actual, pred, validation_tiss, predictions ~ region_1)
  },"Analytical derivation is only available for continuous outcomes")
})
