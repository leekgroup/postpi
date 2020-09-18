
data(RINdata,package="postpi")
testing    <- RINdata[1:2000,]
validation <- RINdata[2001:nrow(RINdata),]
relation_dat   <- data.frame(actual = testing$actual, predictions = testing$predictions)
relation_model <- postpi_relate(relation_dat,actual)


test_that("seed setting works", {

  inf_par1    <- postpi(validation,
                       relation_model,
                       predictions ~ region_1, seed=123)

  inf_par2    <- postpi(validation,
                        relation_model,
                        predictions ~ region_1, seed=123)

  expect_equal(inf_par1,inf_par2)

})


test_that("df and tibble both work for valid_dat", {

  inf_par1    <- postpi(validation,
                        relation_model,
                        predictions ~ region_1,seed=123)

  validation_tbl_df = as_tibble(validation)
  inf_par2    <- postpi(validation_tbl_df,
                        relation_model,
                        predictions ~ region_1, seed=123)

  expect_equal(inf_par1,inf_par2)
})


