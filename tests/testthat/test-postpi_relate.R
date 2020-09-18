data(RINdata,package="postpi")
data(TISSUEdata,package="postpi")
testing    <- RINdata[1:2000,]
testing_tiss <- TISSUEdata
relation_dat   <- data.frame(actual = testing$actual, pred = testing$predictions)
relation_dat_tiss   <- data.frame(actual = testing_tiss$actual, pred = testing_tiss$predictions)


test_that("both df and tibble work for relation_dat", {
  relation_dat_tbl_df = as_tibble(relation_dat)
  relation_model1 <- postpi_relate(relation_dat_tbl_df,actual)
  relation_model2 <- postpi_relate(relation_dat,actual)
  expect_identical(relation_model1$coefficients,relation_model2$coefficients)
})


test_that("check for numeric predictions works", {
  relation_dat_tbl_df = as_tibble(relation_dat)
  relation_model1 <- postpi_relate(relation_dat_tbl_df,actual)
  relation_model2 <- postpi_relate(relation_dat,actual)

  expect_s3_class(relation_model1,"Gam")
  expect_s3_class(relation_model2,"Gam")

})


test_that("check for categorical predictions works", {
  relation_dat_tiss_tbl_df = as_tibble(relation_dat_tiss)
  relation_model1 <- postpi_relate(relation_dat_tiss_tbl_df,actual)
  relation_model2 <- postpi_relate(relation_dat_tiss,actual)

  expect_s3_class(relation_model1,"train")
  expect_s3_class(relation_model2,"train")
  expect_equal(relation_model1$method,"knn")

})
