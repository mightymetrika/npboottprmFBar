test_that("persimon works", {

  set.seed(135)
  res <- persimon(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                  n1 = c(6,7), n2 = c(6,7), n3 = c(6,7),
                  n_simulations = 2, nboot = 5, conf.level = 0.95)





  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 16)
})
