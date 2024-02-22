test_that("persimon works", {

  set.seed(135)
  res <- persimon(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                  n1 = c(6,7), n2 = c(6,7), n3 = c(6,7),
                  n_simulations = 2, nboot = 5, conf.level = 0.95)

  expect_equal(length(res), 2)
  expect_equal(nrow(res$results), 2)
  expect_equal(ncol(res$results), 16)
  expect_equal(nrow(res$success), 2)
  expect_equal(ncol(res$success), 16)
})

test_that("persimon works with non-null skew", {

  set.seed(1265)
  res <- persimon(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                  Sk1 = 0.8, Sk2 = 0.8, Sk3 = 0.8,
                  n1 = c(6,7), n2 = c(6,7), n3 = c(6,7),
                  n_simulations = 2, nboot = 5, conf.level = 0.95)


  expect_equal(length(res), 2)
  expect_equal(nrow(res$results), 2)
  expect_equal(ncol(res$results), 16)
  expect_equal(nrow(res$success), 2)
  expect_equal(ncol(res$success), 16)
})
