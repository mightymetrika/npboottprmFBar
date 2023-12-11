test_that("bootFbar works as expected with inequality constraints", {

  res <- bootFbar(data = iris, formula = Sepal.Length ~ -1 + Species,
                  grp = "Species",
                  constraints = 'Speciessetosa < Speciesversicolor < Speciesvirginica',
                  nboot = 10, conf.level = 0.95, seed = NULL, na_rm = FALSE)

  expect_equal(length(res), 11)
  expect_null(res$TsF)
  expect_true(is.na(res$pvalueF))

})

test_that("bootFbar works as expected when constraint only has equalities", {

  res <- bootFbar(data = iris, formula = Sepal.Length ~ -1 + Species,
                  grp = "Species",
                  constraints = 'Speciessetosa == Speciesversicolor == Speciesvirginica',
                  nboot = 10, conf.level = 0.95, seed = NULL, na_rm = FALSE)

  expect_equal(length(res), 11)
  expect_null(res$TsA)
  expect_null(res$TsB)
  expect_true(is.na(res$pvalueA))
  expect_true(is.na(res$pvalueB))

})

test_that("bootFbar's na_rm functions as expected", {

  # Get regular model with default iris & na_rm
  res <- bootFbar(data = iris, formula = Sepal.Length ~ -1 + Species,
                  grp = "Species",
                  constraints = 'Speciessetosa == Speciesversicolor == Speciesvirginica',
                  nboot = 10, conf.level = 0.95, seed = NULL, na_rm = FALSE)

  # Create missing data on var included in formula
  iris_miss <- iris
  iris_miss[1:5, "Sepal.Length"] <- NA

  # Run on iris_miss with na_rm = TRUE
  res_miss <- bootFbar(data = iris_miss, formula = Sepal.Length ~ -1 + Species,
                  grp = "Species",
                  constraints = 'Speciessetosa == Speciesversicolor == Speciesvirginica',
                  nboot = 10, conf.level = 0.95, seed = NULL, na_rm = TRUE)

  expect_equal(nrow(res$modelo$model) - 5, nrow(res_miss$modelo$model))

  rm(iris_miss)

  # Create missing data on var excluded from formula
  iris_miss <- iris
  iris_miss[1:5, "Petal.Length"] <- NA

  # Run on iris_miss with na_rm = TRUE
  res_miss <- bootFbar(data = iris_miss, formula = Sepal.Length ~ -1 + Species,
                       grp = "Species",
                       constraints = 'Speciessetosa == Speciesversicolor == Speciesvirginica',
                       nboot = 10, conf.level = 0.95, seed = NULL, na_rm = TRUE)

  expect_equal(nrow(res$modelo$model), nrow(res_miss$modelo$model))

  rm(iris_miss)


})
