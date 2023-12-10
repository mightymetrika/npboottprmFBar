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
