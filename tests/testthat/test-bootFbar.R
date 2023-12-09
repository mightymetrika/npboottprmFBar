# test_that("bootFbar", {
#
#   model <- stats::lm(formula = Sepal.Length ~ -1 + Species, data = iris)
#   iht_res <- restriktor::iht(model, constraints = 'Speciessetosa < Speciesversicolor < Speciesvirginica')
#   pval_B <- iht_res$B$pvalue[1]
#   pval_A <- iht_res$A$pvalue[1]
#
#
#
#
#   FbarOut <- bootFbar(data = iris, formula = "Sepal.Length ~ -1 + Species", grp = "Species",
#            constraints = 'Speciessetosa < Speciesversicolor < Speciesvirginica',
#            nboot = 100, conf.level = 0.95)
#
#   FbarOut <- bootFbar(data = iris, formula = "Sepal.Length ~ -1 + Species", grp = "Species",
#                       constraints = 'Speciesvirginica < Speciesversicolor < Speciessetosa',
#                       nboot = 100, conf.level = 0.95)
#
#   FbarOut <- bootFbar(data = iris, formula = "Sepal.Length ~ -1 + Species", grp = "Species",
#                       constraints = 'Speciessetosa < Speciesvirginica < Speciesversicolor',
#                       nboot = 100, conf.level = 0.95)
#
#   FbarOut <- bootFbar(data = iris, formula = "Sepal.Length ~ -1 + Species", grp = "Species",
#                       constraints = 'Speciesversicolor < Speciessetosa <  Speciesvirginica',
#                       nboot = 100, conf.level = 0.95)
#
#
#
#   FbarOut <- bootFbar(data = iris, formula = "Sepal.Length ~ -1 + Species + Sepal.Width", grp = "Species",
#                       constraints = 'Speciesversicolor < Speciessetosa <  Speciesvirginica',
#                       nboot = 100, conf.level = 0.95)
#
#   iris2 <- iris
#   iris2$Sepal.Width <- 0
#   FbarOut <- bootFbar(data = iris2, formula = "Sepal.Length ~ -1 + Species + Sepal.Width", grp = "Species",
#                       constraints = 'Speciesversicolor < Speciessetosa <  Speciesvirginica',
#                       nboot = 100, conf.level = 0.95)
#   expect_equal(2 * 2, 4)
# })
