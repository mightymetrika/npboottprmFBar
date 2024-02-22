#' Performance Simulation On Type I Error or Statistical Power
#'
#' Simulates performance (Type I Error or Statistical Power) for a range of
#' statistical tests, including bootFbar, default setting restriktor::iht, and
#' parametric bootstrap restriktor::iht. The function is set up to replicate the
#' table structure in Dwivedi et al. (2017) Supplemental Tables 2 & 3.
#'
#' @param M1 Mean value for group 1.
#' @param M2 Mean value for group 2.
#' @param M3 Mean value for group 3.
#' @param S1 Standard deviation for group 1.
#' @param S2 Standard deviation for group 2.
#' @param S3 Standard deviation for group 3.
#' @param Sk1 Skewness parameter for group 1; NULL for normal distribution.
#' @param Sk2 Skewness parameter for group 2; NULL for normal distribution.
#' @param Sk3 Skewness parameter for group 3; NULL for normal distribution.
#' @param n1 Vector of sample sizes for group 1.
#' @param n2 Vector of sample sizes for group 2.
#' @param n3 Vector of sample sizes for group 3.
#' @param n_simulations Number of simulations to run (default is 10000).
#' @param nboot Number of bootstrap samples (default is 1000).
#' @param conf.level Confidence level for the tests (default is 0.95).
#'
#' @return A list of data frames. One with the proportions of rejecting the null
#' hypothesis for each test and sample size combination, and the other with the
#' number of models which did not produce errors for each combination.
#'
#' @examples
#' set.seed(135)
#' persimon(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
#'          n1 = 6, n2 = 6, n3 = 6, n_simulations = 2, nboot = 4,
#'          conf.level = 0.95)
#'
#' @details
#' The `persimon` function generates data for three groups with specified mean, standard deviation,
#' and skewness, and then applies a range of statistical tests to this data, simulating the process
#' across a specified number of iterations to assess performance in terms of Type I Error or
#' Statistical Power. The output mirrors the format of the supplemental tables in Dwivedi et al. (2017).
#'
#' @references
#' Dwivedi, A. K., Mallawaarachchi, I., & Alvarado, L. A. (2017). Analysis of small sample size studies
#' using nonparametric bootstrap test with pooled resampling method. Statistics in Medicine, 36(14), 2187–2205.
#' https://doi.org/10.1002/sim.7263
#'
#' @importFrom lmPerm lmp
#'
#' @export
persimon <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
                     Sk1 = NULL, Sk2 = NULL, Sk3 = NULL,
                     n1 = c(2,3,4,5,6,7,8,9,10,15),
                     n2 = c(2,3,4,5,6,7,8,9,10,15),
                     n3 = c(2,3,4,5,6,7,8,9,10,15),
                     n_simulations = 10000, nboot = 1000,
                     conf.level = 0.95){

  # Ensure n1 and n2 and n3 are of equal length
  if (!(length(n1) == length(n2) & length(n2) == length(n3))) {
    stop("n1 and n2 and n3 must be of equal length")
  }

  # Get results for one iteration of the simulation or one pair of sample sizes
  get_result <- function(n1, n2, n3) {

    # Generate normal or skew normal data
    generate_data <- function(n, mean, sd, skew) {
      if (is.null(skew)) {
        stats::rnorm(n, mean, sd)
      } else {
        fGarch::rsnorm(n, mean, sd, xi = skew)
      }
    }

    F1 <- generate_data(n1, M1, S1, Sk1)
    F2 <- generate_data(n2, M2, S2, Sk2)
    F3 <- generate_data(n3, M3, S3, Sk3)

    df <- data.frame(x = c(F1, F2, F3), grp = rep(c("F1", "F2", "F3"),
                                                  c(n1, n2, n3)))

    # Test p-value for each method
    npbft <- tryCatch({
      npboottprm::nonparboot(df, x = "x", grp = "grp", nboot = nboot, test = "F",
                             conf.level = conf.level)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    anov <- tryCatch({
      stats::anova(stats::lm(x ~ factor(grp), df))$`F value`[1] <= 1 - conf.level
    }, error = function(e) NA)

    kw <- tryCatch({
      stats::kruskal.test(x ~ factor(grp), df)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    pft <- tryCatch({
        lmPerm::aovp(x ~ factor(grp),df, settings = FALSE)$perm$P[2,1] <= 1 - conf.level
    }, error = function(e) NA)

    bFbar0 <- tryCatch({
      bootFbar(df, formula = "x ~ -1 + grp", grp = "grp",
               constraints = 'grpF1 == grpF2 == grpF3',
               nboot = nboot, conf.level = conf.level)$pvalueF <= 1 - conf.level

    }, error = function(e) NA)

    bFbar1 <- tryCatch({
      bout1 <- bootFbar(df, formula = "x ~ -1 + grp", grp = "grp",
                        constraints = 'grpF1 < grpF3', nboot = nboot,
                        conf.level = conf.level)

      bout1$pvalueB > 1 - conf.level & bout1$pvalueA <= 1 - conf.level

    }, error = function(e) NA)

    bFbar2 <- tryCatch({
      bout2 <- bootFbar(df, formula = "x ~ -1 + grp", grp = "grp",
                        constraints = 'grpF1 < grpF2 < grpF3',
                        nboot = nboot, conf.level = conf.level)

      bout2$pvalueB > 1 - conf.level & bout2$pvalueA <= 1 - conf.level

    }, error = function(e) NA)

    rFbar0 <- tryCatch({
      restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                      constraints = 'grpF1 == grpF2 == grpF3')$pvalue <= 1 - conf.level

    }, error = function(e) NA)

    rFbar1 <- tryCatch({
      rout1 <- restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                               constraints = 'grpF1 < grpF3')

      rout1$B$pvalue > 1 - conf.level & rout1$A$pvalue <= 1 - conf.level

    }, error = function(e) NA)

    rFbar2 <- tryCatch({
      rout2 <- restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                               constraints = 'grpF1 < grpF2 < grpF3')

      rout2$B$pvalue > 1 - conf.level & rout2$A$pvalue <= 1 - conf.level

    }, error = function(e) NA)

    rbFbar0 <- tryCatch({
      restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                      constraints = 'grpF1 == grpF2 == grpF3',
                      boot = "parametric", R = nboot)$pvalue <= 1 - conf.level

    }, error = function(e) NA)

    rbFbar1 <- tryCatch({
      rbout1 <- restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                                constraints = 'grpF1 < grpF3',
                                boot = "parametric", R = nboot)

      rbout1$B$pvalue > 1 - conf.level & rbout1$A$pvalue <= 1 - conf.level

    }, error = function(e) NA)

    rbFbar2 <- tryCatch({
      rbout2 <- restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                                constraints = 'grpF1 < grpF2 < grpF3',
                                boot = "parametric", R = nboot)

      rbout2$B$pvalue > 1 - conf.level & rbout2$A$pvalue <= 1 - conf.level

    }, error = function(e) NA)

    # Returning a named list
    return(
      list(
        results = c(NPBFT = npbft, ANOV = anov, KW = kw, PFT = pft,
                    bFBAR0 = bFbar0, bFBAR1 = bFbar1, bFBAR2 = bFbar2,
                    rFBAR0 = rFbar0, rFBAR1 = rFbar1, rFBAR2 = rFbar2,
                    rbFBAR0 = rbFbar0, rbFBAR1 = rbFbar1, rbFBAR2 = rbFbar2),
        success = c(NPBFT = sum(!is.na(npbft)),
                    ANOV = sum(!is.na(anov)),
                    KW = sum(!is.na(kw)),
                    PFT = sum(!is.na(pft)),
                    bFBAR0 = sum(!is.na(bFbar0)),
                    bFBAR1 = sum(!is.na(bFbar1)),
                    bFBAR2 = sum(!is.na(bFbar2)),
                    rFBAR0 = sum(!is.na(rFbar0)),
                    rFBAR1 = sum(!is.na(rFbar1)),
                    rFBAR2 = sum(!is.na(rFbar2)),
                    rbFBAR0 = sum(!is.na(rbFbar0)),
                    rbFBAR1 = sum(!is.na(rbFbar1)),
                    rbFBAR2 = sum(!is.na(rbFbar2)))
        )
    )

  }

  # Run n_simulations at each sample size pair
  results <- lapply(seq_along(n1), function(i) {
    replicate(n_simulations, get_result(n1[i], n2[i], n3[i]))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size combination
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(do.call(cbind,
                                    lapply(results[[i]][1,],
                                           function(x) as.matrix(unlist(x)))),
                            na.rm = TRUE)
    data.frame(
      n1 = n1[i],
      n2 = n2[i],
      n3 = n3[i],
      ANOV = proportions["ANOV"],
      KW = proportions["KW"],
      NPBFT = proportions["NPBFT"],
      PFT = proportions["PFT"],
      bFBAR0 = proportions["bFBAR0"],
      bFBAR1 = proportions["bFBAR1"],
      bFBAR2 = proportions["bFBAR2"],
      rFBAR0 = proportions["rFBAR0"],
      rFBAR1 = proportions["rFBAR1"],
      rFBAR2 = proportions["rFBAR2"],
      rbFBAR0 = proportions["rbFBAR0"],
      rbFBAR1 = proportions["rbFBAR1"],
      rbFBAR2 = proportions["rbFBAR2"]
    )
  })

  # Use lapply to calculate success and create a data frame for each sample size combination
  success_list <- lapply(seq_along(results), function(i) {
    counts <- rowSums(do.call(cbind,
                              lapply(results[[i]][2,],
                                     function(x) as.matrix(unlist(x)))),
                      na.rm = TRUE)
    data.frame(
      n1 = n1[i],
      n2 = n2[i],
      n3 = n3[i],
      ANOV = counts["ANOV"],
      KW = counts["KW"],
      NPBFT = counts["NPBFT"],
      PFT = counts["PFT"],
      bFBAR0 = counts["bFBAR0"],
      bFBAR1 = counts["bFBAR1"],
      bFBAR2 = counts["bFBAR2"],
      rFBAR0 = counts["rFBAR0"],
      rFBAR1 = counts["rFBAR1"],
      rFBAR2 = counts["rFBAR2"],
      rbFBAR0 = counts["rbFBAR0"],
      rbFBAR1 = counts["rbFBAR1"],
      rbFBAR2 = counts["rbFBAR2"]
    )
  })

  # Combine the list of data frames into a single data frame
  proportions_df <- do.call(rbind, proportions_list)
  success_df <- do.call(rbind, success_list)

  return(list(results = proportions_df,
              success = success_df)
         )
}
