persimon_TypeI <- function(M1 = 5, S1 = 1, M2 = 5, S2 = 1, M3 = 5, S3 = 1,
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

    df <- data.frame(x = c(F1, F2, F3), grp = rep(c("F1", "F2", "F3"), c(n1, n2, n3)))

    # Test p-value for each method
    npbft <- tryCatch({
      npboottprm::nonparboot(df, x = "x", grp = "grp", nboot = nboot, test = "F", conf.level = conf.level)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    anov <- tryCatch({
      stats::anova(stats::lm(x ~ factor(grp), df))$`F value`[1] <= 1 - conf.level
    }, error = function(e) NA)

    kw <- tryCatch({
      stats::kruskal.test(x ~ factor(grp), df)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    pft <- tryCatch({
      lmPerm::aovp(x ~ factor(grp),df)$perm$P[2,1] <= 1 - conf.level
    }, error = function(e) NA)

    bFbar0 <- tryCatch({
      bootFbar(df, formula = "x ~ -1 + grp", grp = "grp", constraints = 'grpF1 = grpF2 = grpF3',
               nboot = nboot, conf.level = conf.level)$pvalueF <= 1 - conf.level

    }, error = function(e) NA)

    bFbar1 <- tryCatch({
      bout1 <- bootFbar(df, formula = "x ~ -1 + grp", grp = "grp", constraints = 'grpF1 < grpF3',
               nboot = nboot, conf.level = conf.level)
      bout1$pvalueB >= 1 - conf.level & bout1$pvalueA <= 1 - conf.level

    }, error = function(e) NA)

    bFbar2 <- tryCatch({
      bout2 <- bootFbar(df, formula = "x ~ -1 + grp", grp = "grp", constraints = 'grpF1 < grpF2 < grpF3',
               nboot = nboot, conf.level = conf.level)
      bout2$pvalueB >= 1 - conf.level & bout1$pvalueA <= 1 - conf.level

    }, error = function(e) NA)

    Fbar0 <- tryCatch({
      restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                      constraints = 'grpF1 = grpF2 = grpF3')$pvalue <= 1 - conf.level
    }, error = function(e) NA)

    Fbar1 <- tryCatch({
      out1 <- restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                      constraints = 'grpF1 < grpF3')

      out1$B$pvalue >= 1 - conf.level & out1$A$pvalue <= 1 - conf.level
    }, error = function(e) NA)

    Fbar2 <- tryCatch({
      out2 <- restriktor::iht(stats::lm(formula = x ~ -1 + grp, data =df),
                              constraints = 'grpF1 < grpF2 < grpF3')

      out2$B$pvalue >= 1 - conf.level & out2$A$pvalue <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named vector
    return(c(NPBFT = npbft, ANOV = anov, KW = kw, PFT = pft,
             bFBAR0 = bFbar0, bFBAR1 = bFbar1, bFBAR2 = bFbar2,
             FBAR0 = Fbar0, FBAR1 = Fbar1, FBAR2 = Fbar2))
  }

  # Run n_simulations at each sample size pair
  results <- lapply(seq_along(n1), function(i) {
    replicate(n_simulations, get_result(n1[i], n2[i], n3[i]))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size combination
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(results[[i]], na.rm = TRUE)
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
      FBAR0 = proportions["FBAR0"],
      FBAR1 = proportions["FBAR1"],
      FBAR2 = proportions["FBAR2"]
    )
  })

  # Combine the list of data frames into a single data frame
  proportions_df <- do.call(rbind, proportions_list)

  return(proportions_df)
}
