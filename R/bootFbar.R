bootFbar <- function (data, formula, grp, constraints, nboot, conf.level = 0.95,
                        seed = NULL, na_rm = FALSE) {
  # Check parameters
  stopifnot("data must be a data frame" = is.data.frame(data))
  stopifnot("nboot must be a numeric integer greater than 0" = is.numeric(nboot) && length(nboot) == 1L && nboot > 0 && nboot == as.integer(nboot))
  stopifnot("conf.level must be a numeric value between 0 and 1" = is.numeric(conf.level) && length(conf.level) == 1L && conf.level > 0 && conf.level < 1)
  stopifnot("seed must be NULL or numeric" = is.null(seed) || (is.numeric(seed) && length(seed) == 1L))

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

    # Remove NA values if na_rm is TRUE
    if (na_rm == TRUE){
      data <- data[stats::complete.cases(data), ]
    }
    grp_val <- data[[grp]]

    grp_sizes <- table(grp_val)
    model <- stats::lm(stats::as.formula(formula), data = data)
    iht_orig <- restriktor::iht(model, constraints = constraints)

    pre_calc <- list(orig_Ts_B = iht_orig$B$Ts,
                     orig_Ts_A = iht_orig$A$Ts)

  # Perform bootstrap resampling
  bootstrap_results <- replicate(nboot, bootstrap_fbar_sample(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc))

  # Extract the bootstrapped statistics and differences/effects
  stat_values <- bootstrap_results[1, ]
  diff_values <- bootstrap_results[2, ]

  bootResults_B <- unlist(bootstrap_results[1, ])
  bootResults_A <- unlist(bootstrap_results[2, ])

  # Calculate the p-value
  pvalueB <- mean(abs(bootResults_B) >= abs(pre_calc$orig_Ts_B), na.rm = TRUE)
  pvalueA <- mean(abs(bootResults_A) >= abs(pre_calc$orig_Ts_A), na.rm = TRUE)

  # Calculate the confidence interval for the test statistic
  ciB <- stats::quantile(bootResults_B, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))

  # Calculate the confidence interval for the difference/effect
  ciA <- stats::quantile(bootResults_A, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))

  return(list(pvalueA = pvalueA, pvalueB = pvalueB, ciA = ciA, ciB = ciB,
              TsA = bootResults_A, TsB = bootResults_B,
              modelob = model, ihto = iht_orig))
}

bootstrap_fbar_sample <- function(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc) {
  grp_unique <- unique(grp_val)

  # Create bootstrap samples for each group
  bootstrap_samples <- lapply(grp_unique, function(g) {
    data[sample(nrow(data), size = grp_sizes[g], replace = TRUE), ]
  })

  # Check if the standard deviation of each group's bootstrap sample is zero
  # all_sd_zero <- all(sapply(bootstrap_samples, stats::sd) == 0)
  #
  # if (all_sd_zero) {
  #   return(c(NA, NA))
  # }

  bind_boot <- do.call(rbind, bootstrap_samples)
  new_group <- rep(grp_unique, times = sapply(bootstrap_samples, nrow))
  bind_boot[[grp]] <- new_group

  iht_boot <- restriktor::iht(stats::lm(stats::as.formula(formula), data = bind_boot), constraints = constraints)

  return(list(orig_Ts_B = iht_boot$B$Ts,
              orig_Ts_A = iht_boot$A$Ts))
}
