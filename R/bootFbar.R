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

  # Initialize variables
  pvalueA <- pvalueB <- pvalueF <- NA
  ciA <- ciB <- ciF <- NULL
  bootResults_A <- bootResults_B <- bootResults_F <- NULL

  # Prepare the model and initial iht computation
  model <- stats::lm(stats::as.formula(formula), data = data)
  iht_orig <- restriktor::iht(model, constraints = constraints)

  # Check if only equality constraints are used
  only_equality_constraints <- is.null(iht_orig$B)

  # Pre-calculate original test statistics
  pre_calc <- list(orig_Ts_F = if (only_equality_constraints) iht_orig$Ts else NULL,
                   orig_Ts_B = if (!only_equality_constraints) iht_orig$B$Ts else NULL,
                   orig_Ts_A = if (!only_equality_constraints) iht_orig$A$Ts else NULL)

  # Perform bootstrap resampling
  bootstrap_results <- replicate(nboot, bootstrap_fbar_sample(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc, only_equality_constraints))

  # Process results based on constraint type
  if (only_equality_constraints) {
    bootResults_F <- unlist(bootstrap_results)

    # Calculate the p-value and confidence interval for F-test
    pvalueF <- mean(abs(bootResults_F) >= abs(pre_calc$orig_Ts_F), na.rm = TRUE)
    ciF <- stats::quantile(bootResults_F, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))
  } else {
    bootResults_B <- unlist(bootstrap_results[1, ])
    bootResults_A <- unlist(bootstrap_results[2, ])

    # Calculate the p-values and confidence intervals for Type A and B
    pvalueB <- mean(abs(bootResults_B) >= abs(pre_calc$orig_Ts_B), na.rm = TRUE)
    pvalueA <- mean(abs(bootResults_A) >= abs(pre_calc$orig_Ts_A), na.rm = TRUE)
    ciB <- stats::quantile(bootResults_B, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))
    ciA <- stats::quantile(bootResults_A, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))
  }

  return(list(pvalueA = pvalueA, pvalueB = pvalueB, pvalueF = pvalueF,
              ciA = ciA, ciB = ciB, ciF = ciF,
              TsA = bootResults_A, TsB = bootResults_B, TsF = bootResults_F,
              modelob = model, ihto = iht_orig))
}

bootstrap_fbar_sample <- function(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc, only_equality_constraints) {
  grp_unique <- unique(grp_val)

  # Create bootstrap samples for each group
  bootstrap_samples <- lapply(grp_unique, function(g) {
    data[sample(nrow(data), size = grp_sizes[g], replace = TRUE), ]
  })

  bind_boot <- do.call(rbind, bootstrap_samples)
  new_group <- rep(grp_unique, times = sapply(bootstrap_samples, nrow))
  bind_boot[[grp]] <- new_group

  # Use tryCatch to handle potential errors in iht_boot
  iht_boot <- tryCatch({
    restriktor::iht(stats::lm(stats::as.formula(formula), data = bind_boot), constraints = constraints)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(iht_boot)) {
    return(list(orig_Ts_B = NA, orig_Ts_A = NA, orig_Ts_F = NA))
  }

  if (only_equality_constraints) {
    return(list(orig_Ts_F = iht_boot$Ts))
  } else {
    return(list(orig_Ts_B = iht_boot$B$Ts,
                orig_Ts_A = iht_boot$A$Ts))
  }
}










# bootFbar <- function (data, formula, grp, constraints, nboot, conf.level = 0.95,
#                         seed = NULL, na_rm = FALSE) {
#   # Check parameters
#   stopifnot("data must be a data frame" = is.data.frame(data))
#   stopifnot("nboot must be a numeric integer greater than 0" = is.numeric(nboot) && length(nboot) == 1L && nboot > 0 && nboot == as.integer(nboot))
#   stopifnot("conf.level must be a numeric value between 0 and 1" = is.numeric(conf.level) && length(conf.level) == 1L && conf.level > 0 && conf.level < 1)
#   stopifnot("seed must be NULL or numeric" = is.null(seed) || (is.numeric(seed) && length(seed) == 1L))
#
#   # Set seed
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#
#     # Remove NA values if na_rm is TRUE
#     if (na_rm == TRUE){
#       data <- data[stats::complete.cases(data), ]
#     }
#     grp_val <- data[[grp]]
#
#     grp_sizes <- table(grp_val)
#     model <- stats::lm(stats::as.formula(formula), data = data)
#     iht_orig <- restriktor::iht(model, constraints = constraints)
#
#     pre_calc <- tryCatch({
#       list(orig_Ts_B = iht_orig$B$Ts,
#            orig_Ts_A = iht_orig$A$Ts,
#            orig_Ts_F = NULL)
#     }, error = function(e) {
#       return(NULL)
#     })
#
#
#     if (is.null(pre_calc)){
#       pre_calc <- list(orig_Ts_B = NULL,
#                        orig_Ts_A = NULL,
#                        orig_Ts_F = iht_orig$Ts)
#     }
#
#
#   # Perform bootstrap resampling
#   bootstrap_results <- replicate(nboot, bootstrap_fbar_sample(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc))
#
#   # Extract the bootstrapped statistics and differences/effects
#   if (!is.null(bootstrap_results[1, ])){
#     bootResults_B <- unlist(bootstrap_results[1, ])
#     bootResults_A <- unlist(bootstrap_results[2, ])
#
#     # Calculate the p-value
#     pvalueB <- mean(abs(bootResults_B) >= abs(pre_calc$orig_Ts_B), na.rm = TRUE)
#     pvalueA <- mean(abs(bootResults_A) >= abs(pre_calc$orig_Ts_A), na.rm = TRUE)
#
#     # Calculate the confidence interval for the test statistic
#     ciB <- stats::quantile(bootResults_B, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))
#
#     # Calculate the confidence interval for the difference/effect
#     ciA <- stats::quantile(bootResults_A, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))
#   }
#
#   if(!is.null(bootstrap_results[3, ])){
#     bootResults_F <- unlist(bootstrap_results[3, ])
#
#     # Calculate the p-value
#     pvalueF <- mean(abs(bootResults_F) >= abs(pre_calc$orig_Ts_F), na.rm = TRUE)
#
#     # Calculate the confidence interval for the difference/effect
#     ciF <- stats::quantile(bootResults_F, c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))
#   }
#
#   return(list(pvalueA = pvalueA, pvalueB = pvalueB, pvalueF = pvalueF,
#               ciA = ciA, ciB = ciB, ciF = ciF,
#               TsA = bootResults_A, TsB = bootResults_B, TsF = bootResults_F,
#               modelob = model, ihto = iht_orig))
# }
#
# bootstrap_fbar_sample <- function(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc) {
#   grp_unique <- unique(grp_val)
#
#   # Create bootstrap samples for each group
#   bootstrap_samples <- lapply(grp_unique, function(g) {
#     data[sample(nrow(data), size = grp_sizes[g], replace = TRUE), ]
#   })
#
#
#   bind_boot <- do.call(rbind, bootstrap_samples)
#   new_group <- rep(grp_unique, times = sapply(bootstrap_samples, nrow))
#   bind_boot[[grp]] <- new_group
#
#   # Use tryCatch to handle potential errors in iht_boot
#   iht_boot <- tryCatch({
#     restriktor::iht(stats::lm(stats::as.formula(formula), data = bind_boot), constraints = constraints)
#   }, error = function(e) {
#     return(NULL)
#   })
#
#   # Check if iht_boot is NULL (indicating an error occurred)
#   if (is.null(iht_boot)) {
#     return(list(orig_Ts_B = NA, orig_Ts_A = NA, orig_Ts_F = NA))
#   }
#
#   boot_calc <- tryCatch({
#     list(orig_Ts_B = iht_boot$B$Ts,
#          orig_Ts_A = iht_boot$A$Ts,
#          orig_Ts_F = NULL)
#   }, error = function(e) {
#     return(NULL)
#   })
#
#
#   if (is.null(boot_calc)){
#     boot_calc <- list(orig_Ts_B = NULL,
#                      orig_Ts_A = NULL,
#                      orig_Ts_F = iht_boot$Ts)
#   }
#
#   return(boot_calc)
#
# }
