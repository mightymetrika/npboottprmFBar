#' Nonparametric Bootstrap Test with Pooled Resampling for Informative Hypothesis Testing
#'
#' Performs a nonparametric bootstrap test with pooled resampling based on the methods
#' described by Dwivedi et al. (2017) and using the `restriktor::iht` function as
#' proposed by Vanbrabant and Rosseel (2020).
#'
#' @param data A data frame containing the dataset for analysis.
#' @param formula An object of class \code{"formula"} (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param grp A string specifying the grouping variable in the data.
#' @param constraints A matrix or data frame of constraints for the hypothesis test.
#' @param nboot An integer indicating the number of bootstrap resamples (default is 1000).
#' @param conf.level A numeric value specifying the confidence level for the interval
#' (default is 0.95).
#' @param seed An optional integer setting the seed for random number generation (default is NULL).
#' @param na_rm A logical value indicating whether NA values should be removed (default is FALSE).
#'
#' @return A list containing the following components:
#'
#'   -pvalueA, pvalueB, pvalueF: P-values for the different test types.
#'
#'   -ciA, ciB, ciF: Confidence intervals for the test statistics.
#'
#'   -TsA, TsB, TsF: Bootstrap test statistics.
#'
#'   -modelo: The linear model object.
#'
#'   -ihto: The initial iht computation result.
#'
#' @examples
#' bootFbar(data = iris, formula = Sepal.Length ~ -1 + Species,
#'          grp = "Species",
#'          constraints = 'Speciessetosa < Speciesversicolor < Speciesvirginica',
#'          nboot = 10, conf.level = 0.95, seed = NULL, na_rm = FALSE)
#'
#' @references
#' Dwivedi, A. K., Mallawaarachchi, I., & Alvarado, L. A. (2017). Analysis of
#' small sample size studies using nonparametric bootstrap test with pooled
#' resampling method. Statistics in Medicine, 36(14), 2187–2205.
#' https://doi.org/10.1002/sim.7263
#'
#' Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor: Evaluating
#' informative hypotheses for linear models. In R. van de Schoot & M. Miocevic
#' (Eds.), Small Sample Size Solutions: A Guide for Applied Researchers and
#' Practitioners (1st ed., pp. 157 -172). Routledge. https://doi.org/10.4324/9780429273872-14
#'
#' @export
bootFbar <- function (data, formula, grp, constraints, nboot = 1000, conf.level = 0.95,
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

  # Extract variable names from the formula
  vars_in_formula <- all.vars(stats::as.formula(formula))

  # Remove NA values if na_rm is TRUE, but only for the variables in the formula
  if (na_rm == TRUE) {
    subset_data <- data[, vars_in_formula, drop = FALSE]
    complete_cases <- stats::complete.cases(subset_data)
    data <- data[complete_cases, ]
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
              modelo = model, ihto = iht_orig))
}

#' Internal Bootstrap Resampling for F-bar Test
#'
#' This internal function performs bootstrap resampling specific to the F-bar test
#' within the context of the nonparametric bootstrap test with pooled resampling.
#' It is used by `bootFbar` to generate bootstrap samples and compute intermediate
#' statistics.
#'
#' @param data A data frame containing the dataset for analysis.
#' @param formula An object of class \code{"formula"}: a symbolic description of
#' the model to be fitted.
#' @param grp A string specifying the grouping variable in the data.
#' @param constraints A matrix or data frame of constraints for the hypothesis test.
#' @param grp_val A vector containing the values of the group variable.
#' @param grp_sizes A table object containing the sizes of each group.
#' @param pre_calc A list containing pre-calculated original test statistics.
#' @param only_equality_constraints A logical indicating whether only equality
#' constraints are used.
#'
#' @return A list containing the test statistics for each constraint type
#' (B, A, F) based on the bootstrap sample. Returns NA values if the iht
#' computation fails.
#'
#' @details
#' The function creates bootstrap samples for each group in the data, then binds
#' these samples together. It applies the `restriktor::iht` function on these
#' samples to compute the test statistics. Error handling is implemented to manage
#' potential issues in `iht` computation.
#'
#' @keywords internal
bootstrap_fbar_sample <- function(data, formula, grp, constraints, grp_val, grp_sizes, pre_calc, only_equality_constraints) {
  grp_unique <- unique(grp_val)

  # Create bootstrap samples for each group
  bootstrap_samples <- lapply(grp_unique, function(g) {
    data[sample(nrow(data), size = grp_sizes[g], replace = TRUE), ]
  })

  # Combine bootstrap samples and assign new groups
  bind_boot <- do.call(rbind, bootstrap_samples)
  new_group <- rep(grp_unique, times = sapply(bootstrap_samples, nrow))
  bind_boot[[grp]] <- new_group

  # Run model and use tryCatch to handle potential errors in iht_boot
  iht_boot <- tryCatch({
    restriktor::iht(stats::lm(stats::as.formula(formula), data = bind_boot), constraints = constraints)
  }, error = function(e) {
    return(NULL)
  })

  # Return result
  if (is.null(iht_boot)) {
    if (only_equality_constraints) {
      return(list(bsamp_Ts_F = NA))
    } else {
      return(list(bsamp_Ts_B = NA,
                  bsamp_Ts_A = NA))
    }
  }

  if (only_equality_constraints) {
    return(list(bsamp_Ts_F = iht_boot$Ts))
  } else {
    return(list(bsamp_Ts_B = iht_boot$B$Ts,
                bsamp_Ts_A = iht_boot$A$Ts))
  }
}
