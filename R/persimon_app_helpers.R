#' Generate UI Parameters for persimon_app Simulation
#'
#' This internal function creates a list of 'shiny' input elements for the
#' simulation parameters. It defines the UI components for means, standard
#' deviations, skewness, sample sizes, number of simulations, number of bootstrap
#' iterations, and confidence level.
#'
#' @return A list of 'shiny' input elements including:
#'   \item{M1, M2, M3}{Numeric inputs for means of the three groups}
#'   \item{S1, S2, S3}{Numeric inputs for standard deviations of the three groups}
#'   \item{Sk1, Sk2, Sk3}{Numeric inputs for skewness of the three groups}
#'   \item{n1, n2, n3}{Text inputs for sample sizes of the three groups}
#'   \item{n_simulations}{Numeric input for number of simulation iterations}
#'   \item{nboot}{Numeric input for number of bootstrap iterations}
#'   \item{conf.level}{Numeric input for confidence level}
#'
#' @details
#' This function is used internally by the 'shiny' app to generate the
#' UI elements for parameter input.
#'
#' @keywords internal
getUIParams <- function() {

  list(shiny::numericInput("M1", "Mean for the first group:", 5),
       shiny::numericInput("S1", "Standard deviation for the first group:", 1),
       shiny::numericInput("M2", "Mean for the second group:", 5),
       shiny::numericInput("S2", "Standard deviation for the second group:", 1),
       shiny::numericInput("M3", "Mean for the third group:", 5),
       shiny::numericInput("S3", "Standard deviation for the third group:", 1),
       shiny::numericInput("Sk1", "Skew for the first group:", value = NA_real_),
       shiny::numericInput("Sk2", "Skew for the second group:", value = NA_real_),
       shiny::numericInput("Sk3", "Skew for the third group:", value = NA_real_),
       shiny::textInput("n1", "Sample sizes for the first group:", "5"),
       shiny::textInput("n2", "Sample sizes for the second group:", "5"),
       shiny::textInput("n3", "Sample sizes for the third group:", "5"),
       shiny::numericInput("n_simulations", "Number of simulation iterations:", 10),
       shiny::numericInput("nboot", "Number of bootstrap iterations:", 100),
       shiny::numericInput("conf.level", "Confidence level:", 0.95))

}

#' Run Persimon Simulation with User Inputs
#'
#' This internal function executes the persimon simulation using parameters
#' provided by the user through the persimon_app 'shiny' app interface.
#'
#' @param input A list of input values from the 'shiny' app.
#'
#' @return A list containing two data frames:
#'   \item{results}{Data frame with proportions of rejecting the null hypothesis}
#'   \item{success}{Data frame with counts of successful model runs}
#'
#' @details
#' This function serves as a wrapper around the `persimon` function, translating
#' user inputs from the persimon_app 'shiny' app into the format required by
#' `persimon`. It uses helper functions from the `mmints` package to process
#' certain inputs.
#'
#' @seealso
#' \code{\link{persimon}} for the underlying simulation function.
#'
#' @keywords internal
runSimulation <- function(input) {

  persimon(M1 = input$M1, S1 = input$S1,
           M2 = input$M2, S2 = input$S2,
           M3 = input$M3, S3 = input$S3,
           Sk1 = mmints::vec_null(input$Sk1),
           Sk2 = mmints::vec_null(input$Sk2),
           Sk3 = mmints::vec_null(input$Sk3),
           n1 = mmints::text_to_vector(input$n1),
           n2 = mmints::text_to_vector(input$n2),
           n3 = mmints::text_to_vector(input$n3),
           n_simulations = input$n_simulations, nboot = input$nboot,
           conf.level = input$conf.level)

}

#' Append Input Parameters to persimon_app Simulation Results
#'
#' This internal function combines simulation results with success rates and
#' input parameters. It prepares the data for storage in the database by appending
#' all relevant information from a single simulation run.
#'
#' @param df_results A data frame containing the main simulation results.
#' @param df_success A data frame containing the counts of successful model runs.
#' @param input A list of input values from the Shiny app, including simulation parameters.
#'
#' @return A data frame combining simulation results, success rates, and input
#' parameters.
#' The returned data frame includes:
#'   \itemize{
#'     \item All columns from df_results
#'     \item Success rate columns from df_success (appended with '_success')
#'     \item Input parameters (M1, S1, M2, S2, M3, S3, Sk1, Sk2, Sk3, n_simulations,
#'     nboot, conf_level)
#'     \item A unique RunCode for the simulation
#'   }
#'
#' @details
#' This function performs several steps:
#' 1. Appends '_success' to the column names of df_success (except n1, n2, n3).
#' 2. Combines df_results with the modified df_success.
#' 3. Generates a unique run code for the simulation.
#' 4. Creates a data frame of input parameters.
#' 5. Repeats the input parameters to match the number of rows in the results.
#' 6. Combines all the above into a single data frame.
#'
#' @keywords internal
appendInputParams <- function(df_results, df_success, input) {

  # get unique columns from df_success and append with '_success'
  df_success <- df_success[,setdiff(names(df_success), c("n1", "n2", "n3"))]
  colnames(df_success) <- paste0(colnames(df_success), "_success")

  # combine df_results with df_success
  df <- cbind(df_results, df_success)

  # Generate a unique code for the simulation run
  run_code <- mmints::generateRunCode()

  # Create a data frame of input parameters
    params_df <- data.frame(
      M1 = input$M1, S1 = input$S1, M2 = input$M2, S2 = input$S2,
      M3 = input$M3, S3 = input$S3,
      Sk1 = input$Sk1, Sk2 = input$Sk2, Sk3 = input$Sk3,
      n_simulations = input$n_simulations, nboot = input$nboot,
      conf_level = input$conf.level,
      RunCode = run_code, stringsAsFactors = FALSE
    )

  # Repeat the parameters data frame to match the number of rows in df
  params_df <- params_df[rep(1, nrow(df)), ]

  # Combine with the simulation results
  cbind(df, params_df)
}
