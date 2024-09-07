getUIParams <- function() {

  list(shiny::numericInput("M1", "Mean for the first group:", 5),
       shiny::numericInput("S1", "Standard deviation for the first group:", 1),
       shiny::numericInput("M2", "Mean for the second group:", 5),
       shiny::numericInput("S2", "Standard deviation for the second group:", 1),
       shiny::numericInput("M3", "Mean for the third group:", 5),
       shiny::numericInput("S3", "Standard deviation for the third group:", 1),
       shiny::numericInput("Sk1", "Skew for the first group:", value = NA),
       shiny::numericInput("Sk2", "Skew for the second group:", value = NA),
       shiny::numericInput("Sk3", "Skew for the third group:", value = NA),
       shiny::textInput("n1", "Sample sizes for the first group:", "5"),
       shiny::textInput("n2", "Sample sizes for the second group:", "5"),
       shiny::textInput("n3", "Sample sizes for the third group:", "5"),
       shiny::numericInput("n_simulations", "Number of simulation iterations:", 10),
       shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
       shiny::numericInput("conf.level", "Confidence level:", 0.95))

}

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
      Sk1 = input$Sk1, Sk2 = input$Sk2,
      Sk3 = input$Sk3,
      n_simulations = input$n_simulations, nboot = input$nboot,
      conf.level = input$conf.level,
      RunCode = run_code, stringsAsFactors = FALSE
    )

  # Repeat the parameters data frame to match the number of rows in df
  params_df <- params_df[rep(1, nrow(df)), ]

  # Combine with the simulation results
  cbind(df, params_df)
}
