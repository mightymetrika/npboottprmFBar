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


  persimon(M1 = input$M1, S1 = input$S1, M2 = input$M2, S2 = input$S2,
           M3 = input$M3, S3 = input$S3,
           Sk1 = vec_null(input$Sk1), Sk2 = vec_null(input$Sk2),
           Sk3 = vec_null(input$Sk3),
           n1 = text_to_vector(input$n1), n2 = text_to_vector(input$n2),
           n3 = text_to_vector(input$n3),
           n_simulations = input$n_simulations, nboot = input$nboot,
           conf.level = input$conf.level)

}

text_to_vector <- function(text_input) {
  # Check if the input is a simple comma-separated string
  if (!grepl("^[^,]+$", text_input) && !grepl("[()]", text_input)) {
    text_input <- paste0("c(", text_input, ")")
  }
  eval(parse(text = text_input))
}

vec_null <- function(par_input = "", alt_na = NULL) {
  if (is.na(par_input) || par_input == "") {
    return(NULL)
  } else if (!is.null(alt_na)){
    if(par_input == alt_na) return (NULL)
  } else {
    return(text_to_vector(par_input))
  }
}
