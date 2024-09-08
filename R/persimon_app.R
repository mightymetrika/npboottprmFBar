#' Run Shiny Application for bootFbar Performance Simulation
#'
#' This function launches a 'shiny' application that allows users to run
#' performance simulations for the bootFbar method and other statistical tests.
#' The app provides an interface to set simulation parameters, run simulations,
#' view results, and store them in a 'PostgreSQL' database.
#'
#' @param dbname A string specifying the name of the 'PostgreSQL' database to
#' connect to.
#' @param datatable A string specifying the name of the table in the database
#' where results will be stored.
#' @param host A string specifying the host name or IP address of the 'PostgreSQL'
#' server.
#' @param port An integer specifying the port number on which the 'PostgreSQL'
#' server is listening.
#' @param user A string specifying the username for the 'PostgreSQL' database
#' connection.
#' @param password A string specifying the password for the 'PostgreSQL'
#' database connection.
#'
#' @return A 'shiny' application object.
#'
#' @details
#' The 'shiny' application provides a user interface for setting simulation
#' parameters, running simulations using the `persimon` function, and visualizing
#' the results. It allows users to:
#'
#' - Set parameters for the simulation (means, standard deviations, sample sizes, etc.)
#' - Run simulations with the specified parameters
#' - View simulation results and success rates in interactive tables
#' - Store simulation results in a PostgreSQL database
#' - Download stored results from the database
#' - View relevant citations for the methods used
#'
#' The application uses the `mmints` package for database interactions and
#' citation handling.
#'
#' @examples
#' if(interactive()){
#' persimon_app(dbname = "my_database", datatable = "simulation_results",
#'              host = "localhost", port = 5432,
#'              user = "username", password = "password")
#' }
#'
#' @seealso
#' \code{\link{persimon}} for the underlying simulation function.
#'
#' @export
persimon_app <- function(dbname, datatable, host, port, user, password){

  # define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("bootFbar Persimon"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        mmints::postgresUI("postgres")$submit,
        shiny::br(), shiny::br(), # Add a line break
        mmints::postgresUI("postgres")$download,
        mmints::citationUI("citations")$button
      ),
      shiny::mainPanel(
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::br(), shiny::br(), # Add a line break
        shiny::uiOutput("simulation_success_header"),
        DT::DTOutput("successTable"),
        shiny::br(), shiny::br(), # Add a line break
        shiny::h4("Database Content:"),
        mmints::postgresUI("postgres")$table,
        mmints::citationUI("citations")$output
      )
    )
  )

  server <- function(input, output, session) {

    # render the UI for parameters
    output$paramsUI <- shiny::renderUI({
      getUIParams()
    })

    # initialize the postgres module
    postgres_module <- mmints::postgresServer("postgres",
                                              dbname = dbname,
                                              datatable = datatable,
                                              host = host,
                                              port = port,
                                              user = user,
                                              password = password,
                                              data = NULL)

    # reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    success <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # make sure responses are clear
      results(data.frame())
      success(data.frame())

      # call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # update the results, success, and results_exp reactive values
      results(simResults$results)
      success(simResults$success)
      results_exp(appendInputParams(results(), success(), input))

      # submit results to database
      postgres_module$data_to_submit(results_exp())
    })

    # render the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # render the success table
    output$successTable <- DT::renderDT({
      success()
    }, options = list(pageLength = 5))

    # conditionally display the simulation results header
    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

    # conditionally display the simulation success header
    output$simulation_success_header <- shiny::renderUI({
      if (nrow(success()) > 0) {
        shiny::h4("Simulation Success")
      } else {
        NULL
      }
    })

    # build citation list
    citations <- list(
      "Nonparametric Bootstrap Test with Pooled Resampling Method:" = "Dwivedi, A. K., Mallawaarachchi, I., & Alvarado, L. A. (2017). Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Statistics in Medicine, 36(14), 2187-2205. https://doi.org/10.1002/sim.7263",
      "Software Implementing Nonparametric Bootstrap Test with Pooled Resampling:" = function() mmints::format_citation(utils::citation("npboottprm")),
      "Informative Hypothesis Testing Method:" = "Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor: Evaluating informative hypotheses for linear models. In R. van de Schoot & M. Miocevic (Eds.), Small Sample Size Solutions: A Guide for Applied Researchers and Practitioners (1st ed., pp. 157 -172). Routledge. https://doi.org/10.4324/9780429273872-14",
      "Software Implementing Informative Hypothesis Testing:" = function() mmints::format_citation(utils::citation("restriktor"))
    )

    # create citation for display
    mmints::citationServer("citations", citations)
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
