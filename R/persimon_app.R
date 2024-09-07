persimon_app <- function(dbname, datatable, host, port, user, password){

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("bootFbar Persimon"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        mmints::postgresUI("postgres")$submit,
        shiny::br(), shiny::br(), # Add a line break
        mmints::postgresUI("postgres")$download,
        # shiny::br(), shiny::br(), # Add a line break
        mmints::citationUI("citations")$button
      ),
      shiny::mainPanel(
        # # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::uiOutput("simulation_success_header"),
        DT::DTOutput("successTable"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h4("Database Content:"),
        mmints::postgresUI("postgres")$table,
        mmints::citationUI("citations")$output
      )
    )
  )

  server <- function(input, output, session) {

    # Render the UI for parameters
    output$paramsUI <- shiny::renderUI({
      getUIParams()
    })

    # Get database connection details from environment variables
    # db_config <- list(
    #   dbname = Sys.getenv("DBNAME"),
    #   datatable = Sys.getenv("DATATABLE"),
    #   host = Sys.getenv("HOST"),
    #   port = as.integer(Sys.getenv("PORT")),
    #   user = Sys.getenv("USER"),
    #   password = Sys.getenv("PASSWORD")
    # )

    # Initialize the postgres module
    postgres_module <- mmints::postgresServer("postgres",
                                              dbname = dbname,
                                              datatable = datatable,
                                              host = host,
                                              port = port,
                                              user = user,
                                              password = password,
                                              data = NULL)

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    success <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # make sure responses are clear
      results(data.frame())
      success(data.frame())

      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # Update the results reactive value
      results(simResults$results)
      success(simResults$success)
      results_exp(appendInputParams(results(), success(), input))
      postgres_module$data_to_submit(results_exp())
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    output$successTable <- DT::renderDT({
      success()
    }, options = list(pageLength = 5))

    output$expTable <- DT::renderDT({
      results_exp()
    }, options = list(pageLength = 5))

    # Conditionally display the Simulation Results header
    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

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
