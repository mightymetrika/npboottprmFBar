persimon_app <- function(){

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("bootFbar Persimon"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # shiny::selectInput("cellBlock", "Select Cell Block:",
        #                    choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        # shiny::actionButton("submit", "Submit"),
        # shiny::br(),  # Add a line break
        # shiny::br(),  # Add a line break
        # shiny::downloadButton("downloadBtn", "Download Responses"),
        # shiny::actionButton("show_citations", "Citations")
      ),
      shiny::mainPanel(
        # # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::uiOutput("simulation_success_header"),
        DT::DTOutput("successTable"),
        # shiny::br(),  # Add a line break
        # shiny::br(),  # Add a line break
        # # Add a header for the responses table
        # shiny::div(
        #   shiny::h4("All Responses"),
        #   DT::DTOutput("responses")
        # ),
        # shiny::uiOutput("citation_header"),
        # shiny::verbatimTextOutput("citations_output")
      )
    )
  )

  server <- function(input, output, session) {

    # Render the UI for parameters
    output$paramsUI <- shiny::renderUI({
      getUIParams()
    })

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    success <- shiny::reactiveVal(data.frame())     #For display
    # results_exp <- shiny::reactiveVal(data.frame()) #For export

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
      # results_exp(simResults)
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    output$successTable <- DT::renderDT({
      success()
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
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
