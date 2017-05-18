#' Title
#'
#' @param plots 
#'
#' @return
#' @export
#'
#' @examples
check_shiny.gam <- function(plots){
  ui <- miniPage(
    gadgetTitleBar("Checking results"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      fillRow(
        fillCol(
          plotOutput("plot1", height = "100%"),
          plotOutput("plot2", height = "100%")
        ),
        fillCol(
          plotOutput("plot3", height = "100%"),
          plotOutput("plot4", height = "100%")
        )
      )
    )
  )
  server <- function(input, output, session) {
    # Render the plot
    output$plot1 <- renderPlot({
      print(plots[[1]])
    })
    output$plot2 <- renderPlot({
      print(plots[[2]])
    })
    output$plot3 <- renderPlot({
      print(plots[[3]])
    })
    output$plot4 <- renderPlot({
      print(plots[[4]])
    })
  }
  runGadget(ui, server)
}