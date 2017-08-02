#' Shiny Gadget for qq_gam
#' @description ...
#' @import shiny
#' @import miniUI
#' @export
shiny_qq_gam <- function(obj){
  name_obj <- deparse(substitute(obj))
  ui <- miniPage(
    gadgetTitleBar("Q-Q GAM"),
    miniContentPanel(
      fillRow(flex = c(1, 4),
              fillCol(
                selectizeInput(
                  inputId = "shape",
                  label = "Point shape", choices = c(".", 1:25)),
                colourpicker::colourInput(
                  inputId = "color_line",
                  label = "Line color",
                  value = "red"
                ),
                selectizeInput(
                  inputId = "ci",
                  label = "Conf. Int. ?",
                  choices = c(TRUE, FALSE),
                  selected = "FALSE"
                ),
                colourpicker::colourInput(
                  inputId = "color_CI",
                  label = "CI color",
                  value = "gray80"
                ),
                selectizeInput(
                  inputId = "show_reps",
                  label = "Show repetitions ?",
                  choices = c(TRUE, FALSE),
                  selected = "FALSE"
                ),
                colourpicker::colourInput(
                  inputId = "color_rep",
                  label = "Color for rep.",
                  value = "black"
                ),
                sliderInput(
                  inputId = "rep_alpha",
                  label = "Alpha for rep.",
                  min = 0, max = 1,
                  step = 0.01,
                  value = 0.05
                )
              ),
              plotOutput("plot", height = "100%",
                         dblclick = "plot_dblclick",
                         brush = brushOpts(id = "plot_brush",
                                           resetOnNew = TRUE))
      )
    )
  )
  server <- function(input, output, session) {
    ranges <- reactiveValues(x = NULL, y = NULL)
    qq <- qq.gam(obj)
    shape <- reactive(
      if (input$shape %in% as.character(1:25)) {
        as.integer(input$shape)
      } else {
        input$shape
      }
    )
    output$plot <- renderPlot(
      zoom(qq, xlim = ranges$x, ylim = ranges$y,
           shape = shape(),
           CI = as.logical(input$ci),
           show.reps = as.logical(input$show_reps),
           rl.col = input$color_line,
           ci.col = input$color_CI,
           rep.col = input$color_rep,
           rep.alpha = input$rep_alpha)
    )
    observeEvent(input$plot_dblclick, {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    observeEvent(input$done, {
      if (rstudioapi::isAvailable()){
        callText <- paste0(
          "zoom(qq.gam(", name_obj, "), ",
          ifelse(!is.null(ranges$x),
                 sprintf("xlim = %s, ", deparse(signif(ranges$x, 4))), ""),
          ifelse(!is.null(ranges$y),
                 sprintf("ylim = %s, ", deparse(signif(ranges$y, 4))), ""),
          "CI = ", input$ci, ", ",
          "show.reps = ", input$show_reps, ", ",
          "rep.col = \"", input$color_rep, "\", ",
          "rep.alpha = ", input$rep_alpha, ", ",
          "rl.col = \"", input$color_line, "\", ",
          ifelse(is.character(shape()), "shape = \".\", ",
                 sprintf("shape = %i, ", shape())),
          "ci.col = \"", input$color_CI,
          "\")")
        rstudioapi::insertText(callText)
      }
      stopApp()
    })
  }
  runGadget(ui, server, viewer = dialogViewer(dialogName = "Q-Q GAM",
                                              height = 900, width = 900))
}

