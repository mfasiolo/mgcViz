#'
#' Shiny QQ-plots for GAMs
#' 
#' @description This function takes the output of [qq.gamViz] and transforms it
#'              into an interactive shiny app.
#' @param o the output of [qq.gamViz].
#' @param ... currently not used.
#' @details In RStudio, this function returns a call to \code{qq.gamViz} that reproduces the last plot
#'         rendered in the interactive shiny window.
#' @importFrom shiny fillRow fillCol selectizeInput sliderInput plotOutput brushOpts reactiveValues reactive renderPlot observeEvent runGadget dialogViewer stopApp
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @export shine.qqGam
#' @export
#' @examples 
#' \dontrun{
#' 
#' ## simulate binomial data...
#' library(mgcv)
#' library(mgcViz)
#' set.seed(0)
#' n.samp <- 400
#' dat <- gamSim(1,n = n.samp, dist = "binary", scale = .33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1, 3), n.samp, replace = TRUE) ## binomial n
#' dat$y <- rbinom(n, n, p)
#' dat$n <- n
#' lr.fit <- gam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "REML")
#' lr.fit <- getViz(lr.fit)
#' # launch shiny gagdet
#' shine(qq(lr.fit))
#'  
#' }
#' 
shine.qqGam <- function(o, ...){
  name_obj <- deparse(substitute(o))
  ui <- miniPage(
    gadgetTitleBar("Q-Q GAM"),
    miniContentPanel(
      fillRow(flex = c(1, 4),
              fillCol(
                selectizeInput(
                  inputId = "shape",
                  label = "Point shape", choices = c(".", 1:25)),
                # colourpicker::colourInput( # Removed colourpicker import
                #   inputId = "color_line",
                #   label = "Line color",
                #   value = "red"
                # ),
                selectizeInput(
                  inputId = "ci",
                  label = "Conf. Int. ?",
                  choices = c(TRUE, FALSE),
                  selected = "FALSE"
                ),
                # colourpicker::colourInput(
                #   inputId = "color_CI",
                #   label = "CI color",
                #   value = "gray80"
                # ),
                selectizeInput(
                  inputId = "show_reps",
                  label = "Show repetitions ?",
                  choices = c(TRUE, FALSE),
                  selected = "FALSE"
                ),
                selectizeInput(
                  inputId = "worm",
                  label = "Worm-plot ?",
                  choices = c(TRUE, FALSE),
                  selected = "FALSE"
                ),
                # colourpicker::colourInput(
                #   inputId = "color_rep",
                #   label = "Color for rep.",
                #   value = "black"
                # ),
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
    shape <- reactive(
      if (input$shape %in% as.character(1:25)) {
        as.integer(input$shape)
      } else {
        input$shape
      }
    )
    output$plot <- renderPlot(
      zoom(o, xlim = ranges$x, ylim = ranges$y,
           CI = as.logical(input$ci),
           showReps = as.logical(input$show_reps),
           worm = as.logical(input$worm),
           a.qqpoi = list(shape = shape()),
           a.ablin = list(colour = "red"),
           a.cipoly = list(colour = "gray80"),
           a.replin = list(colour = "black", 
                           alpha = input$rep_alpha) )
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
      ## This produces a zoom() call, that can be used to reproduce the shiny plot
      ## Commented it out avoid importing rstudioapi
      # if (rstudioapi::isAvailable()){
      #   callText <- paste0(
      #     # get call as a character (dirty)
      #     "zoom(", paste(format(attr(o, "call")), collapse = ""), ", ",
      #     ifelse(!is.null(ranges$x),
      #            sprintf("xlim = %s, ", deparse(signif(ranges$x, 4))), ""),
      #     ifelse(!is.null(ranges$y),
      #            sprintf("ylim = %s, ", deparse(signif(ranges$y, 4))), ""),
      #     "CI = ", input$ci, ", ",
      #     "showReps = ", input$show_reps, ", ",
      #     "worm = ", input$worm, ", ",
      #     "a.replin = list(colour = \"", input$color_rep, "\", alpha = ", input$rep_alpha, "), ",
      #     "a.ablin = list(colour = \"", input$color_line, "\"), ",
      #     "a.cipoly = list(colour = \"", input$color_CI, "\"), ",
      #     "a.qqpoi = list(", ifelse(is.character(shape()), "shape = \".\"",
      #            sprintf("shape = %i", shape())),"))"
      #      )
      #   rstudioapi::insertText(callText)
      # }
      stopApp()
    })
  }
  runGadget(ui, server, viewer = dialogViewer(dialogName = "Q-Q GAM",
                                              height = 900, width = 900))
}

