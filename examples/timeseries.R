suppressWarnings(library(niivue))
library(shiny)

addResourcePath("images", "../images")

ui <- fluidPage(
  tags$head(tags$style(HTML("body {background-color: #202020; color: white;}"))),
  titlePanel("Time Series Demo"),
  fluidRow(
    splitLayout(
      cellWidths = c("80%", "20%"),
      tags$div(style = "width: 100%; height: 600px;", NiivueWidget$new()$plot),
      tags$div(
        tags$header(
          checkboxInput("normalizeCheckbox", "Normalize Graph", value = FALSE),
          actionButton("prevVolume", "Back"),
          actionButton("nextVolume", "Forward"),
          tags$div(
            style = "margin-top: 5px;",
            actionButton("animateButton", "Animate")
          ),
          tags$div(
            style = "margin-top: 5px;",
            actionButton("saveButton", "Save Scene")
          )
        )
      )
    )
  ),
  tags$footer(textOutput("locationString"))
)

server <- function(input, output, session) {
  volumeList <- list(
    list(url = "images/pcasl.nii.gz", colormap = "gray", opacity = 1, visible = TRUE, frame4D = 2)
  )

  nv <- NiivueWidget$new(list(
    thumbnail = "images/pcasl.png",
    onLocationChange = function(d) {
      output$locationString <- renderText(d$string)
    }
  ))
  nv$setRadiologicalConvention(FALSE)
  nv$loadVolumes(volumeList)
  nv$setSliceType(SLICE_TYPE$MULTIPLANAR)
  nv$graph$autoSizeMultiplanar <- TRUE
  nv$opts$multiplanarForceRender <- TRUE
  nv$graph$normalizeValues <- FALSE
  nv$graph$opacity <- 1.0

  currentVol <- 0
  
  # Add reactive value to store animation status
  animationStatus <- reactiveVal(FALSE)

  observeEvent(input$normalizeCheckbox, {
    nv$graph$normalizeValues <- input$normalizeCheckbox
  }, ignoreInit = TRUE)

  observeEvent(input$prevVolume, {
    currentVol <<- max(currentVol - 1, 0)
    nv$setFrame4D(nv$volumes[[1]]$id, currentVol)
  }, ignoreInit = TRUE)

  observeEvent(input$nextVolume, {
    currentVol <<- currentVol + 1
    currentVol <<- min(currentVol, nv$getFrame4D(nv$volumes[[1]]$id) - 1)
    nv$setFrame4D(nv$volumes[[1]]$id, currentVol)
  }, ignoreInit = TRUE)
  
  observeEvent(input$animateButton, {
    if (animationStatus()) {
      animationStatus(FALSE)
    } else {
      animationStatus(TRUE)
    }
  }, ignoreInit = TRUE)
  
  animationTimer <- reactiveTimer(100) 
  
  observe({
    if (animationStatus()) {
      animationTimer()
      currentVol <<- currentVol + 1
      if (currentVol >= nv$getFrame4D(nv$volumes[[1]]$id)) currentVol <<- 0
      nv$setFrame4D(nv$volumes[[1]]$id, currentVol)
      invalidateLater(100, session)
    }
  })

  observeEvent(input$saveButton, {
    nv$saveScene()
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)