suppressWarnings(library(niivue))
library(shiny)

addResourcePath("images", "../images")

ui <- fluidPage(
  tags$head(tags$style(HTML("body {background-color: #202020; color: white;}"))),
  titlePanel("Tractography"),
  fluidRow(
    column(width = 9,
      tags$div(style = "width: 100%; height: 600px;", NiivueWidget$new()$plot)
    ),
    column(width = 3,
      sliderInput("fiberLengthSlider", "Length", min = 1, max = 80, value = 3),
      sliderInput("fiberDitherSlider", "Dither", min = 0, max = 10, value = 1),
      selectInput(
        "fiberColor", "Choose fiber coloration:",
        choices = c("Global direction", "Local direction", "Fixed",
                    "First Per Vertex Type (if available)",
                    "First Per Streamline Type (if available)")
      ),
      selectInput(
        "fiberDecimation", "Fiber reduction:",
        choices = c("100%" = 1, "50%" = 2, "25%" = 4, "10%" = 10)
      ),
      tags$div(
        style = "margin-top: 5px;",
        actionButton("saveButton", "Save Scene")
      )
    )
  )
)

server <- function(input, output, session) {
  volumeList <- list(list(url = "images/mni152.nii.gz"))
  meshList <- list(list(url = "images/dpsv.trx", rgba255 = c(0, 142, 0, 255)))
  nv <- NiivueWidget$new(list(
    show3Dcrosshair = TRUE,
    backColor = c(0.8, 0.8, 1, 1)
  ))

  nv$opts$isColorbar = TRUE
  nv$setSliceType(SLICE_TYPE$RENDER)
  nv$loadVolumes(volumeList)
  nv$loadMeshes(meshList)

  observeEvent(input$meshes, {
    nv$setMeshProperty(nv$meshes[[1]]$id, "colormap", "blue")
    nv$setClipPlane(c(-0.1, 270, 0))
  }, once = TRUE)

  observeEvent(input$fiberLengthSlider, {
    nv$setMeshProperty(nv$meshes[[1]]$id, "fiberLength", input$fiberLengthSlider)
  }, ignoreInit = TRUE)

  observeEvent(input$fiberDitherSlider, {
    nv$setMeshProperty(nv$meshes[[1]]$id, "fiberDither", input$fiberDitherSlider * 0.1)
  }, ignoreInit = TRUE)

  observeEvent(input$fiberColor, {
    nv$setMeshProperty(nv$meshes[[1]]$id, "fiberColor", input$fiberColor)
  }, ignoreInit = TRUE)

  observeEvent(input$fiberDecimation, {
    nv$setMeshProperty(nv$meshes[[1]]$id, "fiberDecimationStride", input$fiberDecimation)
  }, ignoreInit = TRUE)

  observeEvent(input$saveButton, {
    nv$saveScene()
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
