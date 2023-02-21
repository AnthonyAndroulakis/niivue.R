#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
niivue <- function(
    textHeight = 0.06,
    colorbarHeight = 0.05,
    colorbarMargin = 0.05,
    crosshairWidth = 1,
    rulerWidth = 4,
    backColor = c(0, 0, 0, 1), 
    crosshairColor = c(1, 0, 0, 1), 
    fontColor = c(0.5, 0.5, 0.5, 1),
    selectionBoxColor = c(1, 1, 1, 0.5),
    clipPlaneColor = c(1, 1, 1, 0.5),
    rulerColor = c(1, 0, 0, 0.8),
    show3Dcrosshair = FALSE,
    trustCalMinMax = TRUE,
    clipPlaneHotKey = "KeyC", 
    viewModeHotKey = "KeyV",
    keyDebounceTime = 50, 
    doubleTouchTimeout = 500,
    longTouchTimeout = 1000, 
    isRadiologicalConvention = FALSE, 
    loadingText = "waiting on images...",
    dragAndDropEnabled = TRUE, 
    isNearestInterpolation = FALSE,
    isAtlasOutline = FALSE, 
    isRuler = FALSE, 
    isColorbar = FALSE, 
    isOrientCube = FALSE, 
    multiplanarPadPixels = 0, 
    multiplanarForceRender = FALSE, 
    meshThicknessOn2D = Inf,
    dragMode = 1,
    isDepthPickMesh = FALSE, 
    isCornerOrientationText = FALSE, 
    sagittalNoseLeft = FALSE, 
    isSliceMM = FALSE, 
    isHighResolutionCapable = TRUE, 
    drawingEnabled = FALSE, 
    penValue = Inf,
    isFilledPen = FALSE, 
    maxDrawUndoBitmaps = 8, 
    thumbnail = "",
    width = NULL, 
    height = NULL) {

  # forward options using x
  x = list(
    textHeight = textHeight,
    colorbarHeight = colorbarHeight,
    colorbarMargin = colorbarMargin,
    crosshairWidth = crosshairWidth,
    rulerWidth = rulerWidth,
    backColor = backColor,
    crosshairColor = crosshairColor,
    fontColor = fontColor,
    selectionBoxColor = selectionBoxColor,
    clipPlaneColor = clipPlaneColor,
    rulerColor = rulerColor,
    show3Dcrosshair = show3Dcrosshair,
    trustCalMinMax = trustCalMinMax,
    clipPlaneHotKey = clipPlaneHotKey,
    viewModeHotKey = viewModeHotKey,
    keyDebounceTime = keyDebounceTime,
    doubleTouchTimeout = doubleTouchTimeout,
    longTouchTimeout = longTouchTimeout,
    isRadiologicalConvention = isRadiologicalConvention,
    loadingText = loadingText,
    dragAndDropEnabled = dragAndDropEnabled,
    isNearestInterpolation = isNearestInterpolation,
    isAtlasOutline = isAtlasOutline,
    isRuler = isRuler,
    isColorbar = isColorbar,
    isOrientCube = isOrientCube,
    multiplanarPadPixels = multiplanarPadPixels,
    multiplanarForceRender = multiplanarForceRender,
    meshThicknessOn2D = meshThicknessOn2D,
    dragMode = dragMode,
    isDepthPickMesh = isDepthPickMesh,
    isCornerOrientationText = isCornerOrientationText,
    sagittalNoseLeft = sagittalNoseLeft,
    isSliceMM = isSliceMM,
    isHighResolutionCapable = isHighResolutionCapable,
    drawingEnabled = drawingEnabled,
    penValue = penValue,
    isFilledPen = isFilledPen,
    maxDrawUndoBitmaps = maxDrawUndoBitmaps,
    thumbnail = thumbnail
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'niivue',
    x,
    width = width,
    height = height,
    package = 'niivue'
  )
  
  #return proxy
  #return(niivueProxy("testing123"))
}

#' Shiny bindings for niivue
#'
#' Output and render functions for using niivue within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a niivue
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name niivue-shiny
#'
#' @export
niivueOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'niivue', width, height, package = 'niivue')
}

#' @rdname niivue-shiny
#' @export
renderNiivue <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, niivueOutput, env, quoted = TRUE)
}

addVolume <- function(nv, path) {
  print(nv)
  print(path)
  sendCustomMsg(nv, c(path))
}

niivueProxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  if (is.null(session)) {
    stop("niivueProxy must be called from the server function of a Shiny app")
  }

  object        <- list( id = id, session = session )
  class(object) <- "NiivueProxy"

  return(object)
}

sendCustomMsg <- function(nv, data = c(), buffers = c()) {
  nvP <- niivueProxy()
  msg <- list(
      id = nv$id,
      metadata = data,
      buffers = buffers
  )
  print(nv$session)
  session = shiny::getDefaultReactiveDomain()
  print(session)
  nv$session$sendCustomMessage("niivue-calls", msg)
}
