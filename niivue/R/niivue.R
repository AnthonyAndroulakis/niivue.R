# Import packages
#' @import htmlwidgets
#' @import R6
#' @import shiny
#' @import uuid
#' @import jsonlite
library(htmlwidgets)
library(R6)
library(shiny)
library(uuid)
library(jsonlite)


#' @export
SLICE_TYPE <- list(
  AXIAL = 0,
  CORONAL = 1,
  SAGITTAL = 2,
  MULTIPLANAR = 3,
  RENDER = 4
)

#' @export
DRAG_MODE <- list(
  none = 0,
  contrast = 1,
  measurement = 2,
  pan = 3,
  slicer3D = 4,
  callbackOnly = 5
)

DEFAULT_OPTIONS <- list(
  textHeight = 0.06,
  colorbarHeight = 0.05, 
  crosshairWidth = 1,
  rulerWidth = 4,
  show3Dcrosshair = FALSE,
  backColor = c(0, 0, 0, 1),
  crosshairColor = c(1, 0, 0, 1),
  fontColor = c(0.5, 0.5, 0.5, 1),
  selectionBoxColor = c(1, 1, 1, 0.5),
  clipPlaneColor = c(0.7, 0, 0.7, 0.5),
  rulerColor = c(1, 0, 0, 0.8),
  colorbarMargin = 0.05, 
  trustCalMinMax = TRUE,
  clipPlaneHotKey = "KeyC",
  viewModeHotKey = "KeyV",
  doubleTouchTimeout = 500,
  longTouchTimeout = 1000,
  keyDebounceTime = 50,
  isNearestInterpolation = FALSE,
  isResizeCanvas = TRUE,
  isAtlasOutline = FALSE,
  isRuler = FALSE,
  isColorbar = FALSE,
  isOrientCube = FALSE,
  multiplanarPadPixels = 0,
  multiplanarForceRender = FALSE,
  isRadiologicalConvention = FALSE,
  meshThicknessOn2D = Inf,
  dragMode = DRAG_MODE$contrast,
  isDepthPickMesh = FALSE,
  isCornerOrientationText = FALSE,
  sagittalNoseLeft = FALSE,
  isSliceMM = FALSE,
  isHighResolutionCapable = TRUE,
  logging = FALSE,
  loadingText = "waiting for images...",
  dragAndDropEnabled = TRUE,
  drawingEnabled = FALSE,
  penValue = 1,
  floodFillNeighbors = 6,
  isFilledPen = FALSE,
  thumbnail = "",
  maxDrawUndoBitmaps = 8,
  sliceType = SLICE_TYPE$MULTIPLANAR,
  meshXRay = 0.0,
  isAntiAlias = NULL,
  limitFrames4D = NaN,
  isAdditiveBlend = FALSE
)

MESH_EXTENSIONS <- c(
  "ASC",
  "BYU",
  "DFS",
  "FSM",
  "PIAL",
  "ORIG",
  "INFLATED",
  "SMOOTHWM",
  "SPHERE",
  "WHITE",
  "G",
  "GEO",
  "GII",
  "ICO",
  "MZ3",
  "NV",
  "OBJ",
  "OFF",
  "PLY",
  "SRF",
  "STL",
  "TCK",
  "TRACT",
  "TRI",
  "TRK",
  "TRX",
  "VTK",
  "X3D",
  "JCON",
  "JSON"
);

optsListToReactiveValues <- function(input_list) {
  allowed_keys <- names(DEFAULT_OPTIONS)
  filtered_list <- input_list[names(input_list) %in% allowed_keys]
  
  updated_options <- DEFAULT_OPTIONS
  
  for (key in names(filtered_list)) {
    updated_options[[key]] <- filtered_list[[key]]
  }
  
  reactive_values <- do.call(shiny::reactiveValues, updated_options)
  reactive_values
}

#' @export
niivue <- function(options = list()) {
  
  # Create the widget
  widget <- htmlwidgets::createWidget(
    name = "niivue",
    options,
    package = "niivue",
    width = "100%",
    height = "100%"
  )
  widget
}

# Create the NiivueWidget R6 class
#' @export
NiivueWidget <- R6::R6Class(
  "NiivueWidget",
  private = list(
    add_msg_handlers = function() {
      shiny::observeEvent(self$session$input$volumes, {
        volumes_list <- jsonlite::fromJSON(self$session$input$volumes, simplifyVector = TRUE)

        self$volumes <- lapply(volumes_list, function(x) {
          volume_data <- x[[1]]
          img_data <- x[[2]][[1]]
          volume_data$img <- "Binary img data access is not supported." #decompress(img_data)
          volume_data
        })
      })
      shiny::observeEvent(self$session$input$overlays, {
        overlays_list <- jsonlite::fromJSON(self$session$input$overlays, simplifyVector = TRUE)

        self$overlays <- lapply(overlays_list, function(x) {
          volume_data <- x[[1]]
          img_data <- x[[2]][[1]]
          volume_data$img <- "Binary img data access is not supported." #decompress(img_data)
          volume_data
        })
      })
      shiny::observeEvent(self$session$input$meshes, {
        meshes_list <- jsonlite::fromJSON(self$session$input$meshes, simplifyVector = TRUE)

        self$meshes <- lapply(meshes_list, function(x) {
          mesh_data <- x[[1]]
          img_data <- x[[2]][[1]][[1]]
          mesh_data$img <- "Binary img data access is not supported." #decompress(img_data)
          mesh_data
        })
      })
      shiny::observeEvent(self$session$input$mediaUrlMap, {
        self$mediaUrlMap <- jsonlite::fromJSON(self$session$input$mediaUrlMap, simplifyVector = TRUE)
      })
      shiny::observe({
        if (!is.null(self$session$sendCustomMessage) & !is.null(self$opts) & !is.null(self$graph)) {
          self$session$sendCustomMessage("opts", shiny::reactiveValuesToList(self$opts))
          self$session$sendCustomMessage("graph", shiny::reactiveValuesToList(self$graph))
          self$session$sendCustomMessage("thumbnailVisible", self$thumbnailVisible())
        }
      })
      shiny::observeEvent(self$session$input$onLocationChange, {
        if (!is.null(self$onLocationChange)) {
          self$onLocationChange(jsonlite::fromJSON(self$session$input$onLocationChange, simplifyVector = TRUE))
        }
      })
    },
    send_msg = function(method, d, id = NULL) {
      modifiedData <- list(method = method, args = c(0, d), id = id)
      self$session$sendCustomMessage("method", modifiedData)
    },
    gen_id = function() {
      uuid::UUIDgenerate()
    },
    colormap_names = c("_itksnap", "_slicer3d", "actc", "afni_blues_inv", "afni_reds_inv", "bcgwhw", "bcgwhw_dark", "blue", "blue2cyan", "blue2magenta", "blue2red", "bluegrn", "bone", "bronze", "cet_l17", "cividis", "cool", "copper", "copper2", "ct_airways", "ct_artery", "ct_bones", "ct_brain", "ct_brain_gray", "ct_cardiac", "ct_head", "ct_kidneys", "ct_liver", "ct_muscles", "ct_scalp", "ct_skull", "ct_soft", "ct_soft_tissue", "ct_surface", "ct_vessels", "ct_w_contrast", "cubehelix", "electric_blue", "freesurfer", "ge_color", "gold", "gray", "green", "green2cyan", "green2orange", "hot", "hotiron", "hsv", "inferno", "jet", "linspecer", "magma", "mako", "nih", "plasma", "random", "red", "redyell", "rocket", "roi_i256", "surface", "turbo", "violet", "viridis", "warm", "winter", "x_rain"),
    mesh_shader_names = c("Phong", "Matte", "Harmonic", "Hemispheric", "Edge", "Outline", "Toon", "Flat", "Matcap")
  ),
  public = list(
    widget = NULL,
    session = NULL,
    volumes = NULL,
    meshes = NULL,
    overlays = NULL,
    mediaUrlMap = NULL,
    opts = NULL,
    graph = NULL,
    thumbnailVisible = NULL,
    onLocationChange = NULL,
    initialize = function(opts = list() ) {
      if ("onLocationChange" %in% names(opts) & is.function(opts$onLocationChange)) {
        self$onLocationChange = opts$onLocationChange
      }
      self$widget <- niivue(opts)
      self$opts <- optsListToReactiveValues(opts)
      self$session = shiny::getDefaultReactiveDomain()
      private$add_msg_handlers()

      self$graph <- shiny::reactiveValues(
        LTWH = c(0, 0, 640, 480),
        opacity = 0.0,
        vols = c(0),
        autoSizeMultiplanar = FALSE,
        normalizeValues = FALSE
      )

      self$thumbnailVisible <- shiny::reactiveVal(FALSE)
    },

    #actions
    saveScene = function(filename = "niivue.png") {
      private$send_msg("saveScene", list(filename))
      invisible(self)
    },
    on = function(event, callback) {
      private$send_msg("on", list(event, callback))
      invisible(self)
    },
    off = function(event) {
      private$send_msg("off", list(event))
      invisible(self)
    },
    syncWith = function(otherNV, syncOpts = list(
      `2d` = TRUE,
      `3d` = TRUE
    )) {
      private$send_msg("syncWith", list(otherNV, syncOpts))
      invisible(self)
    },
    addVolumeFromUrl = function(imageOptions) {
      private$send_msg("addVolumeFromUrl", list(imageOptions))
      invisible(self)
    },
    removeVolumeByUrl = function(url) {
      private$send_msg("removeVolumeByUrl", list(url))
      invisible(self)
    },
    setCornerOrientationText = function(isCornerOrientationText) {
      private$send_msg("setCornerOrientationText", list(isCornerOrientationText))
      invisible(self)
    },
    setRadiologicalConvention = function(isRadiologicalConvention) {
      private$send_msg("setRadiologicalConvention", list(isRadiologicalConvention))
      invisible(self)
    },
    setDefaults = function(options = {}, resetBriCon = FALSE) {
      private$send_msg("setDefaults", list(options, resetBriCon))
      invisible(self)
    },
    setMeshThicknessOn2D = function(meshThicknessOn2D) {
      private$send_msg("setMeshThicknessOn2D", list(meshThicknessOn2D))
      invisible(self)
    },
    setSliceMosaicString = function(str) {
      private$send_msg("setSliceMosaicString", list(str))
      invisible(self)
    },
    setSliceMM = function(isSliceMM) {
      private$send_msg("setSliceMM", list(isSliceMM))
      invisible(self)
    },
    setAdditiveBlend = function(isAdditiveBlend) {
      private$send_msg("setAdditiveBlend", list(isAdditiveBlend))
      invisible(self)
    },
    setHighResolutionCapable = function(isHighResolutionCapable) {
      private$send_msg("setHighResolutionCapable", list(isHighResolutionCapable))
      invisible(self)
    },
    addVolume = function(volume) {
      private$send_msg("addVolume", list(volume))
      invisible(self)
    },
    addMesh = function(mesh) {
      private$send_msg("addMesh", list(mesh))
      invisible(self)
    },
    drawUndo = function() {
      private$send_msg("drawUndo", list())
      invisible(self)
    },
    loadDrawingFromUrl = function(fnm, isBinarize = FALSE) {
      private$send_msg("loadDrawingFromUrl", list(fnm, isBinarize))
      invisible(self)
    },
    drawOtsu = function(levels = 2) {
      private$send_msg("drawOtsu", list(levels))
      invisible(self)
    },
    removeHaze = function(level = 5, volIndex = 0) {
      private$send_msg("removeHaze", list(level, volIndex))
      invisible(self)
    },
    saveImage = function(fnm, isSaveDrawing = FALSE) {
      private$send_msg("saveImage", list(fnm, isSaveDrawing))
      invisible(self)
    },
    setMeshProperty = function(id, key, val) {
      private$send_msg("setMeshProperty", list(id, key, val))
      invisible(self)
    },
    reverseFaces = function(mesh) {
      private$send_msg("reverseFaces", list(mesh))
      invisible(self)
    },
    setMeshLayerProperty = function(mesh, layer, key, val) {
      private$send_msg("setMeshLayerProperty", list(mesh, layer, key, val))
      invisible(self)
    },
    setPan2Dxyzmm = function(xyzmmZoom) {
      private$send_msg("setPan2Dxyzmm", list(xyzmmZoom))
      invisible(self)
    },
    setRenderAzimuthElevation = function(a, e) {
      private$send_msg("setRenderAzimuthElevation", list(a, e))
      invisible(self)
    },
    setVolume = function(volume, toIndex = 0) {
      private$send_msg("setVolume", list(volume, toIndex))
      invisible(self)
    },
    removeVolume = function(volume) {
      private$send_msg("removeVolume", list(volume))
      invisible(self)
    },
    removeVolumeByIndex = function(index) {
      private$send_msg("removeVolumeByIndex", list(index))
      invisible(self)
    },
    removeMesh = function(mesh) {
      private$send_msg("removeMesh", list(mesh))
      invisible(self)
    },
    removeMeshByUrl = function(url) {
      private$send_msg("removeMeshByUrl", list(url))
      invisible(self)
    },
    moveVolumeToBottom = function(volume) {
      private$send_msg("moveVolumeToBottom", list(volume))
      invisible(self)
    },
    moveVolumeUp = function(volume) {
      private$send_msg("moveVolumeUp", list(volume))
      invisible(self)
    },
    moveVolumeDown = function(volume) {
      private$send_msg("moveVolumeDown", list(volume))
      invisible(self)
    },
    moveVolumeToTop = function(volume) {
      private$send_msg("moveVolumeToTop", list(volume))
      invisible(self)
    },
    setClipPlane = function(depthAzimuthElevation) {
      private$send_msg("setClipPlane", list(depthAzimuthElevation))
      invisible(self)
    },
    setCrosshairColor = function(color) {
      private$send_msg("setCrosshairColor", list(color))
      invisible(self)
    },
    setCrosshairWidth = function(crosshairWidth) {
      private$send_msg("setCrosshairWidth", list(crosshairWidth))
      invisible(self)
    },
    setDrawingEnabled = function(trueOrFalse) {
      private$send_msg("setDrawingEnabled", list(trueOrFalse))
      invisible(self)
    },
    setPenValue = function(penValue, isFilledPen = FALSE) {
      private$send_msg("setPenValue", list(penValue, isFilledPen))
      invisible(self)
    },
    setDrawOpacity = function(opacity) {
      private$send_msg("setDrawOpacity", list(opacity))
      invisible(self)
    },
    setSelectionBoxColor = function(color) {
      private$send_msg("setSelectionBoxColor", list(color))
      invisible(self)
    },
    setSliceType = function(st) {
      private$send_msg("setSliceType", list(st))
      invisible(self)
    },
    setOpacity = function(volIdx, newOpacity) {
      private$send_msg("setOpacity", list(volIdx, newOpacity))
      invisible(self)
    },
    setScale = function(scale) {
      private$send_msg("setScale", list(scale))
      invisible(self)
    },
    setClipPlaneColor = function(color) {
      private$send_msg("setClipPlaneColor", list(color))
      invisible(self)
    },
    setVolumeRenderIllumination = function(gradientAmount = 0.0) {
      private$send_msg("setVolumeRenderIllumination", list(gradientAmount))
      invisible(self)
    },
    loadDocumentFromUrl = function(url) {
      private$send_msg("loadDocumentFromUrl", list(url))
      invisible(self)
    },
    loadDocument = function(document) {
      private$send_msg("loadDocument", list(document))
      invisible(self)
    },
    saveDocument = function(fileName = "untitled.nvd") {
      private$send_msg("saveDocument", list(fileName))
      invisible(self)
    },
    loadVolumes = function(volumeList) {
      private$send_msg("loadVolumes", list(volumeList))
      invisible(self)
    },
    addMeshFromUrl = function(meshOptions) {
      private$send_msg("addMeshFromUrl", list(meshOptions))
      invisible(self)
    },
    loadMeshes = function(meshList) {
      private$send_msg("loadMeshes", list(meshList))
      invisible(self)
    },
    loadConnectome = function(json) {
      private$send_msg("loadConnectome", list(json))
      invisible(self)
    },
    createEmptyDrawing = function() {
      private$send_msg("createEmptyDrawing", list())
      invisible(self)
    },
    drawGrowCut = function() {
      private$send_msg("drawGrowCut", list())
      invisible(self)
    },
    closeDrawing = function() {
      private$send_msg("closeDrawing", list())
      invisible(self)
    },
    refreshDrawing = function(isForceRedraw = TRUE) {
      private$send_msg("refreshDrawing", list(isForceRedraw))
      invisible(self)
    },
    loadMatCapTexture = function(bmpUrl) {
      private$send_msg("loadMatCapTexture", list(bmpUrl))
      invisible(self)
    },
    loadFont = function(fontSheetUrl, metricsUrl) {
      private$send_msg("loadFont", list(fontSheetUrl, metricsUrl))
      invisible(self)
    },
    setMeshShader = function(id, meshShaderNameOrNumber = 2) {
      private$send_msg("setMeshShader", list(id, meshShaderNameOrNumber))
      invisible(self)
    },
    createCustomMeshShader = function(fragmentShaderText, name = "Custom", vertexShaderText = "") {
      if (name %in% private$mesh_shader_names) {
        private$mesh_shader_names <- private$mesh_shader_names[private$mesh_shader_names != name]
      }
      private$mesh_shader_names <- c(private$mesh_shader_names, name)
      private$send_msg("createCustomMeshShader", list(fragmentShaderText, name, vertexShaderText))
      invisible(self)
    },
    setCustomMeshShader = function(fragmentShaderText = "", name = "Custom") {
      if (name %in% private$mesh_shader_names) {
        private$mesh_shader_names <- private$mesh_shader_names[private$mesh_shader_names != name]
      }
      private$mesh_shader_names <- c(private$mesh_shader_names, name)
      private$send_msg("setCustomMeshShader", list(fragmentShaderText, name))
      invisible(self)
    },
    addColormap = function(key, cmap) {
      private$colormap_names <- sort(c(private$colormap_names, key))
      private$send_msg("addColormap", list(key, cmap))
      invisible(self)
    },
    setColormap = function(id, colormap) {
      private$send_msg("setColormap", list(id, colormap))
      invisible(self)
    },
    setRenderDrawAmbientOcclusion = function(ao) {
      private$send_msg("setRenderDrawAmbientOcclusion", list(ao))
      invisible(self)
    },
    setColormapNegative = function(id, colormapNegative) {
      private$send_msg("setColormapNegative", list(id, colormapNegative))
      invisible(self)
    },
    setModulationImage = function(idTarget, idModulation, modulateAlpha = FALSE) {
      private$send_msg("setModulationImage", list(idTarget, idModulation, modulateAlpha))
      invisible(self)
    },
    setGamma = function(gamma = 1.0) {
      private$send_msg("setGamma", list(gamma))
      invisible(self)
    },
    loadDeferred4DVolumes = function(id) {
      private$send_msg("loadDeferred4DVolumes", list(id))
      invisible(self)
    },
    setFrame4D = function(id, frame4D) {
      private$send_msg("setFrame4D", list(id, frame4D))
      invisible(self)
    },
    setInterpolation = function(isNearest) {
      private$send_msg("setInterpolation", list(isNearest))
      invisible(self)
    },
    moveCrosshairInVox = function(x, y, z) {
      private$send_msg("moveCrosshairInVox", list(x, y, z))
      invisible(self)
    },
    drawMosaic = function(mosaicStr) {
      private$send_msg("drawMosaic", list(mosaicStr))
      invisible(self)
    },

    #getters
    getVolumeIndexByID = function(id) {
      n <- length(self$volumes)
      for (i in 1:n) {
        id_i <- self$volumes[[i]]$id
        if (id_i == id) {
          return(i)
        }
      }
      return(-1)
    },
    getFrame4D = function(id) {
      idx <- self$getVolumeIndexByID(id)
      return(self$volumes[[idx]]$nFrame4D)
    },
    getDescriptives = function(layer = 1, masks = c(), drawingIsMask = FALSE) {
      hdr <- self$volumes[[layer]]$hdr
      slope <- hdr$scl_slope
      if (is.nan(slope)) slope <- 1
      inter <- hdr$scl_inter
      if (is.nan(inter)) inter <- 1
      imgRaw <- self$volumes[[layer]]$img
      nv <- length(imgRaw)
      img <- imgRaw * slope + inter
      mask <- rep(1, nv)
      
      if (length(masks) > 0) {
        for (m in masks) {
          imgMask <- self$volumes[[m]]$img
          if (length(imgMask) != nv) {
            cat(paste0("Mask resolution does not match image. Skipping masking layer ", m, "\n"))
            next
          }
          for (i in 1:nv) {
            if (imgMask[i] == 0 || is.nan(imgMask[i])) mask[i] <- 0
          }
        }
      } else if (length(masks) < 1 && drawingIsMask) {
        for (i in 1:nv) {
          if (self$volumes$drawBitmap[i] == 0 || is.nan(self$volumes$drawBitmap[i])) mask[i] <- 0
        }
      }

      k <- 0
      M <- 0
      S <- 0
      mx <- -Inf
      mn <- Inf
      kNot0 <- 0
      MNot0 <- 0
      SNot0 <- 0

      for (i in 1:nv) {
        if (mask[i] < 1) next
        x <- img[i]
        k <- k + 1
        Mnext <- M + (x - M) / k
        S <- S + (x - M) * (x - Mnext)
        M <- Mnext
        if (x == 0) next
        kNot0 <- kNot0 + 1
        Mnext <- MNot0 + (x - MNot0) / kNot0
        SNot0 <- SNot0 + (x - MNot0) * (x - Mnext)
        MNot0 <- Mnext

        mn <- min(x, mn)
        mx <- max(x, mx)
      }
      stdev <- sqrt(S / (k - 1))
      stdevNot0 <- sqrt(SNot0 / (kNot0 - 1))
      mnNot0 <- mn
      mxNot0 <- mx
      if (k != kNot0) {
        mn <- min(0, mn)
        mx <- max(0, mx)
      }

      return(list(
        mean = M,
        stdev = stdev,
        nvox = k,
        volumeMM3 = k * hdr$pixDims[2] * hdr$pixDims[3] * hdr$pixDims[4],
        volumeML = k * hdr$pixDims[2] * hdr$pixDims[3] * hdr$pixDims[4] * 0.001,
        min = mn,
        max = mx,
        meanNot0 = MNot0,
        stdevNot0 = stdevNot0,
        nvoxNot0 = kNot0,
        minNot0 = mnNot0,
        maxNot0 = mxNot0,
        cal_min = self$volumes[[layer]]$cal_min,
        cal_max = self$volumes[[layer]]$cal_max,
        robust_min = self$volumes[[layer]]$robust_min,
        robust_max = self$volumes[[layer]]$robust_max
      ))
    },
    getOverlayIndexByID = function(id) {
      n <- length(self$overlays)
      for (i in 1:n) {
        id_i <- self$overlays[[i]]$id
        if (id_i == id) {
          return(i)
        }
      }
      return(-1)
    },
    getRadiologicalConvention = function() {
      return(self$opts$isRadiologicalConvention)
    },
    getMediaByUrl = function(url) {
      keys <- names(self$mediaUrlMap)
      filtered_keys <- keys[self$mediaUrlMap == url]
      return(filtered_keys[length(filtered_keys)])
    },
    cloneVolume = function(index) {
      return(duplicate(self$volumes[[index]], recursive = TRUE))
    },


    getFileExt = function(fullname, upperCase = TRUE) {
      re <- "\\.(\\w+)$"
      
      ext <- regmatches(fullname, regexpr(re, fullname))
      ext <- gsub(re, "\\1", ext)
      ext <- toupper(ext)
      
      if (ext == "GZ") {
        ext <- regmatches(substr(fullname, 1, nchar(fullname) - 3), regexpr(re, substr(fullname, 1, nchar(fullname) - 3)))
        ext <- gsub(re, "\\1", ext)
        ext <- toupper(ext)
      }
      
      if (upperCase) {
        return(ext)
      } else {
        return(tolower(ext))
      }
    },
    isMeshExt = function(url) {
      ext <- self$getFileExt(url)
      return(ext %in% MESH_EXTENSIONS)
    },
    calculateMinMaxVoxIdx = function(array) {
      private$send_msg("calculateMinMaxVoxIdx", list(array))
      invisible(self)
    },
    colormaps = function() {
      return(private$colormap_names);
    },
    meshShaderNames = function(sorted = TRUE) {
      if (sorted) {
        return(sort(private$mesh_shader_names));
      } else {
        return(private$mesh_shader_names)
      }
    },
    sph2cartDeg = function(azimuth, elevation) {
      Phi <- -elevation * (pi / 180)
      Theta <- ((azimuth - 90) %% 360) * (pi / 180)
      ret <- c(
        cos(Phi) * cos(Theta),
        cos(Phi) * sin(Theta),
        sin(Phi)
      )
      len <- sqrt(ret[1] * ret[1] + ret[2] * ret[2] + ret[3] * ret[3])
      if (len <= 0) return(ret)
      ret[1] <- ret[1] / len
      ret[2] <- ret[2] / len
      ret[3] <- ret[3] / len
      return(ret)
    }
  ),
  active = list(
    plot = function() {
      self$widget
    }
  )
)