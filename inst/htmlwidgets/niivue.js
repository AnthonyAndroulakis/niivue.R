HTMLWidgets.widget({

  name: 'niivue',

  type: 'output',

  factory: function(el, width, height) {

    var nv;
    var canvas;

    return {

      renderValue: function(x) {

        // TODO: code to reHTMLWidgets.shinyModender the widget, e.g.
        if (nv == undefined) {
          nv = new niivue.Niivue({
            logging: true,
            textHeight: x.textHeight,
            colorbarHeight: x.colorbarHeight,
            colorbarMargin: x.colorbarMargin,
            crosshairWidth: x.crosshairWidth,
            rulerWidth: x.rulerWidth,
            backColor: x.backColor,
            crosshairColor: x.crosshairColor,
            fontColor: x.fontColor,
            selectionBoxColor: x.selectionBoxColor,
            clipPlaneColor: x.clipPlaneColor,
            rulerColor: x.rulerColor,
            show3Dcrosshair: x.show3Dcrosshair,
            trustCalMinMax: x.trustCalMinMax,
            clipPlaneHotKey: x.clipPlaneHotKey,
            viewModeHotKey: x.viewModeHotKey,
            keyDebounceTime: x.keyDebounceTime,
            doubleTouchTimeout: x.doubleTouchTimeout,
            longTouchTimeout: x.longTouchTimeout,
            isRadiologicalConvention: x.isRadiologicalConvention,
            loadingText: x.loadingText,
            dragAndDropEnabled: x.dragAndDropEnabled,
            isNearestInterpolation: x.isNearestInterpolation,
            isAtlasOutline: x.isAtlasOutline,
            isRuler: x.isRuler,
            isColorbar: x.isColorbar,
            isOrientCube: x.isOrientCube,
            multiplanarPadPixels: x.multiplanarPadPixels,
            multiplanarForceRender: x.multiplanarForceRender,
            meshThicknessOn2D: x.meshThicknessOn2D,
            dragMode: x.dragMode,
            isDepthPickMesh: x.isDepthPickMesh,
            isCornerOrientationText: x.isCornerOrientationText,
            sagittalNoseLeft: x.sagittalNoseLeft,
            isSliceMM: x.isSliceMM,
            isHighResolutionCapable: x.isHighResolutionCapable,
            drawingEnabled: x.drawingEnabled,
            penValue: x.penValue,
            isFilledPen: x.isFilledPen,
            maxDrawUndoBitmaps: x.maxDrawUndoBitmaps,
            thumbnail: x.thumbnail
          });
          canvas = document.createElement('canvas');
          el.appendChild(canvas);
          nv.attachToCanvas(canvas);
        }

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      },

      // a method to expose our nv instance to the outside
      getNVInstance: function(){
        return nv;
      }

    };
  }
});

if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler("niivue-calls", function(msg) {
    var nv = getNV(msg.id);
    if (!nv) {
      throw new Error("Couldn't find niivue div with id: " + msg.id);
    }
    if (!nv[msg.method]) {
      throw new Error("Unknown method " + msg.method);
    }
    nv[msg.method].apply(null, msg.args);
  });
}

function getNV(id){
  
  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);
  
  // Use the getChart method we created to get the underlying C3 chart
  var nvObj = htmlWidgetsObj.getNVInstance();

  return(nvObj);
}