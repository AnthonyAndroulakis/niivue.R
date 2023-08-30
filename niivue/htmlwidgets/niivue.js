function mapToObject(map) {
  const obj = {};
  for (let [key, value] of map.entries()) {
    obj[key] = value;
  }
  return obj;
}

function sendDataToR(type, data) {
  var dataArr;
  if (["volumes", "overlays"].includes(type)) {
    dataArr = [];
    for (let d of data) {
      let modifiedObj = {};
      for (let item of constants.volumeProps) {
        if (item !== "img") {
          modifiedObj[item] = d[item];
        } else {
          modifiedObj.img = null;
        }
      }
      dataArr.push([modifiedObj, [null]]); //d.img !== undefined ? compress(d.img) : null
    }
  } else if (type === "meshes") {
    dataArr = [];
    for (let d of data) {
      let modifiedObj = {};
      let values = [];
      for (let item of constants.meshProps) {
        modifiedObj[item] = item !== "layers" ? d[item] : null
        if (d.layers) {
          modifiedObj.layers = [];
          for (let l of d.layers) {
            modifiedLayer = {}
            for (let layerItem of constants.layerProps) {
              modifiedLayer[layerItem] = layerItem !== "values" ? l[layerItem] : null;
            }
            values.push(l.values)
            modifiedObj.layers.push(modifiedLayer)
          }
        }
      }
      dataArr.push([modifiedObj, [[null]]]) //values
    }
  } else if (type === "result") {
    dataArr = data;
  } else if (type === "mediaUrlMap") {
    dataArr = mapToObject(data);
  } else {
    dataArr = {};
  }

  Shiny.onInputChange(type, JSON.stringify(dataArr));
}

function sendLocationToR(loc) {
  Shiny.onInputChange("onLocationChange", JSON.stringify(loc));
}

HTMLWidgets.widget({

  name: "niivue",

  type: "output",

  factory: function(el, width, height) {

    var nv = new niivue.Niivue({
      onLocationChange: sendLocationToR
    });
    var canvas = document.createElement('canvas');
    canvas.setAttribute('id', el.id+'_Canvas');
    el.append(canvas);

    Shiny.addCustomMessageHandler("method", async function(msg) {
      const oldVols = nv.volumes;
      const oldMesh = nv.meshes;
      const oldOverlay = nv.overlays;
      const oldMediaMap = nv.mediaUrlMap;

      //actions
      let result;
      let action = nv[msg.method];
      if (typeof(action) !== "function") {
        throw new Error("Unknown method " + msg.method);
      }
      if (action.constructor.name === 'AsyncFunction') {
        result = await action.apply(nv, msg.args.slice(1));
      } else {
        result = action.apply(nv, msg.args.slice(1));
      }
      if (msg.resultId)
        sendDataToR("result", {id: msg.resultId, result: result});

      //update vars
      if (nv.volumes !== oldVols) {
        sendDataToR("volumes", nv.volumes);
      }
      if (nv.meshes !== oldMesh) {
        sendDataToR("meshes", nv.meshes);
      }
      if (nv.overlays !== oldOverlay) {
        sendDataToR("overlays", nv.overlays);
      }
      if (nv.oldMap !== oldMediaMap) {
        sendDataToR("mediaUrlMap", nv.mediaUrlMap);
      }
    });

    Shiny.addCustomMessageHandler("opts", function(msg) {
      for (const o of Object.keys(msg)) {
        if (nv.opts[o] !== msg[o]) {
          nv.opts[o] = msg[o];
        }
      }
      nv.updateGLVolume();
    })

    Shiny.addCustomMessageHandler("graph", function(msg) {
      for (const o of Object.keys(msg)) {
        if (nv.graph[o] !== msg[o]) {
          if (o === "vols" && !Array.isArray(msg[o])) {
            nv.graph[o] = [msg[o]];
          } else {
            nv.graph[o] = msg[o];
          }
        }
      }
      nv.updateGLVolume();
    })

    Shiny.addCustomMessageHandler("thumbnailVisible", function(msg) {
      nv.thumbnailVisible = msg;
      nv.drawScene();
    })

    return {

      renderValue: function(options) {
        nv.attachToCanvas(canvas);
        for (let o in options) {
          nv.opts[o] = options[o];
        }
      },

      resize: function(width, height) {
        window.nv = nv;
      },
    };
  }
});