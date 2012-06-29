function switch_to_gui(svgPath, startState) {
  function setupGlyph(name) {
    var id = "#" + name
    var glyph = d3.select(id).node()
    var measureGroup = d3.select("defs").append("g").node()
    glyph.parentNode.removeChild(glyph)
    measureGroup.appendChild(glyph)
    var box = measureGroup.getBBox()
    return {
      id: id,
      box: box,
      center:{
        x: box.x + box.width / 2,
        y: box.y + box.height / 2
      },
      width: box.width,
      height: box.height,
      position: {
        x: box.x,
        y: box.y
      },
      size: {
        x: box.width,
        y: box.height
      }
    }
  }

  var movePath = $('#interface form').attr("action")

  d3.xml(svgPath, "image/svg+xml", function(gui) {
      var importedGui = document.importNode(gui.documentElement, true)
      d3.selectAll("body *").remove()
      var body = d3.select("body")
      var boardDiv = document.createElement("div")
      boardDiv.setAttribute("id", "board_div")
      body.node().appendChild(boardDiv)
      boardDiv.appendChild(importedGui)
      d3.select('#layer1').remove()
      //blank slate

      var topLeft = {
        x: importedGui.offsetLeft,
        y: importedGui.offsetTop
      }

      function gridOffset(axis, count, gridRect, offsetRect) {
        return (count - 1) * gridRect[axis] - offsetRect[axis]
      }

      function targetOffset(axis){
        return function(data){
          if(data.type == "post"){
            return gridOffset(axis, data.around[axis], glyphs.square.size, glyphs.post.center)
          } else {
            if(data.between[0].x == data.between[1].x) {
              return gridOffset(axis, Math.max(data.between[0][axis], data.between[1][axis]), glyphs.square.size, glyphs.nsGate.center)
            } else {
              return gridOffset(axis, Math.max(data.between[0][axis], data.between[1][axis]), glyphs.square.size, glyphs.ewGate.center)
            }
          }
        }
      }

      function scalarMultiply(scalar, vector) {
        return {
          x: scalar * vector.x,
          y: scalar * vector.y
        }
      }


      function addVectors(){
        var total = {x: 0, y:0}
        for(var i = 0; i < arguments.length; i++){
          total.x = total.x + arguments[i].x
          total.y = total.y + arguments[i].y
        }
        return total
      }


      var coords = []
      for(var i = 1; i <= startState.dims.x; i++) {
        for(var j = 1; j <= startState.dims.y; j++) {
          coords.push([i,j])
        }
      }

      var glyphs = {
        post: setupGlyph("post"),
        ewGate: setupGlyph("ew-gate"),
        nsGate: setupGlyph("ns-gate"),
        stone: setupGlyph("stone"),
        square: setupGlyph("square")
      }
      glyphs.nsGate.center.x = glyphs.nsGate.box.left + glyphs.nsGate.size.y / 2
      glyphs.ewGate.center.y = glyphs.ewGate.box.top + glyphs.ewGate.size.x / 2

      var stoneOffset = addVectors(scalarMultiply(-1, glyphs.square.center), glyphs.stone.center)

      function sendMove(datum, index){
        $.ajax(movePath, {
            type: 'POST',
            data: { x: datum[0], y: datum[1] },
            dataType: "json",
            statusCode: {
              400: function(){ alert("Invalid move"); }
            },
            success: function(data, textStatus, xhr){
              updateBoard(data)
            }
          })
      }

      function updateBoard(json){
        var squares = d3.select('#board').selectAll("use").data(coords)
        squares.enter().append("use").
        attr("xlink:href", "#square").
        attr("x", function(d){ return gridOffset("x", d[0], glyphs.square.size, glyphs.square.position) }).
        attr("y", function(d){ return gridOffset("y", d[1], glyphs.square.size, glyphs.square.position) }).
        on("click", sendMove)


        var targets = d3.select('#targets').selectAll('use').data(json.targets)
        targets.enter().append("use").
        attr("xlink:href", function(data){
            if(data.type == "post") {
              return glyphs.post.id
            } else {
              if(data.between[0].x == data.between[1].x) {
                return glyphs.nsGate.id
              } else {
                return glyphs.ewGate.id
              }
            }
          }).
        attr("x", targetOffset("x")).
        attr("y", targetOffset("y"))

        var stones = d3.select('#stones').selectAll('use').data(json.moves)
        stones.enter().append("use").
        attr("xlink:href", "#stone").
        attr("x", function(d){ return gridOffset("x", d.x, glyphs.square.size, stoneOffset)}).
        attr("y", function(d){ return gridOffset("y", d.y, glyphs.square.size, stoneOffset)})
      }
      updateBoard(json)
    })
}
