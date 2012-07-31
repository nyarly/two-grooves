BoardInterface.bindDocumentTo(function(snakeGameInterface){
    snakeGameInterface.targetOffset = function(axis, square){
      var board = this
      return function(data) {
        var target = board.whichTarget(data)
        return target.gridOffset(axis, target.gridCount(data, axis), square)
      }
    }

    snakeGameInterface.targetId = function(data){
      var board = this
      return function(data) {
        return board.whichTarget(data).id
      }
    }

    snakeGameInterface.whichTarget = function(data){
      if(data.type == "post") {
        return this.glyphs.post
      } else {
        if(data.between[0].x == data.between[1].x) {
          return this.glyphs["ns-gate"]
        } else {
          return this.glyphs["ew-gate"]
        }
      }
    }

    snakeGameInterface.update = function(json){
      var coords = []
      for(var i = 1; i <= json.dims.x; i++) {
        for(var j = 1; j <= json.dims.y; j++) {
          coords.push({x: i, y: j})
        }
      }

      var board = this
      var glyphs = this.glyphs

      d3.select('svg').attr('viewBox', "0 0 " +
          json.dims.x * glyphs.square.size.x + " " +
          json.dims.y * glyphs.square.size.y)


      var squares = d3.select('#board').selectAll("use").data(coords)
      squares.enter().append("use").
      attr("xlink:href", "#square").
      attr("x", function(d){
          return glyphs.square.gridOffset("x", d.x, glyphs.square)
        }).
      attr("y", function(d){
          return glyphs.square.gridOffset("y", d.y, glyphs.square)
        }).
      on("click", function(d){
          board.server.sendMove(d)
        })


      var targets = d3.select('#targets').selectAll('use').data(json.targets)
      targets.enter().append("use").
      attr("xlink:href", this.targetId()).
      attr("x", this.targetOffset("x", glyphs.square)).
      attr("y", this.targetOffset("y", glyphs.square))

      var stones = d3.select('#stones').selectAll('use').data(json.moves)
      stones.enter().append("use").
      attr("xlink:href", "#stone").
      attr("x", function(d){
          return glyphs.stone.gridOffset("x", d.x, glyphs.square)
        }).
      attr("y", function(d){
          return glyphs.stone.gridOffset("y", d.y, glyphs.square)
        })

      stones.classed("head", function(d, index){
          return index == json.moves.length - 1
        })
    }

    snakeGameInterface.setup = function() {
      this.glyphs = Glyph.setup("post", "ew-gate", "ns-gate", "stone", "square")
      this.glyphs["ns-gate"].center.x = this.glyphs["ns-gate"].box.x + this.glyphs["ns-gate"].size.y / 2
      this.glyphs["ew-gate"].center.y = this.glyphs["ew-gate"].box.y + this.glyphs["ew-gate"].size.x / 2

      var centerOnPosition = Glyph.align("position", "center")
      this.glyphs["ns-gate"].gridPosition = centerOnPosition
      this.glyphs["ew-gate"].gridPosition = centerOnPosition
      this.glyphs["post"].gridPosition    = centerOnPosition

      this.glyphs["square"].gridPosition = function(){ return this.position }
      this.glyphs["post"].gridCount = function(data, axis){ return data.around[axis] }

      var gateGridCount = function(data, axis){
        return Math.max(data.between[0][axis], data.between[1][axis])
      }
      this.glyphs["ew-gate"].gridCount = gateGridCount
      this.glyphs["ns-gate"].gridCount = gateGridCount

      d3.select('#layer1').remove()
    }
  })
