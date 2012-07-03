BoardInterface.bindDocumentTo(function(){
    var htmlInterface = new BoardInterface("#svg-board")

    htmlInterface.update = function(json){
      d3.select("#score").html(json.score)
      d3.select("#moves-remaining").html(5 - json.moves.length)
    }

    htmlInterface.statusHandlers = function(){
      return {
        400: function(){
          d3.select("#events").append("div").classed("rule-violation",true).html("Invalid move")
        }
      }
    }

    return htmlInterface
  })
