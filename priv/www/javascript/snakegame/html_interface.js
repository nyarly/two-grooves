BoardInterface.bindDocumentTo(function(htmlInterface){
    htmlInterface.update = function(json){
      d3.select("#score").html(json.score)
      d3.select("#moves-remaining").html(5 - json.moves.length)
    }

    htmlInterface.statusHandlers = function(){
      board = this
      return {
        200: function(json){
          board.update(json)
        },
        400: function(){
          d3.select("#events").append("div").classed("rule-violation",true).html("Invalid move")
        }
      }
    }

    htmlInterface.childSelectors = ["#svg-board"]
})
