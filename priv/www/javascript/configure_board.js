function configureBoard() {
  var jsonIsCheating = $.parseJSON($("meta[name='resource-json']")[0].content)
  var board = new BoardInterface('#html-board')
  var movePath = $(this).find('#make-move').attr("action")
  var statusHandlers = board.statusHandlerTree()

  board.setupCallbacks(
    {
      sendMove: function(moveData){
        $.ajax(movePath, {
            type: 'POST',
            data: moveData,
            dataType: "json",
            statusCode: statusHandlers,
          })
      }
    }
  )
  board.updateAll(jsonIsCheating)
}

document.configureBoard = configureBoard
