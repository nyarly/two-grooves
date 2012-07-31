function configureBoard() {
  var parlor = new ParlorInterface()
  var movePath = $(this).find('#make-move').attr("action")

  parlor.jsonState = $.parseJSON($("meta[name='resource-json']")[0].content)
  parlor.sendMove = ParlorInterface.sendData(movePath)

  parlor.findChildren("#html-board")
}

document.configureBoard = configureBoard
