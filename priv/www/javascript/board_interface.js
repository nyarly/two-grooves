function BoardInterface() {
  this.children = []
  var board = this

  for(var i = 0; i < arguments.length; i++){
    var childObjects = d3.selectAll(arguments[i])
    childObjects.each(function(d,i){
        childInterface = this.contentDocument.setup()
        board.children.push(childInterface)
      })
  }

  this.setup()
}

BoardInterface.prototype = {}

BoardInterface.prototype.setup = function(){}

BoardInterface.prototype.update = function(){}

BoardInterface.prototype.statusHandlers = function(){
  interface = this
  return {}
}

BoardInterface.prototype.callbacks = {}

BoardInterface.prototype.setupCallbacks = function(callbacks){
  for(name in callbacks) {
    BoardInterface.prototype.callbacks[name] = callbacks[name]
  }
  var callbackMaster = BoardInterface.prototype.callbacks
  this.visit(function(){
      this.callbacks = callbackMaster
    })
}

BoardInterface.prototype.visit = function(visitor){
  visitor.apply(this)
  for(var i = 0; i < this.children.length; i++){
    this.visit.apply(this.children[i], [visitor])
  }
}

BoardInterface.prototype.updateAll = function(json){
  this.visit(function(){
      this.update(json)
    })
}

BoardInterface.prototype.statusHandlerTree = function(){
  var interface = this
  var treeLists = {}

  function addCode(code, handler){
    if (typeof treeLists[code] == "undefined") {
      treeLists[code] = []
    }
    treeLists[code].push(handler)
  }

  addCode(200, function(data){
      interface.updateAll(data)
    })

  this.visit(function(){
      var statusHandlers = this.statusHandlers()
      for(code in statusHandlers) {
        addCode(code, statusHandlers[code])
      }
    })

  function collapseFunctions(list) {
    return function(){
      var i = 0, len = list.length
      for(i = 0; i < len; i++){
        list[i].apply(this, arguments)
      }
    }
  }

  var tree = {}
  for(var code in treeLists) {
    tree[code] = collapseFunctions(treeLists[code])
  }
  return tree
}

BoardInterface.bindDocumentTo = function(buildFunction) {
  document.setup = function(){
    document.interface = buildFunction()
    document.interface.setup()
    document.setup = function(){ return document.interface }
    return document.interface
  }
}
