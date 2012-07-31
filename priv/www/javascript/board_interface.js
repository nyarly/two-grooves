/*
 * New thoughts on the approach here:
 *
 * Rather than a responsibility tree, more straightforward would be a message pipe:
 * A single RootInterface lives at the root document and sends messages over the wire and
 * dispatches the responses based on status code.
 *
 * "update" becomes a simple special case of status listener (i.e. "on 200, update")
 * The "parentBoardInterface" is always the RootInterface MQ thing.  The tree isn't bearing its
 * weight.
 *
 * Eventually, the RootInterface becomes a single point of modification for handling non-HTTP
 * interaction (e.g. sock.js)
 */

function ParlorInterface() {
  this.children = []
  this.jsonState = null
}

ParlorInterface.prototype = {}

ParlorInterface.sendData = function(targetPath) {
  return function(data) {
    parlor = this
    $.ajax(targetPath, {
        type: 'POST',
        data: data,
        dataType: "json",
        statusCode: parlor.statusHandlers()
      })
  }
};

(function(prototype){
    prototype.findChildren = function() {
      for(var i = 0; i < arguments.length; i++){
        var childObjects = d3.selectAll(arguments[i])
        this.bindChildren(childObjects)
      }
    }

    prototype.bindChildren = function(childObjects) {
      var board = this
      function bindChild(event) {
        this.contentDocument.parentBoardInterface = board
        //try {
        if(typeof this.contentDocument.boardInterface != "undefined") {
          this.contentDocument.boardInterface.findParent()
        }
      }

      childObjects.each(function(d,i){
          if( this.contentDocument.readyState == 'complete' ||
            this.contentDocument.readyState == 'interactive' ){
            bindChild.apply(this)
          } else {
            alert("On load branch!")
            this.onload = bindChild
          }
        })
    }

    prototype.registerChild = function(child) {
      this.children.push(child)

      child.callbacks = this.callbacks

      if(this.jsonState != null) {
        child.update(this.jsonState)
      }
    }


    prototype.visit = function(visitor){
      for(var i = 0; i < this.children.length; i++){
        visitor.apply(this.children[i])
      }
    }

    prototype.statusHandlers = function(){
      var interface = this
      var treeLists = {}
      var tree = {}

      function addCode(code, handler){
        if (typeof treeLists[code] == "undefined") {
          treeLists[code] = []
        }
        treeLists[code].push(handler)
      }

      function collapseFunctions(list) {
        return function(){
          var i = 0, len = list.length
          for(i = 0; i < len; i++){
            list[i].apply(this, arguments)
          }
        }
      }

      this.visit(function(){
          var statusHandlers = this.statusHandlers()
          for(code in statusHandlers) {
            addCode(code, statusHandlers[code])
          }
        })

      for(var code in treeLists) {
        tree[code] = collapseFunctions(treeLists[code])
      }
      return tree
    }

  })(ParlorInterface.prototype)

function BoardInterface() {
  this.childSelectors = []
  this.parent = null
}

BoardInterface.prototype = {};

(function(prototype){
    prototype.findParent = function() {
      var parent = document.parentBoardInterface
      if(typeof parent != "undefined") {
        this.parent = parent
        this.server = parent
        parent.registerChild(this)

        for(var i = 0; i < this.childSelectors.length; i++){
          var childObjects = d3.selectAll(this.childSelectors[i])
          parent.bindChildren(childObjects)
        }

        this.findParent = function(){}
      }
    }

    prototype.setup = function(){}
    prototype.update = function(){}

    prototype.statusHandlers = function(){
      board = this
      return {
        200: function(data){ board.update(data) }
      }
    }
  })(BoardInterface.prototype)

BoardInterface.bindDocumentTo = function(buildFunction) {
  document.boardInterface = new BoardInterface()
  buildFunction(document.boardInterface)
  window.onload = function(){
    document.boardInterface.setup()
  }
  document.boardInterface.findParent()
  return document.boardInterface
}
