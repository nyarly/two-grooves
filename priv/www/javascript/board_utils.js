function Vector(x, y) {
  this.x = x
  this.y = y
}

Vector.add = function(){
  var x = 0
  var y = 0
  for(var i = 0; i < arguments.length; i++){
    x = x + arguments[i].x
    y = y + arguments[i].y
  }
  return new Vector(x, y)
}

Vector.prototype = {}
Vector.prototype.add = function() {
  var args = [].slice.call(arguments)
  args.unshift(this)
  return Vector.add.apply(null, args)
}

Vector.prototype.scale = function(scalar) {
  return new Vector(this.x * scalar, this.y * scalar)
}


function Glyph(name) {
  var id = "#" + name
  var glyph = d3.select(id).node()
  var measureGroup = d3.select(glyph.parentNode).append("g").node()
  glyph.parentNode.removeChild(glyph)
  measureGroup.appendChild(glyph)
  var box = measureGroup.getBBox()

  this.id = id
  this.box = box
  this.width = box.width
  this.height = box.height
  this.center = new Vector( box.x + box.width / 2, box.y + box.height / 2)
  this.position = new Vector( box.x, box.y)
  this.size = new Vector( box.width, box.height)
}


Glyph.gridOffset = function(axis, count, gridRect, offsetRect) {
  return (count - 1) * gridRect[axis] - offsetRect[axis]
}

Glyph.setup = function() {
  var glyphs = {}
  for(var i = 0; i < arguments.length; i++){
    glyphs[arguments[i]] = new Glyph(arguments[i])
  }
  return glyphs
}

Glyph.align = function(theirField, myField) {
  return function(gridGlyph) {
    return gridGlyph[theirField].scale(-1).add(this[myField])
  }
}

Glyph.prototype = {}

Glyph.prototype.gridOffset = function(axis, count, gridGlyph) {
  return Glyph.gridOffset(axis, count, gridGlyph.size, this.gridPosition(gridGlyph))
}

Glyph.prototype.gridPosition = Glyph.align("center", "center")
