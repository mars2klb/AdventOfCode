import std/strscans

type
  Point = object
    x, y: int

  Line = object
    p1, p2: Point

  Dir = enum
    pos, neg

  Axis = enum
    xaxis, yaxis, diagonal

proc dump(lines: seq[seq[int]]): string =
  result = ""
  for line in lines:
    for v in line:
      result &= $v & " "
    result &= "\n"

proc parseLines(lines: seq[string]): seq[Line] =
  var parsedlines = newSeq[Line](0)
  for line in lines:
    var x1, y1, x2, y2: int
    if scanf(line, "$i,$i -> $i,$i", x1, y1, x2, y2):
      parsedlines.add(Line(p1: Point(x: x1, y: y1),
                           p2: Point(x: x2, y: y2)))
    else:
      raise newException(ValueError, "unscannable:" & line)  # whar tattoo?
  result = parsedlines

proc gridSize(lines: seq[Line]): (int, int) =
  var maxX = 0
  var maxY = 0
  for line in lines:
    if maxX < line.p1.x:
      maxX = line.p1.x
    if maxX < line.p2.x:
      maxX = line.p2.x
    if maxY < line.p1.y:
      maxY = line.p1.y
    if maxY < line.p2.y:
      maxY = line.p2.y
  result = (maxX, maxY)

proc orderPoints(line: Line): (Point, Point, Axis) =
  if line.p1.x == line.p2.x:
    if line.p1.y > line.p2.y:
      result = (line.p2, line.p1, yaxis)
    else:
      result = (line.p1, line.p2, yaxis)
  elif line.p1.y == line.p2.y:
    if line.p1.x > line.p2.x:
      result = (line.p2, line.p1, xaxis)
    else:
      result = (line.p1, line.p2, xaxis)
  else:
      result = (line.p1, line.p2, diagonal)

proc offset(p1, p2: int): int =
  if p1 > p2:
    result = -1
  elif p1 < p2:
    result = 1
  else:
    result = 0

proc getPoints(line: Line): seq[Point] =
  result = newSeq[Point](0)
  let (src, dst, axis) = orderPoints(line)
  case axis:
    of xaxis:
      for x in src.x..dst.x:
        result.add(Point(x: x, y: src.y))
    of yaxis:
      for y in src.y..dst.y:
        result.add(Point(x: src.x, y: y))
    of diagonal:
      let step = (offset(src.x, dst.x), offset(src.y, dst.y))
      var x = src.x
      var y = src.y
      var point = Point(x: x, y: y)
      result.add(point)
      while point.x != dst.x and point.y != dst.y:
        x += step[0]
        y += step[1]
        point = Point(x: x, y: y)
        result.add(point)

proc part1*(lines: seq[string]): int =
  let plines = parseLines(lines)
  let (maxX, maxY) = gridSize(plines)
  var grid = newSeq[seq[int]](0)
  for i in 0..maxY:
    grid.add(newSeq[int](maxX+1))

  for line in plines:
    if line.p1.x != line.p2.x and line.p1.y != line.p2.y:
      continue
    let points = getPoints(line)
    for point in points:
      inc grid[point.y][point.x]

  var score = 0
  for line in grid:
    for value in line:
      if value > 1:
        inc score

  return score

proc part2*(lines: seq[string]): int =
  let plines = parseLines(lines)
  let (maxX, maxY) = gridSize(plines)
  var grid = newSeq[seq[int]](0)
  for i in 0..maxY:
    grid.add(newSeq[int](maxX+1))

  for line in plines:
    let points = getPoints(line)
    for point in points:
      inc grid[point.y][point.x]

  var score = 0
  for line in grid:
    for value in line:
      if value > 1:
        inc score

  return score
