import std/[sequtils, strformat, strutils]

proc visible(row, column: int, forest: seq[seq[int]]): bool =
  let height = forest.len
  let width = forest[0].len
  var left = true
  var right = true
  var up = true
  var down = true

  #echo fmt"{row},{column}:{forest[row][column]}"
  for x in 0 .. column-1:
    #echo fmt" left:{row},{x}:{forest[row][x]}"
    if forest[row][x] >= forest[row][column]:
      left = false
      break
  for x in column+1 .. width-1:
    #echo fmt" right:{row},{x}:{forest[row][x]}"
    if forest[row][x] >= forest[row][column]:
      right = false
      break
  for y in 0 .. row-1:
    #echo fmt" up:{y},{column}:{forest[y][column]}"
    if forest[y][column] >= forest[row][column]:
      up = false
      break
  for y in row+1 .. height-1:
    #echo fmt" down:{y},{column}:{forest[y][column]}"
    if forest[y][column] >= forest[row][column]:
      down = false
  #echo fmt"visible: {row},{column}:{forest[row][column]} {up=} {down=} {left=} {right=}"
  result = left or right or up or down

proc visibility(row, column: int, forest: seq[seq[int]]): int =
  let height = forest.len
  let width = forest[0].len
  var left = 1
  var right = 1
  var up = 1
  var down = 1

  var i = 0

  echo fmt"{row},{column}:{forest[row][column]}"
  i = column-1
  while i >= 0:
    left = left + 1
    echo fmt" left:{row},{i}:{forest[row][i]}"
    if forest[row][i] >= forest[row][column]:
      break
    i = i - 1

  i = column+1
  while i <= width-1:
    echo fmt" right:{row},{i}:{forest[row][i]}"
    right = right + 1
    if forest[row][i] >= forest[row][column]:
      break
    i = i + 1

  i = row-1
  while i >= 0:
    up = up + 1
    echo fmt" up:{i},{column}:{forest[i][column]}"
    if forest[i][column] >= forest[row][column]:
      break
    i = i - 1

  i = row+1
  while i <= height-1:
    down = down + 1
    echo fmt" down:{i},{column}:{forest[i][column]}"
    if forest[i][column] >= forest[row][column]:
      break
    i = i + 1

  echo fmt"visible: {row},{column}:{forest[row][column]} {up=} {down=} {left=} {right=}"
  result = left * right * up * down

proc score_part1(forest: seq[seq[int]]): int =
  let height = forest.len
  let width = forest[0].len
  result = (height * 2) + ((width - 2) * 2)

  for row in 1 .. height-2:
    for column in 1 .. width-2:
      if visible(row, column, forest):
        result = result + 1

proc score_part2(forest: seq[seq[int]]): int =
  let height = forest.len
  let width = forest[0].len
  var trees: seq[int]

  for row in 1 .. height-2:
    for column in 1 .. width-2:
      if visible(row, column, forest):
        trees.add(visibility(row, column, forest))
  echo fmt"{trees}"
  result = 0

when isMainModule:
  let lines = readFile("day8.test").strip.splitlines()

  var forest: seq[seq[int]]
  for line in lines:
    var row: seq[int]
    for tree in toSeq(line):
      row.add(parseInt($tree))
    forest.add(row)

  echo fmt"part1: {score_part1(forest)}"
  echo fmt"part2: {score_part2(forest)}"
