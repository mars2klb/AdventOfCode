import std/[strutils, strformat]

proc part1*(lines: seq[string]): int =
  var depth = 0
  var distance = 0

  for line in lines:
    let hunks = line.split(" ")
    let mov = parseInt(hunks[1])
    case hunks[0]:
      of "forward": distance = distance + mov
      of "down": depth = depth + mov
      of "up": depth = depth - mov
  echo fmt"distance: {distance}"
  echo fmt"depth: {depth}"
  result = distance * depth

proc part2*(lines: seq[string]): int =
  var aim = 0
  var depth = 0
  var distance = 0

  for line in lines:
    let hunks = line.split(" ")
    let amount = parseInt(hunks[1])
    case hunks[0]:
      of "forward":
        distance = distance + amount
        depth = depth + (amount * aim)
      of "down":
        aim = aim + amount
      of "up":
        aim = aim - amount
  echo fmt"distance: {distance}"
  echo fmt"depth: {depth}"
  result = distance * depth
