import std/[strutils, sequtils, math]

proc step(fish: seq[int]): seq[int] =
  let respawns = fish[0]
  result = fish
  result.delete(0)
  result.add(respawns)
  result[6] += respawns

proc simulate(lines: seq[string], days: int): int =
  let fish = lines[0].split(',').map(parseInt)
  var fishes = repeat(0, 9)
  for fsh in fish:
    inc fishes[fsh]

  for i in 0..days:
    fishes = step(fishes)
  return sum(fishes)


proc part1*(lines: seq[string]): int =
  return simulate(lines, 79)

proc part2*(lines: seq[string]): int =
  return simulate(lines, 255)
