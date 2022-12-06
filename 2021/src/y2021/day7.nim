import std/[strutils, sequtils, math, sets, tables, algorithm, strformat]

proc cost(crabs: seq[int], position: int): int =
  result = sum(map(crabs, proc (c: int): int = abs(c - position)))

proc analyze(crabs: seq[int]): Table[int, seq[int]] =
  let king_crab = max(crabs) - 1
  var crab_bucket = crabs
  sort(crab_bucket)
  var crabs = toOrderedSet(crab_bucket)
  var crabs_table = initTable[int, seq[int]]()

  echo fmt"Building crabs table...{max(crab_bucket)} positions"
  for crab in crab_bucket:
    crabs_table[crab] = newSeq[int](king_crab + 1)

  for crab in crabs:
    for position in 0..king_crab:
      var distance = 0
      for d in 0..abs(crab - position):
        distance += d
      crabs_table[crab][position] = distance
  return crabs_table

proc part1*(lines: seq[string]): int =
  var crabs = lines[0].split(",").map(parseInt)
  return min(crabs.map(proc(crab: int): int = cost(crabs, crab)))

proc part2*(lines: seq[string]): int =
  let crabs = lines[0].split(",").map(parseInt)
  let analysis = analyze(crabs)
  var distances = newSeq[int](0)
  for position in 0..analysis[crabs[0]].len-1:
    var distance = 0
    for crab in crabs:
      distance += analysis[crab][position]
    distances.add(distance)
  return min(distances)
