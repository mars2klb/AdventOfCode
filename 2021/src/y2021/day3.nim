import std/[sequtils, strutils, strformat]

proc most(lines: seq[string], idx: int): int =
  var zero = 0
  var one = 0
  for line in lines:
    case line[idx]:
      of '0': inc zero
      of '1': inc one
      else:
        discard
  if zero > one:
    result = 0
  else:
    result = 1

proc part1*(lines: seq[string]): int =
  var gamma = ""
  var epsilon = ""
  for i in 0..lines[0].high:
    if most(lines, i) == 0:
      gamma = gamma & "0"
      epsilon = epsilon & "1"
    else:
      gamma = gamma & "1"
      epsilon = epsilon & "0"
  echo fmt"gamma:   {fromBin[int](gamma)}"
  echo fmt"epsilon: {fromBin[int](epsilon)}"
  return fromBin[int](gamma) * fromBin[int](epsilon)

proc part2*(lines: seq[string]): int =
  var o2 = deepCopy(lines)
  var co2 = deepCopy(lines)
  for i in 0..lines[0].high:
    if o2.high > 0:
      if most(o2, i) == 0:
        o2 = filter(o2, proc (line: string): bool = line[i] == '0')
      else:
        o2 = filter(o2, proc (line: string): bool = line[i] == '1')

    if co2.high > 0:
      if most(co2, i) == 0:
        co2 = filter(co2, proc (line: string): bool = line[i] == '1')
      else:
        co2 = filter(co2, proc (line: string): bool = line[i] == '0')
  echo fmt"o2:  {o2}"
  echo fmt"co2: {co2}"
  return fromBin[int](o2[0]) * fromBin[int](co2[0])
