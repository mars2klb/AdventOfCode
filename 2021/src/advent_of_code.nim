import commandant
import std/[strutils, strformat]
# import std/macros

import y2021/[day1, day2, day3, day4, day5, day6, day7]

# const lame = {"d1p1": day1.part1, "d1p2": day1.part2,
#               "d2p1": day2.part1, "d2p2": day2.part2,
#               "d3p1": day3.part1, "d3p2": day3.part2}

commandline:
  argument(day, string)
  argument(part, string)
  # argument(variant, string)
  argument(filepath, string)
  errormsg("\nadvent_of_code <day> <part> <file variant>\nex: ./advent_of_code 1 2 input")

# template task(module, part: untyped): untyped = module.part
# macro run(day, part: static[string], lines: seq[string]): untyped =
#   result = nnkStmtList.newNimNode()
#   let day = ident(day)
#   let part = ident(part)
#   result.add quote do:
#     # from y2021/`module` import `part`
#     from y2021/`day` import `part`
#     echo `part`(lines)

when isMainModule:
  # let f = "day" & day & "." & variant
  let f = filepath
  let lines = readFile(f).strip.splitlines()

  # run(fmt"day{day}", fmt"part{part}", lines)

  # echo $lame["d" & day & "p" & part](lines)

  let value = case fmt"day{day}.part{part}" :
    of "day1.part1": day1.part1(lines)
    of "day1.part2": day1.part2(lines)
    of "day2.part1": day2.part1(lines)
    of "day2.part2": day2.part2(lines)
    of "day3.part1": day3.part1(lines)
    of "day3.part2": day3.part2(lines)
    of "day4.part1": day4.part1(lines)
    of "day4.part2": day4.part2(lines)
    of "day5.part1": day5.part1(lines)
    of "day5.part2": day5.part2(lines)
    of "day6.part1": day6.part1(lines)
    of "day6.part2": day6.part2(lines)
    of "day7.part1": day7.part1(lines)
    of "day7.part2": day7.part2(lines)
    else:
      raise newException(ValueError, "invalid entry")
  echo value
