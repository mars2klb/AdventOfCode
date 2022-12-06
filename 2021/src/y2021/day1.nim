import std/[strutils, strformat, sequtils]

proc part1*(lines: seq[string]): int =
  let nums = lines.map(parseInt)

  var last = nums[0]
  result = 0
  for num in nums:
    if num > last:
      inc result
    last = num

proc part2*(lines: seq[string]): int =
  let nums = lines.map(parseInt)
  var windows = newSeq[string](0)

  for i in 0..(nums.high - 2):
    windows.add(fmt"{nums[i] + nums[i + 1] + nums[i + 2]}")

  result = part1(windows)
