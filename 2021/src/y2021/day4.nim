import std/[bitops, sequtils, strutils, strformat]

const win_conditions = @[0b1111100000000000000000000'u32,
                         0b0000011111000000000000000'u32,
                         0b0000000000111110000000000'u32,
                         0b0000000000000001111100000'u32,
                         0b0000000000000000000011111'u32,
                         0b1000010000100001000010000'u32,
                         0b0100001000010000100001000'u32,
                         0b0010000100001000010000100'u32,
                         0b0001000010000100001000010'u32,
                         0b0000100001000010000100001'u32]

proc parseInput(lines: seq[string]): (seq[int], seq[seq[int]]) =
  let nums = lines[0].split(',').map(parseInt)
  let rawboards = lines[2..^1].join("x").split("xx")
  var boards = newSeq[seq[int]](0)
  for rawboard in rawboards:
    boards.add(rawboard.replace("x", " ").split.filter(proc (str: string): bool = str != "").map(parseInt))
  result = (nums, boards)

proc score(winner: int, board: seq[int], tracker: uint32): int =
  var count = 0
  for i in 0..board.high:
    if not tracker.testBit(i):
      count = count + board[i]
  return count * winner

proc part1*(lines: seq[string]): int =
  let (nums, boards) = parseInput(lines)
  var trackers = newSeq[uint32](boards.len)
  for num in nums:
    for idx, board in boards.pairs:
      let pos = find(board, num)
      if pos >= 0:
        trackers[idx].setBit(pos)
        for win in win_conditions:
          if bitand(win, trackers[idx]) == win:
            return score(num, board, trackers[idx])
  return -1

proc part2*(lines: seq[string]): int =
  let (nums, boards) = parseInput(lines)
  var trackers = newSeq[uint32](boards.len)
  var winners = newSeq[int](0)
  for num in nums:
    for idx, board in boards.pairs:
      let pos = find(board, num)
      if pos >= 0:
        trackers[idx].setBit(pos)
        for win in win_conditions:
          if idx in winners:
            continue
          if bitand(win, trackers[idx]) == win:
            winners.add(idx)
            if winners.len == boards.len:
              echo fmt"{board}[{idx}] @{num} {winners.len} ({boards.len})"
              return score(num, board, trackers[idx])
  return -1
