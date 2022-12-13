import std/[strutils, tables]

type
  File = object
    name: string
    size: int

proc newFile(line: string): File =
  var f: File
  let parts = line.split(' ')
  f.size = parseInt(parts[0])
  f.name = parts[1]
  result = f

proc walk(lines: seq[string]): Table[string, seq[File]] =
  var crumbs: seq[string]
  var tree = initTable[string, seq[File]]()

  for line in lines:
    case line[0..3]:
    of "$ cd":
      let dirname = line.substr(5)
      if dirname == "..":
        crumbs.delete(crumbs.len)
      else:
        tree[dirname] = newSeq[File]()
        crumbs.add(dirname)
    of "$ ls", "dir ":
      continue
    else:
      tree[crumbs[^1]].add(newFile(line))

  echo(crumbs)
  result = tree

proc score_part1(tree):
    for

when isMainModule:
  let lines = readFile("day7.test").strip.splitlines()
  let tree = walk(lines)
  let part1 = score_part1(tree)
  echo(tree)
