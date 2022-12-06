import copy
import re

move_rgx = re.compile(r'move ([\d]+) from ([\d]+) to ([\d]+)')

def rotate(data):
    rotated = []
    width = max([len(x) for x in data])
    for column in range(width):
        newstack = []
        for crate in data:
            try:
                if crate[column] != ' ':
                    newstack.append(crate[column])
            except IndexError:
                pass
        rotated.append(list(reversed(newstack)))
    return rotated


def read_crates(f):
    top_down = []
    while (line := f.readline()):
        if line[1] == '1':
            break

        entries = int(len(line) / 4)
        top_down.append([line[x*4+1] for x in range(entries)])
    return top_down

def read_moves(f):
    moves = []
    while (line := f.readline()):
        # minus one to compensate for zero indexed lists
        if (matches := move_rgx.match(line)):
            moves.append((int(matches.group(1)),
                          int(matches.group(2))-1,
                          int(matches.group(3))-1))
    return moves

def transfer(crates, move, reverse):
    howmany, fromwhere, towhere = move
    count = howmany * -1
    tomove = crates[fromwhere][count:]
    if reverse:
        tomove.reverse()

    for x in tomove:
        crates[towhere].append(x)
    crates[fromwhere] = crates[fromwhere][:count]
    return crates

def do_moves(crates, moves, reverse):
    dump(crates)
    for move in moves:
        crates = transfer(crates, move, reverse)
        # dump(crates)
    return crates

def score(crates):
    terms = []
    for crate in crates:
        terms.append(crate[-1:][0])
    return ''.join(terms)

def dump(crates):
    for crate in crates:
        print(crate)

with open('day5.input') as f:
    crates = rotate(read_crates(f))
    f.readline()
    moves = read_moves(f)
    part2_crates = copy.deepcopy(crates)
    crates = do_moves(crates, moves, True)
    print(f'part1:{score(crates)}')
    crates = do_moves(part2_crates, moves, False)
    print(f'part2:{score(crates)}')
