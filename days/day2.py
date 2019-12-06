from copy import deepcopy

def run(tape):
    i = 0
    while tape[i] != 99:
        op, one, two, out = tape[i:i + 4]
        tape[out] = (tape[one] + tape[two]) if op == 1 else (tape[one] * tape[two])
        i += 4
    return tape[0]

def fst():
    tape = [int(part) for part in open('day2.input').read().strip().split(',')]
    tape[1] = 12
    tape[2] = 2
    print(run(tape))

def snd():
    tape = [int(part) for part in open('day2.input').read().strip().split(',')]
    for a in range(100):
        for b in range(100):
            tape_copy = deepcopy(tape)
            tape_copy[1] = a
            tape_copy[2] = b
            if run(tape_copy) == 19690720:
                print(100 * a + b)
                return