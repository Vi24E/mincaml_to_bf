import sys

def run_bf(code):
    tape = [0] * 1000000
    ptr = 0
    pc = 0
    loop_stack = []
    loops = {}
    
    # Precompute loops
    for i, c in enumerate(code):
        if c == '[':
            loop_stack.append(i)
        elif c == ']':
            start = loop_stack.pop()
            loops[start] = i
            loops[i] = start
            
    while pc < len(code):
        c = code[pc]
        if c == '>':
            ptr += 1
        elif c == '<':
            ptr -= 1
        elif c == '+':
            tape[ptr] = (tape[ptr] + 1) % 256
        elif c == '-':
            tape[ptr] = (tape[ptr] - 1) % 256
        elif c == '.':
            sys.stdout.write(chr(tape[ptr]))
            sys.stdout.flush()
        elif c == ',':
            pass # No input for now
        elif c == '[':
            if tape[ptr] == 0:
                pc = loops[pc]
        elif c == ']':
            if tape[ptr] != 0:
                pc = loops[pc]
        pc += 1

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 bf_runner.py <file>")
        sys.exit(1)
        
    with open(sys.argv[1], 'r') as f:
        code = f.read()
    run_bf(code)
