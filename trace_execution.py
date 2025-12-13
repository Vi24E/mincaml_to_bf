
from debugger import Debugger
import sys

def trace(filename, steps=2000000):
    with open(filename, 'r') as f:
        code = f.read()
    
    dbg = Debugger(code)
    print(f"Loaded {len(dbg.ops)} ops")
    
    # Run
    for i in range(steps):
        if dbg.pc >= len(dbg.ops):
            print(f"Finished at step {i}")
            break
            
        op = dbg.ops[dbg.pc]
        # Only print if label or interesting op
        if op.char == '#':
            print(f"Step {i}: Tag {op.label}")
        
        if not dbg.run_step():
            print(f"Error/Halt at step {i}")
            break
            
    print(f"Final PC: {dbg.pc}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 trace_execution.py <bf_file>")
        sys.exit(1)
    trace(sys.argv[1])
