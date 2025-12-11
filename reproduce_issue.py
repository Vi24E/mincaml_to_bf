import sys
from debugger import Debugger

def test_debugger():
    try:
        with open('test.bf', 'r') as f:
            code = f.read()
    except FileNotFoundError:
        print("test.bf not found")
        return

    dbg = Debugger(code)
    print(f"Total ops: {len(dbg.ops)}")
    
    # Print first few ops to check parsing
    print("First 20 ops:")
    for i in range(min(20, len(dbg.ops))):
        print(f"{i}: {dbg.ops[i]}")

    print("\nSearch for tags:")
    # Find indices of tags
    tags = []
    for i, op in enumerate(dbg.ops):
        if op.char == '#':
            tags.append((i, op))
            if len(tags) < 5:
                print(f"Tag at {i}: {op}")

    if not tags:
        print("No tags found!")
        return

    print(f"\nTotal tags found: {len(tags)}")

    # Test run_to_tag
    print("\nTesting run_to_tag(1) (Large)...")
    dbg.pc = 0
    found = dbg.run_to_tag(1)
    if found:
        print(f"Stopped at PC={dbg.pc}: {dbg.ops[dbg.pc]}")
    else:
        print("run_to_tag(1) returned False")

    # Test run_to_tag(2) (Small)
    print("\nTesting run_to_tag(2) (Small)...")
    # Move past current tag 
    dbg.run_step() 
    found = dbg.run_to_tag(2)
    if found:
        print(f"Stopped at PC={dbg.pc}: {dbg.ops[dbg.pc]}")
    else:
        print("run_to_tag(2) returned False")

if __name__ == "__main__":
    test_debugger()
