#!/usr/bin/env python3
import sys
import tty
import termios
import argparse

class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    REVERSE = '\033[7m'

class Instruction:
    def __init__(self, char, count=1, jump_target=None, label=None):
        self.char = char
        self.count = count
        self.jump_target = jump_target
        self.label = label

    def __repr__(self):
        if self.char == '#':
             label_str = f" {self.label}" if self.label else ""
             if self.count > 1:
                 return f"{self.char * self.count}{label_str}" # e.g. ## Name
             return f"{self.char}{label_str}" # e.g. # Name

        if self.jump_target is not None:
            return f"{self.char} (target: {self.jump_target})"
        if self.count > 1:
            return f"{self.char} x{self.count}"
        return f"{self.char}"

def parse_bf(code):
    ops = []
    loop_stack = []
    pc_map = {} # raw index -> op index (not strictly needed if we execute ops)
    
    i = 0
    while i < len(code):
        c = code[i]
        if c in ['+', '-', '>', '<']:
            count = 1
            while i + 1 < len(code) and code[i+1] == c:
                i += 1
                count += 1
            ops.append(Instruction(c, count))
        elif c == '#':
            count = 1
            while i + 1 < len(code) and code[i+1] == '#':
                i += 1
                count += 1
            
            # Read label until newline
            label = ""
            # Peek ahead
            j = i + 1
            while j < len(code) and code[j] != '\n':
                label += code[j]
                j += 1
            
            # Update i to point to newline (next loop iter will skip over it)
            i = j
            
            ops.append(Instruction(c, count, label=label.strip()))
        elif c == '[':
            ops.append(Instruction(c))
            loop_stack.append(len(ops) - 1)
        elif c == ']':
            if not loop_stack:
                print("Error: Unmatched ']'")
                sys.exit(1)
            start_pc = loop_stack.pop()
            end_pc = len(ops)
            ops.append(Instruction(c, jump_target=start_pc))
            ops[start_pc].jump_target = end_pc
        elif c in ['.', ',']:
            ops.append(Instruction(c))
        # Ignore other chars
        i += 1
        
    return ops

import re

class Debugger:
    def __init__(self, code, tape_size=30000):
        self.code_str = code
        self.ops = parse_bf(code)
        self.tape = [0] * tape_size
        self.ptr = 0
        self.pc = 0
        self.breakpoints = set()
        self.output = []
        self.step_count = 0
        
        # Metadata defaults
        self.reg_start = None
        self.buffer_start = None
        self.var_start = None
        self.stack_start = None
        self.parse_metadata()

    def parse_metadata(self):
        # Format: DEBUG_METADATA{REG_START:{} BUFFER_START:{} VAR_START:{} STACK_START:{}}
        pattern = r"DEBUG_METADATA\{REG_START:(\d+)\s+BUFFER_START:(\d+)\s+VAR_START:(\d+)\s+STACK_START:(\d+)\}"
        match = re.search(pattern, self.code_str)
        if match:
            self.reg_start = int(match.group(1))
            self.buffer_start = int(match.group(2))
            self.var_start = int(match.group(3))
            self.stack_start = int(match.group(4))
            print(f"{Colors.GREEN}Metadata found: Reg={self.reg_start}, Buf={self.buffer_start}, Var={self.var_start}, Stack={self.stack_start}{Colors.ENDC}")

    def run_step(self):
        if self.pc >= len(self.ops):
            return False
            
        op = self.ops[self.pc]
        
        # Handle '#' tags as no-ops but advance PC
        if op.char == '#':
            self.pc += 1
            return True

        self.step_count += 1
        
        if op.char == '+':
            self.tape[self.ptr] = (self.tape[self.ptr] + op.count) % 256
            self.pc += 1
        elif op.char == '-':
            self.tape[self.ptr] = (self.tape[self.ptr] - op.count) % 256
            self.pc += 1
        elif op.char == '>':
            self.tape_len = len(self.tape) # optimize access
            self.ptr = (self.ptr + op.count) % self.tape_len
            self.pc += 1
        elif op.char == '<':
            self.tape_len = len(self.tape) 
            self.ptr = (self.ptr - op.count) % self.tape_len
            self.pc += 1
        elif op.char == '[':
            if self.tape[self.ptr] == 0:
                self.pc = op.jump_target + 1 
            else:
                self.pc += 1
        elif op.char == ']':
            if self.tape[self.ptr] != 0:
                self.pc = op.jump_target + 1 
            else:
                self.pc += 1
        return True

    def run_to_tag(self, target_level):
        """
        Run until the next tag of specific level.
        target_level: 1 for '#', 2 for '##'.
        
        Logic:
        - If we are ON a tag currently, we must step past it first.
        - Then run until we see a tag op.
        - If target_level == 1 (#): stop at # (count=1).
        - If target_level == 2 (##): stop at # (count>=1).
          Because a large tag (#) is also a boundary we should stop at?
          Actually user said: 'Skip to next Large/Small'.
          If I skip to Small, and I hit Large, should I stop? 
          Large Instruction usually contains Smalls.
          If I am inside Large, Next Small -> Stop at ##.
          If I hit end of Large (next Large #), I should arguably stop too?
          Let's implement: "Run until any tag is hit."
          But differentiating Large vs Small.
          
          Request: "next Large/Small Instruction skip"
          skip_large: Run until op.char == '#' and op.count == 1
          skip_small: Run until op.char == '#' (any count).
        """
        
        # Step once if current is tag (to move off current tag)
        if self.pc < len(self.ops) and self.ops[self.pc].char == '#':
            self.run_step()
            
        while self.pc < len(self.ops):
            op = self.ops[self.pc]
            if op.char == '#':
                # Found a tag
                if target_level == 1: # Large tag check
                    if op.count == 1:
                        return True
                    else:
                        # Found ##, but we want #. Continue?
                        # If we are looking for Large (#), we skip Small (##).
                        pass
                elif target_level == 2: # Small tag check
                   # Stop at any tag (Small or Large).
                   # Why? Because Large tag is definitely a boundary.
                   # And Small tag is what we look for.
                   # Or strictly ##?
                   # "SubZ内のAdd" -> SubZ starts with #. Add starts with ##.
                   # If I am at SubZ (#), and ask for Next Small.
                   # It should run until ## (Add).
                   if op.count >= 2:
                       return True
                   if op.count == 1:
                       # We hit a Large tag. Logic choice: Stop or Skip?
                       # Usually Next Small means "next detailed step".
                       # If we finish Large op without hitting Small, we hit next Large.
                       # I think stopping at Large is safer to avoid over-running.
                       return True 
            
            if not self.run_step():
                return False

    def get_active_context(self):
        """
        Returns the nearest preceding Large (#) and Small (##) tags.
        """
        ctx_large = "None"
        ctx_small = "None"
        
        # Search backwards from self.pc
        # We can optimize this by caching or maintaining state, but linear scan backwards is fine for now
        # given typical block sizes.
        
        # Find nearest Large
        for i in range(self.pc, -1, -1):
            if i < len(self.ops):
                op = self.ops[i]
                if op.char == '#' and op.count == 1:
                    ctx_large = op.label if op.label else "Unnamed"
                    break
                    
        # Find nearest Small (which might be the same as Large if we are just after Large)
        # Actually Small context usually effectively "resets" at Large boundaries?
        # Or does it?
        # Typically ## is nested in #.
        # So we search backwards for ##.
        # If we hit a # (Large) before finding ##, does that mean no Small context?
        # Or do we just show the nearest ## regardless?
        # Let's show nearest ## regardless for now, as it's most informative.
        
        for i in range(self.pc, -1, -1):
            if i < len(self.ops):
                op = self.ops[i]
                if op.char == '#' and op.count >= 2:
                    ctx_small = op.label if op.label else "Unnamed"
                    break
                # Only check for boundary if strictly requested.
                # If we encounter # (Large), it might be nice to say "None" if we are at the start of Large?
                # But "None" is boring. Let's just find nearest previous ##.
                
        return {'large': ctx_large, 'small': ctx_small}

    def get_variables(self):
        """
        Parses 32-bit integer variables from the tape.
        Range: [self.var_start, self.stack_start)
        Format: 32 cells per variable, little-endian (1 bit per cell).
        """
        if self.var_start is None or self.stack_start is None:
            return []

        variables = []
        var_id = 0
        current_addr = self.var_start
        
        while current_addr + 32 <= self.stack_start:
            # Read 32 bits
            # Handle potential edge case where tape might be shorter than stack_start (though unlikely if initialized)
            if current_addr + 32 > len(self.tape):
                break
                
            bits = self.tape[current_addr : current_addr + 32]
            value = 0
            for i, bit in enumerate(bits):
                if bit != 0:
                    value |= (1 << i)
            
            # Handle 32-bit signed integer (Two's complement)
            if value >= 2**31:
                value -= 2**32

            variables.append({
                'id': var_id,
                'addr': current_addr,
                'value': value
            })
            
            current_addr += 32
            var_id += 1
            
        return variables

    def get_stack_values(self, limit=32):
        """
        Parses 32-bit integer values from the stack.
        Range: [self.stack_start, self.stack_start + limit * 32)
        Format: 32 cells per value, little-endian.
        """
        if self.stack_start is None:
            return []

        stack_vals = []
        # Limit number of items to scan to avoid huge payload
        count = 0
        current_addr = self.stack_start
        
        while count < limit:
            # Check bounds
            if current_addr + 32 > len(self.tape):
                break
                
            bits = self.tape[current_addr : current_addr + 32]
            value = 0
            for i, bit in enumerate(bits):
                if bit != 0:
                    value |= (1 << i)
            
            # Handle 32-bit signed integer (Two's complement)
            if value >= 2**31:
                value -= 2**32

            stack_vals.append({
                'id': count,
                'addr': current_addr,
                'value': value
            })
            
            current_addr += 32
            count += 1
            
        return stack_vals

    def print_region(self, name, start_addr, count, cols=16):
        if start_addr is None:
            return
        
        print(f"{Colors.CYAN}[{name} (Start: {start_addr})]{Colors.ENDC}")
        for i in range(0, count, cols):
            row_start = start_addr + i
            row_vals = []
            has_val = False
            for j in range(cols):
                if i + j < count:
                    val = self.tape[row_start + j]
                    if val != 0:
                        has_val = True
                    
                    val_str = f"{val:03}"
                    if row_start + j == self.ptr:
                         val_str = f"{Colors.REVERSE}{val_str}{Colors.ENDC}"
                    row_vals.append(val_str)
            
            # Print row only if it has non-zero values or contains ptr
            ptr_in_row = (self.ptr >= row_start and self.ptr < row_start + cols)
            if has_val or ptr_in_row:
                 print(f"  +{i:03}: " + " ".join(row_vals))

    def print_state(self):
        # Clear screen? No, simpler to just print.
        print(f"\n{Colors.BOLD}--- Step {self.step_count} ---{Colors.ENDC}")
        print(f"PC: {self.pc} / {len(self.ops)}")
        print(f"Ptr: {self.ptr}")
        
        # Show specific regions if metadata exists
        if self.reg_start is not None:
            self.print_region("Registers", self.reg_start, 128) # 128 size as requested
        if self.buffer_start is not None:
             self.print_region("Buffer", self.buffer_start, 32) # 32 size as requested
             
        # Standard tape window around ptr
        window = 8
        start = max(0, self.ptr - window)
        end = min(len(self.tape), self.ptr + window + 1)
        
        tape_str = ""
        for i in range(start, end):
            val = f"{self.tape[i]:03}"
            if i == self.ptr:
                tape_str += f"{Colors.REVERSE}[{val}]{Colors.ENDC} "
            else:
                tape_str += f" {val}  "
        print(f"Loc: {tape_str}")

        # Code visualization
        context_window = 2
        start_op = max(0, self.pc - context_window)
        end_op = min(len(self.ops), self.pc + context_window + 1)
        
        for i in range(start_op, end_op):
            op_str = str(self.ops[i])
            if i == self.pc:
                print(f"{Colors.GREEN}-> {i:04}: {op_str}{Colors.ENDC}")
            else:
                print(f"   {i:04}: {op_str}")

    def run(self):
        print("BF Debugger started. Commands: (s)tep, (c)ontinue, (q)uit, (m)em dump, enter to repeat last")
        last_cmd = 's'
        while self.pc < len(self.ops):
            self.print_state()
            try:
                cmd = input(f"{Colors.BLUE}(bf-dbg){Colors.ENDC} ").strip()
            except EOFError:
                break
                
            if cmd == '':
                cmd = last_cmd
            
            last_cmd = cmd
            
            if cmd.startswith('s'):
                self.run_step()
            elif cmd.startswith('c'):
                while self.run_step():
                    if self.pc in self.breakpoints:
                        print(f"Breakpoint hit at {self.pc}")
                        break
            elif cmd.startswith('q'):
                break
            elif cmd.startswith('m'):
                # Dump larger memory
                try:
                    parts = cmd.split()
                    addr = int(parts[1]) if len(parts) > 1 else self.ptr
                    count = int(parts[2]) if len(parts) > 2 else 20
                    print("Memory Dump:")
                    for i in range(addr, min(len(self.tape), addr + count)):
                        print(f"[{i:04}]: {self.tape[i]}")
                except:
                    print("Usage: m [addr] [count]")
            elif cmd.startswith('b'):
                try:
                    bp = int(cmd.split()[1])
                    if bp in self.breakpoints:
                        self.breakpoints.remove(bp)
                        print(f"Breakpoint removed at {bp}")
                    else:
                        self.breakpoints.add(bp)
                        print(f"Breakpoint set at {bp}")
                except:
                    print("Usage: b <pc>")
                    
        print("Execution finished.")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: ./debugger.py <bf_file>")
        sys.exit(1)
        
    with open(sys.argv[1], 'r') as f:
        code = f.read()
        
    dbg = Debugger(code)
    dbg.run()
