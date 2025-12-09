pub struct Machine {
    pub memory: Vec<u8>,
    pub ptr: usize,
    pub output: Vec<u8>,
}

impl Machine {
    pub fn new(mem_size: usize) -> Self {
        Machine {
            memory: vec![0; mem_size],
            ptr: 0,
            output: Vec::new(),
        }
    }

    pub fn run(&mut self, code: &str) -> Result<(), String> {
        let code_chars: Vec<char> = code.chars().collect();
        let mut pc = 0;
        let mut loop_stack = Vec::new();
        let mut loop_map = std::collections::HashMap::new();

        // Precompute loop jumps
        for (i, &c) in code_chars.iter().enumerate() {
            if c == '[' {
                loop_stack.push(i);
            } else if c == ']' {
                if let Some(start) = loop_stack.pop() {
                    loop_map.insert(start, i);
                    loop_map.insert(i, start);
                } else {
                    return Err("Unmatched ']'".to_string());
                }
            }
        }
        if !loop_stack.is_empty() {
            return Err("Unmatched '['".to_string());
        }

        let mut ticks = 0;
        let max_ticks = 100_000_000;

        while pc < code_chars.len() {
            ticks += 1;
            if ticks > max_ticks {
                return Err("Timeout".to_string());
            }

            match code_chars[pc] {
                '>' => {
                    self.ptr = (self.ptr + 1) % self.memory.len();
                }
                '<' => {
                    if self.ptr == 0 {
                        self.ptr = self.memory.len() - 1;
                    } else {
                        self.ptr -= 1;
                    }
                }
                '+' => {
                    self.memory[self.ptr] = self.memory[self.ptr].wrapping_add(1);
                }
                '-' => {
                    self.memory[self.ptr] = self.memory[self.ptr].wrapping_sub(1);
                }
                '.' => {
                    self.output.push(self.memory[self.ptr]);
                }
                ',' => {
                    // Input not implemented for test
                }
                '[' => {
                    if self.memory[self.ptr] == 0 {
                        pc = *loop_map.get(&pc).unwrap();
                    }
                }
                ']' => {
                    if self.memory[self.ptr] != 0 {
                        pc = *loop_map.get(&pc).unwrap();
                    }
                }
                _ => {}
            }
            pc += 1;
        }
        Ok(())
    }
}
