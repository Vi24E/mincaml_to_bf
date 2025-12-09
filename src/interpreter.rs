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

        let mut instructions_count = 0;

        while pc < code_chars.len() {
            instructions_count += 1;
            if instructions_count >= 25_000_000 {
                // Dump active blocks before timeout
                if instructions_count % 5_000_000 == 0 {
                    /*
                    print!("Step {}: Active Blocks: ", instructions_count);
                    for i in 1..10 { // Check first 10 block flags
                        if self.memory[i] != 0 {
                            print!("{} ", i);
                        }
                    }
                    println!("");
                    */
                }
                if instructions_count >= 50_000_000 {
                    return Err("Timeout".to_string());
                }
            }
            let command = code_chars[pc];

            // Minimal Trace: Check if entering a block (heuristically)
            // If command is '[' and ptr is in block_flag range (1..=10 for now) and memory[ptr] != 0
            if command == '[' && self.ptr >= 1 && self.ptr <= 20 && self.memory[self.ptr] != 0 {
                println!(
                    "TRACE: Executing Block {} at step {}",
                    self.ptr, instructions_count
                );
            }

            match command {
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
