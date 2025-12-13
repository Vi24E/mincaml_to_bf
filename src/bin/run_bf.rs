use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <bf_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let code = match fs::read_to_string(filename) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            process::exit(1);
        }
    };

    let code_chars: Vec<char> = code.chars().collect();
    let len = code_chars.len();
    let mut pc = 0;
    let mut ptr = 0;
    let mut tape: Vec<u8> = vec![0; 100_000_000]; // 100MB tape
    let mut jump_table = vec![0; len];
    let mut loop_stack = Vec::new();

    // Precompute loops
    for (i, &c) in code_chars.iter().enumerate() {
        if c == '[' {
            loop_stack.push(i);
        } else if c == ']' {
            if let Some(start) = loop_stack.pop() {
                jump_table[start] = i;
                jump_table[i] = start;
            } else {
                eprintln!("Unmatched ']' at {}", i);
                process::exit(1);
            }
        }
    }

    if !loop_stack.is_empty() {
        eprintln!("Unmatched '[' at {}", loop_stack[0]);
        process::exit(1);
    }

    let stdout = io::stdout();
    let mut handle = stdout.lock();

    // Run
    while pc < len {
        match code_chars[pc] {
            '>' => ptr += 1,
            '<' => ptr -= 1,
            '+' => tape[ptr] = tape[ptr].wrapping_add(1),
            '-' => tape[ptr] = tape[ptr].wrapping_sub(1),
            '.' => {
                let _ = handle.write(&[tape[ptr]]);
                let _ = handle.flush();
            }
            ',' => {
                // Input not supported for this test
            }
            '[' => {
                if tape[ptr] == 0 {
                    pc = jump_table[pc];
                }
            }
            ']' => {
                if tape[ptr] != 0 {
                    pc = jump_table[pc];
                }
            }
            _ => {}
        }
        pc += 1;
    }
}
