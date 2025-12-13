// Move ptr right by count * scale
// count_addr: Address of count variable (preserved)
// scale: multiplier
// temp: scratch address (must be 0)
fn move_ptr_right_dynamic(
    bf_code: &mut String,
    current_ptr: &mut u32,
    count_addr: u32,
    scale: u32,
    temp: u32,
) {
    // Copy count to temp to use as iterator
    copy(bf_code, current_ptr, count_addr, temp, temp + 1, 32); // Use temp+1 as buffer for copy? 
    // Wait, copy needs buffer. temp+1 might overlap?
    // Assuming temp has space.

    move_ptr(bf_code, current_ptr, temp);
    bf_code.push('[');
    bf_code.push('-');
    // Move Right 'scale'
    bf_code.push_str(&">".to_string().repeat(scale as usize));
    // Move back to temp ??
    // We cannot move back easily if we are moving ptr!
    // Dynamic move moves the PHYSICAL ptr.
    // So we are at `temp + scale`.
    // We need to return to `temp`.
    // `<` * scale.
    bf_code.push_str(&"<".to_string().repeat(scale as usize));
    // This just moves back and forth!
    // We want to END UP at `current + N*scale`.
    // Strategy: Move Val from Temp to "Anchor".
    // Then shift Anchor?
    // BF Dynamic Ptr Move:
    // `count [ >(scale) count- ]` NO. Count is at `base`.
    // If we move away from `base`, we lose `count`.
    // Standard Pattern: "Move Data Frame".
    // Or "Tortoise and Hare"?
    // NO.
    // To move Ptr by N.
    // We need logic that runs N times.
    // But `[` loop condition is at `ptr`.
    // If we move `ptr`. We are checking `new_ptr`.
    // So we cannot control loop count easily from remote.
}
