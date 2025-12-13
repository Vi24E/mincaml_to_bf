
// Decrement the 32-bit integer at `addr` by 1.
// Assumes Little Endian binary representation (0/1).
// Logic:
//   Scan from LSB.
//   While bit is 0: Set to 1. Move to next bit.
//   If bit is 1: Set to 0. Stop and return to LSB.
//   Handles safeguards to ensure ptr returns to `addr`.
fn decrement(bf_code: &mut String, current_ptr: &mut u32, addr: u32, temp_addr: u32) {
    // We need a loop in BF that runs while we have carry (borrow).
    // State: We are at Bit K.
    // If Bit K == 0: Set 1. Carry continues. Move K+1.
    // If Bit K == 1: Set 0. Carry stops. Move back to start.
    
    // To implement "Move back to start", we can mark our path?
    // Or simpler:
    // Just unroll the loop 32 times with conditional skip!
    // Since 32 is small.
    // But we depend on runtime value.
    
    // Unrolled logic with "Active" flag.
    // Flag = 1 (Borrow active).
    // For i in 0..32:
    //   If Flag == 1:
    //      Check Bit[i].
    //      If 0: Bit[i]=1. Flag=1.
    //      If 1: Bit[i]=0. Flag=0.
    
    // Used registers:
    // `addr` ... `addr+31` : Data.
    // `temp_addr` : Flag (Borrowed).
    // `temp_addr+1` : Temp for copy/check.
    
    let flag = temp_addr;
    let temp = temp_addr + 1;
    
    // Set Flag = 1
    move_ptr(bf_code, current_ptr, flag);
    bf_code.push_str("[-]+");
    
    for i in 0..32 {
        let bit = addr + i;
        move_ptr(bf_code, current_ptr, flag);
        // If Flag is 1:
        bf_code.push('['); 
            // Flag is 1. We process this bit.
            // Move to Bit.
            move_ptr(bf_code, current_ptr, bit);
            
            // Check if Bit is 0 or 1.
            // Copy Bit to Temp.
            // copy_cell logic inline (to avoid clobbering ptr tracking if copy helper is heavy).
            // bit -> temp.
            // bit[->temp+>+<<] ...
            // Simplified copy since we can destroy bit? No we need to flip it.
            
            // Check Bit:
            // Move to Temp. Clear.
            move_ptr(bf_code, current_ptr, temp);
            bf_code.push_str("[-]");
            // Move to Bit.
            move_ptr(bf_code, current_ptr, bit);
            // Copy bit to temp, preserving bit? 
            // Let's destroy bit temporarily.
            // [->temp+<]
            bf_code.push('[');
            move_ptr(bf_code, current_ptr, temp);
            bf_code.push('+');
            move_ptr(bf_code, current_ptr, bit);
            bf_code.push('-');
            bf_code.push(']');
            // Now Bit is 0. Temp has Value.
            
            // Analyze Temp.
            move_ptr(bf_code, current_ptr, temp);
            // If Temp was 1 (Bit was 1):
            //   We need: Bit=0. Flag=0.
            //   (Bit is already 0).
            //   Set Flag 0.
            bf_code.push('[');
                move_ptr(bf_code, current_ptr, flag);
                bf_code.push_str("[-]"); // Flag=0.
                move_ptr(bf_code, current_ptr, temp);
                bf_code.push_str("[-]"); // Clear Temp.
            bf_code.push(']');
            
            // If Temp was 0 (Loop didn't run):
            //   We need: Bit=1. Flag=1.
            //   (Bit is 0).
            //   Set Bit=1.
            //   Flag stays 1.
            //   (Note: Temp is already 0).
            
            // But wait, if Temp was 1, we cleared it.
            // If Temp was 0, it is 0.
            // How to detect if we cleared validly?
            // "If Temp was 0" logic requires logic "If not non-zero".
            
            // Better logic:
            // Restore Bit from Temp?
            // If Temp=1: Bit=0. Flag=0.
            // If Temp=0: Bit=1. Flag=1.
            
            // We can calculate ResultBit = 1 - imm(Temp).
            // And NewFlag = 1 - imm(Temp). (Actually same).
            // If Temp=1 -> Result=0, Flag=0.
            // If Temp=0 -> Result=1, Flag=1.
            
            // Logic:
            // Result = 1. Flag = 1.
            // If Temp=1:
            //    Result = 0. Flag = 0.
            
            // Setup Result/Flag at 1.
            move_ptr(bf_code, current_ptr, bit);
            bf_code.push('+'); 
            // Flag is already 1.
            
            move_ptr(bf_code, current_ptr, temp);
            bf_code.push('[');
                // Temp is 1.
                // Clear Bit.
                move_ptr(bf_code, current_ptr, bit);
                bf_code.push('-'); // 1->0.
                // Clear Flag.
                move_ptr(bf_code, current_ptr, flag);
                bf_code.push('-'); // 1->0.
                // Clear Temp.
                move_ptr(bf_code, current_ptr, temp);
                bf_code.push_str("[-]");
            bf_code.push(']');
            
            // Done.
            
            // Move back to Flag for loop condition check
            move_ptr(bf_code, current_ptr, flag);
            // If Flag is 0, we exit loop (effectively, for this iteration it skips).
            // Wait, we are inside `Flag [` loop.
            // We must ensure we terminate THIS loop iteration correctly.
            // We want to execute body ONCE.
            // So we set Flag to 0? No, Flag carries state to NEXT bit.
            // We need a separate LoopControl flag?
            // Or just enter, do work, move to a known 0 to break inner loop?
            // But we need Flag to persist for NEXT unroll.
            
            // Trick:
            // Use `flag` as data.
            // Copy `flag` to `active`.
            // Loop `active`.
            // Inside: do work. Update `flag`. Set `active` 0.
            
        // End of logic.
        // But `bf_code.push('[')` starts a loop on `flag`.
        // If `flag` remains 1, it repeats!
        // We MUST break.
        // But we want `flag` to vary.
        
        // Correct Pattern:
        // Copy `flag` to `active`.
        // `active` [ Run Logic; Update `flag`; `active`=0 ]
        
    // Refined Inner Loop:
    // Move to Flag.
    // Move to Active (temp). Clear.
    // Copy Flag -> Active.
    // Move to Active.
    // [
    //    Logic... updates Flag (and Bit).
    //    Active [-]
    // ]
    
    // We need 2 temps? `temp` and `active`.
    // `temp_addr + 1` = temp.
    // `temp_addr + 2` = active.
    
    // I will rewrite this properly in replace block.
    
    bf_code.push(']'); // Placeholder for struct
}
