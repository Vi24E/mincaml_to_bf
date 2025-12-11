#[cfg(test)]
mod reproduction_test {
    use super::*;
    use crate::interpreter::Machine;

    #[test]
    fn test_movedata_corruption_repro() {
        let mut bf_code = String::new();
        let mut ptr = 0;

        let src = 310;
        let dest = 982;
        let buffer = 118;
        let size = 32;

        // Simulating the environment
        let mut machine = Machine::new(2000);

        // Set src = 3 (bits 0 and 1 set)
        machine.memory[src as usize] = 1; // bit 0
        machine.memory[src as usize + 1] = 1; // bit 1

        // HYPOTHESIS: Buffer is dirty at offset 24..31
        // 118 + 24 = 142.
        // Let's dirty it to simulate overlap with registers (if any used)
        machine.memory[142] = 1; // Dirty bit 24

        copy(&mut bf_code, &mut ptr, src, dest, buffer, size);

        match machine.run(&bf_code) {
            Ok(_) => {}
            Err(e) => panic!("Run failed: {}", e),
        }

        // Check Dest
        let mut dest_val = 0;
        for i in 0..32 {
            if machine.memory[dest as usize + i] != 0 {
                dest_val |= 1 << i;
            }
        }
        println!("Dest Value: {}", dest_val);
        assert_eq!(dest_val, 3);

        // Check Source (Should be restored to 3)
        let mut src_val: i32 = 0;
        for i in 0..32 {
            if machine.memory[src as usize + i] != 0 {
                src_val |= 1 << i;
            }
        }
        println!("Source Value: {}", src_val);

        // If buffer was dirty, src_val will have bit 24 set.
        // 3 | (1<<24) = 3 | 16777216 = 16777219.
        // User saw -704643069 (0xD6000003).
        // This means bits 24, 25, 27, 28, 30, 31 were set.
        // This corresponds to Reg/Buf indices 24..31.

        // If clean buffer:
        // assert_eq!(src_val, 3);
    }
}
