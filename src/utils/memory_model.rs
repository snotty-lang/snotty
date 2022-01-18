#[derive(Debug, Clone)]
pub struct Memory {
    pub last_memory_index: usize,
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            last_memory_index: 0,
        }
    }

    pub fn allocate(&mut self, size: usize) -> usize {
        let m = self.last_memory_index;
        self.last_memory_index += size;
        m
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory::new()
    }
}
