#[derive(Debug, Clone)]
pub struct Memory {
    static_mem_size: usize,
    last_static: usize,
    used_static: usize,
    pub last_memory_index: usize,
}

impl Memory {
    pub fn new(static_mem: usize) -> Self {
        Memory {
            last_memory_index: static_mem,
            static_mem_size: static_mem,
            used_static: 0,
            last_static: 0,
        }
    }

    pub fn allocate(&mut self, size: usize) -> usize {
        let m = self.last_memory_index;
        self.last_memory_index += size;
        m
    }

    pub fn allocate_static(&mut self, size: usize) -> usize {
        let m = self.last_static;
        self.last_static += size;
        if self.last_static > self.static_mem_size {
            unreachable!("{} {}", self.last_static, self.static_mem_size);
        }
        m
    }

    pub fn get_static(&mut self, size: usize) -> usize {
        let m = self.used_static;
        self.used_static += size;
        if self.used_static > self.last_static {
            unreachable!()
        }
        m
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory::new(0)
    }
}
