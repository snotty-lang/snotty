#[derive(Debug, Clone, Copy)]
pub struct Number(pub u8);

#[derive(Debug)]
pub enum ReturnVal {
    Number(Number),
    None,
}

impl ReturnVal {
    pub fn get_number(&self) -> Option<Number> {
        match self {
            ReturnVal::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, ReturnVal::None)
    }
}

impl Number {
    pub fn add(self, rhs: Self) -> Self {
        let mut num = self.0 as u32 + rhs.0 as u32;
        if num > 255 {
            num -= 255;
        }
        Self(num as u8)
    }

    pub fn subtract(self, rhs: Self) -> Self {
        if self.0 > rhs.0 {
            Self(self.0 - rhs.0)
        } else {
            Self(self.0 + (255 - rhs.0))
        }
    }
}
