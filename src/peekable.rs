use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Peekable<I: Iterator> {
    peeked: VecDeque<I::Item>,
    iter: I,
}

impl<I: Iterator> Peekable<I> {
    pub fn new(iter: I) -> Peekable<I> {
        Self {
            peeked: VecDeque::new(),
            iter,
        }
    }

    pub fn peek(&mut self, n: usize) -> Option<&I::Item> {
        for _ in self.peeked.len()..n {
            self.peeked.push_back(self.iter.next()?);
        }
        self.peeked.get(n - 1)
    }
}

// impl<I: Iterator> Peekable<I>
// where
//     I::Item: Clone,
// {
//     pub fn peek_till_cloned<F>(&mut self, mut f: F) -> Option<I::Item>
//     where
//         F: FnMut(&I::Item) -> bool,
//     {
//         if let Some(a) = self.peeked.iter().find(|&item| f(item)) {
//             return Some(a.clone());
//         }

//         loop {
//             self.peeked.push_back(self.iter.next()?);
//             let item = self.peeked.iter().last().unwrap();
//             if f(item) {
//                 return Some(item.clone());
//             }
//         }
//     }
// }

impl<I: Iterator> Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.pop_front().or_else(|| self.iter.next())
    }
}
