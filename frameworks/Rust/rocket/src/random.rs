extern crate lazy_static;
extern crate rand;

use rand::seq::SliceRandom;
use rand::thread_rng;
use std::sync::Mutex;

lazy_static! {
    static ref RANDOM_ARRAY: Mutex<RandomArray> = Mutex::new(RandomArray::new(10000));
}

pub fn random_number() -> i32 {
    RANDOM_ARRAY
        .lock()
        .expect("Failed to lock RANDOM_ARRAY")
        .next()
}

struct RandomArray {
    pointer: usize,
    size: i32,
    data: Vec<i32>,
}

impl RandomArray {
    fn new(size: i32) -> Self {
        let mut data: Vec<i32> = (1..=size).collect();
        let mut rng = thread_rng();
        data.shuffle(&mut rng);

        RandomArray {
            pointer: 0,
            size,
            data,
        }
    }

    fn next(&mut self) -> i32 {
        if self.pointer >= self.size as usize {
            self.pointer = 1;
        } else {
            self.pointer += 1;
        }
        self.data[self.pointer - 1]
    }
}
