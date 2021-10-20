extern crate lazy_static;
extern crate rand;

#[cfg(test)]
use std::collections::HashMap;
use crate::request::RequestId;
#[cfg(not(test))]
use rand::seq::SliceRandom;
#[cfg(not(test))]
use rand::thread_rng;
use std::sync::Mutex;
#[cfg(test)]
use std::sync::atomic::{AtomicI32, Ordering};

#[cfg(not(test))]
lazy_static! {
    static ref RANDOM_ARRAY: Mutex<RandomArray> = Mutex::new(RandomArray::new(10000));
}

#[cfg(test)]
lazy_static! {
    static ref COUNTER_MAP: Mutex<HashMap<RequestId, AtomicI32>> = Mutex::new(HashMap::new());
}

#[cfg(not(test))]
pub fn random_number(_request_id: &RequestId) -> i32 {
    RANDOM_ARRAY
        .lock()
        .expect("Failed to lock RANDOM_ARRAY")
        .next()
}

#[cfg(test)]
pub fn random_number(request_id: &RequestId) -> i32 {
    if !COUNTER_MAP.lock().expect("Failed to lock COUNTER_MAP").contains_key(&request_id) {
        COUNTER_MAP.lock().expect("Failed to lock COUNTER_MAP").insert(*request_id, AtomicI32::new(1));
    }

    COUNTER_MAP.lock().expect("Failed to lock COUNTER_MAP").get_key_value(request_id).expect("Could not get RequestId")
        .1.fetch_add(1, Ordering::Relaxed)
}

#[cfg(not(test))]
struct RandomArray {
    pointer: usize,
    size: i32,
    data: Vec<i32>,
}

#[cfg(not(test))]
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

