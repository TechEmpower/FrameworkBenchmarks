use std::{env, str::FromStr};

use core::fmt::Debug;
use rand::{distributions::Uniform, rngs::SmallRng, Rng};
pub mod models;
pub mod utils;

#[cfg(feature = "simd-json")]
pub mod simd_json;

#[allow(dead_code)]
pub const SELECT_ALL_FORTUNES: &str = "SELECT * FROM fortune";
#[allow(dead_code)]
pub const SELECT_WORLD_BY_ID: &str =
    "SELECT id, randomnumber FROM world WHERE id = $1 LIMIT 1";
#[allow(dead_code)]
pub const SELECT_ALL_CACHED_WORLDS: &str =
    "SELECT id, randomnumber FROM world ORDER BY id";
#[allow(dead_code)]
pub const UPDATE_WORLDS: &str = "WITH vals AS (SELECT * FROM UNNEST($1::int[], $2::int[]) AS v(id, rnum))
    UPDATE world SET randomnumber = new.rnum FROM
  (SELECT w.id, v.rnum FROM world w INNER JOIN vals v ON v.id = w.id ORDER BY w.id FOR UPDATE) AS new
  WHERE world.id = new.id";

/// Return the value of an environment variable.
#[allow(dead_code)]
pub fn get_env<T: FromStr>(key: &str) -> T
where
    <T as FromStr>::Err: Debug,
{
    env::var(key)
        .unwrap_or_else(|_| panic!("{key} environment variable was not set"))
        .parse::<T>()
        .unwrap_or_else(|_| panic!("could not parse {key}"))
}

/// Generate a single integer in the range 1 to 10,000 (inclusive)
#[allow(dead_code)]
#[inline]
pub fn random_id(rng: &mut SmallRng) -> i32 {
    rng.gen_range(1..10_001)
}

/// Generate vector of integers in the range 1 to 10,000 (inclusive)
#[allow(dead_code)]
#[inline]
pub fn random_ids(rng: &mut SmallRng, count: usize) -> Vec<i32> {
    rng.sample_iter(Uniform::new(1, 10_001))
        .take(count)
        .collect()
}
