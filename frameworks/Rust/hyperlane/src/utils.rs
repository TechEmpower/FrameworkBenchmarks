use crate::*;

#[inline]
pub fn escape_html(input: &str) -> String {
    let mut result: String = String::new();
    for ch in input.chars() {
        match ch {
            '<' => result.push_str("&lt;"),
            '>' => result.push_str("&gt;"),
            '&' => result.push_str("&amp;"),
            '"' => result.push_str("&quot;"),
            '\'' => result.push_str("&#39;"),
            _ => result.push(ch),
        }
    }
    result
}

#[inline]
pub fn get_random_id() -> Queries {
    let mut rand: WyRand = WyRand::new();
    let random_id: Queries = rand.generate::<Queries>() % (RANDOM_MAX as Queries + 1);
    random_id
}

#[inline]
pub fn get_random_id_list(limit: Queries) -> Vec<i32> {
    let mut id_list: Vec<i32> = (1..=limit).collect();
    let mut rng: WyRand = WyRand::new();
    let len: usize = id_list.len();
    for i in (1..len).rev() {
        let j = rng.generate_range(0..=i as u32) as usize;
        id_list.swap(i, j);
    }
    id_list.truncate(limit as usize);
    id_list
}
