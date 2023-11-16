use anansi::web::Parameters;

pub fn get_query(params: &Parameters) -> i16 {
    if let Ok(q) = params.get("q") {
        if let Ok(q) = q.parse() {
            if q > 1 {
                return if q <= 500 {
                    q
                } else {
                    500
                };
            }
        } 
    }
    1
}
