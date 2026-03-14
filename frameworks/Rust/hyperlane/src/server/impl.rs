use super::*;

impl QueryRow {
    #[inline(always)]
    pub fn new(id: Queries, random_number: Queries) -> Self {
        Self {
            id,
            randomNumber: random_number,
        }
    }
}

impl Fortunes {
    #[inline(always)]
    pub fn new(id: Queries, message: String) -> Self {
        Self { id, message }
    }
}

impl FortunesTemplate {
    #[inline(always)]
    pub fn new(list: Vec<Fortunes>) -> Self {
        Self(list)
    }
}

impl fmt::Display for FortunesTemplate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(
            f,
            "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
        );
        for tem in self.0.iter() {
            let row: String = format!(
                "<tr><td>{}</td><td>{}</td></tr>",
                tem.id,
                escape_html(&tem.message)
            );
            let _ = write!(f, "{row}");
        }
        let _ = write!(f, "</table></body></html>");
        Ok(())
    }
}
