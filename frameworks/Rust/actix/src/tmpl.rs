use horrorshow::helper::doctype;

use models::Fortune;

#[inline]
pub fn render_fortune(items: &Vec<Fortune>) -> String {
    format!(
        "{}",
        html! {
            : doctype::HTML;
            html {
                head {
                    title : "Fortunes";
                }
                body {
                    table {
                        tr {
                            th { : "id" }
                            th { : "message" }
                        }
                        // You can embed for loops, while loops, and if statements.
                        @ for item in items {
                            tr {
                                td { :&item.id }
                                td { :&item.message }
                            }
                        }
                    }
                }
            }
        }
    )
}
