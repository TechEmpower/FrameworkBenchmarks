#[derive(Serialize, Deserialize)]
struct Message {
    message: String,
}

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize)]
struct DatabaseRow {
	id: i32,
	randomNumber: i32
}