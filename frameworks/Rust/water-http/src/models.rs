use std::borrow::Cow;
use sonic_rs::{Serialize};
// use askama::Template;
#[derive(Serialize)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}
#[derive(Serialize,Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

#[derive(yarte::TemplateBytes)]
#[template(path = "fortune.hbs")]
pub struct FortuneTemplate<'a>{
    pub items:&'a Vec<Fortune>
}
#[derive(Serialize,Debug)]
pub struct JsonHolder {
    pub message:&'static str
}



// pub async fn to(model:FortuneTemplate<'_>){
//     model.r
// }