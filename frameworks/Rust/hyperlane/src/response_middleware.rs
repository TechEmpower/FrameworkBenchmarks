use crate::*;

pub async fn response(ctx: Context) {
    let _ = ctx.send().await;
}
