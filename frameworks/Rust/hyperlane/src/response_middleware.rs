use crate::*;

pub async fn response(controller_data: ControllerData) {
    let _ = controller_data.send().await;
}
