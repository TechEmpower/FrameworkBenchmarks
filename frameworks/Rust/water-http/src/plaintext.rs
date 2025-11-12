use water_http::{InitControllersRoot, RunServer, WaterController};
use water_http::server::ServerConfigurations;

InitControllersRoot! {
    name:ROOT,
    holder_type:MainType,
}



pub type MainType = u8;


fn main() {
    run_server();
}


pub    fn run_server(){

    let cpu_nums = num_cpus::get();


    println!("start listening on port 8080 while workers count {cpu_nums}");
    let mut conf = ServerConfigurations::bind("0.0.0.0",8080);
    conf.worker_threads_count = cpu_nums * 1 ;

    RunServer!(
        conf,
        ROOT,
        EntryController
    );
}

const JSON_RESPONSE:&'static [u8] = br#"{"message":"Hello, World!"}"#;
const P:&'static [u8] = br#"Hello, World!"#;


WaterController! {
    holder -> super::MainType,
    name -> EntryController,
    functions -> {

         GET => plaintext => p(cx) {
            let mut sender = cx.sender();
            sender.set_header_ef("Content-Type","text/plain; charset=utf-8");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
           _=sender.send_data_as_final_response(http::ResponseData::Str("Hello, World!")).await;
        }
    }
}

