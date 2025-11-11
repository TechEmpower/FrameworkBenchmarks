
use std::pin::Pin;
use std::rc::Rc;
use water_http::{InitControllersRoot, RunServer, WaterController};
use water_http::server::ServerConfigurations;
use crate::db::{DbConnectionPool};
InitControllersRoot! {
    name:ROOT,
    holder_type:MainType,
    shared_type:SharedType,
}

pub struct ThreadSharedStruct{
    pg_connection: DbConnectionPool
}

pub type MainType = u8;
pub type SharedType = Rc<ThreadSharedStruct>;




pub    fn run_server(){

    let cpu_nums = num_cpus::get();


    println!("start listening on port 8080 while workers count {cpu_nums}");
    let mut conf = ServerConfigurations::bind("0.0.0.0",8080);
    #[cfg(feature = "json_plaintext")]
    {
        conf.worker_threads_count = cpu_nums * 2 ;
    }
    #[cfg(not(feature = "json_plaintext"))]
    {
        conf.worker_threads_count = cpu_nums * 1 ;
    }

   // let addresses =  (0..cpu_nums).map(|_| {
   //      ("0.0.0.0".to_string(),8080)
   //  }).collect::<Vec<(String,u16)>>();
   //  conf.addresses = addresses;
    RunServer!(
        conf,
        ROOT,
        EntryController,
       shared_factory
    );
}

fn shared_factory()->Pin<Box<dyn futures_util::Future<Output=SharedType>>>{
    Box::pin(async {

        // const URL:&'static str = "postgres://postgres:root@localhost:5432/techmpower";
        const URL:&'static str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
        let  mut pool = DbConnectionPool{
            connections:Vec::with_capacity( 1
            ),
            next:0.into(),
            // rt:tokio::runtime::Builder::new_multi_thread().enable_all().worker_threads(cpu_nums).build().unwrap()
        };
        pool.fill_pool(URL, 1).await;
        Rc::new(ThreadSharedStruct{
            pg_connection:pool
        })
    })
}

#[cfg(any(feature = "json_plaintext",feature = "all"))]
const JSON_RESPONSE:&'static [u8] = br#"{"message":"Hello, World!"}"#;
#[cfg(any(feature = "json_plaintext",feature = "all"))]
const P:&'static [u8] = br#"Hello, World!"#;

#[cfg(feature = "all")]
WaterController! {
    holder -> super::MainType,
    shared -> super::SharedType,
    name -> EntryController,
    functions -> {


         GET => json => j(cx){
            let mut sender = cx.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _=sender.send_data_as_final_response(http::ResponseData::Slice(super::JSON_RESPONSE)).await;
        }

         GET => plaintext => p(cx){
            let mut sender = cx.sender();
            sender.set_header_ef("Content-Type","text/plain; charset=utf-8");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
           _=sender.send_data_as_final_response(http::ResponseData::Slice(super::P)).await;
        }


        GET -> db -> db (context){
            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data =  connection.get_world().await;
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(data)
            ).await;
        }
          GET -> queries -> query (context){
         let q = context
             .get_from_path_query("q")
             .and_then(|v| v.parse::<usize>().ok()) // safely parse
             .unwrap_or(1)                          // default to 1 if missing or invalid
             .clamp(1, 500);

            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data =  connection.get_worlds(q).await;
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(data)
            ).await;
        }

           GET -> updates -> update (context){
            let q = context
             .get_from_path_query("q")
             .and_then(|v| v.parse::<usize>().ok()) // safely parse
             .unwrap_or(1)                          // default to 1 if missing or invalid
             .clamp(1, 500);
            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data =  connection.update(q).await;
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(data)
            ).await;
        }


        GET -> fortunes -> ft (context){

            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data = match connection.tell_fortune().await {
                Ok(r)=>{r},
                _=>{
                    _= context.send_str("failed to connect").await;
                    return
                }
            };
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","text/html; charset=UTF-8");
              sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(&data)
            ).await;
        }

    }
}



#[cfg(all(not(feature = "all"),feature = "json_plaintext"))]
WaterController! {
    holder -> super::MainType,
    shared -> super::SharedType,
    name -> EntryController,
    functions -> {


         GET => json => j(cx){
            let mut sender = cx.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _=sender.send_data_as_final_response(http::ResponseData::Slice(super::JSON_RESPONSE)).await;
        }

         GET => plaintext => p(cx) {
            let mut sender = cx.sender();
            sender.set_header_ef("Content-Type","text/plain; charset=utf-8");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
           _=sender.send_data_as_final_response(http::ResponseData::Slice(super::P)).await;
        }
    }
}

#[cfg(all(not(feature = "all"),feature = "db"))]
WaterController! {
    holder -> super::MainType,
    shared -> super::SharedType,
    name -> EntryController,
    functions -> {


        GET -> db -> db (context){
            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data =  connection.get_world().await;
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(data)
            ).await;
        }
          GET -> queries -> query (context){
         let q = context
             .get_from_path_query("q")
             .and_then(|v| v.parse::<usize>().ok()) // safely parse
             .unwrap_or(1)                          // default to 1 if missing or invalid
             .clamp(1, 500);

            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data =  connection.get_worlds(q).await;
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(data)
            ).await;
        }

           GET -> updates -> update (context){
            let q = context
             .get_from_path_query("q")
             .and_then(|v| v.parse::<usize>().ok()) // safely parse
             .unwrap_or(1)                          // default to 1 if missing or invalid
             .clamp(1, 500);
            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data =  connection.update(q).await;
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(data)
            ).await;
        }


        GET -> fortunes -> ft (context){

            let   connection:Shared = context.thread_shared_struct.clone().unwrap().clone();
            let connection = connection.pg_connection.get_connection();
            let data = match connection.tell_fortune().await {
                Ok(r)=>{r},
                _=>{
                    _= context.send_str("failed to connect").await;
                    return
                }
            };
            let mut sender = context.sender();
            sender.set_header_ef("Content-Type","text/html; charset=UTF-8");
              sender.set_header_ef("Server","water");
            let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",date);
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(&data)
            ).await;
        }
    }
}