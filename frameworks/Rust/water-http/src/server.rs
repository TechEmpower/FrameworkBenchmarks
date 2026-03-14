
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

   conf.max_cached_buffers_count = 2500;
   conf.default_write_buffer_size = conf.default_read_buffer_size * 2;
   conf.max_buffer_size_for_cache = conf.default_write_buffer_size;
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


#[cfg(any(feature = "json_plaintext",feature = "all",feature = "uring"))]
const P:&'static [u8] = br#"Hello, World!"#;


#[cfg(any(feature = "all",feature = "uring"))]
WaterController! {
    holder -> super::MainType,
    shared -> super::SharedType,
    name -> EntryController,
    functions -> {


         GET => json => j(cx){
            let mut sender = cx.sender();
            sender.set_header_ef("Content-Type","application/json");
            sender.set_header_ef("Server","water");
            let js = crate::models::JsonHolder{message:"Hello, World!"};
            let mut buffer = crate::buf::PooledBuffer::new().take_inner();
            _=sonic_rs::to_writer(&mut buffer,&js);
            sender.set_header_ef("Date",crate::date::get_date_fast());
            _=sender.send_data_as_final_response(http::ResponseData::Slice(&buffer)).await;
            crate::buf::PooledBuffer::recycle(buffer);
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
            sender.set_header_ef("Date",crate::date::get_date_fast());
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(&data)
            ).await;
            crate::buf::PooledBuffer::recycle(data);
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
            sender.set_header_ef("Date",crate::date::get_date_fast());
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(&data)
            ).await;
            crate::buf::PooledBuffer::recycle(data);
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
            sender.set_header_ef("Date",crate::date::get_date_fast());
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(&data)
            ).await;
            crate::buf::PooledBuffer::recycle(data);
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
            sender.set_header_ef("Server","W");
            // let date = httpdate::fmt_http_date(std::time::SystemTime::now());
            sender.set_header_ef("Date",crate::date::get_date_fast());
            _= sender.send_data_as_final_response(
                http::ResponseData::Slice(&data)
            ).await;
            crate::buf::PooledBuffer::recycle(data);
        }

    }
}


