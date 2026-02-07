#![allow(static_mut_refs)]
use std::io;
use std::fmt::Arguments;
use std::io::Write;
use std::mem::MaybeUninit;
use std::rc::Rc;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use nanorand::{Rng, WyRand};
use tokio_postgres::{connect, Client, NoTls};
use tokio_postgres::types::private::BytesMut;
use sonic_rs::prelude::WriteExt;
use std::pin::Pin;
use tokio::task::LocalSet;
use water_http::{InitControllersRoot, RunServer, WaterController};
use water_http::http::{HttpSender, ResponseData};
use water_http::server::{HttpContext, ServerConfigurations};
use water_http::http::HttpSenderTrait;

pub struct DbConnectionPool {
    pub connections: Vec<Rc<PgConnection>>,
    pub next: UnsafeCell<usize>,
}

impl DbConnectionPool {
    /// Get a connection from the pool (round-robin, relaxed ordering)
    #[inline(always)]
    pub fn get_connection(&self) -> &Rc<PgConnection> {
        let n = unsafe{&mut *self.next.get()};
        *n +=1;
        let idx = *n % self.connections.len();
        unsafe { self.connections.get_unchecked(idx) }
    }

    /// Fill the pool with connections
    pub async fn fill_pool(&mut self, url: &'static str, size: usize) {
        let mut tasks = Vec::with_capacity(size);
        for _ in 0..size {
            tasks.push(tokio::task::spawn_local(async move {
                for attempt in 0..5 {
                    match PgConnection::connect(url).await {
                        Ok(conn) => {

                            return Ok(conn); },
                        Err(_) if attempt < 4 => {
                            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                        }
                        Err(_) => return Err(()),
                    }
                }
                Err(())
            }));
        }
        for t in tasks {
            if let Ok(Ok(conn)) = t.await {
                self.connections.push(Rc::new(conn));
            }
        }
    }


}




pub struct PgConnection {
    cl:Client,
    _connection_task: tokio::task::JoinHandle<()>,
}

// Safety: Only used within LocalSet, no cross-thread access
impl PgConnection {
    /// Connect to the database

    pub async fn connect(db_url: &str) -> Result<PgConnection, ()> {
        let (cl, c) = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            connect(db_url, NoTls),
        )
            .await
            .map_err(|_| ())?
            .map_err(|_| ())?;

        let connection_task = tokio::task::spawn_local(async move {
            let _ = c.await;
        });

        Ok(PgConnection {
            _connection_task: connection_task,
            cl
        })
    }
}

/// Zero-copy writer for BytesMut
pub struct BytesMuteWriter<'a>(pub &'a mut BytesMut);

impl BytesMuteWriter<'_> {

    #[inline(always)]
    pub fn extend_from_slice(&mut self,data:&[u8]){
        self.0.extend_from_slice(data);
    }
}

impl Write for BytesMuteWriter<'_> {
    #[inline(always)]
    fn write(&mut self, src: &[u8]) -> Result<usize, io::Error> {
        self.0.extend_from_slice(src);
        Ok(src.len())
    }

    #[inline(always)]
    fn flush(&mut self) -> Result<(), io::Error> {
        Ok(())
    }
}

impl std::fmt::Write for BytesMuteWriter<'_> {
    #[inline(always)]
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.extend_from_slice(s.as_bytes());
        Ok(())
    }

    #[inline(always)]
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        let mut buf = [0u8; 4];
        self.0.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
        Ok(())
    }

    #[inline(always)]
    fn write_fmt(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        std::fmt::write(self, args)
    }
}

impl WriteExt for BytesMuteWriter<'_> {
    #[inline(always)]
    fn reserve_with(&mut self, additional: usize) -> Result<&mut [MaybeUninit<u8>], io::Error> {
        self.0.reserve(additional);
        unsafe {
            let ptr = self.0.as_mut_ptr().add(self.0.len()) as *mut MaybeUninit<u8>;
            Ok(std::slice::from_raw_parts_mut(ptr, additional))
        }
    }

    #[inline(always)]
    unsafe fn flush_len(&mut self, additional: usize) -> io::Result<()> {
        self.0.set_len(self.0.len() + additional);
        Ok(())
    }
}


InitControllersRoot! {
    name:ROOT,
    holder_type:MainType,
    shared_type:SH,
}

pub struct ThreadSharedStruct{
    writing_buffer:UnsafeCell<BytesMut>,
    rng:WyRand,
}


impl ThreadSharedStruct {

    #[inline(always)]
    pub fn get_value(id:i32)->Option<&'static i32>{
        let map = unsafe {CACHED_VALUES.as_ref().unwrap().get(&id)} ;
        map
    }
    pub fn get_cached_queries(&self,num:usize)->&[u8]{
        let buf = unsafe{&mut *(self.writing_buffer.get())};
        buf.clear();
        buf.extend_from_slice(br#"["#);
        let mut writer = BytesMuteWriter(buf);
        let mut rn = self.rng.clone();
        for _ in 0..num {
            let rd = (rn.generate::<u32>() % 10_000 ) as i32;
            let v = match Self::get_value(rd) {
                None => {continue}
                Some(c) => {c}
            };
            writer.extend_from_slice(br"{");
            _ = write!(writer, r#""id":{},"randomnumber":{}"#, rd, v);
            writer.extend_from_slice(br"},");
        }
        if buf.len() >1  {buf.truncate(buf.len() - 1);}
        buf.extend_from_slice(b"]");
        return &buf[..]
    }
}

pub type MainType = u8;
pub type SH = Rc<ThreadSharedStruct>;


static mut CACHED_VALUES:Option<HashMap<i32,i32>> = None;

pub  fn run_server(){

    _= std::thread::spawn(
        ||{
            let rt = tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap();
            rt.block_on(async move {
                const URL:&'static str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
                // const URL:&'static str = "postgres://postgres:root@localhost:5432/techmpower";

                let  mut pool = DbConnectionPool{
                    connections:Vec::with_capacity( 1
                    ),
                    next:0.into(),
                    // rt:tokio::runtime::Builder::new_multi_thread().enable_all().worker_threads(cpu_nums).build().unwrap()
                };

                let local_set = LocalSet::new();

                _= local_set.run_until(async move {
                    tokio::task::spawn_local(async move {
                        pool.fill_pool(URL, 1).await;
                        let connection = pool.get_connection();
                        let statement = connection.cl.prepare("SELECT id,randomnumber FROM World").await.unwrap();
                        let res = connection.cl.query(&statement,&[]).await.unwrap();
                        let mut map = HashMap::new();
                        for row in res {
                            map.insert(row.get(0),row.get(1));
                        }
                        unsafe {
                            let static_map = &mut CACHED_VALUES;
                            *static_map = Some(map);
                        }
                    }).await
                }).await;

            });
        }
    ).join();
    let cpu_nums = num_cpus::get();


    println!("start listening on port 8080 while workers count {cpu_nums}");
    let mut conf = ServerConfigurations::bind("0.0.0.0",8080);
    conf.worker_threads_count = cpu_nums * 1 ;

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

fn shared_factory()->Pin<Box<dyn futures_util::Future<Output=SH>>>{
    Box::pin(async {

        // const URL:&'static str = "postgres://postgres:root@localhost:5432/techmpower";

        Rc::new(ThreadSharedStruct{
            writing_buffer:UnsafeCell::new(BytesMut::with_capacity(100_000)),
            rng:WyRand::new()
        })
    })
}

pub async fn handle_cached_queries<const HS:usize,const QS:usize>(context:&mut HttpContext<'_,MainType,SH,HS,QS>){
    let q = context
        .get_from_path_query("q")
        .and_then(|v| v.parse::<usize>().ok()) // safely parse
        .unwrap_or(1)                          // default to 1 if missing or invalid
        .clamp(1, 500);

    let   connection:SH = context.thread_shared_struct.clone().unwrap().clone();
    let data = connection.get_cached_queries(q);
    let mut sender:HttpSender<HS,QS> = context.sender();
    sender.set_header_ef("Content-Type","application/json");
    sender.set_header_ef("Server","water");
    let date = httpdate::fmt_http_date(std::time::SystemTime::now());
    sender.set_header_ef("Date",date);
    _= sender.send_data_as_final_response(
        ResponseData::Slice(data)
    ).await;
}

WaterController! {
    holder -> super::MainType,
    shared -> super::SH,
    name -> EntryController,
    functions -> {

        GET -> "cached-queries" -> query (context) {
            _=super::handle_cached_queries(context).await;
        }
    }
}

fn main() {
    run_server();
}

