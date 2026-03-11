use std::cell::OnceCell;
use water_http::http::status_code::HttpStatusCode;
use water_http::server::mini::{CtxPtr, HandlerFn, serve};
use water_http::server::ServerConfigurations;
use crate::buf::{FortunesPool, PooledBuffer};
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;


mod date;
mod models;
mod buf;
mod db;
use crate::db::PgConnection;
use crate::date::get_date_fast;
const URL:&'static str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
// const URL:&'static str = "postgres://postgres:root@localhost:5432/techmpower";

const PLAINTEXT:&'static [u8] = br#"Hello, World!"#;

fn main() {
    let thread_init = || async {
         get_connection().await;
    };
    let mut conf = ServerConfigurations::bind("0.0.0.0", 8080);
    conf.max_cached_buffers_count = 2500;
    conf.default_write_buffer_size = conf.default_read_buffer_size * 2;
    conf.max_buffer_size_for_cache = conf.default_write_buffer_size;
    serve::<16, 10, _,_,_>(conf, HandlerFn(|ctx: CtxPtr<16, 10>| handler(ctx)),Some(thread_init));
}

async fn handler(mut ctx: CtxPtr<16,10>){
    let ctx = ctx.get();
    let req = ctx.request();

    let path = req.path();
    match path {
        "/plaintext"=>{
            ctx.set_header("Content-Type","text/plain; charset=utf-8");
            ctx.set_header("Content-Length",PLAINTEXT.len());
            ctx.set_header("Server","W");
            ctx.set_header("Date",get_date_fast());
            ctx.write_body_bytes(PLAINTEXT);
        }
        "/json" =>{
            let js = models::JsonHolder{message:"Hello, World!"};
            let mut buffer = buf::PooledBuffer::new().take_inner();
            _=sonic_rs::to_writer(&mut buffer,&js);
            ctx.set_header("Content-Type","application/json");
            ctx.set_header("Content-Length",buffer.len());
            ctx.set_header("Server","W");
            ctx.set_header("Date",get_date_fast());
            ctx.write_body_bytes(&buffer);
            buf::PooledBuffer::recycle(buffer)
        }

        "/fortunes"=>{
            let connection = get_connection().await;
            let fortunes = connection.tell_fortune().await.unwrap();
            ctx.set_header("Content-Type","text/html; charset=UTF-8");
            ctx.set_header("Server","W");
            ctx.set_header("Date",get_date_fast());
            ctx.set_header("Content-Length",fortunes.len());
            ctx.write_body_bytes(&fortunes);
            FortunesPool::save_heap_allocation(fortunes);
        }

        "/db" => {
            let connection = get_connection().await;
            let db = connection.get_world().await;
            ctx.set_header("Content-Type","application/json");
            ctx.set_header("Server","W");
            ctx.set_header("Date",get_date_fast());
            ctx.set_header("Content-Length",db.len());
            ctx.write_body_bytes(&db);
            PooledBuffer::recycle(db);
        }

        _=>{
            if path.contains("?") {
                let value = path.split("=").last()
                    .and_then(|v| v.parse::<usize>().ok())
                    .unwrap_or(1)
                    .clamp(1, 500);
                let s =&path[0..=1];
                match s {
                    "/q"=>{
                        let q = get_connection().await;
                        let data = q.get_worlds(value).await;
                        ctx.set_header("Content-Type","application/json");
                        ctx.set_header("Server","W");
                        ctx.set_header("Date",get_date_fast());
                        ctx.set_header("Content-Length",data.len());
                        ctx.write_body_bytes(&data);
                        PooledBuffer::recycle(data);
                        return
                    }
                    "/u"=> {
                        let q = get_connection().await;
                        let data = q.update(value).await;
                        ctx.set_header("Content-Type","application/json");
                        ctx.set_header("Server","W");
                        ctx.set_header("Date",get_date_fast());
                        ctx.set_header("Content-Length",data.len());
                        ctx.write_body_bytes(&data);
                        PooledBuffer::recycle(data);
                        return
                    }
                    _=>{}
                }

            }
            ctx.set_status_code(HttpStatusCode::NOT_FOUND);
        }
    }

}


thread_local! {
    static PG:OnceCell<PgConnection> = OnceCell::new();
}

async fn get_connection() -> &'static PgConnection {
    // 1. Try to get the pointer from thread-local
    let ptr = PG.with(|cell| {
        cell.get().map(|conn| conn as *const PgConnection)
    });

    if let Some(p) = ptr {
        // SAFETY: We are promoting a thread-local pointer to 'static.
        // This is only safe in TFB if the thread never dies.
        return unsafe { &*p };
    }

    let mut t_count = 0_usize;
    // 2. Initialize
    let conn = loop {
        match PgConnection::connect(URL)
            .await {
            Ok(r) => {
                break r
            }
            Err(_) => {
                t_count+=1;
                println!("failed count = {t_count}");
                if t_count > 10 {
                    panic!("could not connect to db");
                }
                continue }
        }
    };
    println!("{:?} connected successfully", std::thread::current().name().unwrap());
    // 3. Store it
    PG.with(|cell| {
         _= cell.set(conn);
    });

    // 4. Retrieve the pointer again
    let ptr = PG.with(|cell| {
        cell.get().map(|conn| conn as *const PgConnection)
    }).expect("Should be initialized");

    unsafe { &*ptr }
}