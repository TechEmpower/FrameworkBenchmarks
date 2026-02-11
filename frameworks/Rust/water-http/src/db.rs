#![cfg(any(feature = "db",feature = "all"))]
use std::{borrow::Cow, io, ptr};
use std::fmt::Arguments;
use std::io::Write;
use std::mem::MaybeUninit;
use std::rc::Rc;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use bytes::Buf;
use nanorand::{Rng, WyRand};
use tokio_postgres::{connect, Client, Statement, NoTls, Error};
use tokio_postgres::types::private::BytesMut;
use crate::models::{Fortune, FortuneTemplate, World};
use sonic_rs::prelude::WriteExt;
use yarte::TemplateBytesTrait;
pub static  mut CACHED_VALUES:Option<HashMap<i32,i32>> = None;

/// Database connection pool with thread-local RNG
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


/// Reusable buffer pool per connection
struct BufferPool {
    body: BytesMut,
    fortunes: Vec<Fortune>,
    fortune_output: Vec<u8>,
}

impl BufferPool {
    fn new() -> Self {
        Self {
            body: BytesMut::with_capacity(4096),

            fortunes: Vec::with_capacity(501),
            fortune_output: Vec::with_capacity(4096),
        }
    }

}

/// PostgreSQL connection wrapper with pre-allocated buffers
pub struct PgConnection {
    pub cl: Client,
    pub fortune: Statement,
    pub world: Statement,
    pub updates: Vec<Statement>,
    rang:WyRand,
    buffers: UnsafeCell<BufferPool>,
    _connection_task: tokio::task::JoinHandle<()>,
}

// Safety: Only used within LocalSet, no cross-thread access
impl PgConnection {
    /// Connect to the database

    pub async fn connect(db_url: &str) -> Result<PgConnection, ()> {
        let (cl, conn) = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            connect(db_url, NoTls),
        )
            .await
            .map_err(|_| ())?
            .map_err(|_| ())?;

        let connection_task = tokio::task::spawn_local(async move {
            let _ = conn.await;
        });

        let fortune = cl.prepare("SELECT * FROM fortune").await.map_err(|_| ())?;
        let world = cl.prepare("SELECT id,randomnumber FROM world WHERE id=$1 LIMIT 1").await.map_err(|_| ())?;

        // Pre-compile update statements for batch sizes 1-500
        let mut updates = vec![];
        for num  in 1..=500 {
            let sql = Self::generate_update_values_stmt(num);
            updates.push(cl.prepare(&sql).await.unwrap());
        }

        Ok(PgConnection {
            cl,
            fortune,
            world,
            updates,
            buffers: UnsafeCell::new(BufferPool::new()),
            _connection_task: connection_task,
            rang: WyRand::new()
        })
    }    /// Connect to the database

    #[inline(always)]
    pub fn generate_update_values_stmt(batch_size: usize) -> String {

        let mut sql = String::from("UPDATE world SET randomNumber = w.r FROM (VALUES ");

        for i in 0..batch_size {
            let id_param = i * 2 + 1;
            let val_param = id_param + 1;
            sql.push_str(&format!("(${}::int, ${}::int),", id_param, val_param));
        }

        // Remove the trailing comma
        sql.pop();

        sql.push_str(") AS w(i, r) WHERE world.id = w.i");
        sql
    }

    /// Get mutable access to buffers (safe because connection pool ensures single access)
    #[inline(always)]
    fn buffers(&self) -> &mut BufferPool {
        unsafe { &mut *self.buffers.get() }
    }

    /// Get a single random world - optimized with buffer reuse
    #[inline]
    pub async fn get_world(&self) -> &[u8] {
        let rd = (self.rang.clone().generate::<u32>() % 10_000 ) as i32;
        let row = self.cl.query_one(&self.world, &[&rd]).await.unwrap();
        let buffers = self.buffers();
        buffers.body.clear();
        sonic_rs::to_writer(
            BytesMuteWriter(&mut buffers.body),
            &World {
                id: row.get(0),
                randomnumber: row.get(1),
            },
        ).unwrap();
        buffers.body.chunk()
    }

    /// Get multiple random worlds - optimized with buffer reuse
    pub async fn get_worlds(&self, num: usize) -> &[u8] {
        let buffers = self.buffers();
        let mut worlds = Vec::with_capacity(num);
        for _ in 0..num {
            let id = (self.rang.clone().generate::<u32>() % 10_000 ) as i32;
            let row = self.cl.query_one(&self.world, &[&id]).await.unwrap();
           worlds.push(World {
                id: row.get(0),
                randomnumber: row.get(1),
            });
        }
        buffers.body.clear();
        sonic_rs::to_writer(BytesMuteWriter(&mut buffers.body), &worlds).unwrap();
        buffers.body.chunk()
    }
    /// Update worlds in batch - optimized with buffer reuse
    /// Update worlds in batch - optimized with buffer reuse

    /// Update worlds in batch - optimized with buffer reuse

    /// Update worlds in batch - optimized with buffer reuse

    /// Update worlds in batch - optimized with RETURNING clause to minimize reads
    /// Update worlds - fetch and update each row to handle duplicates correctly
    /// Update worlds in batch using CASE statement
    pub async fn update(&self, num: usize) -> &[u8] {

        let buffers = self.buffers();
        let mut ids:Vec<i32> = Vec::with_capacity(num);
        let mut rng = self.rang.clone();
        let mut params: Vec<&(dyn tokio_postgres::types::ToSql + Sync)> =
            Vec::with_capacity(num * 2);
        let mut futures =vec![];
        for _ in 0..num {
            let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            ids.push(w_id);
        }
        futures.extend(ids.iter().map(|x| async move {self.cl.query_one(&self.world,&[&x]).await}));
        futures_util::future::join_all(futures).await;
        ids.sort_unstable();
        let mut worlds = Vec::with_capacity(num);
        let mut numbers = Vec::with_capacity(num);
        for index in 0..num {
            let s_id = (rng.generate::<u32>() % 10_000 + 1 ) as i32;
            worlds.push(World{
                id:ids[index],
                randomnumber:s_id
            });
           numbers.push(s_id);
        }
        buffers.body.clear();
        for index in 0..num {
            params.push(&ids[index]);
            params.push(&numbers[index]);
        }

        _=self.cl.execute(&self.updates[num - 1], &params).await.unwrap();
        sonic_rs::to_writer(BytesMuteWriter(&mut buffers.body), &worlds).unwrap();
        buffers.body.chunk()
    }


    /// Tell fortunes - optimized with buffer reuse
    pub async fn tell_fortune(&self) -> Result<&[u8], ()> {
        let res = self.cl.query(&self.fortune, &[]).await.map_err(|_| ())?;

        let buffers = self.buffers();
        buffers.fortunes.clear();
        buffers.fortune_output.clear();

        buffers.fortunes.push(Fortune {
            id: 0,
            message: Cow::Borrowed("Additional fortune added at request time."),
        });

        for row in res {
            buffers.fortunes.push(Fortune {
                id: row.get(0),
                message: Cow::Owned(row.get(1)),
            });
        }

        buffers.fortunes.sort_unstable_by(|a, b| a.message.cmp(&b.message));

        let template = FortuneTemplate { items: &buffers.fortunes };
        template.write_call(&mut buffers.fortune_output);

        // Return reference to buffer - zero-copy!
        Ok(&buffers.fortune_output)
    }


    pub fn get_cached_queries(&self,num:usize)->&[u8]{
        let buf = self.buffers();
        let buf = &mut buf.body;
        buf.clear();
        buf.extend_from_slice(br#"["#);
        let mut writer = BytesMuteWriter(buf);
        for _ in 0..num {
            let rd = (self.rang.clone().generate::<u32>() % 10_000 ) as i32;
            let v = match self.get_world_id_for_cache(rd){
                None => {continue}
                Some(e)=>{e}
            };
            writer.extend_from_slice(br"{");
            _ = write!(writer, r#""id":{},"randomnumber":{}"#, rd, v);
            writer.extend_from_slice(br"},");
        }
        if buf.len() >1  {buf.truncate(buf.len() - 1);}
        buf.extend_from_slice(b"]");
        return &buf[..]
    }

    fn get_world_id_for_cache(&self, id: i32) -> Option<&i32> {
        unsafe {
            let ptr = ptr::addr_of!(CACHED_VALUES);

            match &*ptr {
                Some(map) => map.get(&id),
                None => None,
            }
        }
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