#![cfg(any(feature = "db",feature = "all",feature = "uring",feature = "mini"))]
use std::{borrow::Cow, io};
use std::fmt::Arguments;
use std::io::Write;
use std::mem::MaybeUninit;
use std::rc::Rc;
use std::cell::UnsafeCell;
use bytes::{Buf, BufMut};
use nanorand::{Rng, WyRand};
use smallvec::SmallVec;
use tokio_postgres::{connect, Client, Statement, NoTls};
use tokio_postgres::types::private::BytesMut;
use crate::models::{Fortune, FortuneTemplate, World};
use sonic_rs::prelude::WriteExt;
use water_buffer::WaterBuffer;
use yarte::TemplateBytesTrait;
use crate::buf::{FortunesPool, IDsPool, PooledBuffer, WorldsPool};


use tokio::pin;

/// Database connection pool with thread-local RNG
pub struct DbConnectionPool {
    pub connections: Vec<Rc<PgConnection>>,
    pub next: UnsafeCell<usize>,
}

unsafe impl Sync for DbConnectionPool {}
unsafe impl Send for DbConnectionPool {}

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
            tasks.push(tokio::task::spawn(async move {
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
    worlds: Vec<World>,
    numbers: Vec<i32>,
    fortunes: Vec<Fortune>,
    fortune_output: Vec<u8>,
}

impl BufferPool {
    fn new() -> Self {
        Self {
            body: BytesMut::with_capacity(4096),
            worlds: Vec::with_capacity(501),
            numbers: Vec::with_capacity(501),

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

         _= tokio::task::spawn(async move {
            let _ = conn.await;
        });

        let fortune = cl.prepare("SELECT * FROM fortune").await.map_err(|_| ())?;
        let world = cl.prepare("SELECT id,randomnumber FROM world WHERE id=$1 LIMIT 1").await.map_err(|_| ())?;

        // Pre-compile update statements for batch sizes 1-500
        let mut updates = vec![];
        for num in 1..=500 {
            let sql = Self::generate_update_values_stmt(num);
            updates.push(cl.prepare(&sql).await.unwrap());
        }

        Ok(PgConnection {
            cl,
            fortune,
            world,
            updates,
            buffers: UnsafeCell::new(BufferPool::new()),
            rang: WyRand::new()
        })
    }
    /// Connect to the database
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
    pub async fn get_world(&self) -> Vec<u8> {
        let rd = (self.rang.clone().generate::<u32>() % 10_000 + 1) as i32;
        let row = self.cl.query_one(&self.world, &[&rd]).await.unwrap();

        let mut buffer = PooledBuffer::new().take_inner();

        sonic_rs::to_writer(
            &mut buffer,
            &World {
                id: row.get(0),
                randomnumber: row.get(1),
            },
        ).unwrap();

       buffer
    }

    /// Get multiple random worlds - optimized with buffer reuse
    pub async fn get_worlds(&self, num: usize) -> Vec<u8> {
        let mut worlds = WorldsPool::new().take_inner();
        worlds.clear();
        let mut rn = self.rang.clone();
        for _ in 0..num {
            let id: i32 = (rn.generate::<u32>() & 0x3FFF) as i32 % 10_000 + 1;
            let row = self.cl.query_one(&self.world, &[&id]).await.unwrap();
            worlds.push(World {
                id: row.get(0),
                randomnumber: row.get(1),
            });
        }
        let mut buffer = PooledBuffer::new().take_inner();
        sonic_rs::to_writer(&mut buffer, &worlds).unwrap();
        WorldsPool::save_heap_allocation(worlds);
        buffer
    }
    /// Update worlds in batch - optimized with buffer reuse
    /// Update worlds in batch - optimized with buffer reuse

    /// Update worlds in batch - optimized with buffer reuse

    /// Update worlds in batch - optimized with buffer reuse

    /// Update worlds in batch - optimized with RETURNING clause to minimize reads
    /// Update worlds - fetch and update each row to handle duplicates correctly
    /// Update worlds in batch using CASE statement

     pub async fn update(&self,num:usize)->Vec<u8>{

        let mut output = PooledBuffer::new().take_inner();
        let (mut ids,mut numbers) = IDsPool::new().take_inner();
        let mut worlds = WorldsPool::new().take_inner();
        let mut rn = self.rang.clone();
        let mut futures = Vec::with_capacity(num);
        let mut params: Vec<&(dyn tokio_postgres::types::ToSql + Sync)> =
                    Vec::with_capacity(num * 2);

        if worlds.len() == num {
            for w in &worlds {
                ids.push(w.id);
                params.push(&w.id);
                numbers.push(w.randomnumber);
                params.push(&w.randomnumber);
            }
        } else {
            worlds.clear();
            (0..num).for_each(|n|{
                let id = (rn.generate::<u32>() % 10_000 + 1) as i32;
                let number = (rn.generate::<u32>() % 10_000 + 1) as i32;
                ids.push(id);
                numbers.push(number);
            });

            ids.sort();
            for ind in 0..num {
                worlds.push(
                    World{
                        id:ids[ind],
                        randomnumber:numbers[ind]
                    }
                );
                params.push(&ids[ind]);
                params.push(&numbers[ind]);
            }
        }
        futures.extend(ids.iter().map(|x| async move { self.cl.query_one(&self.world, &[&x]).await }));
        futures_util::future::join_all(futures).await;
        _ = self.cl.execute(&self.updates[num - 1], &params).await;
        _= sonic_rs::to_writer(&mut output,&worlds);
        WorldsPool::save_heap_allocation(worlds);
        IDsPool::save_heap_allocation((ids,numbers));
        output
    }

    // pub async fn update(&self, num: usize) -> &[u8] {
    //     let buffers = self.buffers();
    //     let mut ids: Vec<i32> = Vec::with_capacity(num);
    //     let mut rng = self.rang.clone();
    //     let mut params: Vec<&(dyn tokio_postgres::types::ToSql + Sync)> =
    //         Vec::with_capacity(num * 2);
    //     let mut futures = vec![];
    //     for _ in 0..num {
    //         let w_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
    //         ids.push(w_id);
    //     }
    //     futures.extend(ids.iter().map(|x| async move { self.cl.query_one(&self.world, &[&x]).await }));
    //     futures_util::future::join_all(futures).await;
    //     ids.sort_unstable();
    //     buffers.worlds.clear();
    //     for index in 0..num {
    //         let s_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
    //         buffers.worlds.push(World {
    //             id: ids[index],
    //             randomnumber: s_id
    //         });
    //         buffers.numbers.push(s_id);
    //     }
    //     buffers.body.clear();
    //     for index in 0..num {
    //         params.push(&ids[index]);
    //         params.push(&buffers.numbers[index]);
    //     }
    //
    //     _ = self.cl.execute(&self.updates[num - 1], &params).await.unwrap();
    //     sonic_rs::to_writer(BytesMuteWriter(&mut buffers.body), &buffers.worlds).unwrap();
    //     buffers.body.chunk()
    // }


    pub async fn tell_fortune(&self) -> Result<Vec<u8>, ()> {
        // 1. Explicitly type the empty params to satisfy the compiler's inference
        let mut res = self.cl.query(&self.fortune, &[]).await.unwrap();
        let mut fortunes = FortunesPool::new().take_inner();
        if fortunes.is_empty() {
            fortunes.push(Fortune{
                    id:0,
                    message:Cow::Borrowed("Additional fortune added at request time.")
                });
            for r in res {
                let id :&'static str = unsafe {
                    let a :&str = r.get(1);
                    &*(a as *const str)
                };
                fortunes.push(
                    Fortune {
                        id:r.get(0),
                        message:Cow::Borrowed(id)
                    }
                );
            }
            fortunes.sort_by(|a, b| a.message.cmp(&b.message));
        }
        let mut output = PooledBuffer::new().take_inner();
        let template = FortuneTemplate {
            items: &fortunes,
        };
        template.write_call(&mut output);
        Ok(output)
    }
}

unsafe impl Sync for PgConnection {}
unsafe impl Send for PgConnection {}
/// Zero-copy writer for BytesMut
pub struct BytesMuteWriter<'a>(pub &'a mut BytesMut);

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

/// Zero-copy writer for WaterBuffer
pub struct WaterMutWriter<'a>(pub &'a mut WaterBuffer<u8>);

impl Write for WaterMutWriter<'_> {
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

impl std::fmt::Write for WaterMutWriter<'_> {
    #[inline(always)]
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.extend_from_slice(s.as_bytes());
        Ok(())
    }

    #[inline(always)]
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        let mut buf = [0u8; 4];
        self.0
            .extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
        Ok(())
    }

    #[inline(always)]
    fn write_fmt(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        std::fmt::write(self, args)
    }
}

impl WriteExt for WaterMutWriter<'_> {
    #[inline(always)]
    fn reserve_with(
        &mut self,
        additional: usize,
    ) -> Result<&mut [MaybeUninit<u8>], io::Error> {
        self.0.reserve(additional);

        unsafe {
            let ptr = self.0.as_mut_ptr().add(self.0.len()) as *mut MaybeUninit<u8>;
            Ok(std::slice::from_raw_parts_mut(ptr, additional))
        }
    }

    #[inline(always)]
    unsafe fn flush_len(&mut self, additional: usize) -> io::Result<()> {
        self.0.advance_mut(self.0.len() + additional);
        Ok(())
    }
}