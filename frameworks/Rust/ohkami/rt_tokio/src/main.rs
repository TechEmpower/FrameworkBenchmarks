fn runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
}

async fn serve<ServeFuture: Future>(
    server: impl FnOnce(tokio::net::TcpListener) -> ServeFuture,
) -> std::io::Result<()> {
    println!("start serving !");

    let socket = tokio::net::TcpSocket::new_v4()?;
    socket.set_reuseport(true)?;
    socket.set_reuseaddr(true)?;
    socket.set_nodelay(true)?;

    socket.bind("0.0.0.0:8000".parse().unwrap())?;
    server(socket.listen(4096)?).await;

    Ok(())
}

fn main() {
    for _ in 0..(num_cpus::get() - 1/*for main thread*/) {
        std::thread::spawn(|| {
            runtime().block_on(async {
                serve(|listener| async {
                    framework_benchmarks::ohkami().await.howl(listener).await
                }).await.expect("serving error")
            })
        });
    }
    runtime().block_on(async {
        serve(|listener| async {
            framework_benchmarks::ohkami().await.howl(listener).await
        }).await.expect("serving error")
    });
}
