fn main() {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .event_interval(11)
        .global_queue_interval(31)
        .build()
        .unwrap()
        .block_on(async {
            framework_benchmarks::ohkami().await
                .howl("0.0.0.0:8000").await
        });
}
