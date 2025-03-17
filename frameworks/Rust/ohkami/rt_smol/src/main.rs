fn main() {
    smol::block_on(async {
        framework_benchmarks::ohkami().await
            .howl("0.0.0.0:8000").await
    })
}
