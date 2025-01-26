#[async_std::main]
async fn main() {
    framework_benchmarks::ohkami().await
        .howl("0.0.0.0:8000").await
}
