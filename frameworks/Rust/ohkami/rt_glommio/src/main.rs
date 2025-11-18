use glommio::{LocalExecutorPoolBuilder, PoolPlacement, CpuSet};

fn main() {
    LocalExecutorPoolBuilder::new(PoolPlacement::MaxSpread(num_cpus::get(), CpuSet::online().ok()))
        .on_all_shards(|| async {
            framework_benchmarks::ohkami().await
                .howl("0.0.0.0:8000").await
        })
        .unwrap()
        .join_all();
}
