use anansi::*;

mod urls;
mod project;
mod http_errors;
mod hello;

apps! {
    hello,
}

app_statics! {}

min_main!(server, {
    use anansi::cache::prelude::*;
    use hello::records::World;
    use anansi::records::Record;
    use anansi::db::AsDb;
    let worlds = World::get_all().raw_query(server.app_data.as_db()).await.expect("problem fetching worlds");
    let mut items = vec![];
    for world in worlds {
        let id = world.pk().to_string();
        let mut bytes = serde_json::to_vec(&world).expect("problem serializing world");
        bytes.push(',' as u8);
        items.push((id, bytes));
    }
    server.cache.set_many(&items).await.expect("problem caching world");
});
