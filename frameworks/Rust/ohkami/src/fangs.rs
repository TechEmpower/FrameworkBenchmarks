use ohkami::prelude::*;

#[derive(Clone)]
pub struct SetServer;
impl FangAction for SetServer {
    #[inline(always)]
    async fn back<'a>(&'a self, res: &'a mut ohkami::Response) {
        res.headers.set().server("ohkami");
    }
}
