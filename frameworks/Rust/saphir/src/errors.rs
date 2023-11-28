
use saphir::prelude::*;
use mongodm::prelude::*;
use std::num::ParseIntError;
use tokio::task::JoinError;

pub enum BenchmarkControllerError {
    MongodbError(MongoError),
    ParseError,
    JoinError(JoinError),
    CannotFindRandomWorld,
}

impl Responder for BenchmarkControllerError {
    fn respond_with_builder(self, builder: Builder, _ctx: &HttpContext) -> Builder {
        builder.status(500)
    }
}


impl From<MongoError> for BenchmarkControllerError {
    fn from(e: MongoError) -> Self {
        BenchmarkControllerError::MongodbError(e)
    }
}

impl From<ParseIntError> for BenchmarkControllerError {
    fn from(_: ParseIntError) -> Self {
        BenchmarkControllerError::ParseError
    }
}

impl From<JoinError> for BenchmarkControllerError {
    fn from(e: JoinError) -> Self {
        BenchmarkControllerError::JoinError(e)
    }
}