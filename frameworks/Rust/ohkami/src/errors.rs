#![cfg(feature = "rt_tokio")]

pub enum AppError {
    Db(tokio_postgres::Error),
}

impl From<tokio_postgres::Error> for AppError {
    fn from(e: tokio_postgres::Error) -> Self {
        Self::Db(e)
    }
}

impl ohkami::IntoResponse for AppError {
    fn into_response(self) -> ohkami::Response {
        match self {
            Self::Db(_e) => {
                #[cfg(debug_assertions)]
                {
                    eprintln!("{_e:?}");
                }
                ohkami::Response::InternalServerError()
            }
        }
    }
}
