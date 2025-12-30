use sea_orm::entity::prelude::*;
use serde::Serialize;
use std::cmp::Ordering;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize, Eq)]
#[sea_orm(table_name = "Fortune")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i32,

    pub message: String,
}

#[derive(Copy, Clone, Debug, EnumIter)]
pub enum Relation {}

impl RelationTrait for Relation {
    fn def(&self) -> RelationDef {
        unimplemented!()
    }
}

impl ActiveModelBehavior for ActiveModel {}

impl PartialOrd for Model {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Model {
    fn cmp(&self, other: &Self) -> Ordering {
        self.message.cmp(&other.message)
    }
}
