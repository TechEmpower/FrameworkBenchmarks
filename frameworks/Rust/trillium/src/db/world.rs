use sea_orm::entity::prelude::*;
use serde::Serialize;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, Serialize)]
#[sea_orm(table_name = "World")]
#[serde(rename_all = "camelCase")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i32,

    #[sea_orm(column_name = "randomnumber")]
    pub random_number: i32,
}

#[derive(Copy, Clone, Debug, EnumIter)]
pub enum Relation {}

impl RelationTrait for Relation {
    fn def(&self) -> RelationDef {
        unimplemented!()
    }
}

impl TryFrom<ActiveModel> for Model {
    type Error = ActiveModel;

    fn try_from(value: ActiveModel) -> Result<Self, Self::Error> {
        if value.id.is_unchanged() && value.random_number.is_unchanged() {
            Ok(Self {
                id: value.id.unwrap(),
                random_number: value.random_number.unwrap(),
            })
        } else {
            Err(value)
        }
    }
}

impl ActiveModelBehavior for ActiveModel {}
