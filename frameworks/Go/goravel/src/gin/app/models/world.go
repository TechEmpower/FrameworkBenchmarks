package models

type World struct {
	ID           int32 `gorm:"primaryKey" json:"id"`
	RandomNumber int32 `gorm:"column:randomnumber" json:"randomNumber"`
}

func (r *World) TableName() string {
	return "World"
}
