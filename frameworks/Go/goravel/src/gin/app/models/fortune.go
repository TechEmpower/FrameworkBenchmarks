package models

type Fortune struct {
	ID      uint   `gorm:"primaryKey" json:"id"`
	Message string `gorm:"column:message" json:"message"`
}

func (r *Fortune) TableName() string {
	return "Fortune"
}
