package storage

//easyjson:json
type World struct {
	ID           int `json:"id"`
	RandomNumber int `json:"randomnumber"`
}

//easyjson:json
type Worlds []World
