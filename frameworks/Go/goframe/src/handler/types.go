package handler

type Message struct {
	Message string `json:"message"`
}

type World struct {
	ID           int32 `json:"id"`
	RandomNumber int32 `json:"randomnumber"`
}

type Worlds struct {
	W []World
}
