package models

type (
	Message struct {
		Message string `json:"message"`
	}
)

type (
	Worlds []World

	World struct {
		ID           int `json:"id"`
		RandomNumber int `json:"randomNumber"`
	}
)

type (
	Fortune struct {
		ID      int    `json:"id,omitempty"`
		Message string `json:"message,omitempty"`
	}
)
