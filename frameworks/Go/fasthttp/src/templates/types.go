package templates

type Fortune struct {
	ID      int    `json:"id,omitempty"`
	Message string `json:"message,omitempty"`
}

type Fortunes struct {
	F []Fortune
}
