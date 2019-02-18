package templates

//go:generate qtc

// Fortune struct
type Fortune struct {
	ID      int    `json:"id,omitempty"`
	Message string `json:"message,omitempty"`
}
