package templates

import (
	"html/template"
	"sync"
)

//go:generate qtc

const (
	fortuneHTML = `<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
{{range .}}
<tr>
<td>{{.ID}}</td>
<td>{{.Message}}</td>
</tr>
{{end}}
</table>
</body>
</html>`
)

var (
	// FortuneTemplate ...
	FortuneTemplate = template.Must(template.New("fortune.html").Parse(fortuneHTML))
)

// Fortune struct
type Fortune struct {
	ID      int    `json:"id,omitempty"`
	Message string `json:"message,omitempty"`
}

// FortunesPool *sync.Pool
var FortunesPool *sync.Pool

// InitFortunesPool ()
func InitFortunesPool() {
	FortunesPool = &sync.Pool{
		New: func() interface{} {
			return make([]Fortune, 0, 16)
		},
	}
}
