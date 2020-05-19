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

// FortunePool ...
var FortunePool = &sync.Pool{
	New: func() interface{} {
		return new(Fortune)
	},
}

// AcquireFortune returns new message from pool
func AcquireFortune() *Fortune {
	return FortunePool.Get().(*Fortune)
}

// ReleaseFortune resets the message and return it to the pool
func ReleaseFortune(f *Fortune) {
	f.ID = 0
	f.Message = ""
	FortunePool.Put(f)
}

// Fortunes ...
type Fortunes []Fortune

// FortunesPool ...
var FortunesPool = sync.Pool{
	New: func() interface{} {
		return make(Fortunes, 0, 16)
	},
}

// AcquireFortunes returns new fortunes from pool
func AcquireFortunes() Fortunes {
	return FortunesPool.Get().(Fortunes)
}

// ReleaseFortunes resets the fortunes and return it to the pool
func ReleaseFortunes(f Fortunes) {
	f = f[:0]
	FortunesPool.Put(f)
}
