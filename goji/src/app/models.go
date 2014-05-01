package main

type Hello struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type FortuneMessageComparator struct{ Fortunes }

func (s FortuneMessageComparator) Less(i, j int) bool {
	return s.Fortunes[i].Message < s.Fortunes[j].Message
}
