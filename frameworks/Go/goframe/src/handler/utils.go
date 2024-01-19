package handler

import (
	"math/rand"
	"net/http"
	"strconv"
)

func queriesParam(r *http.Request) int {
	n, _ := strconv.Atoi(r.URL.Query().Get("queries"))
	if n < 1 {
		n = 1
	} else if n > maxWorlds {
		n = maxWorlds
	}
	return n
}

func randomWorldNum() int {
	return rand.Intn(worldsCount) + 1
}
