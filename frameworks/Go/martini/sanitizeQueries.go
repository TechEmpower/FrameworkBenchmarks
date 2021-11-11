package main

import (
  "net/http"
  "strconv"
)

func SanitizeQueries(r *http.Request) int {
  n := 1
  max := 500
  min := 1

  if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
  }


  if n < min {
		return min
	} else if n > max {
		return max
  }

	return n
}
