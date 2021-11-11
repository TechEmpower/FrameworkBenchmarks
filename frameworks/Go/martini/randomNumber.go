package main

import "math/rand"

func RandomNumber() uint16 {
  max := 10000

  return uint16(rand.Intn(max) + 1)
}
