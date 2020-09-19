package main

import (
	"io/ioutil"
	"math/rand"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/utils"
	"github.com/valyala/fastrand"
)

// go test -v  -run=^$ -bench=Benchmark_Random -benchmem -count=4
func Benchmark_Random(b *testing.B) {
	var rnd int
	var max = 500

	var rndU uint32
	var maxU uint32 = 500

	b.Run("fast", func(b *testing.B) {
		b.ReportAllocs()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			rndU = fastrand.Uint32n(maxU)
		}
	})
	b.Run("default", func(b *testing.B) {
		b.ReportAllocs()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			rnd = rand.Intn(max) + 1
		}
	})

	utils.AssertEqual(b, false, rndU < 0)
	utils.AssertEqual(b, false, rnd < 0)
}

// go test -v  -run=^$ -bench=Benchmark_Plaintext -benchmem -count=4
func Benchmark_Plaintext(b *testing.B) {
	app := fiber.New(fiber.Config{
		DisableStartupMessage: true,
	})

	app.Get("/plaintext", plaintextHandler)

	var (
		resp *http.Response
		err  error
	)

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		resp, err = app.Test(httptest.NewRequest("GET", "/plaintext", nil))
	}

	utils.AssertEqual(b, nil, err, "app.Test(req)")
	utils.AssertEqual(b, 200, resp.StatusCode, "Status code")
	utils.AssertEqual(b, fiber.MIMETextPlainCharsetUTF8, resp.Header.Get("Content-Type"))
	body, _ := ioutil.ReadAll(resp.Body)
	utils.AssertEqual(b, helloworldRaw, body)
}

// go test -v  -run=^$ -bench=Benchmark_JSON -benchmem -count=4
func Benchmark_JSON(b *testing.B) {
	app := fiber.New(fiber.Config{
		DisableStartupMessage: true,
	})

	app.Get("/json", jsonHandler)

	var (
		resp *http.Response
		err  error
	)

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		resp, err = app.Test(httptest.NewRequest("GET", "/json", nil))
	}

	utils.AssertEqual(b, nil, err, "app.Test(req)")
	utils.AssertEqual(b, 200, resp.StatusCode, "Status code")
	utils.AssertEqual(b, fiber.MIMEApplicationJSON, resp.Header.Get("Content-Type"))
	body, _ := ioutil.ReadAll(resp.Body)
	utils.AssertEqual(b, `{"message":"Hello, World!"}`, string(body))
}
