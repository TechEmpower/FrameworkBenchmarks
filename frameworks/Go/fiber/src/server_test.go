package main

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/goccy/go-json"
	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/utils"
)

// go test -v  -run=^$ -bench=Benchmark_Plaintext -benchmem -count=4
func Benchmark_Plaintext(b *testing.B) {
	app := fiber.New(fiber.Config{
		DisableStartupMessage: true,
		JSONEncoder:           json.Marshal,
		JSONDecoder:           json.Unmarshal,
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
		JSONEncoder:           json.Marshal,
		JSONDecoder:           json.Unmarshal,
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
