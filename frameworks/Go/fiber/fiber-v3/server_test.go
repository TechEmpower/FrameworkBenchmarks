package main

import (
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/goccy/go-json"
	"github.com/gofiber/fiber/v3"
)

// go test -v  -run=^$ -bench=Benchmark_Plaintext -benchmem -count=4
func Benchmark_Plaintext(b *testing.B) {
	app := fiber.New(fiber.Config{
		JSONEncoder: json.Marshal,
		JSONDecoder: json.Unmarshal,
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

	assert.Equal(b, nil, err, "app.Test(req)")
	assert.Equal(b, 200, resp.StatusCode, "Status code")
	assert.Equal(b, fiber.MIMETextPlainCharsetUTF8, resp.Header.Get("Content-Type"))
	body, _ := io.ReadAll(resp.Body)
	assert.Equal(b, helloworldRaw, body)
}

// go test -v  -run=^$ -bench=Benchmark_JSON -benchmem -count=4
func Benchmark_JSON(b *testing.B) {
	app := fiber.New(fiber.Config{
		JSONEncoder: json.Marshal,
		JSONDecoder: json.Unmarshal,
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

	assert.Equal(b, nil, err, "app.Test(req)")
	assert.Equal(b, 200, resp.StatusCode, "Status code")
	assert.Equal(b, fiber.MIMEApplicationJSON, resp.Header.Get("Content-Type"))
	body, _ := io.ReadAll(resp.Body)
	assert.Equal(b, `{"message":"Hello, World!"}`, string(body))
}
