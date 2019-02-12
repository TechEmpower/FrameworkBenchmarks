Iris Form Binder for `context.ReadForm`
======

form-binder is a clone of the [formam](https://github.com/monoculum/forma) package, slightly edited to use the "form" tag instead of "formam".

Features
--------

* Nesting ad infinitum in `maps`, `structs` and `slices`.
* `UnmarshalText` in values and keys of maps.
* the `map`'s key supported are: `string`, `int` and variants, `uint` and variants, `uintptr`, `float32`, `float64`, `bool`, `struct`, `custom types` to one of the above types registered by function or `UnmarshalText` method, a `pointer` to one of the above types
* A field with `interface{}` that has a `map`, `struct` or `slice` as value is perfectly possible access to them! (see example below)
* decode `time.Time` with format "2006-01-02" by its UnmarshalText method.
* decode `url.URL`
* The `slice` and `array` is possible to access without to indicate a index (If it is the last field, of course)`
* You can to register a `func` for a `custom type` for all fields that include it or one in particular! (see example below)

Types
-----

The supported field types in the destination struct are:

* `string`
* `bool`
* `int`, `int8`, `int16`, `int32`, `int64`
* `uint`, `uint8`, `uint16`, `uint32`, `uint64`
* `float32`, `float64`
* `slice`, `array`
* `struct` and `struct anonymous`
* `map`
* `interface{}`
* `time.Time`
* `url.URL`
* `custom types` to one of the above types
* a `pointer` to one of the above types

**NOTE**: the nesting in `maps`, `structs` and `slices` can be ad infinitum.

Usage
-----

### Client-side - in form html

- Use symbol `.` for access a field of a struct. (i.e, `struct.field1`)
- Use `[<index>]` for access to index of a slice/array. (i.e, `struct.array[0]`). If the array/slice is the last field of the path, it is not necessary to indicate the index
- Use `[<key>]` for access to key of a map. (i.e, `struct.map[es-ES]`).

```html
<!-- file: views/form.html -->
<form method="POST">
  <input type="text" name="Name" value="Sony"/>
  <input type="text" name="Location.Country" value="Japan"/>
  <input type="text" name="Location.City" value="Tokyo"/>
  <input type="text" name="Products[0].Name" value="Playstation 4"/>
  <input type="text" name="Products[0].Type" value="Video games"/>
  <input type="text" name="Products[1].Name" value="TV Bravia 32"/>
  <input type="text" name="Products[1].Type" value="TVs"/>
  <input type="text" name="Founders[0]" value="Masaru Ibuka"/>
  <input type="text" name="Founders[0]" value="Akio Morita"/>
  <input type="text" name="Employees" value="90000"/>
  <input type="text" name="public" value="true"/>
  <input type="url" name="website" value="http://www.sony.net"/>
  <input type="date" name="foundation" value="1946-05-07"/>
  <input type="text" name="Interface.ID" value="12"/>
  <input type="text" name="Interface.Name" value="Go Programming Language"/>
  <input type="submit"/>
</form>
```

### Server-Side - in iris app

```go

// file: main.go
package main

import (
    "net/url"
    "time"

    "github.com/kataras/iris"
)

type InterfaceStruct struct {
    ID   int
    Name string
}

type Company struct {
  Public     bool      `form:"public"`
  Website    url.URL   `form:"website"`
  Foundation time.Time `form:"foundation"`
  Name       string
  Location   struct {
    Country  string
    City     string
  }
  Products   []struct {
    Name string
    Type string
  }
  Founders   []string
  Employees  int64

  Interface interface{}
}

func main() {
    app := iris.New()
    app.RegisterView(iris.HTML("./views", ".html"))

    app.Get("/", func(ctx iris.Context) {
        ctx.View("form.html")
    })

    // IMPORTANT
    app.Post("/", func(ctx iris.Context) {
        m := Company{
            Interface: &InterfaceStruct{}, // its is possible to access to the fields although it's an interface field!
        }
        err := ctx.ReadForm(&m)
        if err != nil {
            ctx.StatusCode(iris.StatusBadRequest)
            ctx.WriteString(err.Error())
        }
    })
}
```