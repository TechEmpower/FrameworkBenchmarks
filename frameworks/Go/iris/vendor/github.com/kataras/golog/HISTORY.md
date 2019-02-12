## We 02 August 2017 | v0.0.8

Add `fatal` level and `Fatal/Fatalf` funcs.

### v0.0.6 

## Su 30 July 2017 | v0.0.6 && v0.0.7

### v0.0.6 

Add a `SetPrefix(string)` and `Child(string) *Logger`.

Default package-level logger
```go
// automatically prefixed as "Router: "
golog.Child("Router").Errorf("Route %s already exists", "/mypath")
// able to prefix the main logger or child 
golog.Child("Server").SetPrefix("HTTP Server: ").Infof("Server is running at %s", ":8080")
// Child does return a new *Logger based on its parent
srvLogger := golog.Child("Server")
```

Same for independent instances
```go
log := golog.New()
log.SetPrefix("App#1: ")

routerLogger := log.Child("Router")
routerLogger.Errorf("Route %s already exists", "/mypath")
```

Example can be found [here](_examples/child/main.go).

### v0.0.7

Users are now able to add custom or modify existing levels with an easy to remember API.

```go
package main

import (
	"github.com/kataras/golog"
)

func main() {
	// Let's add a custom level,
	//
	// It should be starting from level index 6,
	// because we have 6 built'n levels  (0 is the start index):
	// disable,
	// fatal,
	// error,
	// warn,
	// info
	// debug

	// First we create our level to a golog.Level
	// in order to be used in the Log functions.
	var SuccessLevel golog.Level = 6
	// Register our level, just three fields.
	golog.Levels[SuccessLevel] = &golog.LevelMetadata{
		Name:    "success",
		RawText: "[SUCC]",
		// ColorfulText (Green Color[SUCC])
		ColorfulText: "\x1b[32m[SUCC]\x1b[0m",
	}

	// create a new golog logger
	myLogger := golog.New()

	// set its level to the higher in order to see it
	// ("success" is the name we gave to our level)
	myLogger.SetLevel("success")

	// and finally print a log message with our custom level
	myLogger.Logf(SuccessLevel, "This is a success log message with green color")
}
```

Example can be found [here](_examples/customize-levels/new-level/main.go).

## Sa 29 July 2017 | v0.0.4 & v0.0.5

### v0.0.4
- Fix an issue occurred by previous chnages, which [pio](https://github.com/kataras/pio) appends a trailing new line.

- Add a new method `golog#NewLine` which can override the default line breaker chars "\n".

### v0.0.5

Default output is `os.Stdout` instead of `os.Stderr` now, you can change it by `golog#SetOutput`.

Users are now able to customize both raw and colorful leveled log messages' prefix, example below.

Example Code:

```go
package main

import (
    "github.com/kataras/golog"
)

func main() {

    // First argument is the raw text for outputs
    // that are not support colors,
    // second argument is the full colorful text (yes it can be different if you wish to).
    //
    // If the second argument is empty then golog will update the colorful text to the
    // default color (i.e red on ErrorText) based on the first argument.

    // Default is "[ERRO]"
    golog.ErrorText("|ERROR|", "")
    // Default is "[WARN]"
    golog.WarnText("|WARN|", "")
    // Default is "[INFO]"
    golog.InfoText("|INFO|", "")
    // Default is "[DBUG]"
    golog.DebugText("|DEBUG|", "")

    // Business as usual...
    golog.SetLevel("debug")

    golog.Println("This is a raw message, no levels, no colors.")
    golog.Info("This is an info message, with colors (if the output is terminal)")
    golog.Warn("This is a warning message")
    golog.Error("This is an error message")
    golog.Debug("This is a debug message")
}
```

> This feature has been implemented after @carlca 's suggestion, [here](https://github.com/kataras/golog/issues/2).


## Th 27 July 2017 | v0.0.3

Increase the logger's performance by reducing the use of buffers on the [pio library](https://github.com/kataras/pio)