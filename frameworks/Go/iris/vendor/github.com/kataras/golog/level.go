package golog

import (
	"strings"

	"github.com/kataras/pio"
)

// Level is a number which defines the log level.
type Level uint32

// The available built'n log levels, users can add or modify a level via `Levels` field.
const (
	// DisableLevel will disable the printer.
	DisableLevel Level = iota
	// FatalLevel will `os.Exit(1)` no matter the level of the logger.
	// If the logger's level is fatal, error, warn, info or debug
	// then it will print the log message too.
	FatalLevel
	// ErrorLevel will print only errors.
	ErrorLevel
	// WarnLevel will print errors and warnings.
	WarnLevel
	// InfoLevel will print errors, warnings and infos.
	InfoLevel
	// DebugLevel will print on any level, fatals, errors, warnings, infos and debug logs.
	DebugLevel
)

// Levels contains the levels and their
// mapped (pointer of, in order to be able to be modified) metadata, callers
// are allowed to modify this package-level global variable
// without any loses.
var Levels = map[Level]*LevelMetadata{
	DisableLevel: {
		Name:             "disable",
		AlternativeNames: []string{"disabled"},
		RawText:          "",
		ColorfulText:     "",
	},
	FatalLevel: {
		Name:    "fatal",
		RawText: "[FTAL]",
		// white foreground but red background, it's nice
		ColorfulText: pio.RedBackground("[FTAL]"),
	},
	ErrorLevel: {
		Name:         "error",
		RawText:      "[ERRO]",
		ColorfulText: pio.Red("[ERRO]"),
	},
	WarnLevel: {
		Name:             "warn",
		AlternativeNames: []string{"warning"},
		RawText:          "[WARN]",
		ColorfulText:     pio.Purple("[WARN]"),
	},
	InfoLevel: {
		Name:         "info",
		RawText:      "[INFO]",
		ColorfulText: pio.LightGreen("[INFO]"),
	},
	DebugLevel: {
		Name:         "debug",
		RawText:      "[DBUG]",
		ColorfulText: pio.Yellow("[DBUG]"),
	},
}

func fromLevelName(levelName string) Level {
	for level, meta := range Levels {
		if meta.Name == levelName {
			return level
		}

		for _, altName := range meta.AlternativeNames {
			if altName == levelName {
				return level
			}
		}
	}
	return DisableLevel
}

// LevelMetadata describes the information
// behind a log Level, each level has its own unique metadata.
type LevelMetadata struct {
	// The Name of the Level
	// that named (lowercased) will be used
	// to convert a string level on `SetLevel`
	// to the correct Level type.
	Name string
	// AlternativeNames are the names that can be referred to this specific log level.
	// i.e Name = "warn"
	// AlternativeNames = []string{"warning"}, it's an optional field,
	// therefore we keep Name as a simple string and created this new field.
	AlternativeNames []string
	// Tha RawText will be the prefix of the log level
	// when output doesn't supports colors.
	//
	// When RawText is changed its ColorfulText is also changed
	// to a default color, but callers are able to change it too.
	RawText string
	// The ColorfulText will be the prefix of the log level
	// when output supports colors, almost everything except
	// os files and putty-based terminals(?).
	//
	// If ColorfulText is empty then built'n colors
	// are being used to wrap the "RawText".
	ColorfulText string
}

// Text returns the text that should be
// prepended to the log message when a specific
// log level is being written.
func (m *LevelMetadata) Text(enableColor bool) string {
	if enableColor {
		return m.ColorfulText
	}
	return m.RawText
}

// SetText can modify the prefix that will be prepended
// to the output message log when `Error/Errorf` functions are being used.
//
// If "newRawText" is empty then it will just skip the Text set-ing.
// If "newColorfulText" is empty then it will update the text color version using
// the default values by using the new raw text.
func (m *LevelMetadata) SetText(newRawText string, newColorfulText string) {
	if newRawText != "" {
		oldRawText := m.RawText
		m.RawText = newRawText
		m.ColorfulText = strings.Replace(m.ColorfulText, oldRawText, newRawText, -1)
	}
	if newColorfulText != "" {
		m.ColorfulText = newColorfulText
	}

}

var (
	// ErrorText can modify the prefix that will be prepended
	// to the output message log when `Error/Errorf` functions are being used.
	//
	// If "newColorfulText" is empty then it will update the text color version using
	// the default values by using the new raw text.
	//
	// Defaults to "[ERRO]" and pio.Red("[ERRO]").
	//
	// Deprecated Use `Levels[ErrorLevel].SetText(string, string)` instead.
	ErrorText = Levels[ErrorLevel].SetText

	// WarnText can modify the prefix that will be prepended
	// to the output message log when `Warn/Warnf` functions are being used.
	//
	// If "newColorfulText" is empty then it will update the text color version using
	// the default values by using the new raw text.
	//
	// Defaults to "[WARN]" and pio.Purple("[WARN]").
	//
	// Deprecated Use `Levels[WarnLevel].SetText(string, string)` instead.
	WarnText = Levels[WarnLevel].SetText

	// InfoText can modify the prefix that will be prepended
	// to the output message log when `Info/Infof` functions are being used.
	//
	// If "newColorfulText" is empty then it will update the text color version using
	// the default values by using the new raw text.
	//
	// Defaults to "[INFO]" and pio.LightGreen("[INFO]").
	//
	// Deprecated Use `Levels[InfoLevel].SetText(string, string)` instead.
	InfoText = Levels[InfoLevel].SetText

	// DebugText can modify the prefix that will be prepended
	// to the output message log when `Info/Infof` functions are being used.
	//
	// If "newColorfulText" is empty then it will update the text color version using
	// the default values by using the new raw text.
	//
	// Defaults to "[DBUG]" and pio.Yellow("[DBUG]").
	//
	// Deprecated Use `Levels[DebugLevel].SetText(string, string)` instead.
	DebugText = Levels[DebugLevel].SetText

	// GetTextForLevel is the function which
	// has the "final" responsibility to generate the text (colorful or not)
	// that is prepended to the leveled log message
	// when `Error/Errorf, Warn/Warnf, Info/Infof or Debug/Debugf`
	// functions are being called.
	//
	// It can be used to override the default behavior, at the start-up state.
	GetTextForLevel = func(level Level, enableColor bool) string {
		if meta, ok := Levels[level]; ok {
			return meta.Text(enableColor)
		}
		return ""
	}
)
