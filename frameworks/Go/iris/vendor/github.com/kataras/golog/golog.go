package golog

import (
	"io"

	"github.com/kataras/pio"
)

// NewLine can override the default package-level line breaker, "\n".
// It should be called (in-sync) before  the print or leveled functions.
//
// See `github.com/kataras/pio#NewLine` and `Logger#NewLine` too.
func NewLine(newLineChar string) {
	pio.NewLine = []byte(newLineChar)
}

// Default is the package-level ready-to-use logger,
// level had set to "info", is changeable.
var Default = New()

// Reset re-sets the default logger to an empty one.
func Reset() {
	Default = New()
}

// SetOutput overrides the Default Logger's Printer's output with another `io.Writer`.
func SetOutput(w io.Writer) {
	Default.SetOutput(w)
}

// AddOutput adds one or more `io.Writer` to the Default Logger's Printer.
//
// If one of the "writers" is not a terminal-based (i.e File)
// then colors will be disabled for all outputs.
func AddOutput(writers ...io.Writer) {
	Default.AddOutput(writers...)
}

// SetPrefix sets a prefix for the default package-level Logger.
//
// The prefix is the first space-separated
// word that is being presented to the output.
// It's written even before the log level text.
//
// Returns itself.
func SetPrefix(s string) *Logger {
	return Default.SetPrefix(s)
}

// SetTimeFormat sets time format for logs,
// if "s" is empty then time representation will be off.
func SetTimeFormat(s string) {
	Default.SetTimeFormat(s)
}

// SetLevel accepts a string representation of
// a `Level` and returns a `Level` value based on that "levelName".
//
// Available level names are:
// "disable"
// "fatal"
// "error"
// "warn"
// "info"
// "debug"
//
// Alternatively you can use the exported `Default.Level` field, i.e `Default.Level = golog.ErrorLevel`
func SetLevel(levelName string) {
	Default.SetLevel(levelName)
}

// Print prints a log message without levels and colors.
func Print(v ...interface{}) {
	Default.Print(v...)
}

// Println prints a log message without levels and colors.
// It adds a new line at the end.
func Println(v ...interface{}) {
	Default.Println(v...)
}

// Logf prints a leveled log message to the output.
// This method can be used to use custom log levels if needed.
// It adds a new line in the end.
func Logf(level Level, format string, args ...interface{}) {
	Default.Logf(level, format, args...)
}

// Fatal `os.Exit(1)` exit no matter the level of the logger.
// If the logger's level is fatal, error, warn, info or debug
// then it will print the log message too.
func Fatal(v ...interface{}) {
	Default.Fatal(v...)
}

// Fatalf will `os.Exit(1)` no matter the level of the logger.
// If the logger's level is fatal, error, warn, info or debug
// then it will print the log message too.
func Fatalf(format string, args ...interface{}) {
	Default.Fatalf(format, args...)
}

// Error will print only when logger's Level is error, warn, info or debug.
func Error(v ...interface{}) {
	Default.Error(v...)
}

// Errorf will print only when logger's Level is error, warn, info or debug.
func Errorf(format string, args ...interface{}) {
	Default.Errorf(format, args...)
}

// Warn will print when logger's Level is warn, info or debug.
func Warn(v ...interface{}) {
	Default.Warn(v...)
}

// Warnf will print when logger's Level is warn, info or debug.
func Warnf(format string, args ...interface{}) {
	Default.Warnf(format, args...)
}

// Info will print when logger's Level is info or debug.
func Info(v ...interface{}) {
	Default.Info(v...)
}

// Infof will print when logger's Level is info or debug.
func Infof(format string, args ...interface{}) {
	Default.Infof(format, args...)
}

// Debug will print when logger's Level is debug.
func Debug(v ...interface{}) {
	Default.Debug(v...)
}

// Debugf will print when logger's Level is debug.
func Debugf(format string, args ...interface{}) {
	Default.Debugf(format, args...)
}

// Install receives  an external logger
// and automatically adapts its print functions.
//
// Install adds a golog handler to support third-party integrations,
// it can be used only once per `golog#Logger` instance.
//
// For example, if you want to print using a logrus
// logger you can do the following:
// `golog.Install(logrus.StandardLogger())`
//
// Look `golog#Handle` for more.
func Install(logger ExternalLogger) {
	Default.Install(logger)
}

// InstallStd receives  a standard logger
// and automatically adapts its print functions.
//
// Install adds a golog handler to support third-party integrations,
// it can be used only once per `golog#Logger` instance.
//
// Example Code:
//	import "log"
//	myLogger := log.New(os.Stdout, "", 0)
//	InstallStd(myLogger)
//
// Look `golog#Handle` for more.
func InstallStd(logger StdLogger) {
	Default.InstallStd(logger)
}

// Handle adds a log handler to the default logger.
//
// Handlers can be used to intercept the message between a log value
// and the actual print operation, it's called
// when one of the print functions called.
// If it's return value is true then it means that the specific
// handler handled the log by itself therefore no need to
// proceed with the default behavior of printing the log
// to the specified logger's output.
//
// It stops on the handler which returns true firstly.
// The `Log` value holds the level of the print operation as well.
func Handle(handler Handler) {
	Default.Handle(handler)
}

// Hijack adds a hijacker to the low-level logger's Printer.
// If you need to implement such as a low-level hijacker manually,
// then you have to make use of the pio library.
func Hijack(hijacker func(ctx *pio.Ctx)) {
	Default.Hijack(hijacker)
}

// Scan scans everything from "r" and prints
// its new contents to the logger's Printer's Output,
// forever or until the returning "cancel" is fired, once.
func Scan(r io.Reader) (cancel func()) {
	return Default.Scan(r)
}

// Child (creates if not exists and) returns a new child
// Logger based on the default package-level logger instance.
//
// Can be used to separate logs by category.
func Child(name string) *Logger {
	return Default.Child(name)
}
