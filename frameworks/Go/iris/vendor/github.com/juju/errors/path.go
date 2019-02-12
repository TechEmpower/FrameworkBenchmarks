// Copyright 2013, 2014 Canonical Ltd.
// Licensed under the LGPLv3, see LICENCE file for details.

package errors

import (
	"fmt"
	"go/build"
	"os"
	"path/filepath"
	"strings"
)

var goPath = build.Default.GOPATH
var srcDir = filepath.Join(goPath, "src")

func trimGoPath(filename string) string {
	return strings.TrimPrefix(filename, fmt.Sprintf("%s%s", srcDir, string(os.PathSeparator)))
}
