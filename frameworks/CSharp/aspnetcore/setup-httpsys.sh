#!/bin/bash
fw_depends dotnetcore
powershell ./run-windows.ps1 -scenarios [default] -server httpsys
