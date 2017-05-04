#!/bin/bash
fw_depends dotnetcore
powershell ./run-windows.ps1 -scenarios 'mvcjson,mvcplain' -server httpsys
