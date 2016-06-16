#!/bin/bash

fw_depends dotnetcore-prereqs

powershell run-windows.ps1 -scenarios 'mvcjson,mvcplain' -server kestrel
