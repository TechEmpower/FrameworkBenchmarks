#!/bin/bash

fw_depends dotnetcore-prereqs

powershell run-windows.ps1 -scenarios [default] -server weblistener
