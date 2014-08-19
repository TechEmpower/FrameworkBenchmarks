#!/bin/bash

echo "Installing RootCAs from Mozilla..."; 
mozroots --import --sync;
sudo mozroots --import --sync;

fw_depends nginx mono xsp

echo "Installing RootCAs from Mozilla..."; 
mozroots --import --sync;
sudo mozroots --import --sync;