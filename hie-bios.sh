#!/usr/bin/env bash
echo "Outputing ghci config to $HIE_BIOS_OUTPUT" > hie-output
ob internal export-ghci-configuration > "$HIE_BIOS_OUTPUT"
