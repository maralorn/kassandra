#!/usr/bin/env bash
echo "Outputing ghci config to $HIE_BIOS_OUTPUT" > hie-output
ob internal export-ghci-configuration --no-interpret .obelisk/impl > "$HIE_BIOS_OUTPUT"
