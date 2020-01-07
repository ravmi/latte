#!/bin/bash
ls good/* | grep ".lat"  | while read -r line; do ./latc_x86_64 $line; done
