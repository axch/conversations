#!/bin/bash

rm -rf _site/ _cache/ && cabal run conversations -- build
