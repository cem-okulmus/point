#!/bin/bash

ocamlbuild normform.native
mv _build/normform.native point
chmod +x point