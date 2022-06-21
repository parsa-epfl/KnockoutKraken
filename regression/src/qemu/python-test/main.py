#!/usr/bin/env python3

import os

with open("stupid.py") as f:
  l = f.read()

eval(l)

for x in os.listdir("."):
  print("{} -> {}".format(len(x), x))