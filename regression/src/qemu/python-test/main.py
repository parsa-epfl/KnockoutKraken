#!/usr/bin/env python3

import os

with open("python-test/stupid.py") as f:
  l = f.read()

eval(l)

for x in os.listdir("python-test"):
  print("{} -> {}".format(len(x), x))