#!/bin/env python

from nimrod_vim import execNimCmd

proj = "/foo"

while True:
  line = raw_input("enter command: ")
  async = False
  
  if line == "quit":
    async = True
  
  print execNimCmd(proj, line, async)

  if line == "quit":
    break
  
