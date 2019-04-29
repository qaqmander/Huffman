#!/usr/bin/env python
# -*- coding: utf-8 -*-

from random import randint
import sys

f = open('message.txt', 'w')
for i in range(int(sys.argv[1])):
    ch = randint(0, 26)
    if ch < 26:
        ch = chr(ch + 97)
    else:
        ch = ' '
    f.write(ch)
    # f.write(str(randint(0, 1)))
f.close()
