import sys
from random import randint
from math import log, sqrt
from time import sleep

length = int(sys.argv[1])
base   = int((8 - log(length) / log(10)) * 0.1 * length)
base   = max(base, int(length * 0.05))
more   = base + randint(-base * 0.05, base * 0.05)

sleep(sqrt(length) // 300)

print('    0     1 {} message.txt'.format(length))
print('    0     1 {} message.txt.encrypted'.format(length + more))
print('    0     2 {} total'.format(length * 2 + more))

