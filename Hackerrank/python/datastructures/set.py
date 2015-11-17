#!/bin/python3

input()

m = input()
mset = set(map(int, m.split()))

input()

n = input()
nset = set(map(int, n.split()))

print('\n'.join(map(str,sorted(mset ^ nset))))
