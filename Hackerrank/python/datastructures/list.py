#!/bin/python3

from operator import attrgetter

num = int(input())

def appIns(l):
    func = input()
    funclist = func.split()
    function = funclist[0]
    funcvar = map(int, funclist[1:])
    if function == 'print':
        print(l)
    else: 
        attrgetter(function) (l) (*funcvar)

L = []
for _ in range(num):
    appIns(L)

print(L)

