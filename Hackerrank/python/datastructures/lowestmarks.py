#!/bin/python3

numStudents = input()

dictlist = {} 
for _ in range(int(numStudents)):
    name = input()
    score = float(input())
    if score in dictlist:
        dictlist[score].append(name)
    else:
        dictlist[score] = [name]
scores = sorted(dictlist.keys())

sndlow = scores[1]

print('\n'.join(sorted(dictlist[sndlow])))
