#!/bin/python3

input()

num = input()
numlist = list(map(int, num.split()))

start = numlist[0]
rest = numlist[1:]

largest = start 
large = -100 
for x in rest:
    if x == largest:
        pass
    elif x > largest:
        large = largest
        largest = x
    elif x > large:
        large = x
    else:
        pass

print(large)
