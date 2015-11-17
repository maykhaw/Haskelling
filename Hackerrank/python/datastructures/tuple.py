#!/bin/python3
input() 

num = input()

numlist = map(int, num.split()) 

print(hash(tuple(numlist)))
