#!/bin/python3

num = int(input())


def addEntry(inputString, dictionary):
    a = inputString.split() 
    print(a)
    key = a[0]
    val = sum(float(i) for i in a[1:]) / (len(a) - 1)
    dictionary[key] = val 



# val = sum(map(int,a[1:])) // (len(a) - 1)

i = 1 
students = {}
while i <= num:
    a = input()
    addEntry(a, students)
    i+= 1

name = str(input())


print('{:.2f}'.format(students[name]))
