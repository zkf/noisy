#!/usr/bin/python

import json
import matplotlib.pyplot as plt
import tkFileDialog

filename = tkFileDialog.askopenfilename()
data = list()
for line in open(filename):
    li = line.strip()
    if not (li.startswith("#") or li == ""):
        data.append(line.split())

for item in data:
    print item
x, y, z = zip(*data)
a = [float(i) for i in x]
b = [float(i) for i in y]
c = [float(i) for i in z]

plt.errorbar(a, b, yerr=c, fmt='ro', linestyle='-')
plt.show()

