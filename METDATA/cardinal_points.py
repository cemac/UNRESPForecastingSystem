# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 14:02:48 2020

@author: earmgr
"""
import numpy as np

fmt_string = "{:6.2f}"

x = np.array([1, 1.0, 1, 0.578, 0, -1, -1,-1, 0,  1])
y = np.array([0, 0.578, 1, 1.0, 1,  1, 0, -1,-1, -1])

ay = np.abs(y) # was thinking of using with np.sign(x)
ax = np.abs(x) # was thinking of using with np.sign(y)

wd = np.arctan2(y,x)
###print (wd)
###print("===")
wd *= 180/np.pi
###print (wd)
###print("===")
wd += 180
###print (wd)
###print("===")
wd =- wd
###print (wd)
###print("===")
wd += 90
###print (wd)
###print("===")
wd = np.mod(wd,360)

print(" == WD == ")
for i, val in enumerate(wd):
   print("{0:8.2f}".format(val) )

print("=== Angle between North and wind (Yaxis)")
degrees = np.arctan2(x,y)*180/np.pi
print("Degrees = ", degrees)
wd=np.mod(degrees,360)
cardinal = (np.arctan2(x,y))*180/np.pi
print("Cardinal ")
for i, val in enumerate(cardinal):
   print( "{0:10.1f}, {1:7.1f}, {2:7.1f}, {3:10.1f}".format(val , x[i],y[i], wd[i]) )
   
###print('{:6.2f}'.format(cardinal) )
print("===")
print("End Python")