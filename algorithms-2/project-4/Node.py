"""
Node.py
Contains the Node class used in a binary tree structure. Holds an integer
value, with childs called 'A' and 'T'.

Created on Jan 15, 2014
Modified on Jan 20, 2014

@author: Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
"""

class Node:
    """ Defines the internal structure of the node used by the binary tree
    class called BinTree
    """
    
    def __init__(self,value=0):
        self.value = value
        self.A = None
        self.T = None
    
    def getValue(self):
        return self.value
    
    def setValue(self,value):
        self.value = value;
    
    def getA(self):
        return self.A

    def setA(self,A):
        self.A = A
            
    def getT(self):
        return self.T
    
    def setT(self,T):
        self.T = T
    
    def travel(self,char):
        if (char == 'A' and self.A != None):
            return self.getA()
        if (char == 'T' and self.T != None):
            return self.getT()
        
    