"""
Stack.py
Contains the Stack class used by BinTree for some tree traversal operations.

Created on Jan 15, 2014
Modified on Jan 20, 2014

@author: Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
"""

class Stack(object):
    """
    Defines a Stack (FIFO behavior) using the python list
    """

    def __init__(self,startList=None):
        """
        Class Constructor
        """
        if startList != None:
            self.elems = list(startList)
        else:
            self.elems = list()
    
    def add(self,elem):
        if elem != None:
            self.elems.append(elem)
    
    def top(self):
        return self.elems[-1]
    
    def unstack(self):
        return self.elems.pop()
        
    def isEmpty(self):
        if len(self.elems) == 0:
            return True
        else:
            return False
        
    def getSize(self):
        return len(self.elems)
        