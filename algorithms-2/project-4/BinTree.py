"""
BinTree.py
Contains the BinTree class which handles the required operations
over binary trees. Makes usage of the Node class for its elements.

Created on Jan 15, 2014
Modified on Jan 20, 2014

@author: Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
"""

from Node import Node
from Stack import Stack

class BinTree():
    """
    Defines the binary tree structure used to represent the A T bases
    """

    def __init__(self):
        """
        Class Constructor
        """
        self.root = Node()
    
    ## Main Functionality Methods
    
    def get(self,sequence):
        node = self.root                # Reference to traverse tree
        for char in sequence:
            # Travel Down            
            node = node.travel(char);
            # If path does not continue
            if (node == None):
                return sequence + " 0"
        
        return sequence + " " + str(node.getValue())
    
    def add(self,sequence):
        node = self.root                # Reference to traverse tree
        parent = None                   # Auxiliar if path must be created
        for char in sequence:
            parent = node
            node = parent.travel(char)

            # If path is not built
            if (node == None):
                # Build A path
                if (char == 'A'):
                    parent.setA(Node())
                    node = parent.getA()
                # Build T path
                if (char == 'T'):
                    parent.setT(Node())
                    node = parent.getT()
        
        node.setValue(node.getValue()+1);
    
    def getAll(self):
        stack = Stack()             # Stack for tree traversing
        sequence = ""               # Name of the actual traversing sequence
        status = 0;                 # Indicator of the traversing direction
        output = ""                 # String output
                
        stack.add(self.root)
        elem = self.root            # Node Reference
        last = self.root            # Auxiliar Node Reference
        
        """
        Tree Traversing is manually done using a stack and statuses to
        identify the actual traversing status in an iterative manner.
        The conditions used for a pre-order traversing are:
        
        last == stack.top().getT(): switch to status 2 (go to backward traversing)

        Status == 0: process element and stack A (switch to 1 if not)
        Status == 1: stack T and switch to 0 (switch to 2 if not)
        Status == 2: unstack element and switch to 1 (backward traversing)
        
        """
        
        # Process Tree
        while (not stack.isEmpty()):
            elem = stack.top();
            
            if last == stack.top().getT():
                last = self.root
                status = 2
                continue
            
            if status == 0:
                # Process element
                if sequence != "" and elem.getValue() != 0:
                    output += sequence + " " + str(elem.getValue()) + "\n"
                # Can't stack A
                if elem.getA() == None:
                    status = 1
                    continue
                # Stack A
                sequence += 'A'
                stack.add(elem.getA())
            
            if status == 1:
                # Can't stack T
                if elem.getT() == None:
                    status = 2
                    continue
                # Stack T
                sequence += 'T'
                stack.add(elem.getT())
                status = 0
            
            if status == 2:
                # Unstack
                sequence = sequence[:-1]
                last = stack.unstack()
                status = 1   
        # End of Tree Processing
        
        # Clean Output last newline
        output = output[:-1]
        
        return output
    
    def maxLength(self):
        return "maxlength == " + str(BinTree.maxLengthRec(self.root) - 1)
    
    def delete(self,sequence):
        node = self.root                # Reference to traverse tree
        stack = Stack()                 # Stack to trim tree

        for char in sequence:
            stack.add(node)
            node = node.travel(char)
            
            # If path is not built
            if (node == None):
                return "ERROR: Cannot DELETE."

        stack.add(node)
        node.setValue(0)
        BinTree.trimTree(stack)
        return None
    
    def set(self,sequence,value):
        node = self.root                # Reference to traverse tree
        parent = None                   # Auxiliar if path must be created
        stack = Stack()                 # Stack to trim tree
        for char in sequence:
            parent = node
            stack.add(node)
            node = parent.travel(char)

            # If path is not built
            if (node == None):
                # Build A path
                if (char == 'A'):
                    parent.setA(Node())
                    node = parent.getA()
                # Build T path
                if (char == 'T'):
                    parent.setT(Node())
                    node = parent.getT()
        
        stack.add(node)
        node.setValue(value)
        BinTree.trimTree(stack)
    
    def change(self,sequence,sequence2):

        ## Get to origin tree
        node = self.root                # Reference to traverse tree
        parent = None                   # Auxiliar if node must be deleted
        stack = Stack()                 # Stack to trim tree

        for char in sequence:
            parent = node;
            stack.add(node)
            node = node.travel(char)
            
            # If path is not built
            if (node == None):
                return

        ## Get to destination sequence        
        node2 = self.root                # Reference to traverse tree
        parent2 = None                   # Auxiliar if node must be deleted
        new = False

        for char in sequence2:
            parent2 = node2;
            node2 = node2.travel(char)
            
            # If path is not built
            if (node2 == None):
                new = True
                # Build A path
                if (char == 'A'):
                    parent2.setA(Node())
                    node2 = parent2.getA()
                # Build T path
                if (char == 'T'):
                    parent2.setT(Node())
                    node2 = parent2.getT()
        
        ## Check if valid method
        if not new:
            return "ERROR: Cannot CHANGE. Use CHANGEMERGE instead."
        
        ## Disconnect origin tree
        if parent.getA() == node:
            parent.setA(None)
        if parent.getT() == node:
            parent.setT(None)
        
        ## Reconnect tree on destination
        if parent2.getA() == node2:
            parent2.setA(node)
        if parent2.getT() == node2:
            parent2.setT(node)
        
        ## Trim origin
        BinTree.trimTree(stack)
        return None
        
    def changeMerge(self,sequence,sequence2):
        ## Get to origin tree
        node = self.root                # Reference to traverse tree
        parent = None                   # Auxiliar if node must be deleted
        stack = Stack()                 # Stack to trim tree

        for char in sequence:
            parent = node;
            stack.add(node)
            node = node.travel(char)
            
            # If path is not built
            if (node == None):
                return
        
        stack.unstack() # Should not trim last node

        ## Get to destination sequence        
        node2 = self.root                # Reference to traverse tree
        parent2 = None                   # Auxiliar if node must be deleted

        for char in sequence2:
            parent2 = node2;
            node2 = node2.travel(char)
            
            # If path is not built
            if (node2 == None):
                # Build A path
                if (char == 'A'):
                    parent2.setA(Node())
                    node2 = parent2.getA()
                # Build T path
                if (char == 'T'):
                    parent2.setT(Node())
                    node2 = parent2.getT()
        
        ## Disconnect origin tree
        if parent.getA() == node:
            parent.setA(None)
        if parent.getT() == node:
            parent.setT(None)
        
        ## Merge trees
        if parent2.getA() == node:
            char = 'A'
        if parent2.getT() == node:
            char = 'T'
        BinTree.mergeTrees(node,node2,parent2,char)
        
        ## Trim origin
        BinTree.trimTree(stack)
    
    def write(self,string):
        return string.strip("'")
    
    ## Static Methods
    
    @staticmethod
    def trimTree(stack = Stack()):
        node = None                     # Actual node
        
        while not stack.isEmpty():
            node = stack.unstack()
            
            if (node.getValue() == 0
                and node.getA() == None
                and node.getT() == None
                and not stack.isEmpty()):
            
                # Case A
                if stack.top().getA() == node:
                    stack.top().setA(None)
                # Case T
                if stack.top().getT() == node:
                    stack.top().setT(None)
            
            else:
                break
    
    @staticmethod
    def maxLengthRec(node):
        if node == None:
            return 0

        aLength = BinTree.maxLengthRec(node.getA())
        tLength = BinTree.maxLengthRec(node.getT())
        
        if (aLength >= tLength):
            return aLength + 1
        else:
            return tLength + 1
    
    @staticmethod
    def mergeTrees(origin,dest,parent,char):
        # Case 1: No origin
        if origin == None:
            return
        
        # Case 2: No dest
        if dest == None:
            # A Child
            if char == 'A':
                parent.setA(origin)
            # T Child
            if char == 'T':
                parent.setT(origin)
            return
        
        # Case 3: Both exists
        dest.setValue(dest.getValue() + origin.getValue())
        BinTree.mergeTrees(origin.getA(),dest.getA(),dest,'A')
        BinTree.mergeTrees(origin.getT(),dest.getT(),dest,'T')
