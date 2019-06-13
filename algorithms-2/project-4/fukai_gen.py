"""
fukai_gen.py
Contains the main function to properly run the fukai_gen project.

Created on Jan 20, 2014
Modified on Jan 20, 2014

@author: Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
"""

import sys
from BinTree import BinTree

def cmdParser(tree,string):
    words = string.split()
    if words[0] == "GET":
        return tree.get(words[1])
    if words[0] == "ADD":
        tree.add(words[1])
        return None
    if words[0] == "GETALL":
        return tree.getAll()
    if words[0] == "MAXLENGTH":
        return tree.maxLength()
    if words[0] == "DELETE":
        return tree.delete(words[1])
    if words[0] == "SET":
        tree.set(words[1],int(words[2]))
        return None
    if words[0] == "CHANGE":
        return tree.change(words[1],words[2])
    if words[0] == "CHANGEMERGE":
        tree.changeMerge(words[1],words[2])
        return None
    if words[0] == "PRINT":
        return tree.write(" ".join(words[1:]))

def main():
    
    ## Argument Handling
    if len(sys.argv) != 3:
        print("Wrong number or arguments:\nUsage: fukai_gen.py <input> <output>")
        sys.exit(-1)
    
    ## Object Initialization
    try:
        iFile = open(sys.argv[1], 'r')
        oFile = open(sys.argv[2], 'w')
    except IOError:
        print("Error at opening files. Exiting...")
        sys.exit(0)
    tree = BinTree()
    
    ## Command Processing
    try:
        for line in iFile:
                output = cmdParser(tree, line)
                if output != None:
                    oFile.write(output)
                    oFile.write("\n")
    except IOError:
        print("Error at working with files. Exiting...")
        sys.exit(0)

if __name__ == "__main__":
    main()