import random
import sys
import os
import subprocess

class CaseMaker:
    
    def __init__(self,startup,commands,size):
        self.seqs = []
        self.startup = int(startup)
        self.commands = int(commands)
        self.size = int(size)
    
    def genSeq(self,size = 0):
        elems = list()
        if size == 0:
            size = self.size
        for i in range(random.randint(1,size)):
            choice = random.randint(0,1)
            
            if choice == 0:
                elems.append('A')
            else:
                elems.append('T')
        
        out = ''.join(elems)
        self.seqs.append(out);
        return out;
    
    def getSeq(self):
        pos = random.randint(0,len(self.seqs)-1)
        if pos == 0:
            return self.genSeq()
        return self.seqs[pos]
    
    def delSeq(self,seq):
        for i in range(len(self.seqs)):
            if self.seqs[i] == seq:
                del(self.seqs[i])
                break
    
    def genCase(self):
        
        out = []
   
        ## Startup elements (mix of ADD and SET)
        out.append("PRINT 'STARTUP'")
        for i in range(self.startup):
            seq = self.genSeq(random.randint(1,self.size))
            choice = random.randint(0,1);
            if choice == 0:
                out.append("ADD " + seq)
            else:
                out.append("SET " + seq + " " + str(random.randint(1,20)))
                
        ## Test Elements
        out.append("PRINT 'TESTING'")
        for i in range(self.commands):
            choice = random.randint(0,7)
            if choice == 0:             ## GET
                seq = self.getSeq()
                size = random.randint(1,len(seq))
                out.append("GET " + seq[:size])
            if choice == 1:             ## ADD
                seq = self.genSeq()
                out.append("ADD " + seq)
            if choice == 2:             ## GETALL
                out.append("GETALL")
            if choice == 3:             ## MAXLENGTH
                out.append("MAXLENGTH")
            if choice == 4:             ## DELETE
                seq = self.getSeq()
                out.append("DELETE " + seq)
                self.delSeq(seq);
            if choice == 5:             ## SET
                seq = self.genSeq()
                out.append("SET " + seq + " " + str(random.randint(1,20)))
            if choice == 6:             ## CHANGE
                seq1 = self.getSeq()
                seq2 = self.genSeq()
                out.append("CHANGE " + seq1 + " " + seq2)
                self.delSeq(seq1)
            if choice == 7:             ## CHANGEMERGE
                seq1 = self.getSeq()
                seq2 = self.genSeq()
                out.append("CHANGEMERGE " + seq1 + " " + seq2)
                self.delSeq(seq1)
        
        return out

def main():
    
    ## Argument Handling
    if len(sys.argv) != 5:
        print("Wrong number or arguments:\nUsage: caseMaker.py <startupElems> "
              + "<commandElems> <maxSize> <caseFile>")
        sys.exit(-1)
    
    ## File Opening
    try:
        cFile = open(sys.argv[4], 'w')
    except IOError:
        print("Error at output file. Exiting...")
        sys.exit(-1)
    
    ## Case Making
    maker = CaseMaker(sys.argv[1],sys.argv[2],sys.argv[3])
    text = "\n".join(maker.genCase()) + "\nGETALL"
    
    ## Case File Writing
    try:
        cFile.write(text)
    except IOError:
        print("Error at working with output file. Exiting...")
        sys.exit(-1)

if __name__ == "__main__":
    main()