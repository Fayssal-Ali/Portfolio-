import os
from Bio.Seq import Seq
from Bio import SeqIO

file = os.path.expanduser('~/Downloads/rosalind_splc.txt')
arg = list(SeqIO.parse(file, 'fasta'))

string = str(arg[0].seq)

for i in arg[1:]:
    sub = str(i.seq)
    string = string.replace(sub, '')

dna = Seq(string)

pro = dna.translate(to_stop=True)
print(pro)


height= 100
bounce =1
while bounce <=3:
    height=height * (3/5)
    bounce +=1
new = height
print (new)


