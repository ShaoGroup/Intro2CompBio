import numpy as np
import time

# set the input parameters, set to 1,0,0 for problem 2
alpha=1
beta=0
gamma=0

# uncomment/comment these lines for problem 1/2
# a='ACTC'
# b='ACC'

# comment/uncomment these lines for problem 1/2
with open('LongestCommonSeq.txt','r') as f:
    lines=f.readlines()
a=lines[0]
b=lines[1]

# computing start
start=time.time()

# dynamic programming
s=np.zeros((len(a),len(b)))
for i in range(len(a)):
    for j in range(len(b)):
        if i==0 and j==0:
            s[i,j]=0
        else:
            if i==0 or j==0:
                if i==0:
                    s[i,j]=s[i,j-1]+gamma
                if j==0:
                    s[i,j]=s[i-1,j]+gamma
            else:
                if a[i-1]==b[j-1]:
                    s[i,j]=max(s[i-1,j]+gamma,s[i,j-1]+gamma,s[i-1,j-1]+alpha)
                else:
                    s[i,j]=max(s[i-1,j]+gamma,s[i,j-1]+gamma,s[i-1,j-1]+beta)

# compute the final score
sa=s[len(a)-1,:].max()
sb=s[:,len(b)-1].max()
sf=max(sa,sb)

print sf # final score

# track back the longest common seq
ida=0
idb=0
if sa>sb:
    ida=len(a)-1
    idb=np.argmax(s[len(a)-1,:])
else:
    idb=len(b)-1
    ida=np.argmax(s[:,len(b)-1])
track=[]
while ida>0 and idb>0:
    track.append((ida,idb))
    if s[ida,idb]==s[ida-1,idb]+gamma:
        ida-=1
        continue
    if s[ida,idb]==s[ida,idb-1]+gamma:
        idb-=1
        continue
    ida-=1
    idb-=1

# common seq
seq=[]
if a[0]==b[0]:
    seq.append(a[0])
for i in track[::-1]:
    if a[i[0]]==b[i[1]]:
        seq.append(a[i[0]])
if track[0][0]<len(a)-1 or track[0][0]<len(b)-1:
    if a[-1]==b[-1]:
        seq.append(a[-1])


print ''.join(seq) # print common seq
print track


print time.time()-start # time