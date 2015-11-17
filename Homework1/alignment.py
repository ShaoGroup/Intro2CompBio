import sys
import numpy as np

     
def align_long_seq(s1,s2, alpha=1, beta=-1, gamma=-1): #align sequence function
    n = len(s1)
    m = len(s2)
    score = np.zeros((n+1,m+1),dtype=np.int_) #allocate table
    parent = np.zeros((n+1,m+1,2),dtype=np.int_) # allocate trace back table
    
    for i in range(n): # initial situation
        score[i+1,0] = gamma*i
    for i in range(0,n):
        parent[i+1,0] = (i,0)
    for j in range(m):
        score[0,j+1] = gamma*j
    for j in range(m):
        parent[0,j+1] =  (0,j)
    score[0,0] = 0

    for i in range(n):
        if i%100 == 0: # show finished rows
            print i
        for j in range(m):
            score1 = score[i,j] + (s1[i]==s2[j])*alpha + (s1[i]!=s2[j])*beta
            score2 = score[i,j+1] + gamma
            score3 = score[i+1,j] + gamma
            if ((score1 >= score2) and (score1 >= score3)): # dynamic programming here
                score[i+1,j+1] = score1
                parent[i+1,j+1] = (i,j)
            elif ((score2 >= score1) and (score2 >= score3)):
                score[i+1,j+1] = score2
                parent[i+1,j+1] = (i,j+1)
            elif ((score3 >= score1) and (score3 >= score2)):
                score[i+1,j+1] = score3
                parent[i+1,j+1] = (i+1,j)
                
    waypoint = (n,m) # trace back
    waypoints = []
    while (waypoint[0] + waypoint[1] > 0):
        waypoints.append((waypoint[0]-1, waypoint[1]-1))
        waypoint = parent[waypoint[0], waypoint[1]]
        
    waypoints = waypoints[::-1]
    s1_aln = '' # get alignment tags
    s2_aln = ''
    aln_tag = ''
    match = ''
    
    if waypoints[0][0] == -1:
        s1_aln += ('-')
        aln_tag += '*'
    else:
        s1_aln += (s1[waypoints[0][0]])
        if s1[waypoints[0][0]] == s2[waypoints[0][1]]:
            match += s1[waypoints[0][0]]
            aln_tag += '|'
        else:
            aln_tag += '*'
    
    for i in range(1,len(waypoints)):
        if waypoints[i][0] == waypoints[i-1][0]:
            s1_aln += ('-')
            aln_tag += '*'
        else:
            s1_aln += (s1[waypoints[i][0]])
            if s1[waypoints[i][0]] == s2[waypoints[i][1]]:
                match += s1[waypoints[i][0]]
                aln_tag += '|'
            else:
                aln_tag += '*'
    
    if waypoints[0][1] == -1:
        s2_aln +=('-')
    else:
        s2_aln += (s2[waypoints[0][1]])
        
    for i in range(1,len(waypoints)):
        if waypoints[i][1] == waypoints[i-1][1]:
            s2_aln += ('-')
        else:
            s2_aln += (s2[waypoints[i][1]])
            
    return (waypoints, (s1_aln, aln_tag, s2_aln), score[n,m], match) # return results
    
if __name__ == '__main__':
    
    if len(sys.argv) != 3:
        print "Usage: python alignment.py <seq1> <seq2>"
        exit(-1)
        
    seq1 = sys.argv[1]
    seq2 = sys.argv[2]

    (waypoints, (s1_aln, aln_tag, s2_aln), score,_) = align_long_seq(seq1, seq2)
    
    print "Best score:", score
    print "Alignment:"
    
    w = 80 # show results in a BLAST flavor
    for i in range(len(s1_aln)/w + 1):
        start = i*w
        end = (i+1)*w
        if end > len(s1_aln):
            end = len(s1_aln)
    
        print s1_aln[start:end]
        print aln_tag[start:end]  
        print s2_aln[start:end]
        print ''