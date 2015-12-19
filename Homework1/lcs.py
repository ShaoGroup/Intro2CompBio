from alignment import align_long_seq
import numpy as np

import sys
if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.readlines()

seq1, seq2 = lines[0].strip(), lines[1].strip()

_, tags, score, match = align_long_seq(seq1, seq2, alpha=1, beta=0, gamma=0)

print score
print match
