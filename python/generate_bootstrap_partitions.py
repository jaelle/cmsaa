import numpy as np
from random import sample

n = 92
allids = np.arange(n,dtype=int)

partitions = []

for i in range(100):
    print(i)
    partitions += [sample(list(allids),int(n/2))]

np.savetxt("bootstrap_partitions_test.csv",partitions,fmt="%i",delimiter=",")
