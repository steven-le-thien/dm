
import numpy as np
import matplotlib.pyplot as plt

a = []
b = []

with open("/Users/thienle/External_Software/dm/src/haskell/dm/p_dm",'r') as file:
    flag = 1
    i = 0
    for line in file:
        flag2 = 1
        if flag:
            flag = 0
            continue
        else:
            j = 0
            for word in line.split():
                if flag2:
                    flag2 = 0
                    continue
                else:
                    a.append(float(word))
                    if float(word) > 14:
                        b.append((i,j))
                j = j + 1
        i = i + 1

print(np.histogram(a))
print(b)

#plt.hist(a, 1000)
#plt.show()
