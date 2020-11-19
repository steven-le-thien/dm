

import numpy as np
import matplotlib.pyplot as plt


thresh = np.array([0.01, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.75,0.8,0.825,0.85,0.9,1,1.1])

error = np.array([0.991960,
    0.985930,0.968844,0.970854,0.958794,0.959799,0.958794,0.946734,0.950754,0.942714,0.944724,0.944724,0.959799,0.952764,0.953769])

error_inc = np.ones(len(thresh)) * 0.912563

plt.plot(thresh, error,thresh, error_inc)
plt.xlabel('Threshold for deterministic dm')
plt.ylabel('FN error')
plt.legend(['deterministic dm', 'inc'])



plt.savefig('dna_tuning.png', dpi=300, bbox_inches='tight')
plt.show()
