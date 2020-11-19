

import numpy as np
import matplotlib.pyplot as plt


thresh = np.array([1,1.5,2, 2.5, 3, 3.5,4])
error_gaussian = np.array([0.981964,0.971944,0.944890,0.911824,0.855711,
    0.731463, 0.685371])

error_uniform = np.array([0.956914,0.879760,0.712425,0.380762,0.097194,
    0.000000,0.000000])


plt.plot(thresh, error_gaussian, thresh, error_uniform)
plt.xlabel('Threshold for deterministic dm - INC achieves 0 error for both error model')
plt.ylabel('FN error')
plt.legend(['Gaussian(0,1)','Uniform(-1,1)'])


plt.savefig('threshold_vs_fn_ddm.png', dpi=300, bbox_inches='tight')
plt.show()
