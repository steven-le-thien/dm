import numpy as np
import matplotlib.pyplot as plt


dm_det = np.array([2324065000000.0, 2403162000000.0, 2305439000000.0,
    2441835000000.0, 2268826000000.0]) / 10 ** 12
dm_rand = np.array([1922924000000.0, 2082982000000.0, 1960010000000.0, 2103461000000.0,
        2206508000000]) / 10 ** 12
#inc_vanilla = []

dm_det_mean = np.mean(dm_det)
dm_rand_mean = np.mean(dm_rand)
#inc_vanilla_mean = np.mean(inc_vanilla)

dm_det_std = np.std(dm_det)
dm_rand_std = np.std(dm_rand)
#inc_vanilla_std = np.std(inc_vanilla)

algo = ['deterministic dm', 'randomized dm']
x_pos = np.arange(len(algo))
CTEs = [dm_det_mean, dm_rand_mean]
error = [dm_det_std, dm_rand_std]

# Build the plot
fig, ax = plt.subplots()
ax.bar(x_pos, CTEs, yerr=error, align='center', alpha=0.5, ecolor='black', capsize=10)
ax.set_ylabel('Runtime in s')
ax.set_xticks(x_pos)
ax.set_xticklabels(algo)
ax.yaxis.grid(True)

# Save the figure and show
plt.tight_layout()
plt.savefig('speed.png')
plt.show()
