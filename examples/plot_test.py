import numpy as np
import scipy as sp
import matplotlib.pyplot as plt

# Draw samples from the distribution:

mu, sigma = 0.0, 2.0 # mean and standard deviation
s = np.random.normal(mu, sigma, 10000)

# Display the histogram of the samples, along with
# the probability density function:

count, bins, ignored = plt.hist(s, 1000, normed=True)
plt.plot(bins, 1/(sigma * np.sqrt(2 * np.pi)) *
               np.exp( - (bins - mu)**2 / (2 * sigma**2) ),
         linewidth=2, color='r')
plt.show()
