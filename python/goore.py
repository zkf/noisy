#!/usr/bin/python

import numpypy
import math
import random
#from scipy.stats import norm
#import matplotlib.pyplot as plt
import numpy as np
from multiprocessing import Pool

class LTS:
    def __init__(self,N, init_mu, init_sd, observation_noise):
        """
        init_variance and observation_variance are in there standard deviation form.
        so the variance is the square of them.
        """
        self._arms = [(init_mu, init_sd) for _ in range(0, N)]
        self._observation_noise = observation_noise
        self._last_arm_pulled = None
        
        
    def select(self):
        """
        returns a arm index in [0, N-1] 
        """
        self._last_arm_pulled = max( enumerate(self._arms), key=lambda x:random.gauss(*x[1]))[0] 
        return self._last_arm_pulled
    
    def update(self, reward):
        """
        Takes the reward and updates the last arm pulled based on the conjugate prior. 
        """
        arm          = self._arms[self._last_arm_pulled]

        arm_mean     = arm[0]        
        arm_variance = arm[1] * arm[1]
        ob_variance  = self._observation_noise * self._observation_noise
        
        
        mu = (arm_variance)*reward + (ob_variance)*arm_mean
        mu /= (arm_variance + ob_variance)
        
        
        var = arm_variance * ob_variance
        var /= (arm_variance + ob_variance)
        sd  = math.sqrt(var)
        
        self._arms[self._last_arm_pulled] = (mu, sd)
        
    def print_arm(self):
        for item in self._arms:
            print "mean: " + str(item[0])
            print "variance: " + str(item[1])

        
"""
Simple simulation
"""

# environment setup
arms = 2
bandits = list()

T = 100
average_cumulative_reward = 0.0

# bandits setup
init_mean_for_bandits = 3.5
init_sd_for_bandits = 3.0
observation_noise = 0.1
step = 0.1
# amount of bandits
N = 2 
# amounts of repetitions
I = 10

noise = 2.0
y = list()
x = list()
variance = list()
var = list()

def normal(x,u,o):	
	return (1.0/(math.sqrt(2.0*math.pi)*o))*(math.e)**(-((x-u)**2.0)/(2.0*o**2.0))

while (observation_noise < 3.5):
    for i in range (I):
        del bandits[0:N]

        # creates new bandits for each iteration
        for n in range(N):
            bandits.append(LTS( arms, init_mean_for_bandits, init_sd_for_bandits, observation_noise ))
        cumulative_reward = 0.0
        
        for t in range (T):
            yes = 0.0
            no = 0.0

            # gather choices
            for item in bandits:
                selected_arm = item.select()
                if (selected_arm == 0): yes += 1
                else: no += 1 
            l = yes / (yes + no)

            # update each bandits last selected arm with a reward
            for item in bandits:

                # keep a watchful eye on sigma, its a slippery one
                #reward = norm.pdf(l, 0.0, 0.1) + random.gauss(0.0, noise)
		reward = normal(l, 0.0, 0.1) + random.gauss(0.0, noise)
                cumulative_reward += reward
                item.update(reward)

        var.append(cumulative_reward)
    print str(observation_noise)    
    y.append(np.mean(var))
    x.append(observation_noise)
    variance.append(np.std(var))
    del var[0:I]
    average_cumulative_reward = 0
    observation_noise += step

# plt.plot(x, y)
#plt.errorbar(x, y, yerr=variance, fmt='ro', linestyle='-')
filename = str(I) + "_" + str(T) + "_" + str(N)
#plt.savefig(filename)
#plt.show()
open(filename).write('\n'.join('%s %s %s' % z for z in variance))

  
    
    


        
         
