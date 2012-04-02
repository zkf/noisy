#!/usr/bin/python

import math
import random
from scipy.stats import norm
import matplotlib.pyplot as plt
import numpy as np
from multiprocessing import Pool
import time
import sys
import json

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

        
"""
Simple simulation
"""

# Watch out for allocating inside loops, .append and such
def calc(obs):

    I = 10
    N = 2
    T = 10
    
    # environment setup
    arms = 2
    bandits = list()
    average_cumulative_reward = 0.0

    # bandits setup
    init_mean_for_bandits = 3.5
    init_sd_for_bandits = 3.0

    # From 2.0 to 0.2
    noise = 0.2
    var = list()
    
    for i in range (I):
        del bandits[0:N]

        # creates new bandits for each iteration
        for n in range(N):
            bandits.append(LTS( arms, init_mean_for_bandits, init_sd_for_bandits, obs ))
        cumulative_reward = 0.0
        
        for t in range (T):
            yes = 0.0
            no = 0.0

            # gather choices
            for bandit in bandits:
                selected_arm = bandit.select()
                if (selected_arm == 0): yes += 1
                else: no += 1 
            l = yes / (yes + no)

            # update each bandits last selected arm with a reward
            for bandit in bandits:

                # keep a watchful eye on sigma, its a slippery one. Try 30/70 and 20/80 yes/no
                reward = norm.pdf(l, 0.0, 0.1) + random.gauss(0.0, noise)
                cumulative_reward += reward
                bandit.update(reward)

        var.append(cumulative_reward)
    y = np.mean(var)
    variance= np.std(var)
    average_cumulative_reward = 0
    return obs, y, variance

if __name__ == '__main__':
    start_time = time.time()
    pool = Pool(processes=8)
    observation_noises = list()
    step = 0.1
    obs_start = 0.1
    obs_max = 10
   
    while (obs_start < obs_max):
        observation_noises.append(obs_start)
        obs_start += step
    
    # plt.plot(x, y)
    results = pool.map_async(calc, observation_noises)
    x, y, variance = zip(*results.get(None))
    
    plt.errorbar(x, y, yerr=variance, fmt='ro', linestyle='-')
    # filename = str(I) + "_" + str(T) + "_" + str(N)
    filename = "10_10_10"
    plt.savefig(filename)
    FILE = open(filename, "w")
    FILE.write("#Observation noise, mean, std-dev of cumulative reward over X rounds")
    json.dump(zip(x, y, variance), FILE, indent = 4)
    FILE.close()
    print time.time() - start_time, "seconds"
    plt.show()

  
    
    


        
         
