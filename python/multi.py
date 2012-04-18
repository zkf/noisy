#!/usr/bin/python

import numpypy as np
import math
import random
#from scipy.stats import norm
#import numpy as np
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

def normal(x,u,o):    
    return (1.0/(math.sqrt(2.0*math.pi)*o))*(math.e)**(-((x-u)**2.0)/(2.0*o**2.0))   

     
"""
Simple simulation
"""

def writeToFile(allResults):
    output = zip(*allResults)   
    writeingOnFile = list()
    filename = "invalidFile.data"

    for n in output:
        for t in n:
            (rounds, obs, mean, std) = t
            savedata = (obs, mean, std)
            writeingOnFile.append(savedata)
            filename = str(I) + "_" + str(T) + "_" + str(N) + "Rounds:"+str(rounds)+" v2.data"

        FILE = open(filename, "w")
        FILE.write("#Observation noise, mean, std-dev of cumulative reward over X rounds" + "\n")
        FILE.write('\n'.join('%s %s %s' % z for z in writeingOnFile))
        FILE.close()
        writeingOnFile = []
    


    

def meanAndstd(thelist):
    lenght = len(thelist)
    m1 = (1.0/lenght)
    x = 0
    y = 0
    for i in range(lenght):
        x += thelist[i]
    my = x*m1
        
    m2 = (1.0/(lenght-1))
    for i in range(lenght):
        y += (thelist[i]-my)**2    
    sig = math.sqrt(y*m2)
    return my,sig

# Watch out for allocating inside loops, .append and such
def calc(obs):   

    
    gooregame = list()
    # environment setup
    arms = 2

    # bandits setup
    init_mean_for_bandits = 3.5
    init_sd_for_bandits = 3.0

    # From 2.0 to 0.2
    noise = 0.2
    
    var = list()
    resultlist = list()
    for i in range (I):
        # creates new bandits for each iteration
        bandits = list()
        for n in range(N):
            bandits.append(LTS( arms, init_mean_for_bandits, init_sd_for_bandits, obs ))
        cumulative_reward = 0.0
        gooregame.append((bandits, cumulative_reward))

    #Starts using the bandits, going through all itterasions for each round.
    for t in range (T):
        for i in range(I):
            (bandits, cumulative_reward) = gooregame[i]
            yes = 0.0
            no = 0.0

            # gather choices
            for bandit in bandits:
                selected_arm = bandit.select()
                if (selected_arm == 0): yes += 1
                else: no += 1 
                l = yes / (yes + no)

            # update each bandits last selected arm with a reward
            
                # keep a watchful eye on sigma, its a slippery one. Try 30/70 and 20/80 yes/no
                reward = normal(l, 0.0, 0.1) + random.gauss(0.0, noise)
                cumulative_reward += reward
                bandit.update(reward)
            gooregame[i] = (bandits, cumulative_reward)

        for c in checkpoints:
            if t == c-1:
                for (b, co) in gooregame:
                    var.append(co)
                mean,std = meanAndstd(var)
                roundslist = (c, obs, mean, std)
                resultlist.append(roundslist)
                var = []

        
    #y = np.mean(var)
    #variance= np.std(var)
    return resultlist

# Iterations, bandits, rounds
I = 10
N = 5
T = 100

checkpoints = [T]+[y * 10.0**x for y in [1.0,5.0] for x in range(1, math.trunc(math.log10(T/5.0))+1)]


if __name__ == '__main__':    
    start_time = time.time()
    pool = Pool(processes=8)
    observation_noises = list()
    step = 0.1
    obs_start = 0.1
    obs_max = 5.0


    
    while (obs_start < obs_max):
        observation_noises.append(obs_start)
        obs_start += step
    
    results = pool.map_async(calc, observation_noises)
    allResults = results.get(None)
    
    writeToFile(allResults)

    
    print time.time() - start_time, "seconds"


  
    
    


        
         
