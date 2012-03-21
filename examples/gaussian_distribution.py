#!/usr/bin/python

import math

class Ref:
    def __init__(self, N, mu, sigma):
        self._k = N / (sigma * math.sqrt(2 * math.pi))
        self._s = -1.0 / (2 * sigma * sigma)
        self._mu = mu
        self._x = 0
    
    def chance(self):
        return self._k * math.exp(self._s * (self._x -  self._mu) * (self._x - self._mu))

    def choice(self, x):
        self._x += x
"""
Create N bandits with 2 choices(). Create referee (check).
Create normal distribution at startup (check). Take inn all choices(sort of).
Calculate chances of a reward (check).
"""

"""
Environment
"""
mu = 0.0
N = 4.0
sigma = 2.0
referee = Ref(N, mu, sigma)
referee.choice(1.0)
referee.choice(1.0)
referee.choice(-1.0)

print "The chance for a reward is " + str(referee.chance())

