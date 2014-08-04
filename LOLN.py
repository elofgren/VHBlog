from __future__ import division
import numpy as np
from matplotlib import pyplot as plt

# Define necessary functions
def DiceRoller(n_rolls):
	try:
		dice = int(n_rolls)
	except ValueError:
		print("Number of dice to roll needs to be an integer")
	rolls = np.random.random_integers(low=1, high=6, size=(1.,dice))
	return rolls

def Shooting(BS, n_shots):
	try:
		BS = int(BS)
	except ValueError:
		print("BS needs to be an integer")
	try:
		n_shots = int(n_shots)
	except ValueError:
		print("Number of shots needs to be an integer")
	shots = DiceRoller(n_shots)
	hits = np.sum(shots.__le__(BS))
	return hits

# Admin variables, setting up results containers
rolls = 5
BS = 4
marines = 2
sm = np.zeros((marines,rolls))
effbs = np.zeros((marines,rolls))

for i in range(rolls):
    sm[0,i] = Shooting(BS,1)
    sm[0,i] = Shooting(BS,1)
    
hits = 0
counts = 1

for j in range(marines):
	for k in range(rolls):
		counts = k + 1
    	if sm[j,k] == 1:
        	hits += 1
    	else:
        	pass
    	effbs[j,k] = (hits/counts)/(1/6)
    
sim = plt.plot(effbs[1])
#theory = plt.axhline(y=BS, xmin=0, xmax=rolls, linewidth=2, color = 'm',ls=":")
#plt.axis([0, rolls, 0, 6])
#plt.ylabel("Effective BS")
#plt.xlabel("Die Rolls")
#plt.legend(sim,theory)
#labels = ('Simulated Ballistic Skill','Theoretical Ballistic Skill')
#plt.legend(labels)
plt.show()

