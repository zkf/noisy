import os.path
import glob

#boolen = os.path.isfile("datafolder/repetitions-1000_bandits-2_rounds-50.0_ratio-0.5_noise-0.2.data")


#FILE = open("datafolder/repetitions-1000_bandits-2_rounds-100.0_ratio-0.2_noise-0.2.data", "r")
#line = FILE.readline()
#line2 = FILE.readline()
#line3 = FILE.readline()

#line = line.replace('# Observation noise, mean, std-dev of cumulative reward over ','')
#line = line.replace(' rounds. (1000 repetitions) \n','')
#FILE.close()
FILEOUT = open("3ddata/new_created_data.data", "w")
navnliste = glob.glob('datafolder/repetitions-1000*')
FILEOUT.write('#Runder-Banditer-Ratio-ObsNoise-Noise\n')
verdiliste = list()

for navn in navnliste:
	FILEOPEN = open(navn, "r")
	lines = FILEOPEN.read().splitlines()
	i = 0
	besteLinje = 0.0
	bestObs = 1
	for line in lines:
		if (i < 3):
			i = i+1
			
		else:
			datafelt = line.split(' ')
			if (float(datafelt[1])>besteLinje):
				besteLinje = float(datafelt[1])
				besteObs = float(datafelt[0])
				
	FILEOPEN.close()


	verdier = navn.split('_')
	verdier1 = float(verdier[0].replace('datafolder/repetitions-',''))
	verdier2 = float(verdier[1].replace('bandits-',''))
	verdier3 = float(verdier[2].replace('rounds-',''))
	verdier4 = float(verdier[3].replace('ratio-',''))
	verdier5 = float((verdier[4].replace('noise-','')).replace('.data',''))
	verdiliste.append((verdier3, verdier2, verdier4, verdier5, besteObs))



verdiliste.sort()
forje = verdiliste[0]
utskirftsliste = list()
i = 0
for n in verdiliste:
	utskirftsliste.append(n)
	if (n[0] != forje[0]):
		forje = (forje[0], forje[1], forje[2], forje[3], str(forje[4])+'\n')
		utskirftsliste[i-1] = forje
		print forje
	forje = n
	i = i+1
FILEOUT.write('\n'.join('%s %s %s %s %s' % z for z in utskirftsliste))
FILEOUT.close()

