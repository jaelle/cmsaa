import numpy as np
from scipy.optimize import curve_fit
from math import exp

import matplotlib.pyplot as plt


def attentional_bias(expRTs):
    
    return (2000 - np.array(expRTs))/2000


def optimize_prioritymap(attended_location, x, y, init_vals, min_bounds, max_bounds,pmtype=''):

    pm = PriorityMap(attended_location)
    #print(len(init_vals))
    #print(pmtype)

    if len(init_vals) == 4:
        (best_vals,covar) = curve_fit(pm.standard, x, y, p0=init_vals, bounds=(min_bounds,max_bounds))
    elif len(init_vals) == 2:
        (best_vals,covar) = curve_fit(pm.gmonly, x, y, p0=init_vals, bounds=(min_bounds,max_bounds))
    elif len(init_vals) == 3:
        (best_vals,covar) = curve_fit(pm.constantsm, x, y, p0=init_vals, bounds=(min_bounds,max_bounds))
    elif len(init_vals) == 5:
        (best_vals,covar) = curve_fit(pm.inhibitedgm, x, y, p0=init_vals, bounds=(min_bounds,max_bounds))
        
    return best_vals

def rmse(pm,experimental):
    error = 0

    xs = [0,44,89,134,179]
    
    i = 0
    for x in xs:
        error += (experimental[i] - pm[x]) ** 2
        i += 1
        
    return error

def pm_summary(attended_location, x, gm_mag, gm_stdev, sm_mag, sm_stdev):
    pm = PriorityMap(attended_location)
    pmmap = pm.standard(x, gm_mag, gm_stdev, sm_mag, sm_stdev)

    return [pmmap[0], pmmap[44], pmmap[89], pmmap[134], pmmap[179]]

def pm_summary_gmonly(attended_location, x, gm_mag, gm_stdev):
    pm = PriorityMap(attended_location)
    pmmap = pm.gmonly(x, gm_mag, gm_stdev)

    return [pmmap[0], pmmap[44], pmmap[89], pmmap[134], pmmap[179]]

def pm_summary_constantsm(attended_location, x, gm_mag, gm_stdev, sm_mag):
    pm = PriorityMap(attended_location)
    pmmap = pm.constantsm(x, gm_mag, gm_stdev, sm_mag)

    return [pmmap[0], pmmap[44], pmmap[89], pmmap[134], pmmap[179]]

def pm_summary_inhibitedgm(attended_location, x, gm_mag, gm_stdev, gm_mag2, gm_stdev2, sm_mag):
    pm = PriorityMap(attended_location)
    pmmap = pm.inhibitedgm(x, gm_mag, gm_mag2, gm_stdev, gm_stdev2, sm_mag)

    return [pmmap[0], pmmap[44], pmmap[89], pmmap[134], pmmap[179]]

def plot_results_w_test(x,y,test_y,pm, filename):
    # range is from the first x value to the last one
    degrees = np.arange(x[0],x[len(x)-1],1)


    plt.plot(x,y,'yo')
    plt.plot(x,test_y,'kx')
    plt.plot(degrees,pm.goalmap,'b')
    if (len(pm.saliencymap) > 0):
        plt.plot(degrees,pm.saliencymap,'r')
    plt.plot(degrees,pm.prioritymap,'g')

    if filename != '':
        plt.savefig(filename,format='png')
    else:
        plt.show()

    plt.close()

def plot_results(x,y,pm, filename):
    # range is from the first x value to the last one
    degrees = np.arange(x[0],x[len(x)-1],1)


    plt.plot(x,y,'yo')
    plt.plot(degrees,pm.goalmap,'b')
    if (len(pm.saliencymap) > 0):
        plt.plot(degrees,pm.saliencymap,'r')
    plt.plot(degrees,pm.prioritymap,'g')

    if filename != '':
        plt.savefig(filename,format='png')
    else:
        plt.show()

    plt.close()

class GoalMap:
    def __init__(self, attended_location):
        self.attended_location = attended_location
    
    def standard(self, x, mag, stdev):
        # gaussian equation
        return mag * np.exp(-abs(self.attended_location-x)**2 / (2*stdev**2))
    
    # shifts the goal map up or down by the minshift value
    def standard_minshift(self, x, mag, stdev, minshift):
        return minshift + standard(x,stdev, mag)

    def inhibition(self, x, mag, mag2, stdev, stdev2):

        return mag * np.exp(-abs(self.attended_location-x)**2 / (2*stdev**2)) + (mag2 - mag2 * np.exp(-abs(self.attended_location-x)**2 / (2*stdev2**2)))

class SaliencyMap:
    def __init__(self, attended_location):
        self.attended_location = attended_location
    
    def standard(self, x, mag, stdev):
        # inverted gaussian equation
        return mag - mag * np.exp(-abs(self.attended_location-x)**2 / (2*stdev**2))
    
    # returns a constant attentional bias at every location in the map range
    def constant(self, x, value):
        return [value]*len(x)
    
    # returns a map where each degree location in the map range represents the probability of a sound at that location.
    # Probabilities are learned from the individual trials
    # Inputs: x - a list representing the range of locations that sounds can be presented from
    #         attended_location - represents the attended location for the current condition (probably -90,0,90 in the 180 degree case)
    #         trials - a 2d list, where each row contains [trial id, sound location and frequency] 
    # Outputs: saliency map - a list of size x. Each item in the list contains the probability that a sound will come from that location.
    #                         Probability is calculated using the equations found in Lejarraga 2010. https://onlinelibrary.wiley.com/doi/abs/10.1002/bdm.722 
    def ibl(self, x, attended_location, trials):
        
        saliency_map = []
        
        # TODO: replace this with IBL algorithm
        # - Calculate the activation for each trial in trials (Equation 3 in paper)
        # - For each location in x:
        # - - calculate the activation value as if that location were the next trial in trials (trial id + 1) (Equation 3 in paper)
        # - - calculate the probability of a sound coming from that location (Equation 2 in paper)
        # - - append probability to saliency_map.
        
        # given a 2d array of data (each row represents one trial)
        return saliency_map
        

class PriorityMap:
    def __init__(self, attended_location):
        self.attended_location = attended_location
        
    def standard(self, x, gm_mag, gm_stdev, sm_mag, sm_stdev):
        
        gm = GoalMap(self.attended_location)
        sm = SaliencyMap(self.attended_location)
        
        self.goalmap = gm.standard(x, gm_mag, gm_stdev)
        self.saliencymap = sm.standard(x, sm_mag, sm_stdev)
        self.prioritymap = self.goalmap + self.saliencymap
        
        return self.prioritymap

    def gmonly(self, x, gm_mag, gm_stdev):
        gm = GoalMap(self.attended_location)

        self.goalmap = gm.standard(x,gm_mag,gm_stdev)
        self.saliencymap = []
        self.prioritymap = self.goalmap

        return self.prioritymap

    def constantsm(self, x, gm_mag, gm_stdev, sm_mag):
        gm = GoalMap(self.attended_location)
        sm = SaliencyMap(self.attended_location)

        self.goalmap = gm.standard(x,gm_mag,gm_stdev)
        self.saliencymap = sm.constant(x, sm_mag)
        self.prioritymap = self.goalmap + self.saliencymap

        return self.prioritymap

    def inhibitedgm(self,x, gm_mag, gm_mag2, gm_stdev, gm_stdev2, sm_mag):
        gm = GoalMap(self.attended_location)
        sm = SaliencyMap(self.attended_location)
        
        self.goalmap = gm.inhibition(x, gm_mag, gm_mag2, gm_stdev, gm_stdev2)
        self.saliencymap = sm.constant(x, sm_mag)
        self.prioritymap = self.goalmap + self.saliencymap
        
        return self.prioritymap
        
        
