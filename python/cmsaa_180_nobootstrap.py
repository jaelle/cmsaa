from cmsaa import attentional_bias, optimize_prioritymap, rmse,plot_results
from cmsaa import GoalMap, SaliencyMap, PriorityMap
import matplotlib.pyplot as plt
import csv
from scipy.integrate import simps,trapz

import numpy as np


data_180 = {}
test_180 = {}
stimuli_locations_180 = [-90,-45,0,45,90]


np_data = np.loadtxt('data/180degree.csv',delimiter=',',skiprows=1)

x = np.array(stimuli_locations_180)

init_vals = [0.761579, 50, 0.761579, 50]
min_bounds = [0.65, 0, 0.65, 0]
max_bounds = [0.761579,1000000,0.761579,1000000]

save_rows = []
# bootstrap_means = np.mean(training_set,axis=0)
alldata_means = np.mean(np_data,axis=0)

data_180['-90'] = alldata_means[0:5]
data_180['0'] = alldata_means[5:10]
data_180['90'] = alldata_means[10:15]

bias = {}
bias['-90'] = np.append(attentional_bias(data_180['-90']),[0.65,0.65,0.65])
bias['0'] = np.append(attentional_bias(data_180['0']),[0.65,0.65,0.65])
bias['90'] = np.append(attentional_bias(data_180['90']),[0.65,0.65,0.65])

print("AUC")
print('-90: ',simps(bias['-90'],dx=45)) 
print('0: ',simps(bias['0'],dx=45))
print('90: ',simps(bias['90'],dx=45)) 

"""
for attended_location in [-90,0,90]:

    # attentional bias derived from the mean reaction times at the attended location
    y = np.array(attentional_bias(data_180[str(attended_location)]))
    best_vals = optimize_prioritymap(attended_location, x, y, init_vals, min_bounds, max_bounds)

    degrees = np.arange(x[0],x[4]+1,1)
    pm = PriorityMap(attended_location)
    pm.standard(degrees,*best_vals)


    error = rmse(x,pm.prioritymap,y,90)
    auc = pm.auc()

    plot_results(x, y, pm, 'results/180/images/' + str(attended_location) + '.png')
    save_cols = [180,attended_location]
    save_cols = np.append(save_cols,best_vals)
    save_cols = np.append(save_cols,[error,auc])

    save_cols = np.array(save_cols,dtype=np.str)

    save_rows += [save_cols]

save_rows = np.array(save_rows).tolist()
save_rows = [['standard location','stimuli location','gm mag','gm stdev','sm mag','sm stdev','error','auc']] + save_rows

with open('results/180/180params.csv','w') as fp:
    writer = csv.writer(fp,lineterminator='\n')
    writer.writerows(save_rows)
"""
print('Done!')
