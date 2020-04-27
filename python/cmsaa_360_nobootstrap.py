from cmsaa import attentional_bias, optimize_prioritymap, rmse,plot_results
from cmsaa import GoalMap, SaliencyMap, PriorityMap
import matplotlib.pyplot as plt
import csv

import numpy as np


data_360 = {}

stimuli_locations_360 = [0,72,144,216,288]

x = np.array(stimuli_locations_360)

init_vals = [0.7662, 50, 0.760506149, 50]
min_bounds = [0, 20, 0, 20]
max_bounds = [1,1000000,1,1000000]

save_rows = []

# Attended Location: 0
data_360['0'] = [439.9624292,469.5857333,469.7750167,458.5353417,476.6414542]

# Attended Location: 90
data_360['90'] = [443.7915833,461.009575,462.1513417,476.8956583,451.2019208]

# Attended Location: 180
data_360['180'] = [460.2720833,475.1819167,476.3540917,489.2502083,490.6492125]

# Attended Location 270 (-90)
data_360['270'] = [450.4125083,468.191525,490.643275,472.9244708,467.6859792]

for attended_location in [0,90,180,270]:

    # attentional bias derived from the mean reaction times at the attended location
    y = np.array(attentional_bias(data_360[str(attended_location)]))
    best_vals = optimize_prioritymap(attended_location, x, y, init_vals, min_bounds, max_bounds)

    degrees = np.arange(x[0],x[4]+1,1)
    pm = PriorityMap(attended_location)
    pm.standard(degrees,*best_vals)


    error = rmse(x,pm.prioritymap,y)
    auc = pm.auc()

    plot_results(x, y, pm, 'results/360/images/' + str(attended_location) + '/' + str(attended_location) + '.png')
    save_cols = [360,attended_location]
    save_cols = np.append(save_cols,best_vals)
    save_cols = np.append(save_cols,[error,auc])

    save_cols = np.array(save_cols,dtype=np.str)

    save_rows += [save_cols]

save_rows = np.array(save_rows).tolist()
save_rows = [['standard location','stimuli location','gm mag','gm stdev','sm mag','sm stdev','error','auc']] + save_rows

with open('results/360/360params.csv','w') as fp:
    writer = csv.writer(fp,lineterminator='\n')
    writer.writerows(save_rows)

print('Done!')
