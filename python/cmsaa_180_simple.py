from cmsaa import GoalMap, SaliencyMap, PriorityMap
from scipy.optimize import curve_fit

init_vals = [0.7662, 50]
min_bounds = 0
max_bounds = [0.79,1000000]
x = [-90, -45,   0,  45,  90]
y = [ 0.72777419,0.6836129,0.70412903,0.68805645,  0.72197581]

pm = PriorityMap(0)

(best_vals,covar) =  curve_fit(pm.gmonly, x, y, p0=init_vals, bounds=(min_bounds,max_bounds))
print(best_vals)

