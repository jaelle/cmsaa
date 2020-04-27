from cmsaa import pm_summary,pm_summary_gmonly, pm_summary_constantsm, pm_summary_inhibitedgm
from cmsaa import GoalMap, SaliencyMap, PriorityMap
import numpy as np

x = np.arange(-90,90)
#GLFree: -90
#vals = pm_summary(-90,x,0.7714,36.5089,0.7557,37.6229)
#GLFree: 0
#vals = pm_summary(0,x,0.7757,38.3343,0.7799,39.9178)
#GLFree: 90
#vals = pm_summary(90,x,0.7684,14.1678,0.7552,14.7835)

#GMOnly: -90
#vals = pm_summary_gmonly(-90,x,0.8000,200.0000)
#GMOnly: 0
#vals = pm_summary_gmonly(0,x,0.7869,200.0000)
#GMOnly: 90
#vals = pm_summary_gmonly(90,x,0.8000,200.0000)

#ConstantSM: -90
vals = pm_summary_constantsm(90,x,0.3000,200,0.4940)
#ConstantSM: 0
#vals = pm_summary_constantsm(0,x,0.3000,200,0.4940)
#ConstantSM: 90
#vals = pm_summary_constantsm(90,x,0.3000,200,0.5)

#InhibitedGM: -90
#vals = pm_summary_inhibitedgm(-90,x,0.3157,35.7374,0.3000,38.5088,0.4557)
#InhibitedGM: 0
vals = pm_summary_inhibitedgm(0,x,0.3709,54.5736,0.4969,72.3209,0.4047)
#InhibitedGM: 90
vals = pm_summary_inhibitedgm(90,x,0.3945,14.0453,0.3813,15.1841,0.3739)


print(vals)
