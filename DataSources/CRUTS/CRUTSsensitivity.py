import pandas as pd
import numpy as np
import matplotlib as plt

precip = pd.read_csv('CRUTS_pre_mm_per_day.csv')
precip_act = (precip.iloc[:, :5])
precip_act['mean_total'] = precip_act.iloc[:, 1:5].mean(axis=1) / 25.4
precip_act['mean_without_bl'] = precip_act.iloc[:, 2:5].mean(axis=1) / 25.4

plt.plot(precip_act['Date'], precip_act['mean_without_first'])
