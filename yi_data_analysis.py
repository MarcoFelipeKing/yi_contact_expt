# This scripts reads in Yi.data.longformat
# plots the data

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import math

#import the data
df = pd.read_csv('data/Yi.data.longformat.20191121_censoring_handled.csv', sep=',')
print(df.head())

#Scatter plot of RawIntD against ContactNumber and hue by gloves, facet for each ID
#Use lograrithmic scale for y axis
#Use lograrithmic scale for x axis
#Use facetgrid to plot each ID on a separate plot

plt.scatter(df['RawIntD'], df['ContactNumber'], c=df['Gloves'], cmap='viridis', s=10, alpha=0.5)
plt.show()


