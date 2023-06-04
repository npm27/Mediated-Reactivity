####Experiment 2-4####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Recog Experiments.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(8,14)

ax1 = fig.add_subplot(3, 1, 1)
ax2 = fig.add_subplot(3, 1, 2)
ax3 = fig.add_subplot(3, 1, 3)

#fig.tight_layout()
fig.subplots_adjust(top = .925)
                    
plt.subplots_adjust(hspace = 0.35)

fig.suptitle('Experiments 2-4: Recogniton Testing', fontsize = 24, fontweight = 'bold')

####Subset by Experiment####
ex2 = dat[dat['Experiment'] == 2]
ex3 = dat[dat['Experiment'] == 3]
ex4 = dat[dat['Experiment'] == 4]

####Experiment 2####
#subset by task
j1 = ex2[ex2['Task'] == 'JOL']
s1 = ex2[ex2['Task'] == 'Study']

#get all the things to plug into the plots
#separate out averages and conf interval
j1_average = j1['Average']
s1_average = s1['Average']

j1_conf = j1['diff2']
s1_conf = s1['diff2']

ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.35 #bar width 

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, s1_average, width, yerr = s1_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'No-JOL')

#Make the plot spiffy
ax1.set_title('Experiment 2', fontsize = 20, fontweight = 'bold')
ax1.set_ylabel('Prop. "Old" Responses', fontsize = 15, fontweight = 'bold')
ax1.set_xlabel('Pair Type', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Forward', 'Mediated', 'Unrelated', 'New'), fontsize = 16)
ax1.legend(fontsize = 16)
ax1.set_ylim([0,1])

####Experiment 3#####
#subset by task
j2 = ex3[ex3['Task'] == 'JOL']
s2 = ex3[ex3['Task'] == 'Study']

#get all the things to plug into the plots
#separate out averages and conf interval
j2_average = j2['Average']
s2_average = s2['Average']

j2_conf = j2['diff2']
s2_conf = s2['diff2']

ind = np.arange(len(j2_average))  # the x locations for the groups
width = 0.35 #bar width 

rects3 = ax2.bar(ind - width/2, j2_average, width, yerr = j2_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects4 = ax2.bar(ind + width/2, s2_average, width, yerr = s2_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'No-JOL')

#Make the plot spiffy
ax2.set_title('Experiment 3', fontsize = 20, fontweight = 'bold')
ax2.set_ylabel('Prop. "Old" Responses', fontsize = 15, fontweight = 'bold')
ax2.set_xlabel('Pair Type', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax2.set_xticks(ind)
ax2.set_xticklabels(('Forward', 'Mediated', 'Unrelated', 'New'), fontsize = 16)
ax2.legend(fontsize = 16)
ax2.set_ylim([0,1])

####Experiment 4####
#subset by task
j3 = ex4[ex4['Task'] == 'JOL']
s3 = ex4[ex4['Task'] == 'Study']

#get all the things to plug into the plots
#separate out averages and conf interval
j3_average = j3['Average']
s3_average = s3['Average']

j3_conf = j3['diff2']
s3_conf = s3['diff2']

ind = np.arange(len(j3_average))  # the x locations for the groups
width = 0.35 #bar width 

rects5 = ax3.bar(ind - width/2, j3_average, width, yerr = j3_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects6 = ax3.bar(ind + width/2, s3_average, width, yerr = s3_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'No-JOL')

#Make the plot spiffy
ax3.set_title('Experiment 4', fontsize = 20, fontweight = 'bold')
ax3.set_ylabel('Prop. "Old" Responses', fontsize = 15, fontweight = 'bold')
ax3.set_xlabel('Pair Type', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax3.set_xticks(ind)
ax3.set_xticklabels(('Forward', 'Unrelated', 'New'), fontsize = 16)
ax3.legend(fontsize = 16)
ax3.set_ylim([0,1])

fig.savefig('EX2_4_chart.png', dip = 10000)