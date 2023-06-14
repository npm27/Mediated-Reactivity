####Set up####
##load libraries
library(psycho)
library(ez)
library(reshape)
library(psychReport)

##read in data
ex2 = read.csv("Data/sd ex 2.csv")
ex3 = read.csv("Data/sd ex 3.csv")
ex4 = read.csv("Data/sd ex 4.csv")

####Get the data in the right shape####
##For each participant, it looks like I need to sum total for each of the four categories
##Hits, False alarms, misses, and correct rejections

##first, code each trial as being one of the categories
#not presented, 0 == correct rejection
#not presented, 1 == False alarm
#presented, 0 == miss
#presented, 1 == hit

##Start w/ hits
hits2 = subset(ex2,
               ex2$control == "Presented" & ex2$Scored == 1)
hits2$sig_type = rep("hit")

miss2 = subset(ex2,
             ex2$control == "Presented" & ex2$Scored == 0)
miss2$sig_type = rep("miss")

fa2 = subset(ex2,
             ex2$control == "Control" & ex2$Scored == 1)
fa2$sig_type = rep("fa")

cr2 = subset(ex2,
             ex2$control == "Control" & ex2$Scored == 0)
cr2$sig_type = rep("cr")

##put it all back together
combined2 = rbind(hits2, miss2, fa2, cr2)

####Now sum the categories for each participant####
##Loop time?
sig_detect = data.frame()

for(i in unique(combined2$Username)){
  
  #loop through participants
  temp = subset(combined2, 
                combined2$Username == i)
  
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr")
  
  c = length(crs$sig_type)
  
  #get participant's encoding group
  e = temp$encoding[1]
  
  #now slap it back together
  temp2 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp2)
  
}

####compute indices####
indices = data.frame(dprime(sig_detect$h, sig_detect$f, sig_detect$m, sig_detect$c))
ex2_final = cbind(sig_detect, indices)

####Now for Experiment 3####
##Start w/ hits
hits3 = subset(ex3,
               ex3$control == "Presented" & ex3$Scored == 1)
hits3$sig_type = rep("hit")

miss3 = subset(ex3,
               ex3$control == "Presented" & ex3$Scored == 0)
miss3$sig_type = rep("miss")

fa3 = subset(ex3,
             ex3$control == "Control" & ex3$Scored == 1)
fa3$sig_type = rep("fa")

cr3 = subset(ex3,
             ex3$control == "Control" & ex3$Scored == 0)
cr3$sig_type = rep("cr")

##put it all back together
combined3 = rbind(hits3, miss3, fa3, cr3)

####Now sum the categories for each participant####
##Loop time?
sig_detect = data.frame()

for(i in unique(combined3$Username)){
  
  #loop through participants
  temp = subset(combined3, 
                combined3$Username == i)
  
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr")
  
  c = length(crs$sig_type)
  
  #get participant's encoding group
  e = temp$encoding[1]
  
  #now slap it back together
  temp3 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp3)
  
}

####compute indices####
indices = dprime(sig_detect$h, sig_detect$f, sig_detect$m, sig_detect$c)
ex3_final = cbind(sig_detect, indices)

####And Experiment 4####
##Start w/ hits
hits4 = subset(ex4,
               ex4$control == "Presented" & ex4$Scored == 1)
hits4$sig_type = rep("hit")

miss4 = subset(ex4,
               ex4$control == "Presented" & ex4$Scored == 0)
miss4$sig_type = rep("miss")

fa4 = subset(ex4,
             ex4$control == "Control" & ex4$Scored == 1)
fa4$sig_type = rep("fa")

cr4 = subset(ex4,
             ex4$control == "Control" & ex4$Scored == 0)
cr4$sig_type = rep("cr")

##put it all back together
combined4 = rbind(hits4, miss4, fa4, cr4)

####Now sum the categories for each participant####
##Loop time?
sig_detect = data.frame()

for(i in unique(combined4$Username)){
  
  #loop through participants
  temp = subset(combined4, 
                combined4$Username == i)
  
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr")
  
  c = length(crs$sig_type)
  
  #get participant's encoding group
  e = temp$encoding[1]
  
  #now slap it back together
  temp4 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp4)
  
}

####compute indices####
indices = dprime(sig_detect$h, sig_detect$f, sig_detect$m, sig_detect$c)
ex4_final = cbind(sig_detect, indices)

####Write everything to .csv for analyses####
#write.csv(ex2_final, file = "Sig Data/ex2_sig.csv", row.names = F)
#write.csv(ex3_final, file = "Sig Data/ex3_sig.csv", row.names = F)
#write.csv(ex4_final, file = "Sig Data/ex4_sig.csv", row.names = F)
