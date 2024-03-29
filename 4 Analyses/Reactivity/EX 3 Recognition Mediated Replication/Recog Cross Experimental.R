####set up####
##libraries
library(reshape)
library(ez)
library(psychReport)

##read in data
dat1 = read.csv("Cross Experiment/orig.csv")
dat2 = read.csv("Cross Experiment/rep.csv")

dat1$Experiment = rep("original")
dat2$Experiment = rep("Replication")

combined = rbind(dat1, dat2)

#turn off scientific notation
options(scipen = 999)

####Run the ANOVA!####
out1 = ezANOVA(combined,
               wid = Username,
               dv = Scored,
               between = .(encoding, Experiment),
               within = Direction,
               type = 3,
               detailed = T) #Sig effect of encoding, sig effect of direction, no interaction

out1

out1$ANOVA$MSE = out1$ANOVA$SSd/out1$ANOVA$DFd
out1$ANOVA$MSE

aovEffectSize(out1, effectSize = "pes")
