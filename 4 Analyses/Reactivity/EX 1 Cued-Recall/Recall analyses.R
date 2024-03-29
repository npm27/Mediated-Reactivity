##read in data
JOL = rbind(read.csv("USM/JOL Scored.csv"), read.csv("Prolific/JOL Scored_P.csv"))
Read = rbind(read.csv("USM/Read Scored.csv"), read.csv("Prolific/Read Scored_P.csv"))

#load libraries
library(reshape)
library(ez)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

##drop unused columns
JOL2 = JOL[ , -c(2:4, 8:9, 11:14)]
Read2 = Read[ , -c(2:4, 8:9, 11:13)]

#slap together and rearrange columns
combined = rbind(JOL2, Read2)

combined = combined[ , c(1, 3, 4, 5, 6, 2)]

#rename columns
colnames(combined)[c(3:4, 6)] = c("encoding", "direction", "score")

combined$encoding[combined$encoding == "JOL Recall"] = "JOL"
combined$encoding[combined$encoding == "Read Recall"] = "Read"

#get score on correct scale
combined$score = combined$score * 100

tapply(combined$score, list(combined$encoding, combined$direction), mean, na.omit = T)

##Check for outliers and weirdness here
summary(combined)

JOL3 = subset(combined,
              combined$encoding == "JOL")

Read3 = subset(combined,
               combined$encoding == "Read")

JOL.wide = cast(JOL3, id ~ direction, mean)
Read.wide = cast(Read3, id ~ direction, mean)

combined = subset(combined,
                  combined$id != "W10057641NS" & combined$id != "10068469SJ" & combined$id != "W_10132867_LAA" &
                  combined$id != "62f17b6b63bd9e6627a19956" & combined$id != "5a8b1ee2000dab00018cc7cd" &
                  combined$id != "62d43cee3d60ac98c1dcacc8" & combined$id != "63474e67a5fd298c6103c409" &
                  combined$id != "w963035" & combined$id != "5c2bb03b9fb36d000198e2d7" & combined$id != "w10162630_CAG" &
                  combined$id != "TaravionCosey" & combined$id != "w10055946")

####ANOVA####
model1 = ezANOVA(combined,
                 dv = score,
                 between = encoding,
                 within = direction,
                 wid = id,
                 type = 3,
                 detailed = T)

model1 #basically everything is sig!

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

####Post-hocs####
tapply(combined$score, combined$encoding, mean) #main effect of encoding
tapply(combined$score, combined$direction, mean) #main effect of direction

tapply(combined$score, list(combined$encoding, combined$direction), mean) #interaction
#(tapply(combined$score, list(combined$encoding, combined$direction), sd) / sqrt(length(unique(combined$id)))) * 1.96

###break down direction main effect
combined.direction = cast(combined, id ~ direction, mean)

#F vs M
temp = t.test(combined.direction$F, combined.direction$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#F vs U
temp = t.test(combined.direction$F, combined.direction$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#U vs M
temp = t.test(combined.direction$M, combined.direction$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get d
mean(combined.direction$M); sd(combined.direction$M)
mean(combined.direction$U); sd(combined.direction$U)

#Not surprisingly, everything is sig!

###Interaction
jol3 = subset(combined, combined$encoding == "JOL")
read3 = subset(combined, combined$encoding == "Read")

jol.ph = cast(jol3, id ~ direction, mean)
read.ph = cast(read3, id ~ direction, mean)

(apply(jol.ph[ , -1], 2, sd) / sqrt(nrow(jol.ph))) * 1.96
(apply(read.ph[ , -1], 2, sd) / sqrt(nrow(read.ph))) * 1.96

nrow(jol.ph) #n = 60
nrow(read.ph) #n = 60

##forward
temp = t.test(jol.ph$F, read.ph$F, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##mediated
temp = t.test(jol.ph$M, read.ph$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #Significant! p = .0057
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#unrelated
temp = t.test(jol.ph$U, read.ph$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #Non-Sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#get pbic
pbic1 = jol.ph[ , c(1, 4)]
pbic2 = read.ph[ , c(1, 4)]

pbic1$encoding = rep("JOL")
pbic2$encoding = rep("read")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = U,
        wid = id,
        between = encoding,
        detailed = T,
        type = 3)

#get sds
apply(jol.ph, 2, sd)
apply(read.ph, 2, sd)

#get 95% CI
(apply(jol.ph, 2, sd) / sqrt(nrow(jol.ph))) * 1.96
(apply(read.ph, 2, sd) / sqrt(nrow(read.ph))) * 1.96

##get ns after cleaning
##get N
nrow(jol.ph) #60
nrow(read.ph) #60

##How many from each platform?
jol4 = subset(jol3,
              jol3$Platform == "Prolific")
length(unique(jol4$id)) #21

jol5 = subset(jol3,
              jol3$Platform == "USM")
length(unique(jol5$id)) #39

read4 = subset(read3,
              read3$Platform == "Prolific")
length(unique(read4$id)) #22

read5 = subset(read3,
              read3$Platform == "USM")
length(unique(read5$id)) #38

####platform differences?####
combined2 = rbind(jol4, read4)
combined3 = rbind(jol5, read5)

tapply(combined2$score, list(combined2$encoding, combined2$direction), mean)
tapply(combined3$score, list(combined3$encoding, combined3$direction), mean) #Not really, its about 8% increase.

####DO JOLs differ?####
##remove outliers
jol = JOL

jol = subset(jol,
              jol$id != "W10057641NS" & jol$id != "10068469SJ" & jol$id != "W_10132867_LAA" &
              jol$id != "62f17b6b63bd9e6627a19956" & jol$id != "5a8b1ee2000dab00018cc7cd" &
              jol$id != "62d43cee3d60ac98c1dcacc8" & jol$id != "63474e67a5fd298c6103c409" &
              jol$id != "w963035" & jol$id != "5c2bb03b9fb36d000198e2d7" & jol$id != "w10162630_CAG" &
              jol$id != "TaravionCosey" & jol$id != "w10055946")

##descriptives
jol = jol[ , -c(2:4, 6:9, 11, 13:14)]

colnames(jol)[3:4] = c("Direction", "JOL")

jol$JOL = as.numeric(jol$JOL)
jol$JOL[jol$JOL > 100] = NA

tapply(jol$JOL, jol$Direction, mean, na.rm = T)
