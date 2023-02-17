##read in data
JOL = rbind(read.csv("USM/JOL Scored.csv"), read.csv("Prolific/JOL Scored_P.csv"))
Read = rbind(read.csv("USM/Read Scored.csv"), read.csv("Prolific/Read Scored_P.csv"))

#load libraries
library(reshape)
library(ez)
library(psychReport)

##get ns
length(unique(JOL$id)) #25
length(unique(Read$id)) #26

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

#get score on correct scale
combined$score = combined$score * 100

tapply(combined$score, list(combined$encoding, combined$direction), mean, na.omit = T)

##Check for outliers and weirdness here
summary(combined)

####ANOVA####
model1 = ezANOVA(combined,
                 dv = score,
                 between = encoding,
                 within = direction,
                 wid = id,
                 type = 3,
                 detailed = T)

model1 #basically everything is sig!

####Post-hocs####
tapply(combined$score, combined$encoding, mean) #main effect of encoding
tapply(combined$score, combined$direction, mean) #main effect of direction
tapply(combined$score, list(combined$encoding, combined$direction), mean) #interaction

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

#Not surprisingly, everything is sig!

###Interaction
jol3 = subset(combined, combined$encoding == "JOL")
read3 = subset(combined, combined$encoding == "Read")

jol.ph = cast(jol3, id ~ direction, mean)
read.ph = cast(read3, id ~ direction, mean)

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
temp$statistic #non-sig! (most likely a power thing...) #.14
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#unrelated
temp = t.test(jol.ph$U, read.ph$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #Non-Sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#okay, so trending in the right direction!