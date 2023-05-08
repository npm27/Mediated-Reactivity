####Set up####
##read in data
dat1 = read.csv("Recog/USM_recog.csv")
dat2 = read.csv("Recog/MSU_recog.csv")

dat = rbind(dat1, dat2)

##fix np coding
np = subset(dat,
            dat$control == "Control")

np$Scored = (np$Scored - 1) * -1

pr = subset(dat,
            dat$control != "Control")

dat = rbind(np, pr)

##load libraries
library(ez)
library(reshape)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

####Explore the data####
##get n
length(unique(dat$Username)) #133 participants

##general data patterns
tapply(dat$Scored, dat$Direction, mean) ##okay, mediated and F have higher mean recall vs u.

tapply(dat$Scored, list(dat$encoding, dat$Direction), mean)

####Clean the data####
#remove anyone w/ high recognition of nps
jol = subset(dat,
             dat$encoding == "JOL")
jol.long = cast(jol, Username ~ Direction, mean)

read = subset(dat,
              dat$encoding == "Read")
read = na.omit(read)

read.long = cast(read, Username ~ Direction, mean)

##Remove individuals not paying attention
dat = subset(dat,
                dat$Username != "KatelynNguyen" & dat$Username != "KaylaGilliam" & dat$Username != "KallieSiebrasse" &
                dat$Username != "M20315432_AV")

#JOL
#not paying attention
dat = subset(dat,
             dat$Username != "MadisonUlmer" & dat$Username != "IzyclerPimentel" & dat$Username != "makylawatson"
             & dat$Username != "AmyiaKimes")

####Get descriptives####
##general data patterns
tapply(dat$Scored, dat$Direction, mean) ##okay, mediated and F have higher mean recall vs u.

tapply(dat$Scored, list(dat$encoding, dat$Direction), mean)

####ANOVAS####
#let's just look at studied items
presented = subset(dat,
                   dat$control == "Presented")

out1 = ezANOVA(presented,
        wid = Username,
        dv = Scored,
        between = encoding,
        within = Direction,
        type = 3,
        detailed = T) #Sig effect of encoding, sig effect of direction, no interaction

out1$ANOVA$MSE = out1$ANOVA$SSd/out1$ANOVA$DFd
out1$ANOVA$MSE

aovEffectSize(out1, effectSize = "pes")

##Break down effects/interactions
tapply(presented$Scored, presented$Direction, mean) #main effect of direction
tapply(presented$Scored, presented$encoding, mean) #main effect of encoding
tapply(presented$Scored, list(presented$encoding, presented$Direction), mean)

###post-hocs
##Direction

##Interaction
jol2 = subset(presented, presented$encoding == "JOL")
jol3 = cast(jol2, Username ~ Direction, mean)

read2 = subset(presented, presented$encoding == "Read")
read3 = cast(read2, Username ~ Direction, mean)

#F
temp = t.test(jol3$F, read3$F, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#M
temp = t.test(jol3$M, read3$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#U
temp = t.test(jol3$U, read3$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get ns
length(unique(jol3$Username)) #64
length(unique(read3$Username)) #61

##get sds for d
apply(jol3, 2, sd)
apply(read3, 2, sd)

##compare false alarms
FA = subset(dat,
            dat$Direction == "not presented")

FA2 = cast(FA, Username ~ encoding, mean)

apply(FA2, 2, mean, na.rm = T)

#t-test
temp = t.test(FA2$JOL, FA2$Read, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#get sds for d
apply(FA2, 2, sd, na.rm = T)
