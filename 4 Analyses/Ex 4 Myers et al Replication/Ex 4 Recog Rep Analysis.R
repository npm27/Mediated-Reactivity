####Set Up####
dat = read.csv("Recog/Ex 4_recog.csv")

##fix np coding
np = subset(dat,
            dat$control == "Control")

np$Scored = (np$Scored - 1) * -1

pr = subset(dat,
            dat$control != "Control")

dat = rbind(np, pr)

dat = dat[ , c(1, 2, 3, 4, 5, 7, 6)]

##load libraries
library(ez)
library(reshape)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

####Explore the data####
##get n
length(unique(dat$Username)) #125 participants

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

##remove any one with high recog > 90% in unrelated AND one other category (or Extremely low recog < 10%)
dat = subset(dat,
             dat$Username != "5872d1aaa205a90001e7f3a4" & dat$Username != "5c6342cbfaca5c000186e2f5") #JOL

dat = subset(dat,
             dat$Username != "5c0873675b32d500012e3f2c" & dat$Username != "5d77eb3007e8770001acdef8" & 
             dat$Username != "5e904f36cc3d6c355bbf48fa") #Read

####ANOVA####
##Let's just look at correct hits
presented = subset(dat,
                   dat$control == "Presented")

#run the anova!
out1 = ezANOVA(presented,
               wid = Username,
               dv = Scored,
               between = encoding,
               within = Direction,
               type = 3,
               detailed = T,)

out1

out1$ANOVA$MSE = out1$ANOVA$SSd/out1$ANOVA$DFd
out1$ANOVA$MSE

aovEffectSize(out1, effectSize = "pes")

##Break down effects/interactions
tapply(presented$Scored, presented$Direction, mean) #main effect of direction
tapply(presented$Scored, presented$encoding, mean) #main effect of encoding
tapply(presented$Scored, list(presented$encoding, presented$Direction), mean)

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

mean(jol3$F); mean(read3$F)
sd(jol3$F); sd(read3$F)

#U
temp = t.test(jol3$U, read3$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(jol3$U); mean(read3$U)
sd(jol3$U); sd(read3$U)

##get means
(apply(jol3, 2, mean))
(apply(read3, 2, mean))

##get sds
(apply(jol3, 2, sd))
(apply(read3, 2, sd))

##get CIs
(apply(jol3, 2, sd) / sqrt(61)) * 1.96
(apply(read3, 2, sd) / sqrt(59)) * 1.96

##compare false alarms
FA = subset(dat,
            dat$Direction == "not presented")

FA2 = cast(FA, Username ~ encoding, mean)

apply(FA2, 2, mean, na.rm = T)
apply(FA2, 2, sd, na.rm = T)

#t-test
temp = t.test(FA2$JOL, FA2$Read, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#get sds for d
(apply(FA2, 2, sd, na.rm = T) / sqrt(62)) * 1.96

pbic1 = FA2[ , c(1, 2)]
pbic2 = FA2[ , c(1, 3)]

pbic1 = na.omit(pbic1)
pbic2 = na.omit(pbic2)

colnames(pbic1)[2] = "score"
colnames(pbic2)[2] = "score"

pbic1$task = rep("jol")
pbic2$task = rep("read")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Username,
        between = task,
        dv = score,
        detailed = T,
        type = 3)
