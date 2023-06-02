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
length(unique(dat$Username)) #133 total participants
length(unique(dat1$Username)) #77 USM participants
length(unique(dat2$Username)) #56 MSU Texas participants

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

##write to file for cross experimental
#write.csv(presented, file = "orig.csv", row.names = F)

#Anova
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
direction.ph = cast(presented, Username ~ Direction, mean)

#f vs m
temp = t.test(direction.ph$F, direction.ph$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(direction.ph$F); mean(direction.ph$M)
sd(direction.ph$F); sd(direction.ph$M)

#f vs u
temp = t.test(direction.ph$F, direction.ph$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

pbic1 = direction.ph[ , c(1, 2)]
pbic2 = direction.ph[ , c(1, 4)]

pbic1$direction = rep("F")
pbic2$direction = rep("U")

colnames(pbic1)[2] = "score"
colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = score,
        between = direction,
        wid = Username,
        detailed = T,
        type = 3)

#m vs u
temp = t.test(direction.ph$M, direction.ph$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

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

#M
temp = t.test(jol3$M, read3$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(jol3$M); mean(read3$M)
sd(jol3$M); sd(read3$M)

#U
temp = t.test(jol3$U, read3$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(jol3$U); mean(read3$U)
sd(jol3$U); sd(read3$U)

##get ns
length(unique(jol3$Username)) #63
length(unique(read3$Username)) #62

##get sds for d
(apply(jol3, 2, sd) / sqrt(63)) * 1.96
(apply(read3, 2, sd) / sqrt(62)) * 1.96

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
mean(FA2$JOL, na.rm = T); mean(FA2$Read, na.rm = T)
sd(FA2$JOL, na.rm = T); sd(FA2$Read, na.rm = T)

##95% CI
x = apply(FA2, 2, sd, na.rm = T)

#JOL
(x[1] / sqrt(nrow(jol3))) * 1.96

#Read
(x[2] / sqrt(nrow(read3))) * 1.96
