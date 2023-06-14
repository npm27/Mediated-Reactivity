####Read in the data####
ex2 = read.csv("Sig Data/ex2_sig.csv")
ex3 = read.csv("Sig Data/ex3_sig.csv")
ex4 = read.csv("Sig Data/ex4_sig.csv")

##turn off scientific notation
options(scipen = 999)

#load libraries
library(ez)

####Ex 2####
###Make JOL and Read Subsets
JOL2 = subset(ex2,
              ex2$e == "JOL")
Read2 = subset(ex2,
               ex2$e == "Read")

###get means
mean(JOL2$dprime); mean(Read2$dprime) #1.86 vs. 1.03
mean(JOL2$c.1); mean(Read2$c.1) #.06 vs. .07

#get sds
sd(JOL2$dprime); sd(Read2$dprime) 
sd(JOL2$c.1); sd(Read2$c.1) 

##d'
temp = t.test(JOL2$dprime, Read2$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Discriminability is higher for the JOL group, suggests memory accuracy improvement?

##C
temp = t.test(JOL2$c.1, Read2$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

#pbic
pbic = rbind(JOL2[ , c(1, 2, 11)], Read2[, c(1, 2, 11)])

model1 = ezANOVA(pbic,
                 wid = i,
                 between = e,
                 dv = c.1,
                 detailed = T,
                 type = 3)

model1

#####Ex 3####
###Make JOL and Read Subsets
JOL3 = subset(ex3,
              ex3$e == "JOL")
Read3 = subset(ex3,
               ex3$e == "Read")

###get means
mean(JOL3$dprime); mean(Read3$dprime) #1.65 vs. 1.12
mean(JOL3$c.1); mean(Read3$c.1) #.2 vs. .19

#get sds
sd(JOL3$dprime); sd(Read3$dprime) 
sd(JOL3$c.1); sd(Read3$c.1) 

##d'
temp = t.test(JOL3$dprime, Read3$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Discriminability is higher for the JOL group, suggests memory accuracy improvement?

##C
temp = t.test(JOL3$c.1, Read3$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

#pbic
pbic2 = rbind(JOL3[ , c(1, 2, 11)], Read3[, c(1, 2, 11)])

model2 = ezANOVA(pbic2,
                 wid = i,
                 between = e,
                 dv = c.1,
                 detailed = T,
                 type = 3)

model2

####Ex 4####
###Make JOL and Read Subsets
JOL4 = subset(ex4,
              ex4$e == "JOL")
Read4 = subset(ex4,
               ex4$e == "Read")

###get means
mean(JOL4$dprime); mean(Read4$dprime) #1.49 vs. 1.09
mean(JOL4$c.1); mean(Read4$c.1) #.10 vs. .17

#get sds
sd(JOL3$dprime); sd(Read3$dprime) 
sd(JOL3$c.1); sd(Read3$c.1) 

##d'
temp = t.test(JOL4$dprime, Read4$dprime, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Discriminability is higher for the JOL group, suggests memory accuracy improvement?

##C
temp = t.test(JOL4$c.1, Read4$c.1, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #No difference in C

#pbic
pbic3 = rbind(JOL4[ , c(1, 2, 11)], Read4[, c(1, 2, 11)])

model3 = ezANOVA(pbic3,
                 wid = i,
                 between = e,
                 dv = c.1,
                 detailed = T,
                 type = 3)

model3
