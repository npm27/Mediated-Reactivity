####Read in the data####
ex2 = read.csv("Sig Data/ex2_sig.csv")
ex3 = read.csv("Sig Data/ex3_sig.csv")
ex4 = read.csv("Sig Data/ex4_sig.csv")

##turn off scientific notation
options(scipen = 999)

####Ex 2####
###Make JOL and Read Subsets
JOL2 = subset(ex2,
              ex2$e == "JOL")
Read2 = subset(ex2,
               ex2$e == "Read")

###get means
mean(JOL2$dprime); mean(Read2$dprime) #1.9 vs. 1.03
mean(JOL2$c.1); mean(Read2$c.1) #.06 vs. .07

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

#####Ex 3####
###Make JOL and Read Subsets
JOL3 = subset(ex3,
              ex3$e == "JOL")
Read3 = subset(ex3,
               ex3$e == "Read")

###get means
mean(JOL3$dprime); mean(Read3$dprime) #1.65 vs. 1.12
mean(JOL3$c.1); mean(Read3$c.1) #.2 vs. .19

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

####Ex 4####
###Make JOL and Read Subsets
JOL4 = subset(ex4,
              ex4$e == "JOL")
Read4 = subset(ex4,
               ex4$e == "Read")

###get means
mean(JOL4$dprime); mean(Read4$dprime) #1.49 vs. 1.09
mean(JOL4$c.1); mean(Read4$c.1) #.10 vs. .17

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