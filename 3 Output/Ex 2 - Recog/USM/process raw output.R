####This script will be used to read in everything and set EX 2data up for processing####
##Start by gathering all of the data
#JOLs and frequency
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/Mediated-Reactivity/3 Output/Ex 1B - Recog/USM/JOL")

files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#read
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/Mediated-Reactivity/3 Output/Ex 1B - Recog/USM/Read")

files2 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat2$Username))

#Now move back to the original folder
#This is where I'll store the combined final output for scoring
setwd('..')

####Clean up the data files####
##Drop unused columns
dat = dat[ , -c(2:7, 9:10, 12, 20:23, 28:33, 35)]
dat2 = dat2[ , -c(2:7, 9:10, 12, 20:23, 28:35)]

#Next, remove buffer trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Buffer")
dat2 = subset(dat2,
              dat2$Stimuli.Stimuli.Notes != "Buffer")

#Now remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "Instruct")
#Now remove filler task
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "FreeRecall")

####Set the data up for scoring####
#Start by subsetting out the recall and JOL data for each dataset
dat.JOL = subset(dat,
                 dat$Procedure.Trial.Type == "JOL")
dat.Recog1 = subset(dat,
                    dat$Procedure.Trial.Type == "Likert")

dat.Study = subset(dat2,
                 dat2$Procedure.Trial.Type == "Study")
dat.Recog2 = subset(dat2,
                   dat2$Procedure.Trial.Type == "Likert")

#combine encoding data
dat.Study$Response.JOL = rep(NA)

encoding.dat = rbind(dat.Study, dat.JOL)

##combine recog data
#need to get pair type info
#start w/ JOL data
#fix the weirdness
dat.Recog1$Stimuli.Stimuli.Notes[dat.Recog1$Stimuli.Stimuli.Notes == "CONTROL"] = "Control"
dat.Recog2$Stimuli.Stimuli.Notes[dat.Recog2$Stimuli.Stimuli.Notes == "CONTROL"] = "Control"

JOL.presented = subset(dat.Recog1,
                       dat.Recog1$Stimuli.Stimuli.Notes == "Presented")
JOL.control = subset(dat.Recog1,
                     dat.Recog1$Stimuli.Stimuli.Notes == "Control")

#Pull direction info from encoding data
dat.JOL = dat.JOL[order(dat.JOL$Condition.Number), ]
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Answer), ]

JOL.presented = JOL.presented[order(JOL.presented$Condition.Number), ]
JOL.presented = JOL.presented[order(JOL.presented$Stimuli.Answer), ]

JOL.presented2 = cbind(JOL.presented, dat.JOL[ , c(5, 7)])

JOL.presented2 = JOL.presented2[ , -c(2:4, 6, 8:12, 15)]

JOL.control = JOL.control[ , -c(2:4, 6, 8:12, 15)]

colnames(JOL.presented2)[7] = "Direction"
JOL.control$Direction = rep("not presented")

JOL.presented2 = JOL.presented2[ , -6]

JOL.combined = rbind(JOL.presented2, JOL.control)

##Now score!
JOL.combined$Response.Response[JOL.combined$Response.Response == "NO"] = "Control"
JOL.combined$Response.Response[JOL.combined$Response.Response == "YES"] = "Presented"

JOL.combined$Scored = JOL.combined$Stimuli.Stimuli.Notes == JOL.combined$Response.Response

JOL.combined$Scored = as.numeric(JOL.combined$Scored)

#cleanup
JOL.combined = JOL.combined[ , -c(2, 5)]

colnames(JOL.combined)[2] = "control"

JOL.combined$encoding = rep("JOL")
JOL.combined$source = rep("USM")

JOL.combined = JOL.combined[ , c(1, 2, 3, 4, 6, 7, 5)]

###Now do the Read Group
Study.presented = subset(dat.Recog2,
                       dat.Recog2$Stimuli.Stimuli.Notes == "Presented")
Study.control = subset(dat.Recog2,
                     dat.Recog2$Stimuli.Stimuli.Notes == "Control")

#Pull direction info from encoding data
dat.Study = dat.Study[order(dat.Study$Condition.Number), ]
dat.Study = dat.Study[order(dat.Study$Stimuli.Answer), ]

Study.presented = Study.presented[order(Study.presented$Condition.Number), ]
Study.presented = Study.presented[order(Study.presented$Stimuli.Answer), ]

Study.presented2 = cbind(Study.presented, dat.Study[ , c(5, 7)])

Study.presented2 = Study.presented2[ , -c(2:4, 6, 8:12)]

Study.control = Study.control[ , -c(2:4, 6, 8:12)]

colnames(Study.presented2)[7] = "Direction"
Study.control$Direction = rep("not presented")

Study.presented2 = Study.presented2[ , -6]

Study.combined = rbind(Study.presented2, Study.control)

##Now score!
Study.combined$Response.Response[Study.combined$Response.Response == "NO"] = "Control"
Study.combined$Response.Response[Study.combined$Response.Response == "YES"] = "Presented"

Study.combined$Scored = Study.combined$Stimuli.Stimuli.Notes == Study.combined$Response.Response

Study.combined$Scored = as.numeric(Study.combined$Scored)

#cleanup
Study.combined = Study.combined[ , -c(2, 5)]

colnames(Study.combined)[2] = "control"

Study.combined$encoding = rep("Read")
Study.combined$source = rep("USM")

Study.combined = Study.combined[ , c(1, 2, 3, 4, 6, 7, 5)]

##combined JOL and read
combined = rbind(JOL.combined, Study.combined)

#sort on Username
combined = combined[order(combined$Username), ]

#write to file
#write.csv(combined, file = "USM_recog.csv", row.names = F)

#write the encoding data to file
#write.csv(encoding.dat, file = "USM_encoding.csv", row.names = F)
