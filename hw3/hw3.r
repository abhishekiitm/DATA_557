library(plyr)
library(plyr)

lead = read.csv("lead_study.csv")
with(lead, 
     t.test(IQ[SEX == 1], IQ[SEX == 2], var.equal=F))

IQ_M = lead[lead$SEX==1,]$IQ
IQ_F = lead[lead$SEX==2,]$IQ

counts <- ddply(lead, .(lead$SEX, lead$GROUP), nrow)
names(counts) <- c("y", "m", "Freq")

summary(aov(lead$IQ ~ factor(lead$GROUP), data=lead))

residual_1 = lead[lead$GROUP==1,]$IQ - with(lead[lead$GROUP==1,],tapply(IQ, GROUP, mean))
residual_2 = lead[lead$GROUP==2,]$IQ - with(lead[lead$GROUP==2,],tapply(IQ, GROUP, mean))
residual_3 = lead[lead$GROUP==3,]$IQ - with(lead[lead$GROUP==3,],tapply(IQ, GROUP, mean))

# plot histogram
hist(residual_1)
hist(residual_2)
hist(residual_3)

var(residual_1)
var(residual_2)
var(residual_3)


IQ_1 = lead[lead$GROUP==1,]$IQ
IQ_2 = lead[lead$GROUP==2,]$IQ
IQ_3 = lead[lead$GROUP==3,]$IQ

t.test(IQ_1, IQ_2)
t.test(IQ_1, IQ_3)
t.test(IQ_2, IQ_3)

with(lead[lead$SEX==1,],tapply(IQ, GROUP, mean))
with(lead[lead$SEX==2,],tapply(IQ, GROUP, mean))
with(lead[lead$SEX==1,],tapply(IQ, GROUP, sd))
with(lead[lead$SEX==2,],tapply(IQ, GROUP, sd))

lead_M = lead[lead$SEX==1,]
lead_F = lead[lead$SEX==2,]
summary(aov(lead_M$IQ ~ factor(lead_M$GROUP), data=lead_M))
summary(aov(lead_F$IQ ~ factor(lead_F$GROUP), data=lead_F))

IQ_M_1 = lead_M[lead_M$GROUP==1,]$IQ
IQ_M_2 = lead_M[lead_M$GROUP==2,]$IQ
IQ_M_3 = lead_M[lead_M$GROUP==3,]$IQ
IQ_F_1 = lead_F[lead_F$GROUP==1,]$IQ
IQ_F_2 = lead_F[lead_F$GROUP==2,]$IQ
IQ_F_3 = lead_F[lead_F$GROUP==3,]$IQ

t.test(IQ_M_1, IQ_M_2)
t.test(IQ_M_1, IQ_M_3)
t.test(IQ_M_2, IQ_M_3)
t.test(IQ_F_1, IQ_F_2)
t.test(IQ_F_1, IQ_F_3)
t.test(IQ_F_2, IQ_F_3)

