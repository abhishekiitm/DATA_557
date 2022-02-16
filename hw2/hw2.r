# Question 1

temp_exp = read.csv("temperature_experiment.csv")
temp_60 = temp_exp[temp_exp$temp == '60',]$output
temp_75 = temp_exp[temp_exp$temp == '75',]$output
sd_err = sqrt(var(temp_60)/length(temp_60) + var(temp_75)/length(temp_75))

Z = (mean(temp_75) - mean(temp_60))/(sd_err);Z

p_val = 1-pnorm(Z) ;p_val

with(temp_exp, 
t.test(output[temp == '75'], output[temp == '60'], alternative = "greater", var.equal=F))

with(temp_exp, 
     t.test(output[temp == '75'], output[temp == '60'], alternative = "greater", var.equal=T))

z.05 = qnorm(0.95)
lower = mean(temp_75) - mean(temp_60) - z.05*sd_err


lopooled.sample.var=sum((n-1)*s^2)/sum(n-1)
z=(m[1]-m[2])/sqrt(pooled.sample.var*sum(1/n))
data.frame(z, p=round(2*(1-pt(z,df=sum(n)-2)),4))





# Question 2

defects = read.csv("defects.csv")

weight_A = defects[defects$Method == 'A',]$Weight
weight_B = defects[defects$Method == 'B',]$Weight
weight_C = defects[defects$Method == 'C',]$Weight
weight_D = defects[defects$Method == 'D',]$Weight

t.test(weight_A, mu=10)
t.test(weight_B, mu=10)
t.test(weight_C, mu=10)
t.test(weight_D, mu=10)


t.test(weight_A, weight_B)
t.test(weight_A, weight_C)
t.test(weight_A, weight_D)
t.test(weight_B, weight_C)
t.test(weight_B, weight_D)
t.test(weight_C, weight_D)


summary(aov(defects$Weight ~ factor(defects$Method), data=defects))

