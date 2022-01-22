# question 1
n = 40
mean = 0.5
variance = 0.00625
sigma = sqrt(variance)

S = 15/n

Z = (S-mean)/sigma

# normal approximation
p_val = 2*pnorm(Z)
p_val

# binomial p value
rejection_region = c(0:14, 26:40)
sum(dbinom(rejection_region, size=n, p=mean))

# 95% confidence interval
sigma_est = sqrt(S*(1-S)/n)
c(S-1.96*sigma_est, S+1.96*sigma_est)

# 90% confidence interval
c(S-1.645*sigma_est, S+1.645*sigma_est)



# question 2
n = 400
S = 305/n

p = 0.7
sigma = sqrt(p*(1-p)/n)

# standard error
sigma_est = sqrt(S*(1-S)/n)
sigma_est

# 95% confidence interval
c(S-1.96*sigma_est, S+1.96*sigma_est)

# calculate Z statistic
Z = (S-p)/sigma

# p value using normal approximation
p_val = 2*(1-pnorm(Z))
p_val

# p value using binomial distribution
rejection_region = c(0:94, 306:400)
sum(dbinom(rejection_region, size=n, p=p))

r = 104
rejection_region = c(0:r, (400-r):400)
sum(dbinom(rejection_region, size=n, p=p))

# power calculation
# alternate hypothesis, p=0.8
r = 104
rejection_region = c(0:r, (400-r):400)
sum(dbinom(rejection_region, size=n, p=0.8))



# question 3
iq = read.csv("iq.csv")

# sample statistics
mean(iq$IQ)
sd(iq$IQ)

# hypothesis test: H0 is that the mean = 100
std_error = (sd(iq$IQ)/sqrt(length(iq$IQ)))
Z = abs(mean(iq$IQ) - 100)/(sd(iq$IQ)/sqrt(length(iq$IQ)));Z

# p-value calculation 
p_val = 2*pt(Z, 123, lower.tail = FALSE); p_val

# 95% confidence interval
c(mean(iq$IQ) - 1.98*std_error, mean(iq$IQ) + 1.98*std_error)


# 99% confidence interval
c(mean(iq$IQ) - 2.62*std_error, mean(iq$IQ) + 2.62*std_error)
