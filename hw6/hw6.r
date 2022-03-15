
library("sandwich")
library("lmtest")

sales = read.csv("Sales_sample.csv")

model1 = lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE + BEDS + BATHS, data = sales)

v = vcovHC(model1)
robust.se = sqrt(diag(v))
round(cbind(summary(model1)$coef,robust.se),4)

p_val = 2*(1-pt(abs(0.8848967), 1000-4));p_val
  
robust.z = summary(model1)$coefficients[,1]/robust.se
cbind(summary(model1)$coef, robust.se, robust.z)

n <- nrow(sales)
b.jack <- rep(0,n)
for(i in 1:n){
  lmi <- lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE + BEDS + BATHS, data=sales, subset=-i)
  b.jack[i] <- lmi$coef[3]
}

SE.jack <- (n-1)*sd(b.jack)/sqrt(n);SE.jack

t_jack = 6.844143/SE.jack;t_jack
p_jack = 2*(1-pt(abs(t_jack), 1000-4));p_jack



model1_A = lm(formula = LAST_SALE_PRICE ~ SQFT + BEDS + BATHS, data = sales)

v = vcovHC(model1_A)
robust_A.se = sqrt(diag(v))
round(cbind(summary(model1_A)$coef,robust_A.se),4)



model1_B = lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE + I(LOT_SIZE^2) + BEDS + BATHS, data = sales)

v = vcovHC(model1_B)
robust_B.se = sqrt(diag(v))
round(cbind(summary(model1_B)$coef,robust_B.se),4)

anova_result = anova(model1_A, model1_B)
SSER = anova_result$RSS[1]
SSEF = anova_result$RSS[2]
dfR = anova_result$Res.Df[1]
dfF = anova_result$Res.Df[2]
F = (SSER-SSEF)/SSEF/(dfR-dfF)*dfF
p_val = 1-pf(F, 2, dfF);p_val

waldtest(model1_A, model1_B, test="Chisq",vcov=vcovHC)



df = data.frame(sales)
df["LOG_PRICE"] = log(df$LAST_SALE_PRICE, 10)

model1_A_LOG = lm(formula = LOG_PRICE ~ SQFT + BEDS + BATHS, data = df)
v = vcovHC(model1_A_LOG)
robust_A_LOG.se = sqrt(diag(v))
round(cbind(summary(model1_A_LOG)$coef,robust_A_LOG.se),4)


model1_B_LOG = lm(formula = LOG_PRICE ~ SQFT + LOT_SIZE + I(LOT_SIZE^2) + BEDS + BATHS, data = df)
v = vcovHC(model1_B_LOG)
robust_B_LOG.se = sqrt(diag(v))
round(cbind(summary(model1_B_LOG)$coef,robust_B_LOG.se),4)


anova_result = anova(model1_A_LOG, model1_B_LOG)
SSER = anova_result$RSS[1]
SSEF = anova_result$RSS[2]
dfR = anova_result$Res.Df[1]
dfF = anova_result$Res.Df[2]
F = (SSER-SSEF)/SSEF/(dfR-dfF)*dfF
p_val = 1-pf(F, 2, dfF);p_val

waldtest(model1_A_LOG, model1_B_LOG, test="Chisq",vcov=vcovHC)
