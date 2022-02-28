
sales = read.csv("Sales.csv")

cor(sales, use="complete.obs")

plot(sales$SQFT, sales$LAST_SALE_PRICE, main="Scatter plot - area vs price",
     xlab="Area of house (sqft)", ylab="Last Sale Price", pch=1)

# cleaning na rows from SQFT, LAST_SALE_PRICE, LOT_SIZE

sales = sales[! (is.na(sales$SQFT) | is.na(sales$LAST_SALE_PRICE) | is.na(sales$LOT_SIZE)),]
summary(lm(formula = LAST_SALE_PRICE ~ SQFT, data = sales))$coef

summary(lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE, data = sales))$coef


summary(lm(formula = LAST_SALE_PRICE ~ SQFT, data = sales))$r.squared
summary(lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE, data = sales))$r.squared

model1 = summary(lm(formula = LAST_SALE_PRICE ~ SQFT, data = sales))
model2 = summary(lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE, data = sales))
sum(model1$residuals*model1$residuals)/(length(model1$residuals) - 2)
sum(model2$residuals*model2$residuals)/(length(model2$residuals) - 3)

model2$coefficients

# full model 
SSE1 = sum(model2$residuals*model2$residuals)
p1 = 3

# reduced model
last_sale_price = sales[!is.na(sales$LAST_SALE_PRICE),]$LAST_SALE_PRICE
mean0 = mean(last_sale_price)
res0 = last_sale_price - mean0
SSE0 = sum(res0*res0)
p0 = 1

F_statistic = (SSE0 - SSE1)/(p1-p0)/SSE1*model2$df[2] ;F_statistic

pf(F_statistic, p1-p0, model2$df[2], lower.tail = FALSE)


anova(lm(formula = LAST_SALE_PRICE ~ 1, data = sales))
anova(lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE, data = sales))
