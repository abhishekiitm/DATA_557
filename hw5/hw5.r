install.packages("car")

sales = read.csv("Sales_sample.csv")

model1 = lm(formula = LAST_SALE_PRICE ~ SQFT + LOT_SIZE + BEDS + BATHS, data = sales)
pred = model1$fitted.values
res = model1$residuals

hist(res, breaks=100)

df = data.frame(sales)
df["res"] = res
df["pred"] = pred

plot(res ~ pred, data=df)
plot(model1)

df["LOG_PRICE"] = log(df$LAST_SALE_PRICE, 10)

model2 = lm(formula = LOG_PRICE ~ SQFT + LOT_SIZE + BEDS + BATHS, data = df)
summary(model2)$coefficients
