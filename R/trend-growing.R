#####################################
# GDP linear TREND GROWING

coefs <- matrix(NA, nrow = (T-1), ncol = 2)
rownames(coefs) <- names(y1[-1])
colnames(coefs) <- c("ln.alpha", "g")

for(i in 2:T)
{
reg1 <- lm(y1[1:i]~t1[1:i]) # linear
coefs[i-1,] <- coef(reg1)
}

head(coefs)

trend  <- date.fit.q(y1[-1], 100*(exp(4*coefs[,2])-1) ); head(trend)
actual <- gdp$ret4     ; head(actual1)
#####################################

#####################################
# PLOTS

plot(actual)
lines(trend, col = "red")

abline(h=100*trend1, col = "blue")

plot(actual-trend)
abline(h=0)

plot(actual-trend1)
abline(h=0)

