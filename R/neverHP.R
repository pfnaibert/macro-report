#############################################
# neverhpfilter
library(neverhpfilter)
library(knitr)
library(xts)

data(GDPC1)
t1 <- yth_filter(100*log(GDPC1), h = 8, p = 4)
head(t1)

plot(t1[,c(1,2)])
plot(t1[,c(3,4)])

yth_filter(y$real.SA)

na.rm
apply(t1, 2, function(x) mean(x, na.rm = TRUE))
