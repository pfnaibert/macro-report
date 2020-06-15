#################################################
# transform

y1 <- sum4(y[,2]) # gdp ac 4q (yearly)
y2 <- ret1(y[,3]) # gdp t/t-1
y3 <- ret4(y[,2]) # gdp t/t-4
y4 <- ret4(y1) # gdp ac t/t-4

gdps <- list(y[,1], y[,2], y[,3], y1, y2, y3, y4)
names(gdps) <- c("nominal", "real", "real.seas", "ac", "ret1", "ret4", "acret4")
