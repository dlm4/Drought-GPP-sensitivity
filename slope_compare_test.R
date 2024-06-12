y1 <- rnorm(10, mean = 50, sd = 10)
y2 <- rnorm(10, mean = 50, sd = 10)
y3 <- rnorm(10, mean = 50, sd = 10)

x <- rnorm(10, mean = 10, sd = 1)

lm(rowMeans(cbind(y1, y2, y3)) ~ x)

mean(c(lm(y1 ~ x)$coefficients[2], lm(y2 ~ x)$coefficients[2], lm(y3 ~ x)$coefficients[2]))

# mean of the slopes is the same as the slope of the means because one of the variables is always the same, cool