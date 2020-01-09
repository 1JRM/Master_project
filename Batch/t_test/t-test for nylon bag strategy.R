x <- rnorm(3, mean = 238, sd = 19)
y = rnorm(3, mean = 267, sd = 28)
t.test(x, y, conf.level = 0.95, alternative = "less")