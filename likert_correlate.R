# correlation test

# create vectors
x <- sample(c(1:9), 500, replace = TRUE)
y <- sample(c(1:9), 500, replace = TRUE)


swtch <- function(y, i, j) {
  y[c(i, j)] <- y[c(j, i)]
  y
}

r_target <- -0.75
r_current <- cor(x, y)

r_diff <- r_target - cor(x, y)


for (i in 1:1000) {
  repeat {
    pick <- sample(1:length(y), 2, replace = FALSE)
    i <- pick[1]
    j <- pick[2]

    if (i != j) break
  }
  y <- swtch(y, i, j)

  if (abs(r_target - cor(x, y)) < abs(r_diff)) {
    r_current <- cor(x, y)
    r_diff <- r_target - r_current
  } else {
    y <- swtch(y, i, j)
  }
  if (r_diff <= 0.0025) break
}

plot(x, y)

library(ggplot2)
library(ggExtra)

dat <- as.data.frame(cbind(x, y))

# summary(dat)

ggplot(dat, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette = 1, direction = 1) +
  theme_bw() +
  theme(legend.position = "none")


ggplot(dat, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette = 1, direction = 1) +
  theme_minimal() +
  theme(legend.position = "none")



g <- ggplot(dat, aes(x, y)) +
  geom_count(colour = "sky blue") +
  geom_smooth(method = "lm", se = F) +
  theme_minimal() +
  theme(legend.position = "none")

ggMarginal(g, type = "boxplot", fill = "transparent")

ggMarginal(g, type = "histogram", fill = "transparent")

ggMarginal(g, type = "histogram", fill = "pink")


ggMarginal(g, type = "density", fill = "sky blue")

