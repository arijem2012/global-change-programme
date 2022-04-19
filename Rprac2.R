library(ggplot2)
library(tidyverse)
view(economics)
ggplot(economics, aes(psavert, uempmed)) + geom_point()
mean_unemployed <- mean(economics$unemploy)
mean_unemployed
