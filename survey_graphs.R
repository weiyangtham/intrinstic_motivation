# Correlation graphs for survey using inverse transform sampling
# see http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html

library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)

s = 0.5
n = 100
corrmat = matrix(c(1, s, s, 1), 2)
mat_draws <- matrix(mvrnorm(n, mu = c(0.5, 0.5), Sigma = corrmat), ncol = 2) 
# apply normal CDF to get uniform distribution
df_draws <- mat_draws %>% as.data.frame %>% tbl_df %>% mutate(x = pnorm(V1), y = pnorm(V2))
cor(df_draws$V1, df_draws$V2)
cor(df_draws$x, df_draws$y)
corrplot <- df_draws %>% ggplot(aes(x, y)) + geom_point()
corrplot + geom_smooth(method = "lm", formula = y ~ x)

ggsave("testplot.png")

# experiment with different themes
corrplot + theme_bw()
corrplot + theme_minimal()
corrplot + theme_classic()
corrplot + theme_light()
