# Correlation graphs for survey
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)

s = 0.5
n = 100
corrmat = matrix(c(1, s, s, 1), 2)
mat_draws <- matrix(mvrnorm(n, mu = c(0.5, 0.5), Sigma = corrmat), ncol = 2) 
df_draws <- mat_draws %>% as.data.frame %>% tbl_df %>% mutate(x = pnorm(V1), y = pnorm(V2))
cor(df_draws$V1, df_draws$V2)
cor(df_draws$x, df_draws$y)
corrplot <- df_draws %>% ggplot(aes(x, y)) + geom_point() 
corrplot + geom_smooth(method = "lm", formula = y ~ x)
# zoom in 
corrplot + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

# experiment with different themes
corrplot + theme_bw()
corrplot + theme_minimal()
corrplot + theme_classic()
corrplot + theme_light()
