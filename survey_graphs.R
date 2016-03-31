# Correlation graphs for survey using inverse transform sampling
# see http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html

library(MASS)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

corrplot <- function(rho, n){
corrmat = matrix(c(1, rho, rho, 1), 2)
mat_draws <- matrix(mvrnorm(n, mu = c(0.5, 0.5), Sigma = corrmat), ncol = 2) 
# apply normal CDF to get uniform distribution
df_draws <- mat_draws %>% as.data.frame %>% tbl_df %>% mutate(x = pnorm(V1), y = pnorm(V2))
cor(df_draws$V1, df_draws$V2)
actual_rho <- round(cor(df_draws$x, df_draws$y), 2)
# corrplot_title = str_c("Corr =", actual_rho, sep = " ")
corrplot <- df_draws %>% ggplot(aes(x, y)) + geom_point(size = 0.3) + theme(text = element_text(size = 7))
print(str_c("saving correlation_plots/testplot_", actual_rho * 100, ".png"))
ggsave(str_c("correlation_plots/testplot_", actual_rho * 100, ".png"), scale = 1/4)
return(corrplot)
}
corrplot(0.95, 100)
corrplot(0.1, 100)
corrplot(0.2, 100)
corrplot(0.4, 100)
corrplot(0.6, 100)
corrplot(0.8, 100)



# experiment with different themes
corrplot + theme_bw()
corrplot + theme_minimal()
corrplot + theme_classic()
corrplot + theme_light()
