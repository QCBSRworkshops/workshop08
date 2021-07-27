## library(ggplot2)
## set.seed(10)
## n = 250
## x = runif(n,0,5)
## y_model = 3*x/(1+2*x)
## y_obs = rnorm(n,y_model,0.1)
## data_plot = qplot(x, y_obs) +
##             geom_line(aes(y=y_model)) +
##             theme_bw()
## print(data_plot)

## library(mgcv)
## linear_model = gam(y_obs~x)
## model_summary=summary(linear_model)
## print(model_summary)
## data_plot = data_plot+
##              geom_line(colour="red",
##              aes(y=fitted(linear_model)))
## print(data_plot)

## gam_model = gam(y_obs~s(x))
## summary(gam_model)
## data_plot = data_plot +
##      geom_line(colour="blue",aes(y=fitted(gam_model)))
## print(data_plot)

## plot(gam_model)

## linear_model = gam(y_obs~x)
## nested_gam_model = gam(y_obs~s(x)+x)
## print(anova(linear_model, nested_gam_model, test="Chisq"))
