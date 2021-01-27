#----------------------------------#
####### 0: loading packages ####### 
#----------------------------------#
require(ggplot2)
require(mgcv)
require(itsadug)



#----------------------------------#
###### 1: Introducing the GAM ######
#----------------------------------#


# ------ 1.1 ------ #
set.seed(10)
n <- 250
x <- runif(n,0,5)
y_model <- 3*x/(1+2*x)
y_obs <- rnorm(n,y_model,0.1)
data_plot <- qplot(x, y_obs) + 
  geom_line(aes(y=y_model))+
  theme_bw()
print(data_plot)


# ------ 1.2 ------ #
linear_model <- gam(y_obs~x)
model_summary <- summary(linear_model)
print(model_summary)
data_plot <- data_plot+
  geom_line(colour="red",
  aes(y=fitted(linear_model)))
print(data_plot)


# ------ 1.3 ------ #
gam_model <- gam(y_obs~s(x))
summary(gam_model)
data_plot <- data_plot + 
  geom_line(colour="blue",aes(y=fitted(gam_model)))
print(data_plot)


# ------ 1.4 ------ #
plot(gam_model)


# ------ 1.5 ------ #
linear_model <- gam(y_obs~x)
nested_gam_model <- gam(y_obs~s(x)+x)
print(anova(linear_model, nested_gam_model, test="Chisq"))


# ------ 1.6 ------ #
n <- 250
x_test <- runif(n,-5,5)
y_test_fit <- 4*dnorm(x_test)
y_test_obs <- rnorm(n,y_test_fit, 0.2)


# ----  Challenge 1: solution ---- #
data_plot <- qplot(x_test, y_test_obs) + 
  geom_line(aes(y=y_test_fit))+
  theme_bw()
print(data_plot)

linear_model_test <- gam(y_test_obs~x_test)
nested_gam_model_test <- gam(y_test_obs~s(x_test)+x_test)
print(anova(linear_model_test, nested_gam_model_test, test="Chisq"))

summary(nested_gam_model_test)$s.table



#----------------------------------#
##### 2: Multiple smooth terms #####
#----------------------------------#


# ------ 2.1 ------ #
gam_data <- gamSim(eg=5)
head(gam_data)


# ------ 2.2 ------ #
basic_model <- gam(y~x0+s(x1), data=gam_data)
basic_summary <- summary(basic_model)
print(basic_summary$p.table)
print(basic_summary$s.table)

plot(basic_model)


# ------ 2.3 ------ #
two_term_model <- gam(y~x0+s(x1)+x2, data=gam_data)
two_term_summary <- summary(two_term_model)
print(two_term_summary$p.table)
print(two_term_summary$s.table)

plot(two_term_model)


# ------ 2.4 ------ #
two_smooth_model <- gam(y~x0+s(x1)+s(x2), data=gam_data)
two_smooth_summary <- summary(two_smooth_model)
print(two_smooth_summary$p.table)
print(two_smooth_summary$s.table)

plot(two_smooth_model,page=1)


# ------ 2.5 ------ #
anova(basic_model,two_term_model,two_smooth_model,
      test="Chisq")


# ----  Challenge 2: solution ---- #
three_term_model <- gam(y~x0+s(x1)+s(x2)+x3, data=gam_data)
three_smooth_model <- gam(y~x0+s(x1)+s(x2)+s(x3), data=gam_data)
three_smooth_summary <- summary(three_smooth_model)

print(three_smooth_summary$p.table)
print(three_smooth_summary$s.table)
plot(three_smooth_model,page=1)
# edf = 1 therefore term is linear.

anova(two_smooth_model,three_term_model,test="Chisq")
# term x3 is not significant



#----------------------------------#
######### 3: Interactions ##########
#----------------------------------#


# ------ 3.1 ------ #
categorical_interact <- gam(y~x0+s(x1)+s(x2,by=x0), 
                           data=gam_data)
categorical_interact_summary <- summary(categorical_interact)
print(categorical_interact_summary$s.table)
plot(categorical_interact,page=1)
vis.gam(categorical_interact,view=c("x2","x0"),theta=40,n.grid=500,border=NA) # or alternatively: plot using vis.gam function, where theta is the degree rotation on the x-y plane
anova(two_smooth_model,categorical_interact,test="Chisq")


# ------ 3.2 ------ #
smooth_interact <- gam(y~x0+s(x1,x2), 
                           data=gam_data)
smooth_interact_summary <- summary(smooth_interact)
print(smooth_interact_summary$s.table)
plot(smooth_interact,page=1,scheme=3)
vis.gam(smooth_interact,view=c("x1","x2"),theta=40,n.grid=500,border=NA) 
# plot(smooth_interact,page=1,scheme=1) will give a similar plot
anova(two_smooth_model,smooth_interact,test="Chisq")



#----------------------------------#
######## 4: Changing basis #########
#----------------------------------#


# ------ 4.1 ------ #
data(nottem)
n_years <- length(nottem)/12
nottem_month <- rep(1:12, times=n_years)
nottem_year <- rep(1920:(1920+n_years-1),each=12)

nottem_plot <- qplot(nottem_month,nottem, 
                    colour=factor(nottem_year), 
                    geom="line") + theme_bw()
print(nottem_plot)


# ------ 4.2 ------ #
year_gam <- gam(nottem~s(nottem_year)+
                      s(nottem_month, bs="cc"))
summary(year_gam)$s.table
plot(year_gam,page=1, scale=0)



#----------------------------------#
##### 5: Quick intro to GAMMs ######
#----------------------------------#


# ------ 5.1 ------ #
par(mfrow=c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")


# ------ 5.2 ------ #
year_gam <- gamm(nottem~s(nottem_year)+
                  s(nottem_month, bs="cc"))
year_gam_AR1 <- gamm(nottem~s(nottem_year)+
                  s(nottem_month, bs="cc"),correlation = corARMA(form = ~ 1|nottem_year, p = 1))
year_gam_AR2 <- gamm(nottem~s(nottem_year)+
                       s(nottem_month, bs="cc"),correlation = corARMA(form = ~ 1|nottem_year, p = 2))
anova(year_gam$lme,year_gam_AR1$lme,year_gam_AR2$lme)


# ------ 5.3 ------ #
gam_data2 <- gamSim(eg=6)
head(gam_data2)

gamm_intercept <- gam(y ~ s(x0) + s(fac, bs="re"), data=gam_data2)
summary(gamm_intercept)$s.table

plot(gamm_intercept, select=2) 

par(mfrow=c(1,2), cex=1.1)
plot_smooth(gamm_intercept, view="x0",
            rm.ranef=TRUE, main="intercept + s(x1)", rug=FALSE)
plot_smooth(gamm_intercept, view="x0", cond=list(fac="1"), 
            main="... + s(fac)", col='orange',
            ylim=c(8,21), rug=FALSE)
plot_smooth(gamm_intercept, view="x0", cond=list(fac="2"), 
            add=TRUE, col='red')
plot_smooth(gamm_intercept, view="x0", cond=list(fac="3"), 
            add=TRUE, col='purple')
plot_smooth(gamm_intercept, view="x0", cond=list(fac="4"), 
            add=TRUE, col='turquoise')


# ------ 5.4 ------ #
gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs="re"), data=gam_data2)
summary(gamm_slope)$s.table

plot_smooth(gamm_slope, view="x0", rm.ranef=TRUE, 
            main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_slope, view="x0", cond=list(fac="1"), 
            main="... + s(fac)", col='orange',ylim=c(7,22), rug=FALSE)
plot_smooth(gamm_slope, view="x0", cond=list(fac="2"), 
            add=TRUE, col='red')
plot_smooth(gamm_slope, view="x0", cond=list(fac="3"), 
            add=TRUE, col='purple')
plot_smooth(gamm_slope, view="x0", cond=list(fac="4"), 
            add=TRUE, col='turquoise')


# ------ 5.5 ------ #
gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs="re") 
                      + s(fac, x0, bs="re"), data=gam_data2)
summary(gamm_int_slope)$s.table

plot_smooth(gamm_int_slope, view="x0", rm.ranef=TRUE, 
            main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="1"), 
            main="... + s(fac) + s(fac, x0)", col='orange',
            ylim=c(7,22), rug=FALSE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="2"), 
            add=TRUE, col='red', xpd=TRUE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="3"), 
            add=TRUE, col='purple', xpd=TRUE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="4"), 
            add=TRUE, col='turquoise', xpd=TRUE)

plot(gamm_int_slope, select=3)


# ------ 5.6 ------ #
gamm_smooth <- gam(y ~ s(x0, fac, bs="fs", m=1), data=gam_data2)
summary(gamm_smooth)$s.table

plot_smooth(gamm_smooth, view="x0", rm.ranef=TRUE, 
            main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_smooth, view="x0", cond=list(fac="1"), 
            main="... + s(x0, fac)", col='orange',
            ylim=c(7,21), rug=FALSE)
plot_smooth(gamm_smooth, view="x0", cond=list(fac="2"), 
            add=TRUE, col='red', xpd=TRUE)
plot_smooth(gamm_smooth, view="x0", cond=list(fac="3"), 
            add=TRUE, col='purple', xpd=TRUE)
plot_smooth(gamm_smooth, view="x0", cond=list(fac="4"), 
            add=TRUE, col='turquoise', xpd=TRUE)

plot(gamm_smooth, select=1)



#---------------------------------------#
######## 6: Other distributions #########
#---------------------------------------#


# ------ 6.1 ------ #
gam_data3 <- read.csv("other_dist.csv")
summary(gam_data3)
str(gam_data3)


# ------ 6.2 ------ #
emptyPlot(range(gam_data3$x1), c(0,1), h=.5,
          main="Probability of successes", ylab="Probability",xlab="x1")

avg <- aggregate(prop ~ x1, data=gam_data3, mean, na.rm=TRUE)
lines(avg$x1, avg$prop, col="orange",lwd=2)


# ------ 6.3 ------ #
prop_model <- gam(prop ~ s(x1), data=gam_data3, weights=total, family="binomial")
prop_summary <- summary(prop_model)
print(prop_summary$p.table)
print(prop_summary$s.table)

plot(prop_model)


# ------ 6.4 ------ #
par(mfrow=c(1,2))
plot(prop_model, select=1, scale=0, shade=TRUE)
abline(h=0)

out <- plot_smooth(prop_model, view="x1",main="")
(diff <- find_difference(out$fv$fit, out$fv$CI, xVals=out$fv$x1))
addInterval(0, lowVals=diff$start, highVals = diff$end, col='red', lwd=2)
abline(v=c(diff$start, diff$end), lty=3, col='red')
text(mean(c(diff$start, diff$end)), 2.1, "sign. more \n success", col='red', font=3)

par(mfrow=c(1,1))
plot_smooth(prop_model, view="x1", main="",
            transform=plogis, ylim=c(0,1))
abline(h=.5, v=diff$start, col='red', lty=2)


