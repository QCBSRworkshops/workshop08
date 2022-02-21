##Section: 01-preparing-for-the-workshop.R 

install.packages("ggplot2")
install.packages("mgcv")
install.packages("itsadug")

library(ggplot2)
library(mgcv)
library(itsadug)


##Section: 02-introduction.R 

library(ggplot2, quietly = TRUE)
library(mgcv, quietly = TRUE)

isit <- read.csv("data/ISIT.csv")
head(isit)

isit2 <- subset(isit, Season == 2)

linear_model <- gam(Sources ~ SampleDepth, data = isit2)
summary(linear_model)

data_plot <- ggplot(data = isit2, aes(y = Sources, x = SampleDepth)) + 
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", size = 1.2) + 
  theme_bw()
data_plot

gam_model <- gam(Sources ~ s(SampleDepth), data = isit2)
summary(gam_model)

data_plot <- data_plot +
     geom_line(aes(y = fitted(gam_model)),
               colour = "blue", size = 1.2)
data_plot

plot(gam_model)

linear_model <- gam(Sources ~ SampleDepth, data = isit2)
smooth_model <- gam(Sources ~ s(SampleDepth), data = isit2)
AIC(linear_model, smooth_model)

isit1 <- subset(isit, Season == 1)

linear_model_s1 <- gam(Sources ~ SampleDepth, data = isit1)
smooth_model_s1 <- gam(Sources ~ s(SampleDepth), data = isit1)

ggplot(isit1, aes(x = SampleDepth, y = Sources)) +
  geom_point() +
  geom_line(colour = "red", size = 1.2,
            aes(y = fitted(linear_model_s1))) +
  geom_line(colour = "blue", size = 1.2,
            aes(y = fitted(smooth_model_s1))) +
  theme_bw()

AIC(linear_model_s1, smooth_model_s1)

smooth_model_s1


##Section: 03-a-closer-look.R 




##Section: 04-smooth-terms.R 

head(isit)
isit$Season <- as.factor(isit$Season)

basic_model <- gam(Sources ~ Season + s(SampleDepth), data = isit, method = "REML")
basic_summary <- summary(basic_model)

basic_summary$p.table

basic_summary$s.table

plot(basic_model, all.terms = TRUE, page = 1)

two_term_model <- gam(Sources ~ Season + s(SampleDepth) + RelativeDepth, 
                      data = isit, method = "REML")
two_term_summary <- summary(two_term_model)

two_term_summary$p.table

two_term_summary$s.table

plot(two_term_model, page = 1, all.terms = TRUE)

two_smooth_model <- gam(Sources ~ Season + s(SampleDepth) + s(RelativeDepth), 
                        data = isit, method = "REML")
two_smooth_summary <- summary(two_smooth_model)

two_smooth_summary$p.table

two_smooth_summary$s.table

plot(two_smooth_model, page = 1, all.terms = TRUE)

AIC(basic_model, two_term_model, two_smooth_model)

# Add Latitude as a linear term
three_term_model <- gam(Sources ~ 
                          Season + s(SampleDepth) + s(RelativeDepth) + 
                          Latitude, 
                        data = isit, method = "REML")
(three_term_summary <- summary(three_term_model))

# Add Latitude as a smooth term
three_smooth_model <- gam(Sources ~ 
                            Season + s(SampleDepth) + s(RelativeDepth) + 
                            s(Latitude),
                          data = isit, method = "REML")
(three_smooth_summary <- summary(three_smooth_model))

plot(three_term_model, page = 1, all.terms = TRUE)

plot(three_smooth_model, page = 1, all.terms = TRUE)

three_smooth_summary$s.table

AIC(three_smooth_model, three_term_model)

AIC(two_smooth_model, three_smooth_model)


##Section: 05-interactions.R 

categorical_interact <- gam(y~x0+s(x1)+s(x2,by=x0),data=gam_data)
categorical_interact_summary <- summary(categorical_interact)
print(categorical_interact_summary$s.table)
plot(categorical_interact,page=1)
# or alternatively: plot using vis.gam function, where theta is the degree rotation on the x-y plane
vis.gam(categorical_interact,view=c("x2","x0"),theta=40,n.grid=500,border=NA)
anova(two_smooth_model, categorical_interact,test="Chisq")

smooth_interact <- gam(y~x0+s(x1,x2),data=gam_data)
smooth_interact_summary <- summary(smooth_interact)
print(smooth_interact_summary$s.table)
plot(smooth_interact,page=1,scheme=3)
# plot(smooth_interact,page=1,scheme=1) will give a similar plot to the vis.gam()
vis.gam(smooth_interact,view=c("x1","x2"),theta=40,n.grid=500,border=NA)
anova(two_smooth_model,smooth_interact,test="Chisq")


##Section: 06-changing-basis.R 

data(nottem)
n_years <- length(nottem)/12
nottem_month <- rep(1:12, times=n_years)
nottem_year <- rep(1920:(1920+n_years-1),each=12)
nottem_plot <- qplot(nottem_month,nottem,
                    colour=factor(nottem_year),
                    geom="line") + theme_bw()
print(nottem_plot)

year_gam <- gam(nottem~s(nottem_year)+s(nottem_month, bs="cc"))
summary(year_gam)$s.table
plot(year_gam,page=1, scale=0)

pred<-predict(year_gam, type = "terms", se = TRUE)
I<-order(nottem_year)
plusCI<-I(pred$fit[,1] + 1.96*pred$se[,1])
minusCI<-I(pred$fit[,1] - 1.96*pred$se[,1])
xx <- c(nottem_year[I],rev(nottem_year[I]))
yy <- c(plusCI[I],rev(minusCI[I]))
plot(xx,yy,type="n",cex.axis=1.2)
polygon(xx,yy,col="light grey",border="light grey")
lines(nottem_year[I], pred$fit[,1][I],lty=1)
abline(h=0)


##Section: 07-other-distributions.R 

gam_data3 <- read.csv("other_dist.csv")
summary(gam_data3)
str(gam_data3)

emptyPlot(range(gam_data3$x1), c(0,1), h=.5,
          main="Probability of successes", ylab="Probability",xlab="x1")

avg <- aggregate(prop ~ x1, data=gam_data3, mean, na.rm=TRUE)
lines(avg$x1, avg$prop, col="orange",lwd=2)

prop_model <- gam(prop~ s(x1), data=gam_data3, weights=total, family="binomial")
prop_summary <- summary(prop_model)
print(prop_summary$p.table)
print(prop_summary$s.table)

plot(prop_model)

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


##Section: 08-GAMMs.R 

par(mfrow=c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")

year_gam <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"))
year_gam_AR1 <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 1))
year_gam_AR2 <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 2))
anova(year_gam$lme,year_gam_AR1$lme,year_gam_AR2$lme)

# generate and view data
gam_data2 <- gamSim(eg=6)
head(gam_data2)

# run random intercept model
gamm_intercept <- gam(y ~ s(x0) + s(fac, bs="re"), data=gam_data2)

# examine model output
summary(gamm_intercept)$s.table

plot(gamm_intercept, select=2)
# select=2 because the random effect appears as the second entry in the summary table.

par(mfrow=c(1,2), cex=1.1)
plot_smooth(gamm_intercept, view="x0", rm.ranef=TRUE, main="intercept + s(x1)", rug=FALSE)
plot_smooth(gamm_intercept, view="x0", cond=list(fac="1"),
            main="... + s(fac)", col='orange', ylim=c(8,21), rug=FALSE)
plot_smooth(gamm_intercept, view="x0", cond=list(fac="2"), add=TRUE, col='red')
plot_smooth(gamm_intercept, view="x0", cond=list(fac="3"), add=TRUE, col='purple')
plot_smooth(gamm_intercept, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise')

gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs="re"), data=gam_data2)
summary(gamm_slope)$s.table

plot_smooth(gamm_slope, view="x0", rm.ranef=TRUE, main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_slope, view="x0", cond=list(fac="1"),
            main="... + s(fac)", col='orange',ylim=c(7,22), rug=FALSE)
plot_smooth(gamm_slope, view="x0", cond=list(fac="2"), add=TRUE, col='red')
plot_smooth(gamm_slope, view="x0", cond=list(fac="3"), add=TRUE, col='purple')
plot_smooth(gamm_slope, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise')

gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs="re")
                      + s(fac, x0, bs="re"), data=gam_data2)
summary(gamm_int_slope)$s.table

plot_smooth(gamm_int_slope, view="x0", rm.ranef=TRUE, main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="1"),
            main="... + s(fac) + s(fac, x0)", col='orange', ylim=c(7,22), rug=FALSE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="2"), add=TRUE, col='red', xpd=TRUE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="3"), add=TRUE, col='purple', xpd=TRUE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise', xpd=TRUE)

plot(gamm_int_slope, select=3)
# select=3 because the random slope appears as the third entry in your summary table.

gamm_smooth <- gam(y ~ s(x0, fac, bs="fs", m=1), data=gam_data2)
summary(gamm_smooth)$s.table

plot(gamm_smooth, select=1)
# select=1 because the smooth slope appears as the first entry in your summary table.


##Section: 09-references.R 




