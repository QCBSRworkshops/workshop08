##Section: 01-preparation-pour-l-atelier.R 

install.packages("ggplot2")
install.packages("mgcv")
install.packages("itsadug")

library(ggplot2)
library(itsadug)
library(itsadug)

source(file.choose()) # use coldiss.R which you have downloaded to your own directory


##Section: 02-introduction-fr.R 

library(ggplot2)
set.seed(10)
n = 250
x = runif(n,0,5)
y_model = 3*x/(1+2*x)
y_obs = rnorm(n,y_model,0.1)
data_plot = qplot(x, y_obs) +
            geom_line(aes(y=y_model)) +
            theme_bw()
print(data_plot)

library(mgcv)
linear_model = gam(y_obs~x)
model_summary=summary(linear_model)
print(model_summary)
data_plot = data_plot+
             geom_line(colour="red",
             aes(y=fitted(linear_model)))
print(data_plot)

gam_model = gam(y_obs~s(x))
summary(gam_model)
data_plot = data_plot +
     geom_line(colour="blue",aes(y=fitted(gam_model)))
print(data_plot)

plot(gam_model)

linear_model = gam(y_obs~x)
nested_gam_model = gam(y_obs~s(x)+x)
print(anova(linear_model, nested_gam_model, test="Chisq"))

n <- 250
x_test <- runif(n,-5,5)
y_test_fit <- 4*dnorm(x_test)
y_test_obs <- rnorm(n,y_test_fit, 0.2)

data_plot <- qplot(x_test, y_test_obs) +
  geom_line(aes(y=y_test_fit))+
  theme_bw()
print(data_plot)

linear_model_test <- gam(y_test_obs~x_test)
nested_gam_model_test <- gam(y_test_obs~s(x_test)+x_test)
print(anova(linear_model_test, nested_gam_model_test, test="Chisq"))

summary(nested_gam_model_test)$s.table


##Section: 03-plusieurs-termes-non-lineaires.R 

gam_data = gamSim(eg=5)
head(gam_data)

basic_model = gam(y~x0+s(x1), data= gam_data)
basic_summary = summary(basic_model)
print(basic_summary$p.table)
print(basic_summary$s.table)
plot(basic_model)

two_term_model <- gam(y~x0+s(x1)+x2, data=gam_data)
two_term_summary <- summary(two_term_model)
print(two_term_summary$p.table)
print(two_term_summary$s.table)

two_smooth_model <- gam(y~x0+s(x1)+s(x2), data=gam_data)
two_smooth_summary <- summary(two_smooth_model)
print(two_smooth_summary$p.table)
print(two_smooth_summary$s.table)
plot(two_smooth_model,page=1)

anova(basic_model,two_term_model,two_smooth_model, test="Chisq")

three_term_model <- gam(y~x0+s(x1)+s(x2)+x3, data=gam_data)
three_smooth_model <- gam(y~x0+s(x1)+s(x2)+s(x3), data=gam_data)
three_smooth_summary <- summary(three_smooth_model)

print(three_smooth_summary$p.table)
print(three_smooth_summary$s.table)
plot(three_smooth_model,page=1)
# edf = 1 -> le terme est donc linéaire.

anova(two_smooth_model,three_term_model,test="Chisq")
# le terme x3 n'est pas significatif


##Section: 04-interactions.R 

categorical_interact <- gam(y~x0+s(x1)+s(x2,by=x0),data=gam_data)
categorical_interact_summary <- summary(categorical_interact)
print(categorical_interact_summary$s.table)
plot(categorical_interact,page=1)
# ou nous pouvons utiliser la fonction vis.gam où theta représente la rotation du plan x-y
vis.gam(categorical_interact,view=c("x2","x0"),theta=40,n.grid=500,border=NA)
anova(two_smooth_model, categorical_interact,test="Chisq")

smooth_interact <- gam(y~x0+s(x1,x2),data=gam_data)
smooth_interact_summary <- summary(smooth_interact)
print(smooth_interact_summary$s.table)
plot(smooth_interact,page=1,scheme=3)
# plot(smooth_interact,page=1,scheme=1) donne un graphique comparable à vis.gam()
vis.gam(smooth_interact,view=c("x1","x2"),theta=40,n.grid=500,border=NA)
anova(two_smooth_model,smooth_interact,test="Chisq")


##Section: 05-changer-la-fonction-de-base.R 

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


##Section: 06-autres-distributions.R 

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

plot_smooth(prop_model, view="x1",main="")
(diff <- find_difference(out$fv$fit, out$fv$CI, xVals=out$fv$x1))
addInterval(0, lowVals=diff$start, highVals = diff$end, col='red', lwd=2)
abline(v=c(diff$start, diff$end), lty=3, col='red')
text(mean(c(diff$start, diff$end)), 2.1, "sign. more \n success", col='red', font=3)

par(mfrow=c(1,1))
plot_smooth(prop_model, view="x1", main="",
            transform=plogis, ylim=c(0,1))
abline(h=.5, v=diff$start, col='red', lty=2)


##Section: 07-GAMMs.R 

par(mfrow=c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")

year_gam <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"))
year_gam_AR1 <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 1))
year_gam_AR2 <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 2))
anova(year_gam$lme,year_gam_AR1$lme,year_gam_AR2$lme)

# Générez des données
gam_data2 <- gamSim(eg=6)
head(gam_data2)

# Faites rouler un modèle avec intercepte aléatoire
gamm_intercept <- gam(y ~ s(x0) + s(fac, bs="re"), data=gam_data2)
summary(gamm_intercept)

plot(gamm_intercept, select=2)
# select=2 parce que le terme aléatoire se trouve sur la 2e ligne du tableau sommaire.

par(mfrow=c(1,2), cex=1.1)
plot_smooth(gamm_intercept, view="x0", rm.ranef=TRUE, main="intercept + s(x1)", rug=FALSE)
plot_smooth(gamm_intercept, view="x0", cond=list(fac="1"),
            main="... + s(fac)", col='orange', ylim=c(8,21), rug=FALSE)
plot_smooth(gamm_intercept, view="x0", cond=list(fac="2"), add=TRUE, col='red')
plot_smooth(gamm_intercept, view="x0", cond=list(fac="3"), add=TRUE, col='purple')
plot_smooth(gamm_intercept, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise')

gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs="re"), data=gam_data2)
summary(gamm_slope)

plot_smooth(gamm_slope, view="x0", rm.ranef=TRUE, main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_slope, view="x0", cond=list(fac="1"),
            main="... + s(fac)", col='orange',ylim=c(7,22), rug=FALSE)
plot_smooth(gamm_slope, view="x0", cond=list(fac="2"), add=TRUE, col='red')
plot_smooth(gamm_slope, view="x0", cond=list(fac="3"), add=TRUE, col='purple')
plot_smooth(gamm_slope, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise')

gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs="re")
                      + s(fac, x0, bs="re"), data=gam_data2)
summary(gamm_int_slope)

plot_smooth(gamm_int_slope, view="x0", rm.ranef=TRUE, main="intercept + s(x0)", rug=FALSE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="1"),
            main="... + s(fac) + s(fac, x0)", col='orange', ylim=c(7,22), rug=FALSE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="2"), add=TRUE, col='red', xpd=TRUE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="3"), add=TRUE, col='purple', xpd=TRUE)
plot_smooth(gamm_int_slope, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise', xpd=TRUE)

plot(gamm_int_slope, select=3)
# select=3 parce que la pente aléatoire se trouve sur la 3e ligne du tableau sommaire.

gamm_smooth <- gam(y ~ s(x0) + s(x0, fac, bs="fs", m=1), data=gam_data2)
summary(gamm_smooth)

plot(gamm_smooth, select=1)
# select=1 parce que le terme se trouve sur la 1e ligne du tableau sommaire.


##Section: 08-GAMs-en-coulisse.R 




##Section: 09-references-fr.R 




