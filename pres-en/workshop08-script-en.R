# Function to control the lines shown in code chunk outputs, to avoid overflow
# note: sometimes cache needs to be set to true in the knitr setup chunk for this to take effect in xaringan::infinite_moon_reader()
library(knitr)
hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
   lines <- options$output.lines
   if (is.null(lines)) {
     return(hook_output(x, options))  # pass to default hook
   }
   x <- unlist(strsplit(x, "\n"))
   more <- "..."
   if (length(lines)==1) {        # first n lines
     if (length(x) > lines) {
       # truncate the output, but add ....
       x <- c(head(x, lines), more)
     }
   } else {
     x <- c(more, x[lines], more)
   }
   # paste these lines together
   x <- paste(c(x, ""), collapse = "\n")
   hook_output(x, options)
 })

# Standard procedure to check and install packages and their dependencies, if needed.

list.of.packages <- c("ggplot2", "itsadug", "mgcv")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0) {
  install.packages(new.packages, dependencies = TRUE) 
  print(paste0("The following package was installed:", new.packages)) 
} else if(length(new.packages) == 0) {
    print("All packages were already installed previously")
}

# Load all required libraries at once
lapply(list.of.packages, require, character.only = TRUE, quietly = TRUE)

isit <- read.csv("data/ISIT.csv")
head(isit)

isit <- read.csv("data/ISIT.csv")

library(ggplot2)
library(mgcv)

isit2 <- subset(isit, Season==2)

linear_model <- gam(Sources ~ SampleDepth, data = isit2)
data_plot <- ggplot(isit2, aes(y = Sources, x = SampleDepth)) + geom_point() +
             geom_line(colour = "red", size = 1.2,aes(y = fitted(linear_model))) + theme_bw()
data_plot

library(ggplot2)
set.seed(10)
n <- 250
x <- runif(n,0,5)
y_model <- 3*x/(1+2*x)
y_obs <- rnorm(n,y_model,0.1)
data_plot <- qplot(x, y_obs) +
  geom_line(aes(y=y_model)) +
  theme_bw()
data_plot

library(ggplot2)
set.seed(10)
n <- 250
x <- runif(n,0,5)
y_model <- 3*x/(1+2*x)
y_obs <- rnorm(n,y_model,0.1)
data_plot <- qplot(x, y_obs) +
  geom_line(aes(y=y_model)) +
  theme_bw()
data_plot

library(mgcv)
linear_model <- gam(y_obs ~ x)
model_summary <- summary(linear_model)
data_plot <- data_plot +
             geom_line(colour = "red", size = 1.2, aes(y = fitted(linear_model)))
data_plot

gam_model <- gam(Sources ~ s(SampleDepth), data = isit2)

gam_model <- gam(Sources ~ s(SampleDepth), data = isit2)
summary(gam_model)

data_plot <- data_plot +
     geom_line(colour = "blue", size = 1.2, 
               aes(y = fitted(gam_model)))
data_plot

library(mgcv)
gam_model <- gam(y_obs ~ s(x))
summary(gam_model)

data_plot <- data_plot +
     geom_line(colour = "blue", size = 1.2,
               aes(y = fitted(gam_model)))
data_plot

gam_model <- gam(y_obs ~ s(x))
summary(gam_model)

data_plot <- data_plot +
     geom_line(colour = "blue", size = 1.2,
               aes(y = fitted(gam_model)))
data_plot

plot(gam_model)

linear_model <- gam(Sources ~ SampleDepth, data = isit2) # fit a regular linear model using gam()
nested_gam_model <- gam(Sources ~ s(SampleDepth), data = isit2)
AIC(linear_model, nested_gam_model)

linear_model <- gam(y_obs ~ x) # fit a regular linear model using gam()
nested_gam_model <- gam(y_obs ~ s(x) + x)
anova(linear_model, nested_gam_model, test = "Chisq")

isit1 <- subset(isit, Season==1)

linear_model_s1 <- gam(Sources ~ SampleDepth, data = isit1)
gam_model_s1 <- gam(Sources ~ s(SampleDepth), data = isit1)


ggplot(isit1,aes(x = SampleDepth, y = Sources)) +
  geom_point() +
  geom_line(colour = "red", size = 1.2,
            aes(y = fitted(linear_model_s1))) +
  geom_line(colour = "blue", size = 1.2,
            aes(y = fitted(gam_model_s1))) +
  theme_bw()

linear_model_s1 <- gam(Sources ~ SampleDepth,data = isit1)
nested_gam_model_s1 <- gam(Sources ~ s(SampleDepth),data = isit1)

AIC(linear_model_s1, nested_gam_model_s1)

nested_gam_model_s1

n <- 250
x_test <- runif(n, -5, 5)
y_test_fit <- 4 * dnorm(x_test)
y_test_obs <- rnorm(n, y_test_fit, 0.2)

linear_model_test <- gam(y_test_obs ~ x_test)
nested_gam_model_test <- gam(y_test_obs ~ s(x_test) + x_test)

anova(linear_model_test, nested_gam_model_test, test="Chisq")

qplot(x_test, y_test_obs) +
  geom_line(aes(y = y_test_fit)) +
  theme_bw()

nested_gam_model_test

head(isit)
isit$Season <- as.factor(isit$Season)

isit$Season <- as.factor(isit$Season)

isit$Season <- as.factor(isit$Season)
basic_model <- gam(Sources ~ Season + s(SampleDepth), data = isit, method = "REML")
basic_summary <- summary(basic_model)

basic_summary$p.table

basic_summary$s.table

# ?gamSim
gam_data <-  gamSim(eg = 5)
head(gam_data)

basic_model <- gam(y ~ x0 + s(x1), data = gam_data)
basic_summary <- summary(basic_model)
basic_summary$p.table

basic_summary$s.table

plot(basic_model, all.terms = TRUE,page=1)

basic_summary$s.table

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

two_term_model <- gam(y ~ x0 + s(x1) + x2, data = gam_data)
two_term_summary <- summary(two_term_model)
two_term_summary$p.table

two_term_summary$s.table

plot(two_term_model)

two_smooth_model <- gam(y ~ x0 + s(x1) + s(x2), data = gam_data)
two_smooth_summary <- summary(two_smooth_model)
two_smooth_summary$p.table

two_smooth_summary$s.table

plot(two_smooth_model, page = 1)

anova(basic_model, two_term_model, two_smooth_model,test='Chisq')

# Add Latitude as a linear term
three_term_model <- gam(Sources ~ 
                          Season + s(SampleDepth) + s(RelativeDepth) + 
                          Latitude, 
                        data = isit, method = "REML")
three_term_summary <- summary(three_term_model)

# Add Latitude as a smooth term
three_smooth_model <- gam(Sources ~ 
                            Season + s(SampleDepth) + s(RelativeDepth) + 
                            s(Latitude),
                          data = isit, method = "REML")
three_smooth_summary <- summary(three_smooth_model)

three_term_model <- gam(y ~ x0 + s(x1) + s(x2) + x3, data = gam_data)
three_smooth_model <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = gam_data)
three_smooth_summary <- summary(three_smooth_model)

plot(three_term_model, page = 1, all.terms = TRUE)

par(mar=c(3.8,3.8,.2,.2))
plot(three_term_model, page = 1, all.terms = TRUE)

plot(three_smooth_model, page = 1, all.terms = TRUE)

par(mar=c(3.8,3.8,.2,.2))
plot(three_smooth_model, page = 1, all.terms = TRUE)

three_smooth_summary$s.table

AIC(three_smooth_model, three_term_model)

AIC(two_smooth_model, three_smooth_model)

three_term_model <- gam(y ~ x0 + s(x1) + s(x2) + x3, data = gam_data)
three_smooth_model <- gam(y~x0 + s(x1) + s(x2) + s(x3), data = gam_data)
three_smooth_summary <- summary(three_smooth_model)

plot(three_smooth_model, page = 1)

par(mar=c(3.8,3.8,.2,.2))
plot(three_smooth_model, page = 1)

three_smooth_summary$s.table

# edf = 1 therefore term is linear.

AIC(two_smooth_model, three_term_model, test = "Chisq")

# term x3 is not significant, it should be dropped!

factor_interact <- gam(Sources ~ Season + 
                         s(SampleDepth, by=Season) + 
                         s(RelativeDepth), 
                       data = isit, method = "REML")

summary(factor_interact)$s.table

factor_interact <- gam(y ~ x0 + s(x1) + s(x2, by = x0), data = gam_data)

summary(factor_interact)$s.table

plot(factor_interact, page = 1)

vis.gam(factor_interact, view = c("x2","x0"), theta = 40, n.grid = 500, border = NA)

AIC(two_smooth_model, factor_interact)

smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth), 
                       data = isit, method = "REML")
summary(smooth_interact)$s.table

plot(smooth_interact, page = 1, scheme = 3)

smooth_interact <- gam(y~x0 + s(x1, x2), data = gam_data)
summary(smooth_interact)$s.table

plot(smooth_interact, page = 1, scheme = 3)

vis.gam(smooth_interact, view = c("SampleDepth", "RelativeDepth"), 
        cond = list(Season = 1), theta=40, n.grid = 50, color = "cm")

AIC(two_smooth_model, smooth_interact)

smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth), 
                       data = isit, method = "REML")

summary(smooth_interact)$p.table

summary(smooth_interact)$s.table

k.check(smooth_interact)

smooth_interact_k60 <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                           data = isit, method = "REML")
summary(smooth_interact_k60)$p.table
summary(smooth_interact_k60)$s.table

k.check(smooth_interact_k60)

smooth_interact <- smooth_interact_k60

par(mfrow = c(2,2)) # Show all 4 plots side by side
gam.check(smooth_interact)

par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
gam.check(smooth_interact)

?family.mgcv

# Normal distribution
smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                          data = isit, method = "REML")
# Tweedie family with log link
smooth_interact_tw <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                          family = tw(link = "log"),
                          data = isit, method = "REML")

smooth_interact_tw <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                          family = tw(link = "log"),
                          data = isit, method = "REML")
summary(smooth_interact_tw)$p.table
summary(smooth_interact_tw)$s.table

k.check(smooth_interact_tw)

par(mfrow=c(2,2))
gam.check(smooth_interact_tw)

par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
gam.check(smooth_interact_tw)

AIC(smooth_interact, smooth_interact_tw)

data(nottem) # Nottingham temperature time series
n_years <- length(nottem)/12
nottem_month <- rep(1:12, times = n_years)
nottem_year <- rep(1920:(1920 + n_years - 1), each = 12)
qplot(nottem_month, nottem, colour = factor(nottem_year), geom = "line") +
  theme_bw()

data(nottem)
n_years <- length(nottem)/12
nottem_month <- rep(1:12, times = n_years)
nottem_year <- rep(1920:(1920 + n_years - 1), each = 12)
qplot(nottem_month, nottem, colour = factor(nottem_year), geom = "line") +
  theme_bw()

year_gam <- gam(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"), method = "REML")
summary(year_gam)$s.table

plot(year_gam, page = 1, scale = 0)

par(mfrow = c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")

par(mfrow = c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")

year_gam <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"))
year_gam_AR1 <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 1),
                   data = data.frame(nottem, nottem_year, nottem_month))
year_gam_AR2 <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 2),
                   data = data.frame(nottem, nottem_year, nottem_month))


AIC(year_gam$lme, year_gam_AR1$lme, year_gam_AR2$lme)

gam_data2 <- gamSim(eg = 6)
str(gam_data2)

par(mar=c(4,4,1,1))
gamm_intercept <- gam(y ~ s(x0) + s(fac, bs = "re"), data = gam_data2, method = "REML")
summary(gamm_intercept)$s.table
plot(gamm_intercept, select = 2)

par(mfrow = c(1,2), cex = 1.1)

plot_smooth(gamm_intercept, view = "x0", rm.ranef = T,
            main = "intercept + s(x1)")

plot_smooth(gamm_intercept, view = "x0", cond = list(fac="1"),
            main = "... + s(fac)", col = 'orange', ylim = c(8,21))

plot_smooth(gamm_intercept, view = "x0", cond = list(fac = "2"), add = T, col = 'red')

plot_smooth(gamm_intercept, view="x0", cond = list(fac = "3"), add = T, col = 'purple')

plot_smooth(gamm_intercept, view="x0", cond = list(fac = "4"), add = T, col = 'turquoise')

par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_intercept, view = "x0", rm.ranef = T,
            main = "intercept + s(x1)", print.summary = F)
plot_smooth(gamm_intercept, view = "x0", cond = list(fac="1"),
            main = "... + s(fac)", col = 'orange', ylim = c(8,21), print.summary = F)
plot_smooth(gamm_intercept, view = "x0", cond = list(fac = "2"), add = T, col = 'red', print.summary = F)
plot_smooth(gamm_intercept, view ="x0", cond = list(fac = "3"), add = T, col = 'purple', print.summary = F)
plot_smooth(gamm_intercept, view ="x0", cond = list(fac = "4"), add = T, col = 'turquoise', print.summary = F)

gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs = "re"), data = gam_data2, method = "REML")

summary(gamm_slope)$s.table

par(mfrow = c(1,2), cex = 1.1)

plot_smooth(gamm_slope, view = "x0", rm.ranef = TRUE, main = "intercept + s(x0)")

plot_smooth(gamm_slope, view = "x0", cond = list(fac = "1"),
            main = "... + s(fac)", col = 'orange', ylim = c(7,22))

plot_smooth(gamm_slope, view = "x0", cond = list(fac = "2"), add = T, col = 'red')

plot_smooth(gamm_slope, view = "x0", cond = list(fac = "3"), add = T, col = 'purple')

plot_smooth(gamm_slope, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise')

par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)",
            print.summary = F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "1"),
            main = "... + s(fac)", col = 'orange', ylim = c(7,22), print.summary = F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "2"), add = T, col = 'red',
            print.summary = F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "3"), add = T, col = 'purple',
            print.summary = F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise',
            print.summary = F)

gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs = "re") + s(fac, x0, bs = "re"),
                      data = gam_data2, method = "REML")

summary(gamm_int_slope)$s.table

par(mfrow = c(1,2), cex = 1.1)

plot_smooth(gamm_int_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)")

plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "1"),
            main="... + s(fac) + s(fac, x0)", col = 'orange', ylim = c(7,22))

plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "2"), add = T, col='red')

plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "3"), add = T, col = 'purple')

plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise')

par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_int_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)", print.summary = F)
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "1"), print.summary = F,
            main="... + s(fac) + s(fac, x0)", col = 'orange', ylim = c(7,22))
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "2"), add = T,
            col='red',print.summary = F)
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "3"), add = T,
            col = 'purple', print.summary = F)
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "4"), add = T,
            col = 'turquoise', print.summary = F)

plot(gamm_int_slope, select = 3)

gamm_smooth <- gam(y ~ s(x0) + s(x0, fac, bs = "fs", m = 1), 
                   data = gam_data2, method = "REML")

summary(gamm_smooth)$s.table

par(mar=c(4,4,.5,.5), lwd = 2)
plot(gamm_smooth, select = 1)

par(mfrow = c(1,2), cex = 1.1)

plot_smooth(gamm_smooth, view = "x0", rm.ranef = T, main = "intercept + s(x0)")

plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "1"),
            main="... + s(x0, fac)", col = 'orange', ylim = c(7,22))

plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "2"), add = T, col='red')

plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "3"), add = T, col = 'purple')

plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise')

par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_smooth, view = "x0", rm.ranef = T, main = "intercept + s(x0)", print.summary = F)
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "1"), print.summary = F,
            main="... + s(x0, fac)", col = 'orange', ylim = c(7,22))
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "2"), add = T,
            col='red',print.summary = F)
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "3"), add = T,
            col = 'purple', print.summary = F)
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "4"), add = T,
            col = 'turquoise', print.summary = F)

AIC(gamm_intercept, gamm_slope, gamm_int_slope, gamm_smooth)

gam_data3 <- read.csv("data/other_dist.csv")
str(gam_data3)

plot(range(gam_data3$x1), c(0,1), type = "n",
     main = "Probability of successes over time",
     ylab = "Probability", xlab = "x1 (time)")
abline(h = 0.5)

avg <- aggregate(prop ~ x1, data = gam_data3, mean)
lines(avg$x1, avg$prop, col = "orange", lwd = 2)

prop_model <- gam(prop ~ s(x1), data = gam_data3, weights = total, 
                  family = "binomial", method = "REML")
prop_summary <- summary(prop_model)

prop_summary$p.table


prop_summary$s.table

prop_summary$p.table

prop_summary$s.table

plot(prop_model)

par(mar = c(4,4,0,0))
plot(prop_model)

library(itsadug)
par(mfrow=c(1,2), mar = c(4,4,0,0))
plot(prop_model, select = 1, scale = 0, shade=TRUE)
abline(h=0)

out <- plot_smooth(prop_model, view = "x1",main = "", print.summary = F)
diff <- find_difference(out$fv$fit, out$fv$CI, xVals=out$fv$x1)
addInterval(0, lowVals = diff$start, highVals = diff$end, col = 'red', lwd = 2)
abline(v=c(diff$start, diff$end), lty = 3, col = 'red')
text(mean(c(diff$start, diff$end)), 2.1, "successes > failures", col = 'red', font = 2)

par(mar=c(3.8,4,0,0))
plot_smooth(prop_model, view = "x1", main = "",
            transform = plogis, ylim = c(0,1), print.summary = F)
abline(h = 0.5, v = diff$start, col = 'red', lty = 2)
