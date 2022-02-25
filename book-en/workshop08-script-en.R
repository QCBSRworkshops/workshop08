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

# Challenge 1 ----
# 
# 1. Fit a linear and smoothed GAM model to the relation between `SampleDepth` and `Sources`.
# 2. Determine if linearity is justified for this data.
# 3. How many effective degrees of freedom does the smoothed term have?

# SOLUTION # -----

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


##Section: 03-how-gams-work.R 




##Section: 04-smooth-terms.R 

head(isit)
isit$Season <- as.factor(isit$Season)

basic_model <- gam(Sources ~ Season + s(SampleDepth), data = isit, method = "REML")
basic_summary <- summary(basic_model)

basic_summary$p.table

basic_summary$s.table

par(mfrow=c(1,2))
plot(basic_model, all.terms = TRUE)

two_term_model <- gam(Sources ~ Season + s(SampleDepth) + RelativeDepth, 
                      data = isit, method = "REML")
two_term_summary <- summary(two_term_model)

two_term_summary$p.table

two_term_summary$s.table

par(mfrow=c(2,2))
plot(two_term_model, all.terms = TRUE)

two_smooth_model <- gam(Sources ~ Season + s(SampleDepth) + s(RelativeDepth), 
                        data = isit, method = "REML")
two_smooth_summary <- summary(two_smooth_model)

two_smooth_summary$p.table

two_smooth_summary$s.table

par(mfrow=c(2,2))
plot(two_smooth_model, page = 1, all.terms = TRUE)

AIC(basic_model, two_term_model, two_smooth_model)

# Challenge 2 ----
# 
# For our second challenge, we will be building onto our model by adding variables which we think might be ecologically significant predictors to explain bioluminescence. 
# 
#
# 1. Create two new models: Add `Latitude` to `two_smooth_model`, first as a linear term, then as a smoothed term.
# 2. Is `Latitude` an important term to include? Does `Latitude` have a linear or additive effect? Use plots, coefficient tables, and the `AIC()` function to help you answer this question.

# SOLUTION # -----

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

par(mfrow=c(2,2))
plot(three_term_model, all.terms = TRUE)

par(mfrow=c(2,2))
plot(three_smooth_model, all.terms = TRUE)

three_smooth_summary$s.table

AIC(three_smooth_model, three_term_model)

AIC(two_smooth_model, three_smooth_model)


##Section: 05-interactions.R 

factor_interact <- gam(Sources ~ Season + 
                         s(SampleDepth, by=Season) + 
                         s(RelativeDepth), 
                       data = isit, method = "REML")

summary(factor_interact)$s.table

par(mfrow = c(2,2))
plot(factor_interact)

vis.gam(factor_interact, theta = 120, n.grid = 50, lwd = .4)

AIC(two_smooth_model, factor_interact)

smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth), 
                       data = isit, method = "REML")
summary(smooth_interact)$s.table

plot(smooth_interact, page = 1, scheme = 2)

vis.gam(smooth_interact, 
        view = c("SampleDepth", "RelativeDepth"), 
        theta = 50, n.grid = 50, lwd = .4)

AIC(two_smooth_model, smooth_interact)


##Section: 06-generalization.R 

smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth), 
                       data = isit, method = "REML")

summary(smooth_interact)$p.table

summary(smooth_interact)$s.table

par(mfrow=c(1,3))
install.packages("patchwork", quiet = TRUE)
library(patchwork)

k_plot <- function(k_value){
    data("eeg")
    m <- mgcv::gam(Ampl ~ s(Time, k = k_value), data = eeg)
     p <- ggplot(eeg, aes(x = Time, y = Ampl)) +
     geom_point(alpha = .1, size = 1) + 
     geom_line(aes(y = predict(m)),
               lwd = 2, col = "black") +
         labs(title = paste("k =", k_value), x = "", y = "") +
         theme_classic() + 
         theme(text = element_text(size = 15), 
               axis.text = element_blank(),
               plot.title = element_text(face = "bold", hjust = 0.5))  
    return(p)
}

k_plot(3) + k_plot(6) + k_plot(10)

k.check(smooth_interact)

smooth_interact_k60 <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                           data = isit, method = "REML")

k.check(smooth_interact_k60)

smooth_interact <- smooth_interact_k60

par(mfrow = c(2,2))
gam.check(smooth_interact)

?family.mgcv

# Challenge 3 ----
# 
# 1. Fit a new model `smooth_interact_tw` with the same formula as the `smooth_interact` model, but with a distribution from the *Tweedie* family (instead of the Normal distribution) and `log` link function. You can do so by using `family = tw(link = "log")` inside `gam()`.
# 2. Check the choice of `k` and the residual plots for the new model.
# 3. Compare `smooth_interact_tw` with `smooth_interact`. Which one would you choose?

# SOLUTION # -----

# Hint!
# Because the Normal distribution is the default setting, 
# we have not specified the distribution in this workshop yet.

# Here is how we would write the model to specify the Normal distribution:

smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                       family = gaussian(link = "identity"), 
                       data = isit, method = "REML")

smooth_interact_tw <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth, k = 60), 
                          family = tw(link = "log"),
                          data = isit, method = "REML")
summary(smooth_interact_tw)$p.table
summary(smooth_interact_tw)$s.table

k.check(smooth_interact_tw)

par(mfrow = c(2,2))
gam.check(smooth_interact_tw)

AIC(smooth_interact, smooth_interact_tw)


##Section: 07-changing-basis.R 

# Nottingham temperature time series
data(nottem)

# the number of years of data (20 years)
n_years <- length(nottem)/12

# categorical variable coding for the 12 months of the year, for every
# year sampled (so, a sequence 1 to 12 repeated for 20 years).
nottem_month <- rep(1:12, times = n_years)

# the year corresponding to each month in nottem_month
nottem_year <- rep(1920:(1920 + n_years - 1), each = 12)

# Plot the time series
qplot(x = nottem_month, y = nottem, 
      colour = factor(nottem_year), 
      geom = "line") +
  theme_bw()

year_gam <- gam(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"), method = "REML")
summary(year_gam)$s.table

plot(year_gam, page = 1, scale = 0)


##Section: 08-GAMMs.R 

par(mfrow = c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")

df <- data.frame(nottem, nottem_year, nottem_month)

year_gam <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"), data = df)

year_gam_AR1 <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 1),
                     data = df)

year_gam_AR2 <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 2),
                     data = df)

AIC(year_gam$lme, year_gam_AR1$lme, year_gam_AR2$lme)

# generate and view data
gam_data2 <- gamSim(eg = 6)
head(gam_data2)

# run random intercept model
gamm_intercept <- gam(y ~ s(x0) + s(fac, bs = "re"), data = gam_data2, method = "REML")

# examine model output
summary(gamm_intercept)$s.table

plot(gamm_intercept, select = 2) 
# select = 2 because the random effect appears as the second entry in the summary table.

par(mfrow = c(1,2), cex = 1.1)

# Plot the summed effect of x0 (without random effects)
plot_smooth(gamm_intercept, view = "x0", rm.ranef = TRUE, 
            main = "intercept + s(x1)")

# Plot each level of the random effect
plot_smooth(gamm_intercept, view = "x0", rm.ranef = FALSE,
            cond = list(fac="1"),
            main = "... + s(fac)", col = 'orange', ylim = c(0,25))
plot_smooth(gamm_intercept, view = "x0", rm.ranef = FALSE,
            cond = list(fac = "2"), 
            add = TRUE, col = 'red')
plot_smooth(gamm_intercept, view="x0", rm.ranef = FALSE,
            cond = list(fac = "3"), 
            add = TRUE, col = 'purple')
plot_smooth(gamm_intercept, view="x0", rm.ranef = FALSE,
            cond = list(fac = "4"), 
            add = TRUE, col = 'turquoise')

gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs = "re"), data = gam_data2, method = "REML")

summary(gamm_slope)$s.table

par(mfrow = c(1,2), cex = 1.1)

# Plot the summed effect of x0 (without random effects)
plot_smooth(gamm_slope, view = "x0", rm.ranef = TRUE, 
            main = "intercept + s(x1)")

# Plot each level of the random effect
plot_smooth(gamm_slope, view = "x0", rm.ranef = FALSE,
            cond = list(fac="1"),
            main = "... + s(fac, x0)", col = 'orange', ylim = c(0,25))
plot_smooth(gamm_slope, view = "x0", rm.ranef = FALSE,
            cond = list(fac = "2"), 
            add = TRUE, col = 'red')
plot_smooth(gamm_slope, view="x0", rm.ranef = FALSE,
            cond = list(fac = "3"), 
            add = TRUE, col = 'purple')
plot_smooth(gamm_slope, view="x0", rm.ranef = FALSE,
            cond = list(fac = "4"), 
            add = TRUE, col = 'turquoise')

gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs = "re") + s(fac, x0, bs = "re"),
                      data = gam_data2, method = "REML")

summary(gamm_int_slope)$s.table

par(mfrow = c(1,2), cex = 1.1)

# Plot the summed effect of x0 (without random effects)
plot_smooth(gamm_int_slope, view = "x0", rm.ranef = TRUE, 
            main = "intercept + s(x1)")

# Plot each level of the random effect
plot_smooth(gamm_int_slope, view = "x0", rm.ranef = FALSE,
            cond = list(fac="1"),
            main = "... + s(fac) + s(fac, x0)", col = 'orange', ylim = c(0,25))
plot_smooth(gamm_int_slope, view = "x0", rm.ranef = FALSE,
            cond = list(fac = "2"), 
            add = TRUE, col = 'red')
plot_smooth(gamm_int_slope, view="x0", rm.ranef = FALSE,
            cond = list(fac = "3"), 
            add = TRUE, col = 'purple')
plot_smooth(gamm_int_slope, view="x0", rm.ranef = FALSE,
            cond = list(fac = "4"), 
            add = TRUE, col = 'turquoise')

plot(gamm_int_slope, select=3) 
# select = 3 because the random slope appears as the third entry in your summary table.

gamm_smooth <- gam(y ~ s(x0) + s(x0, fac, bs = "fs", m = 1), 
                   data = gam_data2, method = "REML")

summary(gamm_smooth)$s.table

plot(gamm_smooth, select=1)
# select = 1 because the smooth slope appears as the first entry in your summary table.

par(mfrow = c(1,2), cex = 1.1)

# Plot the summed effect of x0 (without random effects)
plot_smooth(gamm_smooth, view = "x0", rm.ranef = TRUE, 
            main = "intercept + s(x1)")

# Plot each level of the random effect
plot_smooth(gamm_smooth, view = "x0", rm.ranef = FALSE,
            cond = list(fac="1"),
            main = "... + s(fac) + s(fac, x0)", col = 'orange', ylim = c(0,25))
plot_smooth(gamm_smooth, view = "x0", rm.ranef = FALSE,
            cond = list(fac = "2"), 
            add = TRUE, col = 'red')
plot_smooth(gamm_smooth, view="x0", rm.ranef = FALSE,
            cond = list(fac = "3"), 
            add = TRUE, col = 'purple')
plot_smooth(gamm_smooth, view="x0", rm.ranef = FALSE,
            cond = list(fac = "4"), 
            add = TRUE, col = 'turquoise')

AIC(gamm_intercept, gamm_slope, gamm_int_slope, gamm_smooth)


##Section: 09-references.R 




