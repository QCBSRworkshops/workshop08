## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = TRUE,
  fig.width = 6, fig.height = 6,
  fig.retina = 3,
  fig.align = 'center'
)
options(repos=structure(c(CRAN="http://cran.r-project.org")))


## ----install_pkgs, echo = FALSE, results = "asis"-----------------------------
cat(
  qcbsRworkshops::first_slides(8, c('ggplot2', 'itsadug', 'mgcv'),
    lang = "fr")
)


## ---- eval = FALSE, echo = TRUE-----------------------------------------------
## library(ggplot2)
## set.seed(10)
## n <- 250
## x <- runif(n,0,5)
## y_model <- 3*x/(1+2*x)
## y_obs <- rnorm(n,y_model,0.1)
## data_plot <- qplot(x, y_obs) +
##   geom_line(aes(y=y_model)) +
##   theme_bw()
## data_plot


## ---- eval = TRUE, echo = FALSE-----------------------------------------------
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


## ---- echo = FALSE------------------------------------------------------------
library(mgcv)
linear_model <- gam(y_obs ~ x)
model_summary <- summary(linear_model)
data_plot <- data_plot +
             geom_line(colour = "red", size = 1.2, aes(y = fitted(linear_model)))
data_plot


## ---- eval = FALSE------------------------------------------------------------
## library(mgcv)
## gam_model <- gam(y_obs ~ s(x))
## summary(gam_model)
## 
## data_plot <- data_plot +
##      geom_line(colour = "blue", size = 1.2, aes(y = fitted(gam_model)))
## data_plot


## ---- echo = FALSE------------------------------------------------------------
gam_model <- gam(y_obs ~ s(x))
summary(gam_model)


## ---- echo = FALSE------------------------------------------------------------
data_plot <- data_plot +
     geom_line(colour = "blue", size = 1.2, aes(y = fitted(gam_model)))
data_plot


## ---- fig.width=5.5, fig.height=5.5-------------------------------------------
plot(gam_model)


## -----------------------------------------------------------------------------
linear_model <- gam(y_obs ~ x) # ajuster un modèle linéaire régulier avec gam()
nested_gam_model <- gam(y_obs ~ s(x) + x)
anova(linear_model, nested_gam_model, test = "Chisq")


## -----------------------------------------------------------------------------
linear_model <- gam(y_obs ~ x) # ajuster un modèle linéaire régulier avec gam()
nested_gam_model <- gam(y_obs ~ s(x) + x)
anova(linear_model, nested_gam_model, test = "Chisq")


## -----------------------------------------------------------------------------
n <- 250
x_test <- runif(n, -5, 5)
y_test_fit <- 4 * dnorm(x_test)
y_test_obs <- rnorm(n, y_test_fit, 0.2)


## -----------------------------------------------------------------------------
linear_model_test <- gam(y_test_obs ~ x_test)
nested_gam_model_test <- gam(y_test_obs ~ s(x_test) + x_test)

anova(linear_model_test, nested_gam_model_test, test="Chisq")


## -----------------------------------------------------------------------------
qplot(x_test, y_test_obs) +
  geom_line(aes(y = y_test_fit)) +
  theme_bw()


## -----------------------------------------------------------------------------
nested_gam_model_test


## -----------------------------------------------------------------------------
# ?gamSim
gam_data <-  gamSim(eg = 5)
head(gam_data)


## -----------------------------------------------------------------------------
basic_model <- gam(y ~ x0 + s(x1), data = gam_data)
basic_summary <- summary(basic_model)
basic_summary$p.table

basic_summary$s.table


## -----------------------------------------------------------------------------
basic_summary$s.table


## -----------------------------------------------------------------------------
plot(basic_model)


## -----------------------------------------------------------------------------
two_term_model <- gam(y ~ x0 + s(x1) + x2, data = gam_data)
two_term_summary <- summary(two_term_model)
two_term_summary$p.table

two_term_summary$s.table


## ---- fig.width=5.5, fig.height=5.5-------------------------------------------
plot(two_term_model)


## -----------------------------------------------------------------------------
two_smooth_model <- gam(y ~ x0 + s(x1) + s(x2), data = gam_data)
two_smooth_summary <- summary(two_smooth_model)
two_smooth_summary$p.table

two_smooth_summary$s.table


## ---- fig.width=10------------------------------------------------------------
plot(two_smooth_model, page = 1)


## -----------------------------------------------------------------------------
anova(basic_model, two_term_model, two_smooth_model, test = "Chisq")


## -----------------------------------------------------------------------------
three_term_model <- gam(y ~ x0 + s(x1) + s(x2) + x3, data = gam_data)
three_smooth_model <- gam(y~x0 + s(x1) + s(x2) + s(x3), data = gam_data)
three_smooth_summary <- summary(three_smooth_model)


## ---- eval = FALSE------------------------------------------------------------
## plot(three_smooth_model, page = 1)


## ---- fig.width=8, fig.height=7, echo = FALSE---------------------------------
par(mar=c(3.8,3.8,.2,.2))
plot(three_smooth_model, page = 1)


## -----------------------------------------------------------------------------
three_smooth_summary$s.table

# edf = 1 donc le terme est linéaire.

anova(two_smooth_model, three_term_model, test = "Chisq")

# terme x3 non significatif, il doit être enlevé!


## -----------------------------------------------------------------------------
factor_interact <- gam(y ~ x0 + s(x1) + s(x2, by = x0), data = gam_data)

summary(factor_interact)$s.table


## ---- fig.width=8-------------------------------------------------------------
plot(factor_interact, page = 1)


## ----fig.width=5.5, fig.height=5.5--------------------------------------------
vis.gam(factor_interact, view = c("x2","x0"), theta = 40, n.grid = 500, border = NA)


## -----------------------------------------------------------------------------
anova(two_smooth_model, factor_interact, test = "Chisq")


## -----------------------------------------------------------------------------
smooth_interact <- gam(y~x0 + s(x1, x2), data = gam_data)
summary(smooth_interact)$s.table


## ---- fig.width=7, fig.height=7-----------------------------------------------
plot(smooth_interact, page = 1, scheme = 3)


## ---- fig.width=8, fig.height=8-----------------------------------------------
vis.gam(smooth_interact, view = c("x1", "x2"), theta=40, n.grid = 500, border = NA)
# similaire à plot(smooth_interact, page = 1, scheme = 1)


## -----------------------------------------------------------------------------
anova(two_smooth_model, smooth_interact, test = "Chisq")


## ---- eval = FALSE------------------------------------------------------------
## data(nottem) # Nottingham temperature time series
## n_years <- length(nottem)/12
## nottem_month <- rep(1:12, times = n_years)
## nottem_year <- rep(1920:(1920 + n_years - 1), each = 12)
## qplot(nottem_month, nottem, colour = factor(nottem_year), geom = "line") +
##   theme_bw()


## ---- echo = F, fig.height=8, fig.width=12------------------------------------
data(nottem)
n_years <- length(nottem)/12
nottem_month <- rep(1:12, times = n_years)
nottem_year <- rep(1920:(1920 + n_years - 1), each = 12)
qplot(nottem_month, nottem, colour = factor(nottem_year), geom = "line") +
  theme_bw()


## ---- fig.height=4, fig.width=8-----------------------------------------------
year_gam <- gam(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"))
summary(year_gam)$s.table


## ---- fig.height=5, fig.width=8-----------------------------------------------
plot(year_gam, page = 1, scale = 0)


## -----------------------------------------------------------------------------
gam_data3 <- read.csv("data/other_dist.csv")
str(gam_data3)


## ---- fig.height=4, fig.width=4-----------------------------------------------
plot(range(gam_data3$x1), c(0,1), type = "n",
     main = "Probabilités de succès dans le temps",
     ylab = "Probabilité", xlab = "x1 (temps)")
abline(h = 0.5)

avg <- aggregate(prop ~ x1, data=gam_data3, mean)
lines(avg$x1, avg$prop, col = "orange", lwd = 2)


## ---- warning=F---------------------------------------------------------------
prop_model <- gam(prop ~ s(x1), data = gam_data3, weights = total, family = "binomial")
prop_summary <- summary(prop_model)
prop_summary$p.table
prop_summary$s.table


## ---- echo = FALSE------------------------------------------------------------
prop_summary$p.table


## ---- echo = FALSE------------------------------------------------------------
prop_summary$s.table


## ---- fig.height=3.5, fig.width=4, echo = FALSE-------------------------------
par(mar = c(4,4,0,0))
plot(prop_model)


## ---- echo = FALSE, fig.height = 4.5, fig.width = 9---------------------------
library(itsadug)
par(mfrow=c(1,2), mar = c(4,4,0,0))
plot(prop_model, select=1, scale=0, shade=TRUE)
abline(h=0)

out <- plot_smooth(prop_model, view="x1",main="", print.summary=F)
diff <- find_difference(out$fv$fit, out$fv$CI, xVals=out$fv$x1)
addInterval(0, lowVals = diff$start, highVals = diff$end, col='red', lwd=2)
abline(v=c(diff$start, diff$end), lty=3, col='red')
text(mean(c(diff$start, diff$end)), 2.1, "succès > échecs", col = 'red', font = 2)


## ---- echo = -1, fig.height=3.5, fig.width==4---------------------------------
par(mar=c(3.8,4,0,0))
plot_smooth(prop_model, view = "x1", main = "",
            transform = plogis, ylim = c(0,1), print.summary = F)
abline(h = 0.5, v = diff$start, col = 'red', lty = 2)


## ---- eval = F, fig.width=9, fig.height=4.5-----------------------------------
## par(mfrow = c(1,2))
## acf(resid(year_gam), lag.max = 36, main = "ACF")
## pacf(resid(year_gam), lag.max = 36, main = "pACF")


## ---- echo = F, fig.width=8.5, fig.height=4-----------------------------------
par(mfrow = c(1,2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")


## -----------------------------------------------------------------------------

year_gam <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"))
year_gam_AR1 <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 1),
                   data = data.frame(nottem, nottem_year, nottem_month))
year_gam_AR2 <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~ 1|nottem_year, p = 2),
                   data = data.frame(nottem, nottem_year, nottem_month))
anova(year_gam$lme, year_gam_AR1$lme, year_gam_AR2$lme)


## -----------------------------------------------------------------------------
gam_data2 <- gamSim(eg = 6)
str(gam_data2)


## ---- fig.height=4.5, fig.width=5.5, echo = -1--------------------------------
par(mar=c(4,4,1,1))
gamm_intercept <- gam(y ~ s(x0) + s(fac, bs = "re"), data = gam_data2)
summary(gamm_intercept)$s.table
plot(gamm_intercept, select = 2)


## ---- eval = FALSE------------------------------------------------------------
## par(mfrow = c(1,2), cex = 1.1)
## 
## plot_smooth(gamm_intercept, view = "x0", rm.ranef = T,
##             main = "intercept + s(x1)")
## 
## plot_smooth(gamm_intercept, view = "x0", cond = list(fac="1"),
##             main = "... + s(fac)", col = 'orange', ylim = c(8,21))
## 
## plot_smooth(gamm_intercept, view = "x0", cond = list(fac = "2"), add = T, col = 'red')
## 
## plot_smooth(gamm_intercept, view="x0", cond = list(fac = "3"), add = T, col = 'purple')
## 
## plot_smooth(gamm_intercept, view="x0", cond = list(fac = "4"), add = T, col = 'turquoise')


## ---- echo = F, fig.width=12, fig.height=6------------------------------------
par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_intercept, view = "x0", rm.ranef = T,
            main = "intercept + s(x1)", print.summary=F)
plot_smooth(gamm_intercept, view = "x0", cond = list(fac="1"),
            main = "... + s(fac)", col = 'orange', ylim = c(8,21), print.summary=F)
plot_smooth(gamm_intercept, view = "x0", cond = list(fac = "2"), add = T, col = 'red', print.summary=F)
plot_smooth(gamm_intercept, view="x0", cond = list(fac = "3"), add = T, col = 'purple', print.summary=F)
plot_smooth(gamm_intercept, view="x0", cond = list(fac = "4"), add = T, col = 'turquoise', print.summary=F)


## -----------------------------------------------------------------------------
gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs = "re"), data = gam_data2)

summary(gamm_slope)$s.table


## ---- eval = FALSE------------------------------------------------------------
## par(mfrow = c(1,2), cex = 1.1)
## 
## plot_smooth(gamm_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)")
## 
## plot_smooth(gamm_slope, view = "x0", cond = list(fac = "1"),
##             main = "... + s(fac)", col = 'orange', ylim = c(7,22))
## 
## plot_smooth(gamm_slope, view = "x0", cond = list(fac = "2"), add = T, col = 'red')
## 
## plot_smooth(gamm_slope, view = "x0", cond = list(fac = "3"), add = T, col = 'purple')
## 
## plot_smooth(gamm_slope, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise')


## ---- echo = F, fig.width=12, fig.height=6------------------------------------
par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)",
            print.summary=F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "1"),
            main = "... + s(fac)", col = 'orange', ylim = c(7,22), print.summary=F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "2"), add = T, col = 'red',
            print.summary=F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "3"), add = T, col = 'purple',
            print.summary=F)
plot_smooth(gamm_slope, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise',
            print.summary=F)


## -----------------------------------------------------------------------------
gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs = "re") + s(fac, x0, bs = "re"),
                      data = gam_data2)

summary(gamm_int_slope)$s.table


## ---- eval = FALSE------------------------------------------------------------
## par(mfrow = c(1,2), cex = 1.1)
## 
## plot_smooth(gamm_int_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)")
## 
## plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "1"),
##             main="... + s(fac) + s(fac, x0)", col = 'orange', ylim = c(7,22))
## 
## plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "2"), add = T, col='red')
## 
## plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "3"), add = T, col = 'purple')
## 
## plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise')


## ---- echo = F, fig.width=12, fig.height=6------------------------------------
par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_int_slope, view = "x0", rm.ranef = T, main = "intercept + s(x0)", print.summary=F)
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "1"), print.summary=F,
            main="... + s(fac) + s(fac, x0)", col = 'orange', ylim = c(7,22))
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "2"), add = T,
            col='red',print.summary=F)
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "3"), add = T,
            col = 'purple', print.summary=F)
plot_smooth(gamm_int_slope, view = "x0", cond = list(fac = "4"), add = T,
            col = 'turquoise', print.summary=F)


## ---- fig.width=5, fig.height=5-----------------------------------------------
plot(gamm_int_slope, select = 3)


## -----------------------------------------------------------------------------
gamm_smooth <- gam(y ~ s(x0, fac, bs = "fs", m = 1), data = gam_data2)

summary(gamm_smooth)$s.table


## ---- echo = -1, fig.height=5, fig.width=5------------------------------------
par(mar=c(4,4,.5,.5), lwd = 2)
plot(gamm_smooth, select = 1)


## ---- eval = FALSE------------------------------------------------------------
## par(mfrow = c(1,2), cex = 1.1)
## 
## plot_smooth(gamm_smooth, view = "x0", rm.ranef = T, main = "intercept + s(x0)")
## 
## plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "1"),
##             main="... + s(x0, fac)", col = 'orange', ylim = c(7,22))
## 
## plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "2"), add = T, col='red')
## 
## plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "3"), add = T, col = 'purple')
## 
## plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "4"), add = T, col = 'turquoise')


## ---- echo = F, fig.width=12, fig.height=6------------------------------------
par(mfrow = c(1,2), cex = 1.1, mar=c(4,4,1,1))
plot_smooth(gamm_smooth, view = "x0", rm.ranef = T, main = "intercept + s(x0)", print.summary=F)
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "1"), print.summary=F,
            main="... + s(x0, fac)", col = 'orange', ylim = c(7,22))
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "2"), add = T,
            col='red',print.summary=F)
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "3"), add = T,
            col = 'purple', print.summary=F)
plot_smooth(gamm_smooth, view = "x0", cond = list(fac = "4"), add = T,
            col = 'turquoise', print.summary=F)


## -----------------------------------------------------------------------------
anova(gamm_intercept, gamm_slope, gamm_int_slope, gamm_smooth, test = "Chisq")

