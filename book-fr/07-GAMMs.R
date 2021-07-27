## par(mfrow=c(1,2))
## acf(resid(year_gam), lag.max = 36, main = "ACF")
## pacf(resid(year_gam), lag.max = 36, main = "pACF")

## year_gam <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"))
## year_gam_AR1 <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"),
##                      correlation = corARMA(form = ~ 1|nottem_year, p = 1))
## year_gam_AR2 <- gamm(nottem~s(nottem_year)+s(nottem_month, bs="cc"),
##                      correlation = corARMA(form = ~ 1|nottem_year, p = 2))
## anova(year_gam$lme,year_gam_AR1$lme,year_gam_AR2$lme)

## # Générez des données
## gam_data2 <- gamSim(eg=6)
## head(gam_data2)
## 
## # Faites rouler un modèle avec intercepte aléatoire
## gamm_intercept <- gam(y ~ s(x0) + s(fac, bs="re"), data=gam_data2)
## summary(gamm_intercept)

## plot(gamm_intercept, select=2)
## # select=2 parce que le terme aléatoire se trouve sur la 2e ligne du tableau sommaire.

## par(mfrow=c(1,2), cex=1.1)
## plot_smooth(gamm_intercept, view="x0", rm.ranef=TRUE, main="intercept + s(x1)", rug=FALSE)
## plot_smooth(gamm_intercept, view="x0", cond=list(fac="1"),
##             main="... + s(fac)", col='orange', ylim=c(8,21), rug=FALSE)
## plot_smooth(gamm_intercept, view="x0", cond=list(fac="2"), add=TRUE, col='red')
## plot_smooth(gamm_intercept, view="x0", cond=list(fac="3"), add=TRUE, col='purple')
## plot_smooth(gamm_intercept, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise')

## gamm_slope <- gam(y ~ s(x0) + s(x0, fac, bs="re"), data=gam_data2)
## summary(gamm_slope)
## 
## plot_smooth(gamm_slope, view="x0", rm.ranef=TRUE, main="intercept + s(x0)", rug=FALSE)
## plot_smooth(gamm_slope, view="x0", cond=list(fac="1"),
##             main="... + s(fac)", col='orange',ylim=c(7,22), rug=FALSE)
## plot_smooth(gamm_slope, view="x0", cond=list(fac="2"), add=TRUE, col='red')
## plot_smooth(gamm_slope, view="x0", cond=list(fac="3"), add=TRUE, col='purple')
## plot_smooth(gamm_slope, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise')

## gamm_int_slope <- gam(y ~ s(x0) + s(fac, bs="re")
##                       + s(fac, x0, bs="re"), data=gam_data2)
## summary(gamm_int_slope)
## 
## plot_smooth(gamm_int_slope, view="x0", rm.ranef=TRUE, main="intercept + s(x0)", rug=FALSE)
## plot_smooth(gamm_int_slope, view="x0", cond=list(fac="1"),
##             main="... + s(fac) + s(fac, x0)", col='orange', ylim=c(7,22), rug=FALSE)
## plot_smooth(gamm_int_slope, view="x0", cond=list(fac="2"), add=TRUE, col='red', xpd=TRUE)
## plot_smooth(gamm_int_slope, view="x0", cond=list(fac="3"), add=TRUE, col='purple', xpd=TRUE)
## plot_smooth(gamm_int_slope, view="x0", cond=list(fac="4"), add=TRUE, col='turquoise', xpd=TRUE)

## plot(gamm_int_slope, select=3)
## # select=3 parce que la pente aléatoire se trouve sur la 3e ligne du tableau sommaire.

## gamm_smooth <- gam(y ~ s(x0) + s(x0, fac, bs="fs", m=1), data=gam_data2)
## summary(gamm_smooth)

## plot(gamm_smooth, select=1)
## # select=1 parce que le terme se trouve sur la 1e ligne du tableau sommaire.
