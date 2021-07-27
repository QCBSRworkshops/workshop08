## data(nottem)
## n_years <- length(nottem)/12
## nottem_month <- rep(1:12, times=n_years)
## nottem_year <- rep(1920:(1920+n_years-1),each=12)
## nottem_plot <- qplot(nottem_month,nottem,
##                     colour=factor(nottem_year),
##                     geom="line") + theme_bw()
## print(nottem_plot)

## year_gam <- gam(nottem~s(nottem_year)+s(nottem_month, bs="cc"))
## summary(year_gam)$s.table
## plot(year_gam,page=1, scale=0)

## pred<-predict(year_gam, type = "terms", se = TRUE)
## I<-order(nottem_year)
## plusCI<-I(pred$fit[,1] + 1.96*pred$se[,1])
## minusCI<-I(pred$fit[,1] - 1.96*pred$se[,1])
## xx <- c(nottem_year[I],rev(nottem_year[I]))
## yy <- c(plusCI[I],rev(minusCI[I]))
## plot(xx,yy,type="n",cex.axis=1.2)
## polygon(xx,yy,col="light grey",border="light grey")
## lines(nottem_year[I], pred$fit[,1][I],lty=1)
## abline(h=0)
