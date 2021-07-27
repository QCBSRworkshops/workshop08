## gam_data3 <- read.csv("other_dist.csv")
## summary(gam_data3)
## str(gam_data3)

## emptyPlot(range(gam_data3$x1), c(0,1), h=.5,
##           main="Probability of successes", ylab="Probability",xlab="x1")
## 
## avg <- aggregate(prop ~ x1, data=gam_data3, mean, na.rm=TRUE)
## lines(avg$x1, avg$prop, col="orange",lwd=2)

## prop_model <- gam(prop~ s(x1), data=gam_data3, weights=total, family="binomial")
## prop_summary <- summary(prop_model)
## print(prop_summary$p.table)
## print(prop_summary$s.table)
## 
## plot(prop_model)

## par(mfrow=c(1,2))
## plot(prop_model, select=1, scale=0, shade=TRUE)
## abline(h=0)
## 
## plot_smooth(prop_model, view="x1",main="")
## (diff <- find_difference(out$fv$fit, out$fv$CI, xVals=out$fv$x1))
## addInterval(0, lowVals=diff$start, highVals = diff$end, col='red', lwd=2)
## abline(v=c(diff$start, diff$end), lty=3, col='red')
## text(mean(c(diff$start, diff$end)), 2.1, "sign. more \n success", col='red', font=3)

## par(mfrow=c(1,1))
## plot_smooth(prop_model, view="x1", main="",
##             transform=plogis, ylim=c(0,1))
## abline(h=.5, v=diff$start, col='red', lty=2)
