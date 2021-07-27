## gam_data = gamSim(eg=5)
## head(gam_data)

## basic_model = gam(y~x0+s(x1), data= gam_data)
## basic_summary = summary(basic_model)
## print(basic_summary$p.table)
## print(basic_summary$s.table)
## plot(basic_model)

## two_term_model <- gam(y~x0+s(x1)+x2, data=gam_data)
## two_term_summary <- summary(two_term_model)
## print(two_term_summary$p.table)
## print(two_term_summary$s.table)

## two_smooth_model <- gam(y~x0+s(x1)+s(x2), data=gam_data)
## two_smooth_summary <- summary(two_smooth_model)
## print(two_smooth_summary$p.table)
## print(two_smooth_summary$s.table)
## plot(two_smooth_model,page=1)

## anova(basic_model,two_term_model,two_smooth_model, test="Chisq")

## three_term_model <- gam(y~x0+s(x1)+s(x2)+x3, data=gam_data)
## three_smooth_model <- gam(y~x0+s(x1)+s(x2)+s(x3), data=gam_data)
## three_smooth_summary <- summary(three_smooth_model)
## 
## print(three_smooth_summary$p.table)
## print(three_smooth_summary$s.table)
## plot(three_smooth_model,page=1)
## # edf = 1 therefore term is linear.
## 
## anova(two_smooth_model,three_term_model,test="Chisq")
## # term x3 is not significant
