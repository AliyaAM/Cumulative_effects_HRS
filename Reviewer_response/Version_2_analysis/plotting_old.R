#https://cran.r-project.org/web/packages/casebase/vignettes/plotsmoothHazard.html

library(casebase)
library(visreg)
library(splines)
library(ggplot2)
data("brcancer")
str(brcancer)


#time is stop_new 
#hormone is discrim_bin 
head(brcancer)

mod_cb <- fitSmoothHazard(cens ~ ns(log(time), df = 3) + hormon,
                          data = brcancer,
                          time = "time")

# plot exposed to and not exposed to hormon next to each other: 
plot(mod_cb,
     hazard.params = list(xvar = "time",
                          by = "hormon",
                          alpha = 0.05,
                          ylab = "Hazard"))


###### ALTERNATIVE: 

data("eprchd")
eprchd <- transform(eprchd, 
                    treatment = factor(treatment, levels = c("placebo","estPro")))
str(eprchd)
#> 'data.frame':    16608 obs. of  3 variables:
#>  $ time     : num  0.0833 0.0833 0.0833 0.0833 0.0833 ...
#>  $ status   : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ treatment: Factor w/ 2 levels "placebo","estPro": 1 1 1 1 1 1 1 1 1 1 ...

fit_mason <- fitSmoothHazard(status ~ treatment*time,
                             data = eprchd,
                             time = "time")
summary(fit_mason)