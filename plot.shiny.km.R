###################################################################################
# plots km curves using levels of 'x' if x is a factor or character, or 'high'
# 	and 'low' values (relative to the median) of 'x' if 'x' is numeric 
###################################################################################
plot.shiny.km <- function(time, death, x, title = "", 
                          subset = rep(TRUE, length(time)), 
                          col = NULL,  ...) {
  
  ## filter out missing data ##
  subset = subset & !is.na(time) & !is.na(death) & !is.na(x) & !is.nan(x)
  x = x[subset]; time = time[subset]; death = death[subset]
  if (length(x) ==0 | length(time) == 0 | length(death) == 0) {
    cat("no samples, returning...\n")
    return(data.frame(hr = NA, p.km = NA))
  }
  
  ## use median to split into high and low groups ## 
  newX = x
  cut = median(x,na.rm=TRUE)
  newX[x > cut] = "high"
  newX[x <= cut] = "low"
  x = as.factor(newX)
  
  n = length(levels(x))
  km.group = coxph(Surv(time, death) ~ x)
  p.km = 1 - pchisq(km.group$score,n-1)
  hr = exp(km.group$coefficients)
  n = km.group$n
  
  hr.str = paste("HR = ", round(hr,2), ", ")
  p.str = paste("P = ", round(p.km,4), sep = "")
 
  if (title=="") { 
	  title = paste(hr.str, p.str, sep = "")
  } else {
	  title = paste0(title, "\n",hr.str, p.str)
  }
  
  ## plot graph ### ggplot2/GGally form
  km.group1 = survfit(Surv(time, death) ~ x)
  km.group = ggsurv(km.group1, 
                    main = title, xlab = "Time", ylab = "Survival",
                    surv.col = col, cens.col = "black") +
    theme(plot.title = element_text(vjust = 0.5, colour = "black"))
  
  plot(km.group, ...)
}
 
