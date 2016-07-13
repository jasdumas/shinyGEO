###################################################################################
# plots km curves using levels of 'x' if x is a factor or character, or 'high'
# 	and 'low' values (relative to the median) of 'x' if 'x' is numeric 
###################################################################################
plot.shiny.km <- function(time, death, x, title = "", ids = NULL, 
                          subset = rep(TRUE, length(time)), 
                          col = NULL,  xlab = NULL, ylab = NULL, hr.inverse = FALSE, 
			  no.plot = FALSE, ...) {

  ## filter out missing data ##
  subset = subset & !is.na(time) & !is.na(death) & !is.na(x) & !is.nan(x)
  x = x[subset]; time = time[subset]; death = death[subset]; 
  if (!is.null(ids)) ids = ids[subset]
  if (length(x) ==0 | length(time) == 0 | length(death) == 0 
                    | length(unique(death)) < 2) {
    return(NULL)
  }

  if (is.null(ids)) ids = 1:length(x)
  km.data = data.frame(ID = ids, X = x, Group = NA, time = time, event = death)
 
  ## use median to split into high and low groups ## 
  newX = x
  cut = median(x,na.rm=TRUE)
  newX[x > cut] = "upper 50%"
  newX[x <= cut] = "lower 50%"
  x = as.factor(newX)

  km.data$Group = x

  if (no.plot) return(km.data)

  n = length(levels(x))
  expression = x  
  km.group = coxph(Surv(time, death) ~ expression)
  p.km = 1 - pchisq(km.group$score,n-1)
  hr = exp(km.group$coefficients)
  n = km.group$n
 
  if (hr.inverse) hr = 1/hr 
 
  hr.str = paste("HR = ", round(hr,2), ", ")
  p.str = paste("P = ", round(p.km,4), sep = "")
 
  if (title=="") { 
	  title = paste(hr.str, p.str, sep = "")
  } else {
	  title = paste0(title, "\n",hr.str, p.str)
  }
 
  if (is.null(xlab)) xlab = "Time"
  if (is.null(ylab)) ylab = "Survival"
  
  ## plot graph ### ggplot2/GGally form
  km.group1 = survfit(Surv(time, death) ~ expression)
  km.group = ggsurv(km.group1, 
                    main = title, 
                    surv.col = col, cens.col = col, xlab = xlab, ylab = ylab,...) +
		    ggplot2::coord_cartesian(ylim = c(0, 1))
#    theme(plot.title = element_text(vjust = 0.5, colour = "black"))
  
  plot(km.group)
  return(km.data)
}
 
