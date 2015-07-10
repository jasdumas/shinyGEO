###################################################################################
### plots km curves using 
###  - levels of 'x' if 'x' is a factor (characters are coerced 
###		to factors), or
###  - high and low values of 'x' if 'x' is numeric, where 'high' and 'low'
###       values are relative to the median
###
###  - title: optional title, set to NULL to remove any title
###  - legend.loc = legend location
###  - no.plot: if TRUE, returns results without the plot
###  - subset: vector corresponding of subset of samples to analyze
###  - col: vector of colors for each group
# Note: function requires survival library
###################################################################################

plot.shiny.km <- function(time, death, x, title = "", legend.loc = "bottomleft", 
                          no.plot = FALSE, 
                          subset = rep(TRUE, length(time)), 
                          col = NULL,  ...) {
  
  
  ## filter out missing data ##
  subset = subset & !is.na(time) & !is.na(death) & !is.na(x) & !is.nan(x)
  x = x[subset]; time = time[subset]; death = death[subset]
  if (length(x) ==0 | length(time) == 0 | length(death) == 0) {
    cat("no samples, returning...\n")
    return(data.frame(hr = NA, p.km = NA))
  }
  
  ## make sure x has at least 2 levels if categorical ##  
  if (is.character(x)) x = as.factor(x)
  if (is.factor(x)) {
    n = length(levels(x))
    if (n <2) {
      cat("x must have at least 2 levels, returning...\n")
      return(data.frame(hr = NA, p.km = NA))
    }
  } 
  
  ## use 'km' below for continuous x when x is numeric
  #  km = survfit(Surv(time, death) ~x)  
  
  ## use median to split into high and low groups ## 
  if (is.numeric(x)) {
    newX = x
    cut = median(x,na.rm=TRUE)
    newX[x > cut] = "high"
    newX[x <= cut] = "low"
    x = as.factor(newX)
  }
  
  n = length(levels(x))
  km.group = coxph(Surv(time, death) ~ x)
  p.km = 1 - pchisq(km.group$score,n-1)
  hr = exp(km.group$coefficients)
  n = km.group$n
  
  if(no.plot) {
    return(data.frame(hr = hr, p.km = p.km))
  }
  
  ## plot graph ##
  km.group1 = survfit(Surv(time, death) ~ x)
  km.group = ggsurv(km.group1) # ggplot2
  
  if (is.null(col)) col = 1:n
  plot(km.group, col = col, lty = 1:n, ...)
  legend(legend.loc, legend = levels(x), col = col, lty = 1:n)
  
  if (!is.null(title)) {
    hr.str = "" 
    p.str = paste("P = ", round(p.km,4), sep = "")
    if (length(hr) == 1) {
      hr.str = paste("HR = ", round(hr,2), ", ")
    }
    if (title!="") title = paste(title, "\n", sep = "")
    title = paste(title, hr.str, p.str)
    title(title)
  }
}

