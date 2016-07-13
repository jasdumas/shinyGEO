###################################################################################
# plots km curves for 'high' and 'low' values relative to median (default)
# or using an optimal cutoff if optimal.cut is TRUE 
###################################################################################
plot.shiny.km <- function(time, death, x, title = "", ids = NULL, 
                          subset = rep(TRUE, length(time)), 
                          col = NULL,  xlab = NULL, ylab = NULL, hr.inverse = FALSE, 
                          no.plot = FALSE, optimal.cut = FALSE, ...) {

  ## filter out missing data ##
  subset = subset & !is.na(time) & !is.na(death) & !is.na(x) & !is.nan(x)
  x = x[subset]; time = time[subset]; death = death[subset]; 
  if (!is.null(ids)) ids = ids[subset]
  if (length(x) ==0 | length(time) == 0 | length(death) == 0 
      | length(unique(death)) < 2) {
    return(invisible(NULL))
  }
  if (is.null(ids)) ids = 1:length(x)
  km.data = data.frame(ID = ids, X = x, Group = NA, time = time, event = death)

  ## settings for median cutoff ##
  cut = median(x)
  p.adj = NULL
  upper = "upper 50%"; lower = "lower 50%"

  ## find optimal cutoff if specified ##
  if (optimal.cut) {
     mod <- coxph(Surv(time, event) ~ X, data = km.data)
     cc = try(cutp(mod), silent = TRUE)
     if (class(cc) %in% "try-error") {
	return(invisible(NULL))
     }
     cut = cc$X$X[1]
     p.adj = cc$X$p[1]
     percentile = round(sum(x>=cut) / length(x) * 100,2)
     upper = paste0("upper ", percentile, "%")
     lower = paste0("lower ", 100-percentile, "%")
  }
    
  ## split into high and low groups using appropriate cutoff ## 
  newX = x
  newX[x >= cut] = upper
  newX[x < cut] = lower
  expression = as.factor(newX)
  km.data$Group = expression
  
  if (no.plot) return(invisible(km.data))

  n = length(levels(expression))
  km.group = try(coxph(Surv(time, death) ~ expression), silent = TRUE)
  if (class(km.group) %in% "try-error") return(invisible(NULL))
  p.km = 1 - pchisq(km.group$score,n-1)
  hr = exp(km.group$coefficients)
  n = km.group$n
  
  if (hr.inverse) hr = 1/hr 
  
  hr.str = paste("HR = ", round(hr,2), ", ")
  p.str = paste0("P = ", round(p.km,4))
  if (!is.null(p.adj)) {
    p.str = paste0(p.str, ", P(adjusted) = ", round(p.adj,4))
  }
  
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
  return(invisible(km.data))
}


