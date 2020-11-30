library(readxl)
library(sjmisc)


simulations <- read_excel("simulations.xlsx", 
                          col_types = c("numeric", "numeric", "numeric", 
                                        "numeric", "skip", "skip"))

#Auxiliary plot
plot_curve = function(model, name_x='', name_y=''){	
  y_term = split(paste(model$terms[[2]]), "+")[[1]]
  x_term = split(paste(model$terms[[3]]), "+")[[1]]
  
  y_term_fun = ""
  x_term_fun = ""
  
  if(length(y_term) > 1){
    if(y_term[1] == "log"){
      y_term_fun = "log"
    } 		
    y_term = y_term[2]
  }
  
  if(length(x_term) > 1){
    if(x_term[1] == "log"){
      x_term_fun = "log"
    }else if(x_term[1] == "poly"){
      if(str_contains(x_term[2], "log")){
        y_term_fun = "log"
        x_term[2] = gsub("log\\(|\\)","", x_term[2])
      }
    }
    x_term = x_term[2]
  }
  
  if(name_x == ''){
    name_x = x_term
  }
  if(name_y == ''){
    name_y = y_term
  }
  print(x_term)
  print(y_term)
  #h <- formula(paste( y_term_fun, "(", y_term, ") ~ ", x_term_fun, "(" ,x_term, ")"))
  h <- formula(paste( y_term, " ~ ", x_term))
  
  plot(h, xlab = name_x, ylab = name_y, col = "dodgerblue", pch = 20, cex =2)
  
  
  xplot = seq(min(get(x_term)), max(get(x_term)), length = length(get(x_term)) )
  newdata = data.frame(x = xplot)
  colnames(newdata) = c(x_term)	
  y_hat = predict(model, newdata=newdata)
  
  if(y_term_fun == "log"){
    y_hat = exp(predict(model, newdata=newdata))
  }
  lines(xplot, y_hat, col = "darkorange", lwd = 2, lty = 1)
}

attach(simulations)


corrplot(cor(simulations), method = "number")


#forcexEMG

#linear
linear_model_femg = lm(EMG_RMS ~ Mean_Force)
summary(linear_model_femg)
plot_curve(linear_model_femg, "Mean Force (N)", "EMG RMS (mV)")
#plotPredy(simulations, Mean_Force, EMG_RMS, linear_model_femg)
residualPlot(linear_model_femg)
shapiro_test(residuals(linear_model_femg)) #Normality
bptest(linear_model_femg) #Heteroskedasticity 

#quadratic
poly_2_model_femg = lm(EMG_RMS ~ poly(Mean_Force, 2, raw=TRUE))
summary(poly_2_model_femg)
plot_curve(poly_2_model_femg, "Mean Force (N)", "EMG RMS (mV)")
#plotPredy(simulations, Mean_Force, EMG_RMS, poly_2_model_femg)
residualPlot(poly_2_model_femg)
shapiro_test(residuals(poly_2_model_femg)) #Normality
bptest(poly_2_model_femg) #Heteroskedasticity 

#log-log
ln_ln_model_femg = lm( log(EMG_RMS) ~ log(Mean_Force))
summary(ln_ln_model_femg)
plot_curve(ln_ln_model_femg, "Mean Force (N)", "EMG RMS (mV)")
#plotPredy(data=simulations, x=Mean_Force, y=EMG_RMS, ln_ln_model_femg)
residualPlot((ln_ln_model_femg))
shapiro_test(residuals(ln_ln_model_femg)) #Normality
bptest(ln_ln_model_femg) #Heteroskedasticity 

#poly-log
poly_2_ln_ln_model_femg = lm( log(EMG_RMS) ~ poly(log(Mean_Force), 2, raw=TRUE))
summary(poly_2_ln_ln_model_femg)
plot_curve(poly_2_ln_ln_model_femg, "Mean Force (N)", "EMG RMS (mV)")
#plotPredy(simulations, Mean_Force, EMG_RMS, poly_2_ln_ln_model_femg)
residualPlot((poly_2_ln_ln_model_femg))
shapiro_test(residuals(poly_2_ln_ln_model_femg)) #Normality
bptest(poly_2_ln_ln_model_femg) #Heteroskedasticity 

#anova

anova(ln_ln_model_femg, poly_2_ln_ln_model_femg)


#forcexforceSD
#linear
linear_model_ffsd = lm(Force_SD ~ Mean_Force)
summary(linear_model_ffsd)
plot_curve(linear_model_ffsd, "Mean Force (N)", "Force SD (N)")
#plotPredy(simulations, Mean_Force, EMG_RMS, linear_model_ffsd)
residualPlot((linear_model_ffsd))
shapiro_test(residuals(linear_model_ffsd)) #Normality
bptest(linear_model_ffsd) #Heteroskedasticity 

#quadratic
poly_2_model_ffsd = lm(Force_SD ~ poly(Mean_Force, 2, raw=TRUE))
summary(poly_2_model_ffsd)
plot_curve(poly_2_model_ffsd, "Mean Force (N)", "Force SD (N)")
#plotPredy(simulations, Mean_Force, EMG_RMS, poly_2_model_ffsd)
residualPlot((poly_2_model_ffsd))
shapiro_test(residuals(poly_2_model_ffsd)) #Normality
bptest(poly_2_model_ffsd) #Heteroskedasticity 

#log-log
ln_ln_model_ffsd = lm( log(Force_SD) ~ log(Mean_Force))
summary(ln_ln_model_ffsd)
plot_curve(ln_ln_model_ffsd, "Mean Force (N)", "Force SD (N)")
#plotPredy(data=simulations, x=Mean_Force, y=EMG_RMS, ln_ln_model_ffsd)
residualPlot((ln_ln_model_ffsd))
shapiro_test(residuals(ln_ln_model_ffsd)) #Normality
bptest(ln_ln_model_ffsd) #Heteroskedasticity 

#poly-log
poly_2_ln_ln_model_ffsd = lm( log(Force_SD) ~ poly(log(Mean_Force), 2, raw=TRUE))
summary(poly_2_ln_ln_model_ffsd)
plot_curve(poly_2_ln_ln_model_ffsd, "Mean Force (N)", "Force SD (N)")
#plotPredy(simulations, Mean_Force, EMG_RMS, poly_2_ln_ln_model_ffsd)
residualPlot((poly_2_ln_ln_model_ffsd))
shapiro_test(residuals(poly_2_ln_ln_model_ffsd)) #Normality
bptest(poly_2_ln_ln_model_ffsd) #Heteroskedasticity

#nova
anova(ln_ln_model_ffsd, poly_2_ln_ln_model_ffsd)
