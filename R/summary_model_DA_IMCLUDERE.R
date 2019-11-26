summary_model <- function(mod){

  # library(haven)
  # library(tidyr)
  # library(lmerTest)
  # library(effects)
  library(sjPlot)
  library(ggplot2)
  theme_set(theme_sjplot())
  # es   mod=lm(formula,data=df)
  
  cat("\n")

  print(summary(mod))
  
  cat("\n 
      \n 
      TEST ANOVA PER QUESTO MODELLO
      
      ")
  print(car::Anova(mod))
  
  cat("\n 
      \n 
      ISPEZIONE DEI RESIDUI
      
      ")
  par(mfrow=c(2,2))
  plot(mod,ask=FALSE)
  par(mfrow=c(1,1))
  
  cat("\n 
      \n 
      QUALCHE PLOT DEGLI EFFETTI
      
      ")
  try(print(plot_model(mod, type = "pred", terms = c("ATT_Meat","treat"))),silent=TRUE)
  try(print(plot_model(mod, type = "pred", terms = c("ATT_Veg","treat"))),silent=TRUE)
  
  print(plot_model(mod, type = "pred", terms = c("FNS")))
  
  
  
  # print(plot_model(mod, type = "pred", terms = c("Age","Gen")))
  
  # plot(allEffects(mod))
  return(mod)
}