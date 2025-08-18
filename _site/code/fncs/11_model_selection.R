
model_selection <- function(){
  #### E4.1 TCD ####
  #### gls-models for gps - roe deer ####
  # linear model (homogenous variances)
  tcd_gps_roe_0 <- gls(tcd_forest_prop ~ time+population, 
                       data = sub_roe_point)
  # linear model (separate variance for day & night)
  tcd_gps_roe_1 <- gls(tcd_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time), 
                       data = sub_roe_point)
  # linear model (separate variance for day & night & populations)
  tcd_gps_roe_2 <- gls(tcd_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  tcd_gps_roe_3 <- gls(tcd_forest_prop ~ time*population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # not used, but useful in the model selection, just population
  tcd_gps_roe_4 <- gls(tcd_forest_prop ~ population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # not used, but useful in the model selection, just time
  tcd_gps_roe_5 <- gls(tcd_forest_prop ~ time,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # not used, but useful in the model selection, null model
  tcd_gps_roe_6 <- gls(tcd_forest_prop ~ 1,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # # null model with only population as varIdent
  # tcd_gps_roe_7 <- gls(tcd_forest_prop ~ time + population,
  #                      weights = varIdent(form = ~1 | population), 
  #                      data = sub_roe_point)
  # linear model (separate variance for populations)
  tcd_gps_roe_7 <- gls(tcd_forest_prop ~ time + population,
                       weights = varIdent(form = ~1 | population),
                       data = sub_roe_point)
  
  
  
  ## check whith model is the best
  #anova(tcd_gps_roe_0,tcd_gps_roe_1)
  #anova(tcd_gps_roe_1,tcd_gps_roe_2)
  ## testing between tcd_gps_roe_2 & tcd_gps_roe_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in tcd_gps_roe_3 are not-significant anyway!
  #   summary(tcd_gps_roe_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(tcd_gps_roe_2)
  #plot(tcd_gps_roe_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(tcd_gps_roe_0, "normalized") ~ fitted(tcd_gps_roe_0))
  # plot( resid(tcd_gps_roe_1, "normalized") ~ fitted(tcd_gps_roe_1))
  # plot( resid(tcd_gps_roe_2, "normalized") ~ fitted(tcd_gps_roe_2))
  # plot( resid(tcd_gps_roe_3, "normalized") ~ fitted(tcd_gps_roe_3))
  # plot( resid(tcd_gps_roe_4, "normalized") ~ fitted(tcd_gps_roe_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(tcd_gps_roe_2)
  
  tcd_gps_roe_dredge <- as.data.frame(dredge(tcd_gps_roe_3))
  colnames(tcd_gps_roe_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  tcd_gps_roe_dredge <- rbind(tcd_gps_roe_dredge,data.frame(intercept=tcd_gps_roe_0$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(tcd_gps_roe_0))[1], 
                                                            AICc=AICc(summary(tcd_gps_roe_0)), 
                                                            delta=NA,
                                                            weight=NA))
  tcd_gps_roe_dredge <- rbind(tcd_gps_roe_dredge,data.frame(intercept=tcd_gps_roe_1$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(tcd_gps_roe_1))[1], 
                                                            AICc=AICc(summary(tcd_gps_roe_1)), 
                                                            delta=NA,
                                                            weight=NA))
  tcd_gps_roe_dredge <- rbind(tcd_gps_roe_dredge,data.frame(intercept=tcd_gps_roe_7$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(tcd_gps_roe_7))[1], 
                                                            AICc=AICc(summary(tcd_gps_roe_7)), 
                                                            delta=NA,
                                                            weight=NA))
  
  tcd_gps_roe_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA) # add
  tcd_gps_roe_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA) #add
  tcd_gps_roe_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  
  
  tcd_gps_roe_dredge <- tcd_gps_roe_dredge[order(tcd_gps_roe_dredge$AICc),]
  for(i in 1:(nrow(tcd_gps_roe_dredge))){
    tcd_gps_roe_dredge$delta[i] <- abs(tcd_gps_roe_dredge$AICc[1] - tcd_gps_roe_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  tcd_gps_roe_dredge$model <- paste0(tcd_gps_roe_dredge$population,
                                     tcd_gps_roe_dredge$time,
                                     tcd_gps_roe_dredge$interaction,
                                     tcd_gps_roe_dredge$varidentTime,
                                     tcd_gps_roe_dredge$varidentinteraction,
                                     tcd_gps_roe_dredge$varidentPopulation)
  
  tcd_gps_roe_dredge$Rsquared <- NULL
  
  #  population+time 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_0))^2
  # population+time+ var time 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_1))^2
  # population+time + var interaction 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_2))^2
  # population+time+interaction + var interaction 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_3))^2
  # population + var interaction 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_4))^2
  # time + var interaction 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_5))^2
  # var interaction 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_6))^2
  # population+time + var population 
  tcd_gps_roe_dredge[which(tcd_gps_roe_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_roe_point$tcd_forest_prop, predict(tcd_gps_roe_7))^2
  
  ms <- tcd_gps_roe_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_tcd_gps_roe <- tcd_gps_roe_0}
  if(ms$model=='++NA+NANA')   {  best_model_tcd_gps_roe <- tcd_gps_roe_1}
  if(ms$model=='++NANA+NA')   {  best_model_tcd_gps_roe <- tcd_gps_roe_2}
  if(ms$model=='+++NA+NA')    {  best_model_tcd_gps_roe <- tcd_gps_roe_3}
  if(ms$model=='+NANANA+NA')  {  best_model_tcd_gps_roe <- tcd_gps_roe_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_tcd_gps_roe <- tcd_gps_roe_5}
  if(ms$model=='NANANANA+NA') {  best_model_tcd_gps_roe <- tcd_gps_roe_6}
  if(ms$model=='++NANANA+')   {  best_model_tcd_gps_roe <- tcd_gps_roe_7}
  
  
  #### gls-models for gps - red deer ####
  
  # linear model (homogenous variances)
  tcd_gps_red_0 <- gls(tcd_forest_prop ~ time+population, 
                       data = sub_red_point)
  # linear model (separate variance for day & night)
  tcd_gps_red_1 <- gls(tcd_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time), 
                       data = sub_red_point)
  # linear model (separate variance for day & night & populations)
  tcd_gps_red_2 <- gls(tcd_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  tcd_gps_red_3 <- gls(tcd_forest_prop ~ time*population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # not used, but useful in the model selection, just population
  tcd_gps_red_4 <- gls(tcd_forest_prop ~ population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # not used, but useful in the model selection, just time
  tcd_gps_red_5 <- gls(tcd_forest_prop ~ time,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # not used, but useful in the model selection, null model
  tcd_gps_red_6 <- gls(tcd_forest_prop ~ 1,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # tcd_gps_red_7 <- gls(tcd_forest_prop ~ 1,
  #                      weights = varIdent(form = ~1 | population), 
  #                      data = sub_red_point)
  # linear model (separate variance for populations)
  tcd_gps_red_7 <- gls(tcd_forest_prop ~ time + population,
                       weights = varIdent(form = ~1 | population),
                       data = sub_red_point)
  
  ## check whith model is the best
  #anova(tcd_gps_red_0,tcd_gps_red_1)
  #anova(tcd_gps_red_1,tcd_gps_red_2)
  ## testing between tcd_gps_red_2 & tcd_gps_red_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in tcd_gps_red_3 are not-significant anyway!
  #   summary(tcd_gps_red_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(tcd_gps_red_2)
  #plot(tcd_gps_red_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(tcd_gps_red_0, "normalized") ~ fitted(tcd_gps_red_0))
  # plot( resid(tcd_gps_red_1, "normalized") ~ fitted(tcd_gps_red_1))
  # plot( resid(tcd_gps_red_2, "normalized") ~ fitted(tcd_gps_red_2))
  # plot( resid(tcd_gps_red_3, "normalized") ~ fitted(tcd_gps_red_3))
  # plot( resid(tcd_gps_red_4, "normalized") ~ fitted(tcd_gps_red_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(tcd_gps_red_2)
  
  tcd_gps_red_dredge <- as.data.frame(dredge(tcd_gps_red_3))
  colnames(tcd_gps_red_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  tcd_gps_red_dredge <- rbind(tcd_gps_red_dredge,data.frame(intercept=tcd_gps_red_0$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(tcd_gps_red_0))[1], 
                                                            AICc=AICc(summary(tcd_gps_red_0)), 
                                                            delta=NA,
                                                            weight=NA))
  tcd_gps_red_dredge <- rbind(tcd_gps_red_dredge,data.frame(intercept=tcd_gps_red_1$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(tcd_gps_red_1))[1], 
                                                            AICc=AICc(summary(tcd_gps_red_1)), 
                                                            delta=NA,
                                                            weight=NA))
  tcd_gps_red_dredge <- rbind(tcd_gps_red_dredge,data.frame(intercept=tcd_gps_red_7$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(tcd_gps_red_7))[1], 
                                                            AICc=AICc(summary(tcd_gps_red_7)), 
                                                            delta=NA,
                                                            weight=NA))
  
  tcd_gps_red_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  tcd_gps_red_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  tcd_gps_red_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  tcd_gps_red_dredge <- tcd_gps_red_dredge[order(tcd_gps_red_dredge$AICc),]
  for(i in 1:(nrow(tcd_gps_red_dredge))){
    tcd_gps_red_dredge$delta[i] <- abs(tcd_gps_red_dredge$AICc[1] - tcd_gps_red_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  tcd_gps_red_dredge$model <- paste0(tcd_gps_red_dredge$population,
                                     tcd_gps_red_dredge$time,
                                     tcd_gps_red_dredge$interaction,
                                     tcd_gps_red_dredge$varidentTime,
                                     tcd_gps_red_dredge$varidentinteraction,
                                     tcd_gps_red_dredge$varidentPopulation)
  
  tcd_gps_red_dredge$Rsquared <- NULL
  
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_0))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_1))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_2))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_3))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_4))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_5))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_6))^2
  tcd_gps_red_dredge[which(tcd_gps_red_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_red_point$tcd_forest_prop, predict(tcd_gps_red_7))^2
  
  ms <- tcd_gps_red_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_tcd_gps_red <- tcd_gps_red_0}
  if(ms$model=='++NA+NANA')   {  best_model_tcd_gps_red <- tcd_gps_red_1}
  if(ms$model=='++NANA+NA')   {  best_model_tcd_gps_red <- tcd_gps_red_2}
  if(ms$model=='+++NA+NA')    {  best_model_tcd_gps_red <- tcd_gps_red_3}
  if(ms$model=='+NANANA+NA')  {  best_model_tcd_gps_red <- tcd_gps_red_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_tcd_gps_red <- tcd_gps_red_5}
  if(ms$model=='NANANANA+NA') {  best_model_tcd_gps_red <- tcd_gps_red_6}
  if(ms$model=='++NANANA+') {  best_model_tcd_gps_red <- tcd_gps_red_7}
  
  #### gls-models for kde - roe deer ####
  
  # linear model (homogenous variances)
  tcd_kernel_roe_0 <- gls(tcd_forest_prop ~ time+population, 
                          data = sub_roe_kernel)
  # linear model (separate variance for day & night)
  tcd_kernel_roe_1 <- gls(tcd_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time), 
                          data = sub_roe_kernel)
  # linear model (separate variance for day & night & populations)
  tcd_kernel_roe_2 <- gls(tcd_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  tcd_kernel_roe_3 <- gls(tcd_forest_prop ~ time*population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # not used, but useful in the model selection, just population
  tcd_kernel_roe_4 <- gls(tcd_forest_prop ~ population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # not used, but useful in the model selection, just time
  tcd_kernel_roe_5 <- gls(tcd_forest_prop ~ time,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # not used, but useful in the model selection, null model
  tcd_kernel_roe_6 <- gls(tcd_forest_prop ~ 1,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # tcd_kernel_roe_7 <- gls(tcd_forest_prop ~ 1,
  #                      weights = varIdent(form = ~1 | population), 
  #                      data = sub_roe_kernel)
  # linear model (separate variance for populations)
  tcd_kernel_roe_7 <- gls(tcd_forest_prop ~ time + population,
                          weights = varIdent(form = ~1 | population),
                          data = sub_roe_kernel)
  
  ## check whith model is the best
  #anova(tcd_kernel_roe_0,tcd_kernel_roe_1)
  #anova(tcd_kernel_roe_1,tcd_kernel_roe_2)
  ## testing between tcd_kernel_roe_2 & tcd_kernel_roe_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in tcd_kernel_roe_3 are not-significant anyway!
  #   summary(tcd_kernel_roe_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(tcd_kernel_roe_2)
  #plot(tcd_kernel_roe_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(tcd_kernel_roe_0, "normalized") ~ fitted(tcd_kernel_roe_0))
  # plot( resid(tcd_kernel_roe_1, "normalized") ~ fitted(tcd_kernel_roe_1))
  # plot( resid(tcd_kernel_roe_2, "normalized") ~ fitted(tcd_kernel_roe_2))
  # plot( resid(tcd_kernel_roe_3, "normalized") ~ fitted(tcd_kernel_roe_3))
  # plot( resid(tcd_kernel_roe_4, "normalized") ~ fitted(tcd_kernel_roe_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(tcd_kernel_roe_2)
  
  tcd_kernel_roe_dredge <- as.data.frame(dredge(tcd_kernel_roe_3))
  colnames(tcd_kernel_roe_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  tcd_kernel_roe_dredge <- rbind(tcd_kernel_roe_dredge,data.frame(intercept=tcd_kernel_roe_0$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(tcd_kernel_roe_0))[1], 
                                                                  AICc=AICc(summary(tcd_kernel_roe_0)), 
                                                                  delta=NA,
                                                                  weight=NA))
  tcd_kernel_roe_dredge <- rbind(tcd_kernel_roe_dredge,data.frame(intercept=tcd_kernel_roe_1$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(tcd_kernel_roe_1))[1], 
                                                                  AICc=AICc(summary(tcd_kernel_roe_1)), 
                                                                  delta=NA,
                                                                  weight=NA))
  tcd_kernel_roe_dredge <- rbind(tcd_kernel_roe_dredge,data.frame(intercept=tcd_kernel_roe_7$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(tcd_kernel_roe_7))[1], 
                                                                  AICc=AICc(summary(tcd_kernel_roe_7)), 
                                                                  delta=NA,
                                                                  weight=NA))
  
  tcd_kernel_roe_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  tcd_kernel_roe_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  tcd_kernel_roe_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  tcd_kernel_roe_dredge <- tcd_kernel_roe_dredge[order(tcd_kernel_roe_dredge$AICc),]
  for(i in 1:(nrow(tcd_kernel_roe_dredge))){
    tcd_kernel_roe_dredge$delta[i] <- abs(tcd_kernel_roe_dredge$AICc[1] - tcd_kernel_roe_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  tcd_kernel_roe_dredge$model <- paste0(tcd_kernel_roe_dredge$population,
                                        tcd_kernel_roe_dredge$time,
                                        tcd_kernel_roe_dredge$interaction,
                                        tcd_kernel_roe_dredge$varidentTime,
                                        tcd_kernel_roe_dredge$varidentinteraction,
                                        tcd_kernel_roe_dredge$varidentPopulation)
  
  tcd_kernel_roe_dredge$Rsquared <- NULL
  
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_0))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_1))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_2))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_3))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_4))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_5))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_6))^2
  tcd_kernel_roe_dredge[which(tcd_kernel_roe_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_roe_kernel$tcd_forest_prop, predict(tcd_kernel_roe_7))^2
  
  
  ms <- tcd_kernel_roe_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_tcd_kernel_roe <- tcd_kernel_roe_0}
  if(ms$model=='++NA+NANA')   {  best_model_tcd_kernel_roe <- tcd_kernel_roe_1}
  if(ms$model=='++NANA+NA')   {  best_model_tcd_kernel_roe <- tcd_kernel_roe_2}
  if(ms$model=='+++NA+NA')    {  best_model_tcd_kernel_roe <- tcd_kernel_roe_3}
  if(ms$model=='+NANANA+NA')  {  best_model_tcd_kernel_roe <- tcd_kernel_roe_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_tcd_kernel_roe <- tcd_kernel_roe_5}
  if(ms$model=='NANANANA+NA') {  best_model_tcd_kernel_roe <- tcd_kernel_roe_6}
  if(ms$model=='++NANANA+') {  best_model_tcd_kernel_roe <- tcd_kernel_roe_7}
  
  
  #### gls-models for kde - red deer #### 
  
  # linear model (homogenous variances)
  tcd_kernel_red_0 <- gls(tcd_forest_prop ~ time+population, 
                          data = sub_red_kernel)
  # linear model (separate variance for day & night)
  tcd_kernel_red_1 <- gls(tcd_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time), 
                          data = sub_red_kernel)
  # linear model (separate variance for day & night & populations)
  tcd_kernel_red_2 <- gls(tcd_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  tcd_kernel_red_3 <- gls(tcd_forest_prop ~ time*population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # not used, but useful in the model selection, just population
  tcd_kernel_red_4 <- gls(tcd_forest_prop ~ population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # not used, but useful in the model selection, just time
  tcd_kernel_red_5 <- gls(tcd_forest_prop ~ time,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # not used, but useful in the model selection, null model
  tcd_kernel_red_6 <- gls(tcd_forest_prop ~ 1,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # tcd_kernel_red_7 <- gls(tcd_forest_prop ~ 1,
  #                         weights = varIdent(form = ~1 | population), 
  #                         data = sub_red_kernel)
  # linear model (separate variance for populations)
  tcd_kernel_red_7 <- gls(tcd_forest_prop ~ time + population,
                          weights = varIdent(form = ~1 | population),
                          data = sub_red_kernel)
  
  ## check whith model is the best
  #anova(tcd_kernel_red_0,tcd_kernel_red_1)
  #anova(tcd_kernel_red_1,tcd_kernel_red_2)
  ## testing between tcd_kernel_red_2 & tcd_kernel_red_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in tcd_kernel_red_3 are not-significant anyway!
  #   summary(tcd_kernel_red_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(tcd_kernel_red_2)
  #plot(tcd_kernel_red_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(tcd_kernel_red_0, "normalized") ~ fitted(tcd_kernel_red_0))
  # plot( resid(tcd_kernel_red_1, "normalized") ~ fitted(tcd_kernel_red_1))
  # plot( resid(tcd_kernel_red_2, "normalized") ~ fitted(tcd_kernel_red_2))
  # plot( resid(tcd_kernel_red_3, "normalized") ~ fitted(tcd_kernel_red_3))
  # plot( resid(tcd_kernel_red_4, "normalized") ~ fitted(tcd_kernel_red_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(tcd_kernel_red_2)
  
  tcd_kernel_red_dredge <- as.data.frame(dredge(tcd_kernel_red_3))
  colnames(tcd_kernel_red_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  tcd_kernel_red_dredge <- rbind(tcd_kernel_red_dredge,data.frame(intercept=tcd_kernel_red_0$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(tcd_kernel_red_0))[1], 
                                                                  AICc=AICc(summary(tcd_kernel_red_0)), 
                                                                  delta=NA,
                                                                  weight=NA))
  tcd_kernel_red_dredge <- rbind(tcd_kernel_red_dredge,data.frame(intercept=tcd_kernel_red_1$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(tcd_kernel_red_1))[1], 
                                                                  AICc=AICc(summary(tcd_kernel_red_1)), 
                                                                  delta=NA,
                                                                  weight=NA))
  tcd_kernel_red_dredge <- rbind(tcd_kernel_red_dredge,data.frame(intercept=tcd_kernel_red_7$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(tcd_kernel_red_7))[1], 
                                                                  AICc=AICc(summary(tcd_kernel_red_7)), 
                                                                  delta=NA,
                                                                  weight=NA))
  
  tcd_kernel_red_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  tcd_kernel_red_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  tcd_kernel_red_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  tcd_kernel_red_dredge <- tcd_kernel_red_dredge[order(tcd_kernel_red_dredge$AICc),]
  for(i in 1:(nrow(tcd_kernel_red_dredge))){
    tcd_kernel_red_dredge$delta[i] <- abs(tcd_kernel_red_dredge$AICc[1] - tcd_kernel_red_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  tcd_kernel_red_dredge$model <- paste0(tcd_kernel_red_dredge$population,
                                        tcd_kernel_red_dredge$time,
                                        tcd_kernel_red_dredge$interaction,
                                        tcd_kernel_red_dredge$varidentTime,
                                        tcd_kernel_red_dredge$varidentinteraction,
                                        tcd_kernel_red_dredge$varidentPopulation)
  
  tcd_kernel_red_dredge$Rsquared <- NULL
  
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_0))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_1))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_2))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_3))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_4))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_5))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_6))^2
  tcd_kernel_red_dredge[which(tcd_kernel_red_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_red_kernel$tcd_forest_prop, predict(tcd_kernel_red_7))^2
  
  ms <- tcd_kernel_red_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_tcd_kernel_red <- tcd_kernel_red_0}
  if(ms$model=='++NA+NANA')   {  best_model_tcd_kernel_red <- tcd_kernel_red_1}
  if(ms$model=='++NANA+NA')   {  best_model_tcd_kernel_red <- tcd_kernel_red_2}
  if(ms$model=='+++NA+NA')    {  best_model_tcd_kernel_red <- tcd_kernel_red_3}
  if(ms$model=='+NANANA+NA')  {  best_model_tcd_kernel_red <- tcd_kernel_red_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_tcd_kernel_red <- tcd_kernel_red_5}
  if(ms$model=='NANANANA+NA') {  best_model_tcd_kernel_red <- tcd_kernel_red_6}
  if(ms$model=='++NANANA+') {  best_model_tcd_kernel_red <- tcd_kernel_red_7}
  
  
  #### E4.2 CLC #### 
  
  ####  gls-models for gps - roe deer #### 
  
  # linear model (homogenous variances)
  clc_gps_roe_0 <- gls(clc_forest_prop ~ time+population, 
                       data = sub_roe_point)
  # linear model (separate variance for day & night)
  clc_gps_roe_1 <- gls(clc_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time), 
                       data = sub_roe_point)
  # linear model (separate variance for day & night & populations)
  clc_gps_roe_2 <- gls(clc_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  clc_gps_roe_3 <- gls(clc_forest_prop ~ time*population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # not used, but useful in the model selection, just population
  clc_gps_roe_4 <- gls(clc_forest_prop ~ population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # not used, but useful in the model selection, just time
  clc_gps_roe_5 <- gls(clc_forest_prop ~ time,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # not used, but useful in the model selection, null model
  clc_gps_roe_6 <- gls(clc_forest_prop ~ 1,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_roe_point)
  # clc_gps_roe_7 <- gls(clc_forest_prop ~ 1,
  #                         weights = varIdent(form = ~1 | population), 
  #                         data = sub_roe_point)
  clc_gps_roe_7 <- gls(clc_forest_prop ~ time + population,
                       weights = varIdent(form = ~1 | population),
                       data = sub_roe_point)
  
  ## check whith model is the best
  #anova(clc_gps_roe_0,clc_gps_roe_1)
  #anova(clc_gps_roe_1,clc_gps_roe_2)
  ## testing between clc_gps_roe_2 & clc_gps_roe_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in clc_gps_roe_3 are not-significant anyway!
  #   summary(clc_gps_roe_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(clc_gps_roe_2)
  #plot(clc_gps_roe_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(clc_gps_roe_0, "normalized") ~ fitted(clc_gps_roe_0))
  # plot( resid(clc_gps_roe_1, "normalized") ~ fitted(clc_gps_roe_1))
  # plot( resid(clc_gps_roe_2, "normalized") ~ fitted(clc_gps_roe_2))
  # plot( resid(clc_gps_roe_3, "normalized") ~ fitted(clc_gps_roe_3))
  # plot( resid(clc_gps_roe_4, "normalized") ~ fitted(clc_gps_roe_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(clc_gps_roe_2)
  
  clc_gps_roe_dredge <- as.data.frame(dredge(clc_gps_roe_3))
  colnames(clc_gps_roe_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  clc_gps_roe_dredge <- rbind(clc_gps_roe_dredge,data.frame(intercept=clc_gps_roe_0$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(clc_gps_roe_0))[1], 
                                                            AICc=AICc(summary(clc_gps_roe_0)), 
                                                            delta=NA,
                                                            weight=NA))
  clc_gps_roe_dredge <- rbind(clc_gps_roe_dredge,data.frame(intercept=clc_gps_roe_1$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(clc_gps_roe_1))[1], 
                                                            AICc=AICc(summary(clc_gps_roe_1)), 
                                                            delta=NA,
                                                            weight=NA))
  clc_gps_roe_dredge <- rbind(clc_gps_roe_dredge,data.frame(intercept=clc_gps_roe_7$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(clc_gps_roe_7))[1], 
                                                            AICc=AICc(summary(clc_gps_roe_7)), 
                                                            delta=NA,
                                                            weight=NA))
  
  clc_gps_roe_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  clc_gps_roe_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  clc_gps_roe_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  clc_gps_roe_dredge <- clc_gps_roe_dredge[order(clc_gps_roe_dredge$AICc),]
  for(i in 1:(nrow(clc_gps_roe_dredge))){
    clc_gps_roe_dredge$delta[i] <- abs(clc_gps_roe_dredge$AICc[1] - clc_gps_roe_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  clc_gps_roe_dredge$model <- paste0(clc_gps_roe_dredge$population,
                                     clc_gps_roe_dredge$time,
                                     clc_gps_roe_dredge$interaction,
                                     clc_gps_roe_dredge$varidentTime,
                                     clc_gps_roe_dredge$varidentinteraction,
                                     clc_gps_roe_dredge$varidentPopulation)
  
  
  clc_gps_roe_dredge$Rsquared <- NULL
  
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_0))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_1))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_2))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_3))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_4))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_5))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_6))^2
  clc_gps_roe_dredge[which(clc_gps_roe_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_roe_point$clc_forest_prop, predict(clc_gps_roe_7))^2
  
  
  
  ms <- clc_gps_roe_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_clc_gps_roe <- clc_gps_roe_0}
  if(ms$model=='++NA+NANA')   {  best_model_clc_gps_roe <- clc_gps_roe_1}
  if(ms$model=='++NANA+NA')   {  best_model_clc_gps_roe <- clc_gps_roe_2}
  if(ms$model=='+++NA+NA')    {  best_model_clc_gps_roe <- clc_gps_roe_3}
  if(ms$model=='+NANANA+NA')  {  best_model_clc_gps_roe <- clc_gps_roe_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_clc_gps_roe <- clc_gps_roe_5}
  if(ms$model=='NANANANA+NA') {  best_model_clc_gps_roe <- clc_gps_roe_6}
  if(ms$model=='++NANANA+') {  best_model_clc_gps_roe <- clc_gps_roe_7}
  
  #### gls-models for gps - red deer #### 
  
  # linear model (homogenous variances)
  clc_gps_red_0 <- gls(clc_forest_prop ~ time+population, 
                       data = sub_red_point)
  # linear model (separate variance for day & night)
  clc_gps_red_1 <- gls(clc_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time), 
                       data = sub_red_point)
  # linear model (separate variance for day & night & populations)
  clc_gps_red_2 <- gls(clc_forest_prop ~ time+population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  clc_gps_red_3 <- gls(clc_forest_prop ~ time*population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # not used, but useful in the model selection, just population
  clc_gps_red_4 <- gls(clc_forest_prop ~ population,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # not used, but useful in the model selection, just time
  clc_gps_red_5 <- gls(clc_forest_prop ~ time,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # not used, but useful in the model selection, null model
  clc_gps_red_6 <- gls(clc_forest_prop ~ 1,
                       weights = varIdent(form = ~1 | time*population), 
                       data = sub_red_point)
  # clc_gps_red_7 <- gls(clc_forest_prop ~ 1,
  #                      weights = varIdent(form = ~1 | population), 
  #                      data = sub_red_point)
  clc_gps_red_7 <- gls(clc_forest_prop ~ time + population,
                       weights = varIdent(form = ~1 | population),
                       data = sub_red_point)
  
  ## check whith model is the best
  #anova(clc_gps_red_0,clc_gps_red_1)
  #anova(clc_gps_red_1,clc_gps_red_2)
  ## testing between clc_gps_red_2 & clc_gps_red_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in clc_gps_red_3 are not-significant anyway!
  #   summary(clc_gps_red_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(clc_gps_red_2)
  #plot(clc_gps_red_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(clc_gps_red_0, "normalized") ~ fitted(clc_gps_red_0))
  # plot( resid(clc_gps_red_1, "normalized") ~ fitted(clc_gps_red_1))
  # plot( resid(clc_gps_red_2, "normalized") ~ fitted(clc_gps_red_2))
  # plot( resid(clc_gps_red_3, "normalized") ~ fitted(clc_gps_red_3))
  # plot( resid(clc_gps_red_4, "normalized") ~ fitted(clc_gps_red_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(clc_gps_red_2)
  
  clc_gps_red_dredge <- as.data.frame(dredge(clc_gps_red_3))
  colnames(clc_gps_red_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  clc_gps_red_dredge <- rbind(clc_gps_red_dredge,data.frame(intercept=clc_gps_red_0$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(clc_gps_red_0))[1], 
                                                            AICc=AICc(summary(clc_gps_red_0)), 
                                                            delta=NA,
                                                            weight=NA))
  clc_gps_red_dredge <- rbind(clc_gps_red_dredge,data.frame(intercept=clc_gps_red_1$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(clc_gps_red_1))[1], 
                                                            AICc=AICc(summary(clc_gps_red_1)), 
                                                            delta=NA,
                                                            weight=NA))
  clc_gps_red_dredge <- rbind(clc_gps_red_dredge,data.frame(intercept=clc_gps_red_7$coefficients[1], 
                                                            population='+', 
                                                            time='+', 
                                                            interaction=NA,
                                                            df=NA, 
                                                            logLik=logLik(summary(clc_gps_red_7))[1], 
                                                            AICc=AICc(summary(clc_gps_red_7)), 
                                                            delta=NA,
                                                            weight=NA))
  
  clc_gps_red_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  clc_gps_red_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  clc_gps_red_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  clc_gps_red_dredge <- clc_gps_red_dredge[order(clc_gps_red_dredge$AICc),]
  for(i in 1:(nrow(clc_gps_red_dredge))){
    clc_gps_red_dredge$delta[i] <- abs(clc_gps_red_dredge$AICc[1] - clc_gps_red_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  clc_gps_red_dredge$model <- paste0(clc_gps_red_dredge$population,
                                     clc_gps_red_dredge$time,
                                     clc_gps_red_dredge$interaction,
                                     clc_gps_red_dredge$varidentTime,
                                     clc_gps_red_dredge$varidentinteraction,
                                     clc_gps_red_dredge$varidentPopulation)
  
  clc_gps_red_dredge$Rsquared <- NULL
  
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_0))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_1))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_2))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_3))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_4))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_5))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_6))^2
  clc_gps_red_dredge[which(clc_gps_red_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_red_point$clc_forest_prop, predict(clc_gps_red_7))^2
  
  ms <- clc_gps_red_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_clc_gps_red <- clc_gps_red_0}
  if(ms$model=='++NA+NANA')   {  best_model_clc_gps_red <- clc_gps_red_1}
  if(ms$model=='++NANA+NA')   {  best_model_clc_gps_red <- clc_gps_red_2}
  if(ms$model=='+++NA+NA')    {  best_model_clc_gps_red <- clc_gps_red_3}
  if(ms$model=='+NANANA+NA')  {  best_model_clc_gps_red <- clc_gps_red_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_clc_gps_red <- clc_gps_red_5}
  if(ms$model=='NANANANA+NA') {  best_model_clc_gps_red <- clc_gps_red_6}
  if(ms$model=='++NANANA+') {  best_model_clc_gps_red <- clc_gps_red_7}
  
  #### gls-models for kde - roe deer #### 
  
  # linear model (homogenous variances)
  clc_kernel_roe_0 <- gls(clc_forest_prop ~ time+population, 
                          data = sub_roe_kernel)
  # linear model (separate variance for day & night)
  clc_kernel_roe_1 <- gls(clc_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time), 
                          data = sub_roe_kernel)
  # linear model (separate variance for day & night & populations)
  clc_kernel_roe_2 <- gls(clc_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  clc_kernel_roe_3 <- gls(clc_forest_prop ~ time*population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # not used, but useful in the model selection, just population
  clc_kernel_roe_4 <- gls(clc_forest_prop ~ population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # not used, but useful in the model selection, just time
  clc_kernel_roe_5 <- gls(clc_forest_prop ~ time,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # not used, but useful in the model selection, null model
  clc_kernel_roe_6 <- gls(clc_forest_prop ~ 1,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_roe_kernel)
  # clc_kernel_roe_7 <- gls(clc_forest_prop ~ 1,
  #                      weights = varIdent(form = ~1 | population), 
  #                      data = sub_roe_kernel)
  clc_kernel_roe_7 <- gls(clc_forest_prop ~ time + population,
                          weights = varIdent(form = ~1 | population),
                          data = sub_roe_kernel)
  
  ## check whith model is the best
  #anova(clc_kernel_roe_0,clc_kernel_roe_1)
  #anova(clc_kernel_roe_1,clc_kernel_roe_2)
  ## testing between clc_kernel_roe_2 & clc_kernel_roe_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in clc_kernel_roe_3 are not-significant anyway!
  #   summary(clc_kernel_roe_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(clc_kernel_roe_2)
  #plot(clc_kernel_roe_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(clc_kernel_roe_0, "normalized") ~ fitted(clc_kernel_roe_0))
  # plot( resid(clc_kernel_roe_1, "normalized") ~ fitted(clc_kernel_roe_1))
  # plot( resid(clc_kernel_roe_2, "normalized") ~ fitted(clc_kernel_roe_2))
  # plot( resid(clc_kernel_roe_3, "normalized") ~ fitted(clc_kernel_roe_3))
  # plot( resid(clc_kernel_roe_4, "normalized") ~ fitted(clc_kernel_roe_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(clc_kernel_roe_2)
  
  clc_kernel_roe_dredge <- as.data.frame(dredge(clc_kernel_roe_3))
  colnames(clc_kernel_roe_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  clc_kernel_roe_dredge <- rbind(clc_kernel_roe_dredge,data.frame(intercept=clc_kernel_roe_0$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(clc_kernel_roe_0))[1], 
                                                                  AICc=AICc(summary(clc_kernel_roe_0)), 
                                                                  delta=NA,
                                                                  weight=NA))
  clc_kernel_roe_dredge <- rbind(clc_kernel_roe_dredge,data.frame(intercept=clc_kernel_roe_1$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(clc_kernel_roe_1))[1], 
                                                                  AICc=AICc(summary(clc_kernel_roe_1)), 
                                                                  delta=NA,
                                                                  weight=NA))
  clc_kernel_roe_dredge <- rbind(clc_kernel_roe_dredge,data.frame(intercept=clc_kernel_roe_7$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(clc_kernel_roe_7))[1], 
                                                                  AICc=AICc(summary(clc_kernel_roe_7)), 
                                                                  delta=NA,
                                                                  weight=NA))
  
  clc_kernel_roe_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  clc_kernel_roe_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  clc_kernel_roe_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  clc_kernel_roe_dredge <- clc_kernel_roe_dredge[order(clc_kernel_roe_dredge$AICc),]
  for(i in 1:(nrow(clc_kernel_roe_dredge))){
    clc_kernel_roe_dredge$delta[i] <- abs(clc_kernel_roe_dredge$AICc[1] - clc_kernel_roe_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  clc_kernel_roe_dredge$model <- paste0(clc_kernel_roe_dredge$population,
                                        clc_kernel_roe_dredge$time,
                                        clc_kernel_roe_dredge$interaction,
                                        clc_kernel_roe_dredge$varidentTime,
                                        clc_kernel_roe_dredge$varidentinteraction,
                                        clc_kernel_roe_dredge$varidentPopulation)
  
  clc_kernel_roe_dredge$Rsquared <- NULL
  
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_0))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_1))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_2))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_3))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_4))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_5))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_6))^2
  clc_kernel_roe_dredge[which(clc_kernel_roe_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_roe_kernel$clc_forest_prop, predict(clc_kernel_roe_7))^2
  
  ms <- clc_kernel_roe_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_clc_kernel_roe <- clc_kernel_roe_0}
  if(ms$model=='++NA+NANA')   {  best_model_clc_kernel_roe <- clc_kernel_roe_1}
  if(ms$model=='++NANA+NA')   {  best_model_clc_kernel_roe <- clc_kernel_roe_2}
  if(ms$model=='+++NA+NA')    {  best_model_clc_kernel_roe <- clc_kernel_roe_3}
  if(ms$model=='+NANANA+NA')  {  best_model_clc_kernel_roe <- clc_kernel_roe_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_clc_kernel_roe <- clc_kernel_roe_5}
  if(ms$model=='NANANANA+NA') {  best_model_clc_kernel_roe <- clc_kernel_roe_6}
  if(ms$model=='++NANANA+') {  best_model_clc_kernel_roe <- clc_kernel_roe_7}
  
  #### gls-models for kde - red deer #### 
  
  # linear model (homogenous variances)
  clc_kernel_red_0 <- gls(clc_forest_prop ~ time+population, 
                          data = sub_red_kernel)
  # linear model (separate variance for day & night)
  clc_kernel_red_1 <- gls(clc_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time), 
                          data = sub_red_kernel)
  # linear model (separate variance for day & night & populations)
  clc_kernel_red_2 <- gls(clc_forest_prop ~ time+population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # linear model with interaction time*population 
  # (and separate variance for day & night & populations)
  clc_kernel_red_3 <- gls(clc_forest_prop ~ time*population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # not used, but useful in the model selection, just population
  clc_kernel_red_4 <- gls(clc_forest_prop ~ population,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # not used, but useful in the model selection, just time
  clc_kernel_red_5 <- gls(clc_forest_prop ~ time,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # not used, but useful in the model selection, null model
  clc_kernel_red_6 <- gls(clc_forest_prop ~ 1,
                          weights = varIdent(form = ~1 | time*population), 
                          data = sub_red_kernel)
  # clc_kernel_red_7 <- gls(clc_forest_prop ~ 1,
  #                         weights = varIdent(form = ~1 | population), 
  #                         data = sub_red_kernel)
  clc_kernel_red_7 <- gls(clc_forest_prop ~ time + population,
                          weights = varIdent(form = ~1 | population),
                          data = sub_red_kernel)
  
  
  ## check whith model is the best
  #anova(clc_kernel_red_0,clc_kernel_red_1)
  #anova(clc_kernel_red_1,clc_kernel_red_2)
  ## testing between clc_kernel_red_2 & clc_kernel_red_3 does not work
  ## due to incongruent model structure
  ## but interaction-effects in clc_kernel_red_3 are not-significant anyway!
  #   summary(clc_kernel_red_3)
  
  ## look at best model (model coefficients + residuals)
  #summary(clc_kernel_red_2)
  #plot(clc_kernel_red_2)
  
  
  ## Side note 1:
  ## a generic way to make these residual plots for checking
  ## heteroscedasticity (here done) for the 3 models so that
  ## you can see the effect of fitting separante variances per
  ## group:
  # par(mfrow=c(2,3))
  # plot( resid(clc_kernel_red_0, "normalized") ~ fitted(clc_kernel_red_0))
  # plot( resid(clc_kernel_red_1, "normalized") ~ fitted(clc_kernel_red_1))
  # plot( resid(clc_kernel_red_2, "normalized") ~ fitted(clc_kernel_red_2))
  # plot( resid(clc_kernel_red_3, "normalized") ~ fitted(clc_kernel_red_3))
  # plot( resid(clc_kernel_red_4, "normalized") ~ fitted(clc_kernel_red_4))
  
  ## Side note 2:
  ## To just getch the coefficients for the best model: 
  # coefficients(clc_kernel_red_2)
  
  clc_kernel_red_dredge <- as.data.frame(dredge(clc_kernel_red_3))
  colnames(clc_kernel_red_dredge) <- c('intercept', 'population', 'time', 'interaction','df','logLik','AICc','delta','weight')
  # for dredge 
  clc_kernel_red_dredge <- rbind(clc_kernel_red_dredge,data.frame(intercept=clc_kernel_red_0$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(clc_kernel_red_0))[1], 
                                                                  AICc=AICc(summary(clc_kernel_red_0)), 
                                                                  delta=NA,
                                                                  weight=NA))
  clc_kernel_red_dredge <- rbind(clc_kernel_red_dredge,data.frame(intercept=clc_kernel_red_1$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(clc_kernel_red_1))[1], 
                                                                  AICc=AICc(summary(clc_kernel_red_1)), 
                                                                  delta=NA,
                                                                  weight=NA))
  clc_kernel_red_dredge <- rbind(clc_kernel_red_dredge,data.frame(intercept=clc_kernel_red_7$coefficients[1], 
                                                                  population='+', 
                                                                  time='+', 
                                                                  interaction=NA,
                                                                  df=NA, 
                                                                  logLik=logLik(summary(clc_kernel_red_7))[1], 
                                                                  AICc=AICc(summary(clc_kernel_red_7)), 
                                                                  delta=NA,
                                                                  weight=NA))
  
  clc_kernel_red_dredge$varidentTime <- c(NA,NA,NA,NA,NA,NA,'+',NA)
  clc_kernel_red_dredge$varidentinteraction <- c('+','+','+','+','+',NA,NA,NA)
  clc_kernel_red_dredge$varidentPopulation <- c(NA,NA,NA,NA,NA,NA,NA,'+') #add
  
  clc_kernel_red_dredge <- clc_kernel_red_dredge[order(clc_kernel_red_dredge$AICc),]
  for(i in 1:(nrow(clc_kernel_red_dredge))){
    clc_kernel_red_dredge$delta[i] <- abs(clc_kernel_red_dredge$AICc[1] - clc_kernel_red_dredge$AICc[i])
  }
  
  # BEST MODEL 
  # ADD MODEL STRUCTURE AS COLUMN SO TO AUTOMATICALLY SELECT THE RIGHT MODEL  
  clc_kernel_red_dredge$model <- paste0(clc_kernel_red_dredge$population,
                                        clc_kernel_red_dredge$time,
                                        clc_kernel_red_dredge$interaction,
                                        clc_kernel_red_dredge$varidentTime,
                                        clc_kernel_red_dredge$varidentinteraction,
                                        clc_kernel_red_dredge$varidentPopulation)
  
  clc_kernel_red_dredge$Rsquared <- NULL
  
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == '++NANANANA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_0))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == '++NA+NANA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_1))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == '++NANA+NA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_2))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == '+++NA+NA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_3))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == '+NANANA+NA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_4))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == 'NA+NANA+NA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_5))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == 'NANANANA+NA'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_6))^2
  clc_kernel_red_dredge[which(clc_kernel_red_dredge$model == '++NANANA+'),'Rsquared'] <- cor(sub_red_kernel$clc_forest_prop, predict(clc_kernel_red_7))^2
  
  ms <- clc_kernel_red_dredge[1,]
  
  # WHAT IS THE BEST MODEL? 
  if(ms$model=='++NANANANA')  {  best_model_clc_kernel_red <- clc_kernel_red_0}
  if(ms$model=='++NA+NANA')   {  best_model_clc_kernel_red <- clc_kernel_red_1}
  if(ms$model=='++NANA+NA')   {  best_model_clc_kernel_red <- clc_kernel_red_2}
  if(ms$model=='+++NA+NA')    {  best_model_clc_kernel_red <- clc_kernel_red_3}
  if(ms$model=='+NANANA+NA')  {  best_model_clc_kernel_red <- clc_kernel_red_4}
  if(ms$model=='NA+NANA+NA')  {  best_model_clc_kernel_red <- clc_kernel_red_5}
  if(ms$model=='NANANANA+NA') {  best_model_clc_kernel_red <- clc_kernel_red_6}
  if(ms$model=='++NANANA+') {  best_model_clc_kernel_red <- clc_kernel_red_7}
  
  
  ms_l <- list(tcd_gps_roe_dredge,
               clc_gps_roe_dredge,
               tcd_kernel_roe_dredge,
               clc_kernel_roe_dredge,
               tcd_gps_red_dredge,
               clc_gps_red_dredge,
               tcd_kernel_red_dredge,
               clc_kernel_red_dredge)
  ms_df <- do.call(rbind.data.frame,ms_l)
  ms_df$raster <- rep(c(rep('tcd',8),rep('clc',8)),4)
  ms_df$unit <- rep(c(rep('gps',16),rep('kde',16)),2)
  ms_df$species <- c(rep('roe',32),rep('red',32))
  rownames(ms_df) <- NULL
  
  ms_df <- ms_df[,c('species','unit','raster','population','time','interaction','varidentTime','varidentPopulation','varidentinteraction','logLik','AICc','delta', 'Rsquared')]
  ms_df$logLik <- round(ms_df$logLik, 4)
  ms_df$AICc <- round(ms_df$AICc, 4)
  ms_df$delta <- round(ms_df$delta, 4)
  ms_df$Rsquared <- round(ms_df$Rsquared, 4)
  
  #write.csv(ms_df,paste0(getwd(),'./results/model_selection_results_rsquared_added.csv'))
  #write.table(ms_df,paste0(getwd(),'./results/model_selection_results_rsquared_added.txt'), sep = ",", quote = FALSE, row.names = F)
  
  best <- ms_df[seq(1,nrow(ms_df),8),] 
  
  models <- list(best_model_tcd_gps_roe,
                 best_model_clc_gps_roe,
                 best_model_tcd_kernel_roe,
                 best_model_clc_kernel_roe,
                 best_model_tcd_gps_red,
                 best_model_clc_gps_red,
                 best_model_tcd_kernel_red,
                 best_model_clc_kernel_red)
  
  best$color <- 'grey'
  best[which(best$varidentinteraction == '+'),'color'] <- 'black'
  best[which(best$varidentPopulation == '+'),'color'] <- 'black'
  
  return(list(models,best))
}
