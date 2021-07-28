library(dplyr)
library(Metrics)
library(lmtest)
library(car)
library(glmnet)
library(InformationValue)
library(survival)

#Version 2, includes logistic regression
#Date: 7/20/21
#Author: Kevin


#Create a function which takes as input:
#   df = a dataframe to split into training and validation sets
#   formula = a formula for lm model
#   experimnet_name = what to name the csv output file, by default will overwrite results_default.csv. 
#   ****NOTE**** If the csv file is open on your computer, it will throw an error when R attempts to overwrite file
#   criterion = criterion for selection algorithms to use, by default 2
#   step = boolean to run all three stepwise selection algorithms
#   train_split = float to indicate percentage of training data, default is .8
#   regularize = whether to run regularized regression or not
#   concord = boolean to print out concordance
#.
#.
#.
#Output should be some kind of log file that saves to D:/Summer_Data/experiment_logs/
#to see results of running the experiment.

run_selection_experiemnt_logistic = function(df, df_test, formula, experiment_name = "default", criterion = 2, step = F, forward = F, back = F, train_split = .8, print = F, regularize = F, concord = T, concord_test = T){
  
  
  #Create a dataframe which will store the results of running different experiments
  results = data.frame(
    "model formula",
    "concordance", 
    "AIC",
    "Coefficients"
  )
  
  
  #add an id column to dataframe for splitting
  df = df %>% 
    mutate(id = row_number())
  
  #Create training split
  train = df %>%
    sample_frac(train_split)
  
  # Create validation split
  validation = anti_join(df, train, by = 'id')
  
  #Create full model for selection algorithms
  full_model = glm(formula, data =train, family = binomial(link  = "logit"))
  
  #create empty formula for empty_model, get string value of response 
  #and paste to make empty formula 
  response = as.character(formula[2])
  formula_empty = paste(response, "~","1")
  formula_empty = as.formula(formula_empty)
  
  #Create empty model for selection algorithms
  empty_model = glm(formula_empty, data = train, family = binomial(link  = "logit"))
  
  #create selection criterion for logistic regression
  criterion =  log(dim(train)[1])
  
  #run stepwise selection
  if(step){
    step_model = step(empty_model,
                      scope = list(lower = empty_model,
                                   upper = full_model),
                      direction = "both", k = criterion)
    
    #run an glm with step_model
    step_model_glm = glm(step_model, data = train, family = binomial(link  = "logit"))
    
    #calculate concordance for the step model
    #Old way of calculating concordance concordance_result = concordance(validation[[response]], predict(step_model_glm, type = "response", newdata = validation))
    cord = concordance(step_model_glm)
    
    # This is a test for custom concordance calculation
    # Concordance(test$casual_high, predict(bike_logit, type = "response", newdata = test))
    
    
    #Add model along with coefficients, p-values and concordance results
    results[nrow(results)+1,] = list(paste("Stepwise: ",as.character(step_model[[22]][2])), as.character(cord[2]), as.character(step_model[11]),paste(as.character(row.names(as.data.frame(coef(step_model_glm)))),collapse = " "))
    results[nrow(results)+1,] = list("","","", paste(as.character(coef(step_model_glm)), collapse = " "))
  }
  
  #run forward selection 
  if(forward){
    forward_model = step(empty_model,
                         scope = list(lower = empty_model,
                                      upper = full_model),
                         direction = "both", k = criterion)
    
    #run an glm with forward_model
    forward_model_glm = glm(forward_model, data = train, family = binomial(link  = "logit"))
    
    #calculate concordance for the forward model
    #Old way of calculating concordance concordance_result = concordance(validation[[response]], predict(forward_model_glm, type = "response", newdata = validation))
    #Note this is currently running concordance on the data the model was fitted to i.e. not reliable XD
    #I think there is an issue with the survival::concordance 'newdata' argument which makes it hard to run
    #the fitted model on validation data.
    cord = concordance(forward_model_glm)
    
    #Add model along with coefficients, p-values and concordance results
    results[nrow(results)+1,] = list(paste("forward: ",as.character(forward_model[[22]][2])), as.character(cord[2]), as.character(forward_model[11]))
  }
  
  #run backward selection 
  if(back){
    back_model = step(full_model,
                      scope = list(lower = empty_model,
                                   upper = full_model),
                      direction = "back", k = criterion)
    
    #run an glm with step_model
    back_model_glm = glm(back_model, data = train, family = binomial(link  = "logit"))
    
    #calculate concordance for the step model
    #Old way of calculating concordance concordance_result = concordance(validation[[response]], predict(back_model_glm, type = "response", newdata = validation))
    cord = concordance(back_model_glm)
    
    
    #Add model along with coefficients, p-values and concordance results
    results[nrow(results)+1,] = list(paste("Backward: ",as.character(back_model[[22]][2])), as.character(cord[2]), as.character(back_model[11]))
  }
  
  
  #***************Now time for some regularized regression... *puts on wrist-supporting coding gloves**********
  #*                                                                                                          *
  #*                                                                                                          *
  #*                                                                                                          *
  #************************************************************************************************************
  
  
  if(regularize){
    
    #Create our train x matrix for training and validation
    train_x = model.matrix(formula, data = train)[,-1]
    test_x = model.matrix(formula, data = validation)[,-1]
    
    #Create train y vector and test y vector
    train_y = train[,response]
    test_y = validation[,response]
    
    #LASSO
    
    #Create a cross-validated lasso model 
    lasso = glmnet(x = train_x, y = train_y, alpha = 1, family = 'binomial')
    lasso_cv = cv.glmnet(x = train_x, y = train_y, alpha = 1, family = 'binomial', type.measure = "auc")
    
    #Predict y with y^ for the predicted variable
    #concordance_result = concordance(validation[[response]], predict(lasso, type = "response", s = lasso$lambda.1se, newx = test_x))
    
    #Add results 
    results[nrow(results)+1,] = list("Lasso","concordance doesn't seem to run on glmnet objects")
    
    
    #RIDGE
    
    #Create a cross-validated ridge model 
    ridge = glmnet(x = train_x, y = train_y, alpha = 1, family = 'binomial')
    ridge_cv  = cv.glmnet(x = train_x, y = train_y, alpha = 0, family = 'binomial', type.measure = "auc")
    
    #Predict y with y^ for the predicted variable
    #concordance_result = concordance(validation[[response]], predict(ridge, type = "response", s = lasso$lambda.1se, newx = test_x))
    
    #Add results to be exported to csv later
    results[nrow(results)+1,] = list("Ridge" ,"concordance doesn't seem to run on glmnet objects")
    
    
    #Elastic Net from alpha = 0.1 to .9
    # alphalist = seq(0.1,0.9,by=0.1)
    # elasticnet = lapply(alphalist, function(a){
    #   cv.glmnet(x = train_x, y = train_y, alpha=a, lambda.min.ratio=.001)
    # })
    # 
    # j = 1
    # for(i in alphalist){
    #   #Predict y with y^ for the predicted variable
    #   validation$pred_lm = predict(elasticnet[[j]], s = elasticnet[[j]]$lambda.1se, newx = test_x)
    #   concordance_result = concordance(validation[[response]], predict(ridge, type = "response", s = lasso$lambda.1se, newx = test_x))
    #   j = j + 1
    #   #Calculate MAE and MAPE
    #   temp_mae = mae(test_y, validation$pred_lm)
    #   temp_mape = mape(test_y, validation$pred_lm)
    # 
    #   results[nrow(results)+1,] = list(paste("Elastic Net with Alpha: ",i), temp_mae, temp_mape)
    # }
  }
  
  if(concord){
    
    step_model_glm = glm(formula, data = train, family = binomial(link  = "logit"))
    
    
    validation_copy = validation
    
    f = rep(seq_len(ceiling(nrow(validation)/1000)),each = 1000,length.out = nrow(validation))
    
    df_segments = split(validation_copy, f)
    
    cord = 0
    for( segment in df_segments){
      cord_temp = InformationValue::Concordance(segment[[response]], predict(step_model_glm, type = "response", newdata = segment))
      cord = cord + cord_temp[[1]][1]
    }
    
    if(concord_test == F){
      print("Ghetto Concordance on validation: ", as.character(cord/length(df_segments)))
      return(as.character(cord/length(df_segments)))
    }
    concord_valid = as.character(cord/length(df_segments))
    
  }
  
  if(concord_test){
    
    step_model_glm = glm(formula, data = train, family = binomial(link  = "logit"))
    
    validation_copy = df_test
    
    f = rep(seq_len(ceiling(nrow(validation_copy)/1000)),each = 1000,length.out = nrow(validation_copy))
    
    df_segments = split(validation_copy, f)
    
    cord = 0
    for( segment in df_segments){
      cord_temp = InformationValue::Concordance(segment[[response]], predict(step_model_glm, type = "response", newdata = segment))
      cord = cord + cord_temp[[1]][1]
    }
    
    
    print("Ghetto Concordance on test: ", as.character(cord/length(df_segments)))
    
    return(as.character(cord/length(df_segments)))
    
  }
  
  
  
  #output results to a csv file
  #*** To improve this code, automatically increment the file name so new log
  #*** files are generated each time.
  
  #write.csv(results,paste("D:/Summer_Data/experiment_logs/results_",experiment_name,".csv" ),row.names = FALSE)
}


#The following is only for debugging the experiment function********************************
debug_function = T
if(debug_function){
  #Debugging wrapper function
  temp_function = function(){
    browser()
    run_selection_experiemnt_logistic(df = train_valid,df_test = train_test, formula = formula, experiment_name = "first_experiment_logistic", regularize = T)
  }
  
  #Initiate debugging
  temp_function()
}


#Set example to T to see an example of how to call the function*****************************
example = T
if(example){
  
  # grab a sample to use for train/validation split
  train_valid = train_master_processed[sample(nrow(train_master_processed), 10000),]
  
  train_test = test_master_processed[sample(nrow(train_master_processed), 265000),]
  
  # create a formula to run experiments on
  formula = as.formula(default~dti+tot_cur_bal+loan_amnt+annual_inc+revol_util + inq_last_6mths + purpose)
  
  
  #Debugging all done? Cool run the code for real
  run_selection_experiemnt_logistic(df = train_valid, df_test = train_test, formula = formula, experiment_name = "first_experiment_logistic", concord_test = T, concord = F)
}

