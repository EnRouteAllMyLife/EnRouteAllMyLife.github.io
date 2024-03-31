library(caret)
library(rsample)
## df data need to be trained/ tested
##### sample 
#df_lasso = df |> mutate(IFHOS = factor(IFHOS, levels = c(0, 1), labels = c("no","yes"))) |>
#  filter(DBTYPE == 2) |> 
#  select(-c(zKey,RP,MH,RUHP6Q,DBTYPE,DBDX))
#result_lasso = db_lasso(df_lasso)
## split propotion

db_lasso = function(df_lasso, prop = 0.8,DBtype = c("Type1", "Type2","Others")){
  if (DBtype == "Type1"){
    df_lasso = df_lasso |> 
      mutate(IFHOS = factor(IFHOS, levels = c(0, 1), labels = c("no","yes"))) |>
      #mutate(IFHOS = factor(IFHOS, levels = c(1,0), labels = c("yes","no"))) |>
      filter(DBTYPE == "1") |> 
      select(-c(zKey,RP,MH,RUHP6Q,DBTYPE,DBDX))
  }
  if (DBtype == "Type2"){
    df_lasso = df_lasso |> 
      mutate(IFHOS = factor(IFHOS, levels = c(0, 1), labels = c("no","yes"))) |>
      #mutate(IFHOS = factor(IFHOS, levels = c(1,0), labels = c("yes","no"))) |>
      filter(DBTYPE == "2") |> 
      select(-c(zKey,RP,MH,RUHP6Q,DBTYPE,DBDX))
  }
  else{
    df_lasso = df_lasso |> mutate(IFHOS = factor(IFHOS, levels = c(0, 1), labels = c("no","yes"))) |>
      filter(DBTYPE == "0") |> 
      select(-c(zKey,RP,MH,RUHP6Q,DBTYPE,DBDX,MMAS,DBIN,DBRX))
  }
  
  data_split = initial_split(df_lasso, 
                             prop = prop)
  
  # Extract the training and test data
  training_data = training(data_split)
  testing_data = testing(data_split)
  # matrix of predictors (glmnet uses input matrix)
  x = model.matrix(IFHOS ~ ., training_data)[,-1]
  # vector of response
  y = as.numeric(training_data[, "IFHOS"])
  
 # ctrl = trainControl(method = "cv", number = 10,
 #                     classProbs = TRUE, # Enable probability predictions
#                     summaryFunction = twoClassSummary) # Use AUC for model selection
  
  lasso.fit = train(IFHOS ~ .,
                    data = training_data,
                    method = "glmnet",
                    tuneGrid = expand.grid(alpha = 1, 
                                           lambda = exp(seq(0, -20, length = 100))),
                    trControl = trainControl("cv",
                                             number = 10,
                                             classProbs = TRUE,
                                             savePredictions = TRUE,
                                             summaryFunction = twoClassSummary
                                            ),
                    metric = "ROC")
  plot_lasso_tune = plot(lasso.fit, xTrans = log)
  #lasso.fit$bestTune
  
  # coefficients in the final model
  #coef(lasso.fit$finalModel, s = lasso.fit$bestTune$lambda)
  coef_df = as.data.frame(as.matrix(coef(lasso.fit$finalModel, lasso.fit$bestTune$lambda)))
  coef_df = tibble(variable = rownames(coef_df),
                   estimate = coef_df$s1 )
  
  
  plot_estimate = coef_df %>%
    arrange(desc(abs(estimate))) %>%
    filter(variable != '(Intercept)') %>%
    filter(abs(estimate) > 0) %>%
    ggplot(aes(x = reorder(variable, estimate), y = estimate, fill = reorder(variable, estimate),
               text = paste("Variable:", variable, "<br>Estimate:", round(estimate, 4))
    )) + 
    geom_col() + coord_flip() +
    theme(legend.position = "none") +  
    labs(#title = "Feature Estimate of LASSO Regression",
      x = 'Predictors',
      y = 'Estimate')
  
  # Convert to plotly object and customize hover info
  plot_estimate = ggplotly(plot_estimate, tooltip = "text")
  
  # Evaluate the model
  classes = c("no","yes")
  dat = data.frame( obs = testing_data$IFHOS,
                    pred = predict(lasso.fit, newdata=testing_data),
                    yes = predict(lasso.fit, newdata=testing_data,type = "prob")[,2]) |>
    mutate( no = 1- yes)
  ## extract from the confusion matrix 
  result = confusionMatrix(dat$pred, dat$obs,positive = "yes")
  roc = twoClassSummary(dat, lev = classes)
  # Extract overall statistics and convert to tibble
  overall_stats = result$overall
  stats_tibble = as_tibble(list(Term = names(overall_stats), Value = overall_stats))
  
  # Extracting the class specific statistics if needed
  by_class_stats = result$byClass
  class_stats_tibble = as_tibble(list(Term= names(by_class_stats), Value = by_class_stats))
  
  combined_stats_tibble = bind_rows(stats_tibble, class_stats_tibble) |>
    rbind(tibble(Term = "AUC",
                 Value = roc[1])) |> 
    mutate(Value = round(Value,4))
  
  return(list(
    n = dim(testing_data),
    plot_lasso_tune = plot_lasso_tune,
    plot_estimate = plot_estimate,
    performance_matrix = combined_stats_tibble
  ))
  
  
  
}


