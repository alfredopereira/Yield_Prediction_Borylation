library("caret") 
library("MLmetrics")
library("doParallel") 
library("dplyr")
library("ggpubr")

########################################################################

cl <- makePSOCKcluster(30)
registerDoParallel(cl)

########################################################################

mlProcess <- function(dataset,folder){
  set.seed(7) 

  dataML <- read.csv(dataset, check.names = FALSE)
  dataML <- dataML %>% rename (class = yield)
  
  dir.create(folder)
  
  algorithm <- c( "rf" )
  algorithmName <- c( "RF" )
  
  tunegrid <- expand.grid(.mtry = c(2,5,10,15,20,25,30,40,50,
                                    60,70,80,90,100)) 
  
  trainControl <- trainControl(
    #method = 'cv',
    method = 'repeatedcv', 
    number = 10,
    repeats = 5,
    #search = 'random',
    classProbs = T,
    savePred = "final",
    #summaryFunction = twoClassSummary
  )
  
  training.samples = createFolds(dataML$class, k = 5, returnTrain = TRUE)
  #training.samples = createMultiFolds(dataML$class, k = 10, times = 5)
  
  for(fold in 1:length(training.samples)){
  
    train.data  <- dataML[training.samples[[fold]], -1]
    test.data <- dataML[-training.samples[[fold]], -1]
    
    test.idSample <- dataML[-training.samples[[fold]], 1]
    
    nCol <- ncol(train.data)
  
    set.seed(7)

    mlModel <- train(
                    train.data[,1:(nCol-1)], train.data$class,
                    trControl = trainControl, 
                    method = algorithm,
                    #tuneLength = 1,
                    tuneGrid = tunegrid,
                    metric = "Rsquared",
                    preProcess = c("center","scale")
                  ) 
      
    print(mlModel)
    saveRDS(mlModel, paste(folder, "/mlModel_fold", fold, ".rds", sep = ""))
    
    param <- mlModel$bestTune
  
    predicted <- predict(mlModel, test.data[,1:(nCol-1)])

    idSample <- as.character(test.idSample) 
    actual <- as.numeric(test.data$class) 
    predicted <- as.numeric(predicted)
    
    mae = MAE(actual, predicted)
    mse = MSE(actual, predicted)
    rmse = RMSE(actual, predicted)
    r2 = R2(actual, predicted)
    
    metrics <- data.frame(mae,mse,rmse,r2)
  
    write.csv(as.matrix(cbind(idSample, actual, predicted)),
              file = paste(folder,"/predictions_fold", fold, ".csv", sep = ""),
              row.names = FALSE, quote = FALSE)
      
    write.csv(metrics, file = paste(folder,"/metrics_fold", fold, ".csv", 
                                    sep = ""), 
            row.names = TRUE, quote = FALSE)

    write.csv(param, file = paste(folder,"/param_fold", fold, ".csv", 
                                  sep = ""), 
              row.names = TRUE, quote = FALSE)
    
    varImp <- as.data.frame(mlModel[["finalModel"]][["importance"]])
    varImp$Features <- rownames(varImp)
    varImp <- varImp[order(varImp$IncNodePurity, decreasing = TRUE),] 
    write.csv(varImp, file = paste(folder,"/varImp_fold", fold, ".csv",
                                  sep = ""),
              row.names = TRUE, quote = FALSE)
    
    varImpPlot <- ggbarplot(varImp[1:20,], "Features", "IncNodePurity",
              fill = "steelblue2", color = "steelblue2", alpha = 0.5, 
              orientation = "horiz", sort.val = "asc" )
    
    plot <-
      ggpar(
        varImpPlot,
        ggtheme = theme_pubr(),
        font.x = c(15, "plain", "black"),
        font.y = c(15, "plain", "black"),
        font.tickslab = c(12, "plain", "black")
      ) +
      border()
    
    ggsave(file = paste(folder,"/varImpPlot_fold", fold, ".svg", sep = ""), 
           plot = plot, width = 6, height = 5)
    
    ggsave(file = paste(folder,"/varImpPlot_fold", fold, ".png", sep = ""),  
           plot = plot, width = 6, height = 5, dpi = 600)
  }
}

################################################################################

dataset <- "../5_datasets/Co_dataset_reg.csv"
mlProcess(dataset,"ML_Co_dataset_reg")

dataset <- "../5_datasets/Ni_dataset_reg.csv"
mlProcess(dataset,"ML_Ni_dataset_reg")

################################################################################

stopCluster(cl)

