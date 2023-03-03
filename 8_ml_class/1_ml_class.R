library("caret") 
library("doParallel") 
library("dplyr")
library("ggpubr")

########################################################################

cl <- makePSOCKcluster(30)
registerDoParallel(cl)

########################################################################

mlProcess <- function(dataset,folder,classes){
  set.seed(7) 
  
  postiveClass <- classes[1]
  
  dataML <- read.csv(dataset, check.names = FALSE)
  dataML <- dataML %>% rename (class = yield)
  dataML$class <- factor(dataML$class, levels = classes)
  
  dir.create(folder)
  
  predictedDf <- data.frame()
  
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
    
    if(length(classes)==2){
      metric = "ROC"
    }
    if(length(classes)==3){
      metric = "Accuracy"
    }
    
    mlModel <- train(
                    train.data[,1:(nCol-1)], train.data$class,
                    trControl = trainControl, 
                    method = algorithm,
                    #tuneLength = 10,
                    tuneGrid = tunegrid,
                    metric = metric,
                    preProcess = c("center","scale")
                  ) 
      
    print(mlModel)
    saveRDS(mlModel, paste(folder, "/mlModel_fold", fold, ".rds", sep = ""))
    
    param <- mlModel$bestTune
  
    predicted <- predict(mlModel, test.data[,1:(nCol-1)])
    predictedProb <- predict(mlModel, test.data[,1:(nCol-1)], type = "prob")
  
    cm <- confusionMatrix(table(predicted, test.data$class),
                            positive = postiveClass)
    
    if(length(classes)==2){
      acc <- cm$overall[1]
      sens <- cm$byClass[1]
      spec <- cm$byClass[2]
      metrics <- data.frame(c(acc, sens, spec))
      colnames(metrics) <- "Values"
    }
    if(length(classes)==3){
      acc <- cm$overall[1]
      sens <- c(cm$byClass[1],cm$byClass[2],cm$byClass[3])
      metrics <- data.frame(c(acc, sens))
      colnames(metrics) <- "Values"
      rownames(metrics) <- c("acc","sens_H","sens_M","sens_L")
    }
    
    idSample <- as.character(test.idSample) 
    actual <- as.character(test.data$class) 
    predicted <- as.character(predicted)
  
    write.csv(as.matrix(cbind(idSample, actual, predicted, predictedProb)),
              file = paste(folder,"/predictions_fold", fold, ".csv", sep = ""),
              row.names = FALSE, quote = FALSE)
      
    write.csv(metrics, file = paste(folder,"/metrics_fold", fold, ".csv", 
                                    sep = ""), 
            row.names = TRUE, quote = FALSE)
    write.csv(cm$table, file = paste(folder,"/cm_fold", fold, ".csv", 
                                     sep = ""), 
              row.names = TRUE, quote = FALSE)
    write.csv(param, file = paste(folder,"/param_fold", fold, ".csv", 
                                  sep = ""), 
              row.names = TRUE, quote = FALSE)
    
    Group <- matrix(data = paste(algorithmName, "_fold", fold, sep = ""),
                    nrow = length(actual), ncol = 1)
    predictedDf_aux <- data.frame(cbind(predictedProb, actual, Group))
    predictedDf <- rbind(predictedDf, predictedDf_aux)
    
    varImp <- as.data.frame(mlModel[["finalModel"]][["importance"]])
    varImp$Features <- rownames(varImp)
    varImp <- varImp[order(varImp$MeanDecreaseGini, decreasing = TRUE),] 
    write.csv(varImp, file = paste(folder,"/varImp_fold", fold, ".csv",
                                  sep = ""),
              row.names = TRUE, quote = FALSE)
    
    varImpPlot <- ggbarplot(varImp[1:20,], "Features", "MeanDecreaseGini",
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
  
  if(length(classes)==2){
    colnames(predictedDf) <- c(classes[1],classes[2],"obs","Group")
  }
  if(length(classes)==3){
    colnames(predictedDf) <- c(classes[1],classes[2],classes[3],"obs","Group")
  }
  
  saveRDS(predictedDf,  paste(folder,"/predictedDf.rds", sep = ""))
  
  for (i in 1:5){
    predictedDf$Group[
      predictedDf$Group==
        paste(algorithmName,"_fold", i, sep = "")] <- algorithmName
  
  }
  saveRDS(predictedDf,  paste(folder,"/predictedDfGeneral.rds", sep = ""))
}

################################################################################

dataset <- "../5_datasets/Co_50dataset_2class.csv"
mlProcess(dataset,"ML_Co_50dataset_2class", c("H","L"))

dataset <- "../5_datasets/Co_33dataset_2class.csv"
mlProcess(dataset,"ML_Co_33dataset_2class",c("H","L"))

dataset <- "../5_datasets/Co_33dataset_3class.csv"
mlProcess(dataset,"ML_Co_33dataset_3class", c("H","M","L"))

dataset <- "../5_datasets/Ni_50dataset_2class.csv"
mlProcess(dataset,"ML_Ni_50dataset_2class", c("H","L"))

dataset <- "../5_datasets/Ni_33dataset_2class.csv"
mlProcess(dataset,"ML_Ni_33dataset_2class", c("H","L"))

dataset <- "../5_datasets/Ni_33dataset_3class.csv"
mlProcess(dataset,"ML_Ni_33dataset_3class", c("H","M","L"))

################################################################################

stopCluster(cl)

