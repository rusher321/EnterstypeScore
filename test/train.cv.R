# compute the gbm cv
library(gbm)

mygbm <- function(x){
  gbmGrid <- expand.grid(.interaction.depth=(1:5)*2, .n.trees=(1:5)*20, .shrinkage=.01, .n.minobsinnode = c(10))
  data <- data.frame(phe_rm2, value=x)
  Mod2 <- train(value ~ ., 
                   method = "gbm", 
                   data = data_rm, 
                   verbose = F, 
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = gbmGrid)
  out <- c(median(Mod2$results$Rsquared),sd(Mod2$results$Rsquared))
  return(out)
}
