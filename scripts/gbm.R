mygbm <- function(data){
	```
	here use the gbm model to fit the score
	```
	out <- list()
	gbmGrid <- expand.grid(.interaction.depth=(1:5)*2, .n.trees=(1:5)*20, .shrinkage=.01, .n.minobsinnode = c(10))

	Mod <- train(value ~ .,
                   method = "gbm",
                   data = data,
                   verbose = F,
                   trControl = trainControl(method = "cv", number = 5, savePredictions = "all"),
                   tuneGrid = gbmGrid)


	bestT <- Mod$bestTune
	prevalue <- Mod$pred
	qdat <- subset(prevalue, n.trees==bestT$n.trees & interaction.depth==bestT$interacion.depth)
	out <- c(Mod, qdat)

	}
