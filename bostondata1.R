library(caret)
library(e1071)
tr.control <- trainControl(method="cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001)
tr <- train(MEDV ~ LAT + LON + CRIM +ZN +INDUS+
              CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,
            data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata = test)
best.tree.sse <- sum((best.tree.pred- test$MEDV)^2)
best.tree.sse
linreg.sse