Training = read.csv("~/Downloads/Training.txt")
Test = read.csv("~/Downloads/Test.txt")
Validation = read.csv("~/Downloads/Validation.txt")
head(Training)
y = Training$Occupancy
x1 = as.numerical(strptime(Training$date, "%Y-%m-%d %H:%M:%S"))
x2 = Training$Temperature
x3 = Training$Humidity
x4 = Training$Light
x5 = Training$CO2
x6 = Training$HumidityRatio

summary(regFit)
plot(x1, y)


#logistic regression 
model <- glm(y~x2+x3+x4+x5+x6,family=binomial,data= Training)
#determined from step to drop humidity
newmodel1 <- glm(Occupancy~Temperature+Light+CO2+HumidityRatio,family=binomial,data= Training)

summary(newmodel1)
par(mfrow=c(2,2))
plot(newmodel1)
par(mfrow=c(1,1))
plot(x2,y)
scatterplotMatrix(~Temperature+Humidity+Light+CO2+HumidityRatio | Occupancy, data=Training)

model.null = glm(y ~ 1, 
                 data=Training,
                 family = binomial(link="logit")
)
model.full = glm(y ~ x2 + x3 + x4 + x5 + x6,
                 data=Training,
                 family = binomial(link="logit")
)


summary(model.full)
log.predict= predict(newmodel1,Test)
prob=predict(model,type=c("response"))

pred = prediction(prob, Test$Occupancy) 
perf = performance(pred, measure = "auc")   
plot(perf)
library(pROC)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=Training)
#from the step procedure 
model.final = glm(y ~ x4 + x5 + x2 +x6, data=Training,
family = binomial(link="logit"),
na.action(na.omit)
)

summary(model.final)
Anova(model.final, type="II", test="Wald")
nagelkerke(model.final)

fit <- lda(Occupancy~Temperature+Light+CO2+HumidityRatio, data=Test, 
           na.action="na.omit")
fit
# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(Test$Occupancy, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

pairs(Validation[c("Temperature","Light","CO2","HumidityRatio")], main="LDA ", pch=22, 
      bg=c("red", "yellow", "blue", "green")[unclass(Validation$Occupancy)])

fit

lda.predict <- predict(fit, newdata = Validation)
lda.predict.posteriors <- as.data.frame(lda.predict$posterior)
# Evaluate the model
pred <- prediction(lda.predict.posteriors[,2], Validation$Occupancy)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
# Plot
plot(roc.perf, main = "ROC Plot of LDA")
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

partimat(as.factor(Occupancy)~Temperature+Light+CO2+HumidityRatio, data=Training, method = "lda")

#Trees

# Random forests with m=sqrt(p) and 100 trees
RF =randomForest(Occupancy~Temperature+Light+CO2+HumidityRatio, data=Validation,
                 mtry=4, importance =TRUE, ntree=201)

bestmtry <- tuneRF(Validation[c(-1,-3)],Validation$Occupancy, ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

# Prediction on test set
yhat.RF = predict (RF, newdata =Test)

adult.rf.pred = prediction(yhat.RF, Test$Occupancy)
adult.rf.perf = performance(adult.rf.pred,"tpr","fpr")
plot(adult.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
adult.rf.perf = performance(adult.rf.pred,"auc")
adult.rf.perf
# Plot prediction performance
voccupuancy=Test$Occupancy
plot(RF)
abline (0,1)
RF
# MSE on test set
MSE.RF=mean(( yhat.RF -voccupuancy)^2)
MSE.RF
####

m1 <- randomForest(
  formula = Occupancy ~Temperature+Light+CO2+HumidityRatio,
  data    = Validation
)
m1
plot(m1, main="RMSE by Number of Trees")
# number of trees with lowest MSE
which.min(m1$mse)
## [1] 438

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])
## [1] 0.0651001






###
occupancy.train.rf = randomForest(as.factor(Occupancy) ~Temperature+Light+CO2+HumidityRatio,
                                            data=Training,ntree=438, importance=TRUE)

occupancy.train.rf

importance(occupancy.train.rf)
###
tree =tree(as.factor(Occupancy) ~Temperature+Humidity+Light+CO2+HumidityRatio,
           data=Test)
summary(tree )
# Plot the generated tree
plot(tree )
text(tree, pretty = 0)
# Use the cv.tree() function to see whether pruning the tree will improve performance.
cv =cv.tree(tree)
plot(cv$size, cv$dev, type="b", xlab="Size", ylab="CV MSE", col="red", main="MSE of the Tree by Size")
#try with size 2 

# Use the cv selected tree size to prune the tree
prune =prune.tree(tree, best =2)
summary(prune)
plot(prune, main="Tree of Size 2")
text(prune,pretty =0)
# Calculate prediction error on the test set
yhat=predict (tree, Validation)
test= Validation$Occupancy
plot(yhat, test2)
abline (0,1)
# MSE on full test set
MSE.tree=mean((yhat -test)^2)
#on prune test set
yhat1=predict (prune,newdata=Validation)
test1=Validation$Occupancy
MSE.tree1=mean((yhat1 -test1)^2)
