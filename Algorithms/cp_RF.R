library(randomForest)
library(caTools)
library(caret)
f<-read.csv("train.csv")
head(f)
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g<- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)
#spliting it into train and test data----
set.seed(123)
split<-sample.split(f,SplitRatio=0.8)
tr_data<-subset(f,split==TRUE)
ts_data<-subset(f,split==FALSE)
#Building Random Forest classifier
rf <- randomForest(price_range ~ .,data=tr_data, importance=TRUE)
importance(rf)
plot(rf)
#Predicting 
pred = predict(rf, ts_data)
#pred = as.numeric(pred)
cm = table(ts_data$price_range, pred)
matrix <- confusionMatrix(cm, mode = "everything", positive = "1")


#ROC
pred <- as.integer(pred)
tr_data$price_range<- as.integer(tr_data$price_range)
s4 <- roc(ts_data$price_range, pred)
s4
#For Adding Two ROC Curves
#For Adding Two ROC Curves
plot(s1,col="red", main="ROC Curve")

plot(s4,col="black",add=TRUE)

legend(0.3,0.65,c("MLR","RF"),lty=1,col=c("red","black"),title="Graph type",cex=0.6)
