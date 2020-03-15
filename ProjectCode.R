df=read.csv("Project_Data_Churn.csv")
head(df)
final_df <- na.omit(df)
View(df)
final_df[,1] <- list(NULL)
View(final_df)
final_df$SeniorCitizen <- as.factor(final_df$SeniorCitizen)
install.packages("ggplot2")
library(ggplot2)
M.plot<- ggplot(final_df)

bar_churn<- M.plot+ geom_bar(aes(x=Churn, y=((..count..)/sum(..count..)*100) ), fill= "blue")
bar_churn<-bar_churn+ggtitle("% of Customers by Churn") + ylab("% of CUSTOMERS")


bar_gender<- M.plot + geom_bar(aes(x=gender, fill = Churn ) , stat="count", width=0.7) + ggtitle("Churn by Gender")
bar_gender

bar_phone_senior<- M.plot+ geom_bar(aes(x=SeniorCitizen, fill = PhoneService), stat="count")
bar_phone_senior
bar_Int.Sen <- M.plot + geom_bar(aes(x=SeniorCitizen,y = ((..count..)/sum(..count..)*100), fill = InternetService))
bar_Int.Sen

facet_CC<- bar_Int.Sen + facet_wrap(Contract~Churn) 
facet_CC

theme_set(theme_grey(base_size = 18)) 

tenurebar<- ggplot(final_df,aes(Churn,tenure)) + geom_boxplot()+ theme(axis.title.y = element_text(size = 20))+ theme(axis.title.x =  element_text(size = 20)) +ggtitle("Tenure by Churn")

Mline.Tenure<-tenurebar +facet_wrap(.~MultipleLines)

Int.Tenure<-tenurebar +facet_wrap(.~InternetService)   

OnlSec.Tenure<-tenurebar +facet_wrap(.~OnlineSecurity)

OnBck.Tenure<-tenurebar +facet_wrap(OnlineSecurity~OnlineBackup)+ggtitle("Tenure by Churn- Online Security(top) and Online Backup (Bottom)")

DevProt.Tenure<-tenurebar +facet_wrap(.~DeviceProtection)

TechSup.Tenure<-tenurebar +facet_wrap(.~TechSupport)

MovieorTV.Tenure<-tenurebar +facet_wrap(StreamingTV~StreamingMovies)+ggtitle("Tenure Vs Streaming TV or Movies")

Contr.churn<- ggplot(final_df)+ geom_bar(aes(x=Contract, y=((..count..)/sum(..count..)*100), fill=Churn)) + ggtitle("% of Contract Types according to Churn") +ylab("% of customers")


Paperless.churn<-ggplot(final_df)+ geom_bar(aes(PaperlessBilling, fill=Churn))
Paperless.churn
Payment.churn<-ggplot(final_df)+ geom_bar(aes(PaperlessBilling, fill=Churn)) +facet_wrap(.~PaymentMethod) + ggtitle("Churning for Payment Method and Paperless Billing")
Payment.churn
Payment.churn2<-ggplot(final_df)+ geom_bar(aes(x=PaymentMethod,y = (..count..)/sum(..count..), fill=Churn))
Payment.churn2
Charges<- ggplot(final_df,aes(MonthlyCharges, TotalCharges)) + geom_point()
Charges
Charge.Tenure<-ggplot(final_df, aes(tenure, MonthlyCharges, colour=Churn))+geom_point()
Charge.Tenure + ggtitle("Pricing Strategy and Churn")                                       
Charge.Tenure2<-ggplot(final_df, aes(tenure, TotalCharges, colour=Churn))+geom_point()
Charge.Tenure2+ ggtitle("Total Charge Overview")
#............................................................................
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("caret")
library(caret)
rm(list=ls())
prop.table(table(final_df$Churn))
0.7*7032
final_df<-na.omit(final_df)
class.tree <- rpart(Churn ~ ., data = final_df, 
                    control = rpart.control(maxdepth = 6), method = "class")

prp(class.tree, type = 1, extra = 1, split.font = 3, varlen = -10)
set.seed(1)
train<-final_df[sample(7043,4922),]
set.seed(1)
Validation<-final_df[-sample(7043,4922),]
deeper.ct <- rpart(Churn ~ ., data = train, 
                   method = "class", cp = 0, minsplit = 1)
plot(deeper.ct)
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])  # count number of leaves
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  

default.ct <- rpart(Churn ~ ., data = train, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)
names(train)
train<-train[,-20]
Validation<-Validation[,-20]
pred.default <- predict(default.ct, data = Validation, type = "class")
summary(pred.default)

pred.deeper<-predict(deeper.ct, data = Validation , type = "class")

  summary(pred.deeper)
table(train$Churn)

train<-na.omit(train)
table(train$Churn, pred.train)
class(pred.train.deeper)
summary(pred.default)
View(pred.deeper)
unlist(pred.deeper)
table(train$Churn)
table(pred.default, train$Churn)
