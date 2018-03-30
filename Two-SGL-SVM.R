library(ROCR)
library(caret)
library(kernlab)
library(SGL)
y <- read.csv("D:/生物信息学/Two-class/D.csv",header=FALSE,sep=",")#导入数据集
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
y <- as.data.frame(lapply(y,normalize))
y <- as.matrix(y)
y0 <- c(rep(0,19),rep(1,58))
x_1 <- y[1:19,];x_2 <- y[20:77,];
#KW
i<-1;n2<-as.numeric(dim(y)[2])
h_3<-c(1:n2)
repeat
{	
  if(i>n2) break
else 
   {   
    h_3[i]<-as.numeric(kruskal.test(list(x_1[,i],x_2[,i]))[3]);
    i=i+1;
    }}
Pos <- (order(h_3,decreasing=F)[1:100]);y1 <- y[,Pos]
#SGL
data <- list(x=y1,y=y0)
sgl <- cvSGL(data ,index <- c(rep(1:100,each=1)),min.frac=0.4)
c2 <- c(1:20);i <- 1
for(i in 1:20)
{c2[i] <- length(Pos[which(sgl$fit$beta[,i]!=0)])}
c2
colnames(y[,Pos[which(sgl$fit$beta[,which(c2==8)]!=0)]])
#SVM(AUC)
n <- 10;T <- as.data.frame(y);
class <- c(rep("a",19),rep("b",58))
sglG <- colnames(y[,Pos[which(sgl$fit$beta[,which(c2==8)]!=0)]])
q <- T[,sglG];q <- as.data.frame(cbind(class,q));ave <- c();auc <- c()
for(l in 1:40)#内部循环1-40
  {folda <- createFolds(q[,1],k=n)
  lop <- str(folda);i=1;
  repeat
  {if(i>n) break
    else 
    {test <- q[folda[[i]],];train <- q[-folda[[i]],];
    A1 <- ksvm(class~.,data=train,kernel="rbfdot",prob.model=TRUE)
    A2 <- predict(A1,test,type="response")
    result<-predict(A1,test,type="probabilities")
    #list<-cbind(A2,test$class)
    pred<-prediction(predictions=result[,2],labels=test$class)
    perd<-performance(pred,measure="tpr",x.measure="fpr")
    plot(perd,main="ROC curve for SMS spam filter",col="blue",lwd=2)
    abline(a=0,b=1,lwd=2,lty=2)
    perf.auc<-performance(pred,measure="auc")
    #str(perf.auc)
    #unlist(perf.auc@y.values)
    auc[i] <- unlist(perf.auc@y.values)
    table(A2,test$class)
    agreement <- A2==test$class
    f1 <- table(agreement)
    #print(prop.table(table(agreement)))
    ave[i] <- length(which(agreement==TRUE))/length(agreement)
    i <- i+1;}};mean(ave);mean(auc)
  ACC[l] <- mean(ave);AUC[l] <- mean(auc)}