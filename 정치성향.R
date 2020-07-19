library(dplyr)
choose.files()
data <- "C:\\Users\\USER-PC\\Desktop\\모바일_뉴스_이용_형태.csv"
data <- read.csv(data, stringsAsFactors = F)

data[["정치적성향"]] <- gsub(1,0,data[["정치적성향"]])
data[["정치적성향"]] <- gsub(2,0,data[["정치적성향"]])
data[["정치적성향"]] <- gsub(4,1,data[["정치적성향"]])
data[["정치적성향"]] <- gsub(5,1,data[["정치적성향"]])
data1 <- data[!(data$정치적성향==3),]
data1[["정치적성향"]] <-as.numeric(data1[["정치적성향"]])

data1$정치적성향 %>% table()
195+325
#---------------------------------------

training.ratio <- 0.6
training.data <- data1[0,]
data.size <- nrow(data1)
training.size <- data.size * training.ratio
training.idx <- sample(1:data.size, training.size)
training.data <- rbind(training.data, data1[training.idx,])
training.data$"정치적성향" %>% table()



  
#오버샘플링, 단순 리샘플링
tb <- training.data$"정치적성향" %>% table()
over.n <- tb[2]-tb[1]
training.over.idx <- sample(1:tb[2], over.n, T)
training.data.1 <- data1 %>% filter(정치적성향==0)
training.data <- rbind(training.data, training.data.1[training.over.idx,])
training.data$"정치적성향" %>% table()


test.data <- data1[0,]
test.data <- rbind(test.data, data1[-training.idx,])
test.data$"정치적성향" %>% table()
#-----------------------------


#로지스틱 회귀모형
result.glm <- glm(정치적성향~., data=training.data, family = binomial)
result.glm

result.glm.step <- step(result.glm)
result.glm.step %>% summary()

predic.glm <- predict(result.glm.step, test.data, type ="response")
predic.glm.politics <- ifelse(predic.glm<0.5, 0, 1)
table(test.data$정치적성향, predic.glm.politics)

#오버샘플링하기 전 
#0=보수 1=진보 
#정확도 0.6865385
(73+284)/(122+41+73+284)
#민감도 0.374359
(73)/(73+122)
#특이도 0.8738462 
(284)/(41+284)

#오버샘플링 후 
#0=보수 1=진보
#정확도 62%
(50+79)/(50+33+46+79)
#민감도 60%
50/(50+33)
#특이도 63%
79/(46+79)


#의사결정나무
install.packages("party")
install.packages("rpart")
library(party)
library(rpart)
tree <- ctree(정치적성향~., data=training.data)
plot(tree)

tree.predic <- predict(tree, newdata= test.data, type="response")
tree.predic <- ifelse(tree.predic<0.5,0,1)
table(tree.predic, test.data$"정치적성향")

#정확도 65%
(35+102)/(35+23+48+102)
#민감도 60%
35/(35+23)
#특이도 68%
102/(48+102)

#SVM
library(e1071)
tuned <- tune.svm(정치적성향~., data=training.data, gamma=c(0.01,0.1,1),
                  cost=c(1,10,100))
tuned
#gamma 1 cost 10
result.svm <- svm(정치적성향~., data=training.data, type="C-classification"
                   , cost=10, gamma=1)

svm.predic <- predict(result.svm, test.data, decision.values = TRUE, 
                       type="response")
table(test.data$정치적성향, svm.predic)
#정확도 0.7548
(30+127)/(30+49+2+127)
#민감도 0.3797
30/(30+49)
#특이도 0.9844
127/129

#정확도 90%
(64+125)/(64+19+125)
#민감도 77%
64/(64+19)
#특이도 100%
64/(64+19)

install.packages("pROC")
library(pROC)
roc.glm_bio <- roc(test.data$정치적성향, predic.glm)
roc.glm_tree <- roc(test.data$정치적성향, tree.predic)

pred.values<-attr(svm.predic, "decision.values")
roc.glm_SVM <- roc(test.data$정치적성향, pred.values)

?roc
plot.roc(roc.glm_bio,print.auc=T, col="red", main= "ROC curve")
plot.roc(roc.glm_tree,print.auc=T, col="blue", add=TRUE,print.auc.adj=c(1,0) )
plot.roc(roc.glm_SVM,print.auc=T, col="green", add=TRUE,print.auc.adj=c(2,2) )


