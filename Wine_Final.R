#Wine Testing and Quality Prediction Analysis
wine_quality <- read.csv("C:/Users/mayan/OneDrive/Desktop/wine_quality.csv")
str(wine_quality)
summary(wine_quality)
colnames(wine_quality)
library(ggplot2)
library(colorspace)
library(tidyverse)
library(plotrix)
library(psych)
library(ISLR)
library(leaps)
library(dplyr)

#Checking dimensions of the Wine dataset
dim(wine_quality)

#checking if there is any missing (NA) values
sum(is.na(wine_quality))
wine_quality <- na.omit(wine_quality)
sum(is.na(wine_quality))
view(wine_quality)

attach(wine_quality)

#Plot of free sulfur dioxide vs total sulfur dioxide
ggplot(data = wine_quality)+
  geom_point(aes(x=free_sulfur_dioxide, y=total_sulfur_dioxide, colour=Type))+xlim(0,80)+xlab("Free Sulfur Dioxide")+
  ylab("Total Sulfur Dioxide")+ggtitle("Graph of Free Sulfur Dioxide v/s Total Sulfur Dioxide")

#Plot of fixed acidity vs Density of wine
ggplot(data = wine_quality, mapping = aes(x = fixed_acidity, y =density )) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = Type), size = 3)

#Data Visualization Using different plots

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(fixed_acidity, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 2, ncol = 2)+xlab("Distribution of Fixed Acidity")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(volatile_acidity, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Volatile Acid")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(citric_acid, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Citric Acid")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(residual_sugar, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Residual Sugar")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(chlorides, fill = Type), position = position_dodge2(preserve= "single", padding = 0.1, reverse = FALSE))+
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Chlorides")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(free_sulfur_dioxide, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Free Sulphur Dioxide")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(total_sulfur_dioxide, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 3)+xlab("Distribution of Total Sulphur Dioxide")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(density, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Density")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(pH, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of pH")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(sulphates, fill = Type), position = position_dodge2(preserve = "single", padding = 0.5)) +
  xlab("Distribution of Sulphates")

ggplot(data = wine_quality)+
  geom_histogram(mapping = aes(alcohol, fill = Type), position = position_dodge2(preserve = "single", padding = 0.1)) +
  facet_wrap(vars(Type), nrow = 5, ncol = 2)+xlab("Distribution of Alcohol Content")

ggplot(data = wine_quality)+
  geom_bar(aes(factor(quality), fill = Type))+xlab("Distribution of Quality")

ggplot(aes(factor(quality), fixed_acidity), data = wine_quality) +
  geom_boxplot()+
  geom_jitter(alpha = 0.05)  +
  geom_smooth(aes(quality-2,fixed_acidity), method = 'lm', color = 'red', se=FALSE) + 
  xlab('quality') + 
  ylab('fixed_acidity') +
  ggtitle('Quality vs fixed_acidity of Wine')

cor.test(wine_quality$quality, wine_quality$fixed_acidity, method = 'pearson')

#Box plots of wine type on the basis of various testing parameters
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=fixed_acidity, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of fixed acidity ")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=volatile_acidity, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Volatile Acidity")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=citric_acid, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Citric Acid")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=residual_sugar, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Residual Sugar")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=chlorides, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Chlorides")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=free_sulfur_dioxide, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Free Sulfur Dioxide")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=total_sulfur_dioxide, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Total Sulfur Dioxide")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=density, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Density")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=pH, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of pH")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=sulphates, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Sulphates")
ggplot()+
  geom_boxplot(aes(x=factor(quality), y=alcohol, fill=Type))+xlab("Wine Quality")+ggtitle("Boxplot of Alcohol")

#Jitter plots of Wines on the basis of their testing parameters

ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=fixed_acidity),
                                      position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=volatile_acidity),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=citric_acid),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=residual_sugar),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=chlorides),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=free_sulfur_dioxide),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=total_sulfur_dioxide),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=density),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=pH),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=sulphates),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=factor(quality), y=alcohol),
                                       position = position_jitter(width=0.3))
ggplot(data = wine_quality)+geom_point(mapping=aes(x=Type, y=quality),
                                       position = position_jitter(width=0.3))

#Regression of Wine Quality Analysis
attach(wine_quality)
ggplot()+  geom_point(aes(y=quality, x=fixed_acidity))

model = lm(data=wine_quality, quality~fixed_acidity)
model
summary(model)
ggplot(data=wine_quality) +
  geom_point(aes(y=quality, x=fixed_acidity))+
  xlab("Alcohol Cotent") + ylab("Wine Quality")+geom_smooth(method = "lm", aes(y=density, x=fixed_acidity), se=FALSE)
library(MASS)
library(dplyr)

value = dplyr::select(wine_quality, fixed_acidity, volatile_acidity, citric_acid, residual_sugar, chlorides, free_sulfur_dioxide, total_sulfur_dioxide, density, pH, sulphates, alcohol, quality)
best=regsubsets(quality~., data=value,nvmax = 25)
summary(best)
summary(best)$rsq

#p=1
m1=lm(quality~alcohol)
m2=lm(quality~alcohol+volatile_acidity)
m3=lm(quality~alcohol+volatile_acidity+sulphates)
m4=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar)
m5=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide)
m6=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide)
m7=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides)
m8=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH)
m9=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH+fixed_acidity)
m10=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH+fixed_acidity+density)
m11=lm(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH+fixed_acidity+density+citric_acid)
summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
summary(m4)$r.squared
summary(m5)$r.squared
summary(m6)$r.squared
summary(m7)$r.squared
summary(m8)$r.squared
summary(m9)$r.squared
summary(m10)$r.squared
summary(m11)$r.squared

#List of models obtained from best subset selection method
#(quality~alcohol)
#(quality~alcohol+volatile_acidity)
#(quality~alcohol+volatile_acidity+sulphates)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH+fixed_acidity)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH+fixed_acidity+density)
#(quality~alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+chlorides+pH+fixed_acidity+density+citric_acid)

m12=lm(quality~alcohol+sulphates+residual_sugar+free_sulfur_dioxide+fixed_acidity+pH)
summary(m12)$r.squared
best2 = regsubsets(quality~poly(alcohol,3)+poly(volatile_acidity,3)+ poly(sulphates,3)+ poly(residual_sugar,5)+
                    poly(total_sulfur_dioxide, 2)+poly(free_sulfur_dioxide,3)+poly(chlorides,4)+poly(pH,3)+poly(fixed_acidity,5)+poly(density,3)+poly(citric_acid,3), data = value, nvmax = 11)
summary(best2)$rsq
summary(best2)$adjr2
summary(best2)$cp
summary(best2)$bic

coef(best2, 11, raw=TRUE)

x=seq(1,11, by=1)
y=summary(best2)$rsq
best2Res=data.frame(x,y)
ggplot(aes(x=x,y=y), data=best2Res)+geom_point()+geom_line()

x=seq(1,11, by=1)
y=summary(best2)$cp
cpbest2=data.frame(x,y)
ggplot(aes(x=x,y=y), data=cpbest2)+geom_point()+geom_line()+ylab("cp value")+xlab("Model Number")+scale_x_continuous(breaks = c(seq(0,11)))

x=seq(1,11, by=1)
y=summary(best2)$bic
bicbest2=data.frame(x,y)
ggplot(aes(x=x,y=y), data=bicbest2)+geom_point()+geom_line()+ylab("bic value")+xlab("model number")+scale_x_continuous(breaks = c(seq(0,11)))

x=seq(1,11, by=1)
y=summary(best2)$adjr2
adjr2best=data.frame(x,y)
ggplot(aes(x=x,y=y), data=adjr2best)+geom_point()+geom_line()+ylab("adjr2 value")+xlab("model number")+scale_x_continuous(breaks = c(seq(0,11)))

forward = regsubsets(quality~., method = "forward", data=value)
summary(forward)

summary(forward)$rsq
forward=regsubsets(quality~.+poly(citric_acid, 5, raw = TRUE)-citric_acid, method = "forward", data=value, nvmax=11)
summary(forward)

summary(forward)$rsq
summary(forward)$cp
summary(forward)$bic
summary(forward)$adjr2

#List of Model given by Forward Subset Selection
#1-alcohol
#2-alcohol+volatile_acidity
#3-alcohol+volatile_acidity+sulphates
#4-alcohol+volatile_acidity+sulphates+residual_sugar
#5-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide
#6-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide
#7-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+citric_acid^3
#8-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+citric_acid^3+chlorides
#9-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+citric_acid^3+chlorides+pH
#10-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+citric_acid^3+chlorides+pH+density
#11-alcohol+volatile_acidity+sulphates+residual_sugar+total_sulfur_dioxide+free_sulfur_dioxide+citric_acid^3+chlorides+pH+density+fixed_acidity

x=seq(1,11, by=1)
y=summary(forward)$bic
bicforward=data.frame(x,y)
ggplot(aes(x=x,y=y), data=bicforward)+geom_point()+geom_line()+xlab("Index")+ylab("summary(forward)$bic")

x=seq(1,11, by=1)
y=summary(forward)$cp
cpforward=data.frame(x,y)
ggplot(aes(x=x,y=y), data=cpforward)+geom_point()+geom_line()+xlab("Index")+ylab("summary(forward)$cp")

x=seq(1,11, by=1)
y=summary(forward)$adjr2
adjr2forward=data.frame(x,y)
ggplot(aes(x=x,y=y), data=adjr2forward)+geom_point()+geom_line()+xlab("Index")+ylab("summary(forward)$adjr2")

#Classification 

attach(wine_quality)
ggplot()+geom_histogram(aes(x=alcohol, fill = Type), position = position_dodge2(preserve = "single", padding = 2, width = .1))

ggplot()+geom_point(aes(x=alcohol,y=density, colour = Type))

ggplot(data = wine_quality)+geom_point(aes(x=quality, y=alcohol, colour=Type), 
                                      position = position_jitter(width=0.3))

ggplot()+geom_bar(aes(x=factor(quality), fill = Type))+xlab("Wine Quality")+ylab("Frequency")+ggtitle("Barplot of Wine Quality based on Type")

newQuality = mutate(wine_quality, plottingColour =
                   ifelse(grepl("WHITE", Type), "blue",
                          ifelse(grepl("RED", Type), "red", "other")))
summary(newQuality)
view(newQuality)
summary(wine_quality)

library(GGally)
ggpairs(wine_quality, aes(colour=Type), binwidth = 30)

library(MASS)

Wine1 = lda(Type~fixed_acidity, data = wine_quality)
Wine1

perfPred=predict(Wine1,wine_quality)

#Confusion Matrix
table(perfPred$class, wine_quality$Type)

IDwine = mutate(wine_quality, id=row_number())
view(IDwine)
trainWine = sample_frac(IDwine, .7)
view(trainWine)
testWine = anti_join(IDwine, trainWine, by="id")
view(testWine)

performModel = lda(Type~alcohol+volatile_acidity, data=trainWine)
performTrainPred = predict(performModel, trainWine)
table(performTrainPred$class, trainWine$Type)

performTestPred = predict(performModel, testWine)
table(performTestPred$class, testWine$Type)

#Model Training
modelOnTrain = lda(Type~alcohol, data = trainWine)
trainPred = predict(modelOnTrain, trainWine)
table(trainPred$class, trainWine$Type)
#Model has an error rate of (1380/4538 = 30.41%) on training set

testPred = predict(modelOnTrain, testWine)
table(testPred$class, testWine$Type)
#Model has an error rate of (571/1945 = 29.35%) on test data set

#Subset Selection for Classification Model
#Classification : Predicting a group/set

#models with p = 1
attach(wine_quality)
model1 = lda(data = wine_quality, Type~fixed_acidity)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9162 ****

model2 = lda(data = wine_quality, Type~volatile_acidity)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6989

model3 = lda(data = wine_quality, Type~citric_acid)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6989

model4 = lda(data = wine_quality, Type~residual_sugar)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6989

model5 = lda(data = wine_quality, Type~chlorides)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6989

model6 = lda(data = wine_quality, Type~free_sulfur_dioxide)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6992

model7 = lda(data = wine_quality, Type~total_sulfur_dioxide)
pred = predict(model7, as.factor(model7$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6982

model8 = lda(data = wine_quality, Type~density)
pred = predict(model8, as.factor(model8$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.7178

model9 = lda(data = wine_quality, Type~pH)
pred = predict(model9, as.factor(model9$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.7089

model10 = lda(data = wine_quality, Type~sulphates)
pred = predict(model10, as.factor(model10$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6989

model11 = lda(data = wine_quality, Type~alcohol)
pred = predict(model11, as.factor(model11$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6990

model12 = lda(data = wine_quality, Type~quality)
pred = predict(model12, as.factor(model12$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.6989

#Models with p=2

model1 = lda(data = wine_quality, Type~fixed_acidity+volatile_acidity)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9145

model2 = lda(data = wine_quality, Type~fixed_acidity+citric_acid)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9202

model3 = lda(data = wine_quality, Type~fixed_acidity+residual_sugar)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9196

model4 = lda(data = wine_quality, Type~fixed_acidity+chlorides)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9164

model5 = lda(data = wine_quality, Type~fixed_acidity+free_sulfur_dioxide)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9131

model6 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9206 *****

model7 = lda(data = wine_quality, Type~fixed_acidity+density)
pred = predict(model7, as.factor(model7$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9154

model8 = lda(data = wine_quality, Type~fixed_acidity+pH)
pred = predict(model8, as.factor(model8$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9045

model9 = lda(data = wine_quality, Type~fixed_acidity+sulphates)
pred = predict(model9, as.factor(model9$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9173

model10 = lda(data = wine_quality, Type~fixed_acidity+alcohol)
pred = predict(model10, as.factor(model10$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9049

model11 = lda(data = wine_quality, Type~fixed_acidity+quality)
pred = predict(model11, as.factor(model11$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9083

#Models with p=3

model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9134 ****

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+citric_acid)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9065

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+residual_sugar)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9130

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+chlorides)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9040

model5 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+free_sulfur_dioxide)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9057

model6 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+density)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9117

model7 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+pH)
pred = predict(model7, as.factor(model7$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9003

model8 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+sulphates)
pred = predict(model8, as.factor(model8$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9051

model9 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+alcohol)
pred = predict(model9, as.factor(model9$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9035

model10 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+quality)
pred = predict(model10, as.factor(model10$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9063

#Models with p=4

model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+citric_acid)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9048

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+residual_sugar)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9125

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+chlorides)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9139

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+free_sulfur_dioxide)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9151

model5 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9156 *****

model6 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+pH)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9022

model7 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+pH)
pred = predict(model7, as.factor(model7$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9022

model8 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+sulphates)
pred = predict(model8, as.factor(model8$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9117

model9 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+alcohol)
pred = predict(model9, as.factor(model9$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9117

model10 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+quality)
pred = predict(model10, as.factor(model10$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9134

#Models with p=5

model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+citric_acid)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9037

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+residual_sugar)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9136

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9154 *****

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+free_sulfur_dioxide)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9150

model5 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+pH)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9015

model6 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+residual_sugar)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9136

model7 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+sulphates)
pred = predict(model7, as.factor(model7$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9079

model8 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+alcohol)
pred = predict(model8, as.factor(model8$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9116

model9 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+quality)
pred = predict(model9, as.factor(model9$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9140

#Models with p=6

model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+citric_acid)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9042

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+residual_sugar)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9133

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+free_sulfur_dioxide)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9150

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+pH)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9011

model5 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+sulphates)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9086

model6 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+alcohol)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9108

model7 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality)
pred = predict(model7, as.factor(model7$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9154 *****

#Models with p=7
model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+citric_acid)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9049

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+residual_sugar)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9130

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9149 ****

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+pH)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9022

model5 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+sulphates)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9086

model6 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+alcohol)
pred = predict(model6, as.factor(model6$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9080

#Models with p=8

model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+citric_acid)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9057

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9136 *****

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+pH)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9037

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+sulphates)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9105

model5 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+alcohol)
pred = predict(model5, as.factor(model5$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9062

#Models with p=9

model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9076 ****

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+pH)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9049

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+sulphates)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9074

model4 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+alcohol)
pred = predict(model4, as.factor(model4$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9031

#Models with p=10
model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+pH)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.8972

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.9040 *****

model3 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+alcohol)
pred = predict(model3, as.factor(model3$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.8988

#Models with p=11
model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates+pH)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.8989

model2 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates+alcohol)
pred = predict(model2, as.factor(model2$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.8994 ******

#Models with all predictors
model1 = lda(data = wine_quality, Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates+alcohol+pH)
pred = predict(model1, as.factor(model1$Type))
table(pred$class, wine_quality$Type)
mean(pred$class == wine_quality$Type)
#0.8974

#Cross-Validation for Classification model for Wine Quality Data

#Adding this piece for randomization
wineMix = slice(wine_quality, sample(1:n()))
id=seq(1,nrow(wine_quality), by=1)
wineRandom = mutate(wineMix, id)

#Automate the process for K-fold Analysis
attach(wine_quality)
k=10
numRows=nrow(wine_quality)
errors = rep(0,k)
totalErrors=0

numModels=12
modelnames = rep(0, numModels)
modelnames[1] = "Type~fixed_acidity"
modelnames[2] = "Type~fixed_acidity+total_sulfur_dioxide"
modelnames[3] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity"
modelnames[4] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density"
modelnames[5] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides"
modelnames[6] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality"
modelnames[7] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide"
modelnames[8] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar"
modelnames[9] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid"
modelnames[10] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates"
modelnames[11] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates+alcohol"
modelnames[12] = "Type~fixed_acidity+total_sulfur_dioxide+volatile_acidity+density+chlorides+quality+free_sulfur_dioxide+residual_sugar+citric_acid+sulphates+alcohol+pH"

j=numModels
wineModel = lda(eval(parse(text = paste(modelnames[j])), train))
wineModel

wineMix = slice(wine_quality, sample(1:n()))
wineRandom = mutate(wineMix, id)
errors=matrix(1:numModels*k, 
              dimnames = list(seq(1, numModels, by=1), seq(1,k,by=1)),
              nrow= numModels, ncol = k)
view(errors)
avgError = rep(0,numModels)
for (j in 1:numModels) {
  for(i in 1:k){
    errors[j,i]=0
  }
}

for(j in 1:numModels){
  k = 10
  numRows = nrow(wine_quality)
  totalErrors = rep(0, k)
  for(i in 1:k){
    test = filter(wineRandom, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
    train = anti_join(wineRandom, test, by="id")
    model = lda(eval(parse(text=paste(modelnames[j])), train))
    modelGuesses = predict(model, test)
    #calc the error for this fold (i) of model j
    errors[j,i] = 1-mean(modelGuesses$class == test$Type)
    #keep running some of the the k errors (indexed by i) for this model j
    totalErrors[i] = errors[j,i]+totalErrors[i]
  }
}
mean(errors[1,])
mean(errors[2,])
mean(errors[3,])
mean(errors[4,])
mean(errors[5,])
mean(errors[6,])
mean(errors[7,])
mean(errors[8,])
mean(errors[9,])
mean(errors[10,])
mean(errors[11,])
mean(errors[12,])

avgError = totalErrors/k
avgError
stdError = sqrt(var(errors[6,])/k)
stdError

x=seq(1, numModels, by=1)
y=rep(0,numModels)
bars=rep(0,numModels)

for(i in 1:numModels){
  y[i] = mean(errors[i, ])
  bars[i] = sqrt(var(errors[i,])/k)
}
wineData = data.frame(x, y, bars)
View(wineData)
ggplot(wineData, aes(x=x, y=y)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y-bars, ymax=y+bars), width=0.2,
                position=position_dodge(0.05))+xlab("Model Number")+ylab("avgError")+
  scale_x_continuous(breaks = c(seq(1,12)))

#Model 4 is the best model to choose as model 4 average error rate falls within 1SE rule of best performing model.

#Cross-Validation on Regression Model of Wine Quality Data set

attach(wine_quality)
m1 = lm(quality~poly(alcohol, 3))
summary(m1)$r.squared
m2 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 5))
summary(m2)$r.squared
m3 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2))
summary(m3)$r.squared
m4 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2)
summary(m4)$r.squared
m5 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2+poly(residual_sugar, 3))
summary(m5)$r.squared
m6 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2+poly(residual_sugar, 3)+poly(total_sulfur_dioxide, 2))
summary(m6)$r.squared
m7 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2+poly(residual_sugar, 3)+poly(total_sulfur_dioxide, 2)+poly(free_sulfur_dioxide, 3)^3)
summary(m7)$r.squared
m8 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2+poly(residual_sugar, 3)+poly(total_sulfur_dioxide, 2)+poly(free_sulfur_dioxide, 3)^3+poly(alcohol, 3)^3)
summary(m8)$r.squared
m9 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2+poly(residual_sugar, 3)+poly(total_sulfur_dioxide, 2)+poly(free_sulfur_dioxide, 3)^3+poly(alcohol, 3)^3+poly(volatile_acidity, 3)^3)
summary(m9)$r.squared
m10 = lm(quality~poly(alcohol, 3)+poly(volatile_acidity, 3)+poly(sulphates, 2)+poly(free_sulfur_dioxide, 3)^2+poly(residual_sugar, 2)+poly(total_sulfur_dioxide, 2)+poly(free_sulfur_dioxide, 3)^3+poly(alcohol, 3)^3+poly(volatile_acidity, 3)^3+poly(alcohol, 3)^2+poly(citric_acid, 5)+poly(density, 4))
summary(m10)$r.squared
m11 = lm(quality~alcohol+poly(volatile_acidity, 5)+poly(sulphates, 6)+poly(free_sulfur_dioxide, 5)+poly(residual_sugar,5)+poly(total_sulfur_dioxide, 6)+poly(free_sulfur_dioxide,3)+poly(alcohol,4)+poly(volatile_acidity,4)+poly(alcohol, 3)+poly(citric_acid, 3)+poly(density, 5))
summary(m11)$r.squared

m22=lm(quality~alcohol+volatile_acidity+poly(fixed_acidity,j)+sulphates+free_sulfur_dioxide+residual_sugar+total_sulfur_dioxide+poly(pH,j)+citric_acid+density)
summary(m22)$r.squared
ggplot(data=wine_quality, aes(x=quality, y=alcohol))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x, se = FALSE)+
  geom_smooth(method=lm, formula = y~poly(x, 2), col = "red", se = FALSE)+
  geom_smooth(method=lm, formula = y~poly(x, 3), col = "green", se = FALSE)+
  geom_smooth(method=lm, formula = y~poly(x, 4), col = "brown", se = FALSE)

totalError = 0
sum(is.na(wineRandom))
for(j in 1:10){ # the 10 different models
  k=10
  for(i in 1:k){ #the k folds for each model
    
    
    test = filter(wineRandom, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
    
    train = anti_join(wineRandom, test, by="id")
    
    model = lm(quality~alcohol+volatile_acidity+poly(fixed_acidity,j)+sulphates+free_sulfur_dioxide+residual_sugar+total_sulfur_dioxide+poly(pH,j), data=train)
    
    errors[j,i] = mean((test$quality - predict.lm(model, test))^2)
    
  }
  
}

avgRegEr = rep(0,10)

avgRegEr

for(j in 1:10){
  
  for(i in 1:10){
    
    avgRegEr[j] = avgRegEr[j]+errors[j, i]
    
  }
  
}

avgRegEr/k

cv.error
se = rep(0, 10)

for (i in 1:10){
  
  se[i] = sqrt(var(errors[i,])/k)
  
}
se

#now making data frame for ease of plotting

x = seq(1,10, by = 1)

wineBest = data.frame(x,avgRegEr/k , se)

view(wineBest)

ggplot(data = wineBest, aes(x,avgRegEr.k, se))+
  
  geom_point()+
  
  geom_line()+
  
  geom_errorbar(aes(ymin = avgRegEr.k-se, ymax = avgRegEr.k +se))+
  scale_x_continuous(breaks = c(seq(1,10)))+xlab("Model Number")

