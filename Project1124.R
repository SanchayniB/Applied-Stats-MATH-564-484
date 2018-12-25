# Press Alt+O for better outlook #

#-------------------------------Libraries------------------------------
library(ggplot2)
library(data.table)
library(dplyr)
library(leaps)
library(geosphere)
library(corrplot)
library(leaps)
library(car)
library(MASS)
library(Hmisc)
library(DMwR)
library(glmnet)

#-------------------------------Reading data --------------------------
# paste your file location here:
Incomed <- read.csv('C:\\Users\\Sanchayni\\Desktop\\IIT\\Fall18\\Applied_Stats\\Project\\Database1114(e).csv')
colnames(Incomed)[1] <- 'ID'
colnames(Incomed)

#-------------------------------Preprocessing and EDA ---------------------
# Creating derived variables
Incomed$Working_class <- (Incomed$Age_group_18to34 + Incomed$Age_group_35to49+Incomed$Age_group_50to74)
Incomed$Dependency_ratio <- (Incomed$Age_group_75andover+Incomed$Age_group_5to17)/Incomed$Working_class
sapply(Incomed,function(x) sum(is.na(x)))

#-------------------------------Data Imputation (Tax variables) ---------------------------

#Imputing with a median would be a better call than mean because of the outliers
Incomed <- knnImputation(Incomed, k = 10, meth = 'median')
Incomed$Median_Household_Income <- scale(Incomed$Median_Household_Income)
# Corplot of Numeric variables

Incomed.numeric <- Incomed[c(6:ncol(Incomed))]
M <- cor(Incomed.numeric)
corrplot(M, method="color", type ='lower', tl.col="black")

ggplot(Incomed, aes(Median_Household_Income )) +
  geom_histogram(binwidth=0.2,colour="black", fill="lightblue") +
  labs(title="Histogram for Scaled Median Income",
       x ="Median Income", y = 'count')
  
# Almost normal with few extreme cases

#-------------------------------Basic model ------------------------------------------
Incomed.model1 <- Incomed[c('Sex_ratio','pct_OCC01','pct_OCC02','pct_OCC03',
                            'pct_OCC04','pct_OCC05','pct_COW01','pct_COW02',
                            'pct_COW03','Working_class',
                            'Dependency_ratio','EDU01','EDU02',
                            'EDU03','pct_Married','TAX_P','TAX_SR',
                            'Median_Household_Income')]
M01 <- cor(Incomed.model1)
corrplot(M01, method="color", type ='lower', tl.col="black")
#Highly correlated variables present

Incomed.model1 <- as.data.frame(lapply(Incomed.model1, as.numeric))
model01 <- lm(Median_Household_Income~.,data = Incomed.model1 )
summary(model01)

#-------------------------------Dealing with multicollinearity -----------------------
vif(model01)
mean(vif(model01))
step_model01 <- step(model01)
summary(step_model01)

#------------------------------------PCA ---------------------------------------
    
pca_data <- prcomp(Incomed.model1[,-18],
                 center = TRUE,
                 scale. = TRUE) 


summary(pca_data)
pca_data01 <- as.data.frame(pca_data$sdev^2 *100 / sum(pca_data$sdev^2))
colnames(pca_data01) <- 'varianceexp'
pca_data01$PCno <- 1:length(pca_data01$varianceexp)

# PCA plot
ggplot(data= pca_data01, aes(x= PCno,y = varianceexp)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(varianceexp)), vjust=-0.3, size=3.5) +
  labs(title="Variance explained by each PCA",x ="PCA", y = 'Variance')

#install.packages('factoextra')
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(pca_data)
eig.val

# First 7 PCAs explain 80% of the data
# We would not prefer PCA as it takes 8 variables to explain 80% of the
# variance in data, which would make our interpretation difficult

#------------------------------------Stepwise -------------------------------------

colnames(Incomed.model1)
best1<-regsubsets(x=Incomed.model1[,1:17], y=Incomed.model1$Median_Household_Income,
                  nbest=1,
                  nvmax=17,method="exhaustive")
best17<-regsubsets(x=Incomed.model1[,1:17], y=Incomed.model1$Median_Household_Income,
                  nbest=17,
                  nvmax=17,method="exhaustive")
report1<-summary(best1)
report17<-summary(best17)
report17_var <- as.data.frame(report17$which)
report1_var <- as.data.frame(report1$which)
k <- rowSums(report17$which)
plot(k,report17$rsq,xlab="model size", ylab="R-square")
plot(k,report17$adjr2,xlab="model size", ylab="Adjusted R-square")
plot(k,report17$cp,xlab="model size", ylab="Mallow's Cp")
plot(k,report17$bic,xlab="model size", ylab="BIC")

bic_report <- as.data.frame(report17$bic)
colnames(bic_report) <- 'bic'
bic_report$k <- k


ggplot(data= bic_report, aes(x=k ,y = bic)) + geom_point() +
  labs(title="BIC values vs number of variables in model",x ="k", y = 'BIC value')

# As we can see model size of 10 seems to be a good choice but has a minimum at 13
#minimum BIC
report17_var[which(bic_report$bic == min(report17$bic)),]
#minimum Cp
report17_var[which(report17$cp == min(report17$cp)),]
#minimum adjR
report17_var[which(report17$adjr2 == max(report17$adjr2)),]

report1_var[9,]

# Selected variables
#Sex_ratio
#pct_OCC01
#pct_OCC03
#pct_OCC04
#pct_OCC05
#pct_COW03
#Working_class
#EDU03
#pct_Married


#------------------------------------Lasso --------------------------------------------

X_mat <- as.matrix(cbind(rep(1,nrow(Incomed.model1)),as.data.frame(lapply(Incomed.model1[,-18], scale))))
Y_mat <- as.matrix(Incomed.model1$Median_Household_Income)
colnames(X_mat)[1] <- 'beta0'


lasso.mod <- glmnet(X_mat, Y_mat, alpha = 1, lambda = 0.01)
lasso_all <- as.data.frame(lasso.mod$beta[,1])
colnames(lasso_all) <- 0.01

for(i in 2:30){
  lambda_i = i/100
  lasso.mod <- glmnet(X_mat, Y_mat, alpha = 1, lambda = lambda_i)
  lasso_all <- cbind(lasso_all,lasso.mod$beta[,1])
  col_name <- colnames(lasso_all)
  col_name[ncol(lasso_all)] <- i/100
  colnames(lasso_all) <- col_name
} 

lasso_all <- as.data.frame(t(lasso_all))
lasso_all$lambda <- rownames(lasso_all) 
# we are selecting lambda value high enough for variables to shrink to zero

lasso.mod <- glmnet(X_mat, Y_mat, alpha = 1, lambda = 0.05)
lasso.mod$beta
# Selected variables
#Sex_ratio
#pct_OCC01
#pct_OCC03
#pct_COW01
#pct_COW03
#EDU01 
#EDU02
#EDU03
#pct_Married

#------------------------------------Lasso02 -----------------------------

# using the best lambda
# this is not used in our analysis
lasso.mod2 <- cv.glmnet(X_mat, Y_mat, alpha = 1)
lasso.mod2$lambda.min
lasso.mod3 <- glmnet(X_mat, Y_mat, alpha = 1, lambda = lasso.mod2$lambda.min)
lasso.mod3$beta[,1]



#------------------------------------Ridge -----------------------------
ridge.mod <- glmnet(X_mat, Y_mat, alpha = 0, lambda = 0.097)
ridge.mod$beta[,1]


ridge.mod2 <- lm.ridge(Median_Household_Income~.,data=Incomed.model1,lambda=seq(0,0.1,0.001),model=TRUE)
ridge.mod2$lambda[which.min(ridge.mod2$GCV)]
ridge.mod2$coef[,which.min(ridge.mod2$GCV)]


# Lets use Lasso

#-------------------------------Second subset of data ---------------------
# Based on Lasso or Ridge
Incomed.model2 <- Incomed[c('Sex_ratio','pct_OCC01','pct_OCC03',
                            'pct_COW01','pct_COW03',
                            'EDU01','EDU02','EDU03','pct_Married',
                            'Median_Household_Income')]

M02 <- cor(Incomed.model2)
corrplot(M02, method="color", type ='lower', tl.col="black")
# We still have a lot of correlation present

# Based on Corrplot and our intuitions :p
Incomed.model3 <- Incomed[c('Sex_ratio','pct_OCC02',
                            'pct_OCC04','pct_OCC05','pct_COW01',
                            'pct_COW02','pct_COW03', 'Working_class',
                            'EDU03','pct_Married',
                            'Median_Household_Income')]
M03 <- cor(Incomed.model3)
corrplot(M03, method="color", type ='lower', tl.col="black")

# Based on Subset selection
Incomed.model4 <- Incomed[c('Sex_ratio','pct_OCC01',
                            'pct_OCC03','pct_OCC04','pct_OCC05','pct_COW03',
                            'Working_class','EDU03','pct_Married',
                            'Median_Household_Income')]
M04 <- cor(Incomed.model4)
corrplot(M04, method="color", type ='lower', tl.col="black")

#-------------------------------Models Comparison-----------------------------

model02 <- lm(Median_Household_Income~.-1,data = Incomed.model2 )
summary(model02)

model03 <- lm(Median_Household_Income~.,data = Incomed.model3 )
summary(model03)

model04 <- lm(Median_Household_Income~.,data = Incomed.model4 )
summary(model04)

BIC(model02,model03,model04)
AIC(model02,model03,model04)
anova(model02,model03,model04)


plot(model03)
mean(vif(model03))

#-------------------------------OUTLIERS --------------------------------------------
#------------------------------------Outlying X values-------------------------------

M <- as.matrix(cbind(rep(1,nrow(Incomed.model3)),Incomed.model3[,1:10]))
H <- M%*%solve(t(M)%*%M)%*%t(M)
leverage<-as.data.frame(diag(H))
leverage$ID <- Incomed$Id2
colnames(leverage) <- c('leverage','ID2')

# Threshold should be 2p/n
n <-  nrow(Incomed.model3)
p <- (ncol(Incomed.model3))
2*p/n

X_out <- (leverage[leverage$leverage>= (2*p/n),])
nrow(X_out)
X_out_data <- Incomed.model3[c(which(Incomed$Id2
                                      %in%X_out$ID2)),]
# This account for 9% of the data and thus might not really be outliers
# Lets try removing it
Incomed.model3 <- Incomed.model3[-c(which(Incomed$Id2
                                          %in%X_out$ID2)),]

# Overall Outlying X variables

M1 <- as.matrix(cbind(rep(1,nrow(Incomed.model3)),Incomed.model1[,1:17]))
H1 <- M1%*%solve(t(M1)%*%M1)%*%t(M1)
leverage1<-as.data.frame(diag(H1))
leverage1$ID <- Incomed$Id2
colnames(leverage1) <- c('leverage','ID2')

# Threshold should be 2p/n
n <-  nrow(Incomed.model1)
p <- (ncol(Incomed.model1))
2*p/n
Xall_out <- (leverage1[leverage1$leverage>= (2*p/n),])
# This account for 9% of the data and thus might not really be outliers
# Lets try removing it

Incomed.model1 <- Incomed.model1[-c(which(Incomed$Id2
                                          %in%Xall_out$ID2)),]

Incomed.model4 <- Incomed.model1[c('Sex_ratio','pct_OCC01',
                            'pct_OCC03','pct_OCC04','pct_OCC05','pct_COW03',
                            'Working_class','EDU02','EDU03','pct_Married',
                            'Median_Household_Income')]

#------------------------------------Outlying Y values-------------------------------
# Studentized Deleted Residuals
res <-  model03$residuals
t_value<-as.data.frame(res*sqrt((n-p-1)/(sum(res^2)*(1-leverage$leverage)-res^2)))
t_value$ID2 <- Incomed$Id2
colnames(t_value) <- c('t_vals','ID2')
qt(1-(0.05/(2*n)),n-p-1)

Y_out <- t_value[t_value$t_vals>qt(1-(0.05/(2*n)),n-p-1),]

# Outlying wrt Y
#row  t_vals   ID2
#207  4.388554  6041
#1197 4.782344 24009
#1201 4.479533 24017
#1858 4.701613 36059
#2043 4.519921 38105
#2873 5.570012 51107

Incomed.model3 <- Incomed.model3[-c(which(Incomed$Id2
                                          %in% Y_out$ID2)),]

Incomed.model4 <- Incomed.model4[-c(which(Incomed$Id2
                                          %in% Y_out$ID2)),]


#------------------------------------Influencial points-------------------------------
# Outlying wrt X as well as Y

#DFFITS
DFFITS <- as.data.frame(t_value$t_vals*sqrt(leverage$leverage/(1-leverage$leverage)))
DFFITS$ID2 <- Incomed$Id2
colnames(DFFITS) <- c('DFFITS','ID2')
DFFITS <- DFFITS[order(-DFFITS$DFFITS),]
Influ <- (DFFITS[DFFITS$DFFITS >= 2*sqrt(p/n),])
Influ_01 <- (DFFITS[DFFITS$DFFITS >= 1,])
#Lot of outliers exists under this threshold


#Cooks dist
cooks_data <- influence.measures(model03)
cooks_data <- as.data.frame(cooks_data$infmat)
cooks_data$ID2 <- Incomed$Id2

ggplot(cooks_data, aes(y =cook.d, x= ID2 )) +
  geom_point() +
  labs(title="Cooks distance",x ="ID2", y = 'Cooks distance')
#identify()



#-------------------------------Creating the final model ----------------------

smp_size <- floor(0.75*nrow(Incomed.model3))
set.seed(12356)
train_ind <- sample(seq_len(nrow(Incomed.model3)), size = smp_size)

train_m3 <- Incomed.model3[train_ind, ]
test_m3 <- Incomed.model3[-train_ind, ]

train_m32 <- Incomed.model4[train_ind, ]
test_m32 <- Incomed.model4[-train_ind, ]

final_model <- lm(Median_Household_Income~.,data = train_m3 )
final_model2 <- lm(Median_Household_Income~.,data = train_m32 )

summary(final_model)
summary(final_model2)
plot(final_model)

# Removing insignificant variable pct_OCC04 from the model
final_model <- lm(Median_Household_Income~.- pct_OCC04-1,data = train_m3 )
summary(final_model)
plot(final_model)
AIC(final_model)
BIC(final_model)
#------------------------------------Model Diagnostics ------------------------
# Constancy of Error Variance 
# Breusch-Pagan Test
BP <- as.data.frame(log(final_model$residuals^2))
colnames(BP) <- 'log_res2'
BP <- cbind(BP,train_m3[,-11])
modelBP <- lm(log_res2~.,data = BP)

SSR_bp <- sum((modelBP$fitted.values - mean(BP$log_res2))^2)
SSE_bp <- sum((modelBP$fitted.values - BP$log_res2)^2)

chi_bp <- (SSR_bp/(ncol(BP)-1))/(SSE_bp/nrow(BP))^2
chi_bp
# 4.597412   
qchisq(.95, df=ncol(BP)-1)  
# 18.30704

# Conclusion: Error Variance is constant

resqq <- qqnorm(final_model$residuals)
qqline(final_model$residuals)
cor(resqq$y,resqq$x)

# Normality of error terms
qqnorm(final_model$residuals)
qqline(final_model$residuals)

# Normality of Y 
qqnorm(train_m3$Median_Household_Income)
qqline(train_m3$Median_Household_Income)

# Check if all beta k values should be zero
qf(1-0.1,ncol(train_m3)-1, nrow(train_m3)-ncol(train_m3))

# smaller than F stats value thus conclude Ha

test_m3$predict <- predict(final_model,test_m3)
MSPE <- sum((test_m3$predict- test_m3$Median_Household_Income)^2)/(nrow(test_m3)-ncol(train_m3))
MSPE

MSPE_mean <- sum((mean(test_m3$Median_Household_Income)- test_m3$Median_Household_Income)^2)/(nrow(test_m3)-ncol(train_m3))
MSPE_mean

abs(MSPE_mean-MSPE)*100/MSPE_mean
#other measure
sum(abs(test_m3$predict- test_m3$Median_Household_Income)/abs(test_m3$Median_Household_Income))/nrow(test_m3)



cor(test_predict,test_m3$Median_Household_Income)
test_m3$index <- 1:nrow(test_m3)

ggplot(test_m3) +
  geom_point( aes(x=index , y = Median_Household_Income, colour='Actual' )) +
  geom_point(aes(x=index , y = predict , colour = 'Predicted')) +
  labs(title="Predictions",
       x ="Index", y = 'Median Income')
