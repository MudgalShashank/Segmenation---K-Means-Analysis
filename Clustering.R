# Clear environment
rm(list = ls())


#Importing the dataset

CC_SEGMENTATION <- read.table(choose.files(), header=TRUE, sep=",")


#Dropping First variable

CC_SEGMENTATION <- CC_SEGMENTATION[-1]


#Converting all variables into numeric

CC_SEGMENTATION$TENURE <- as.numeric(CC_SEGMENTATION$TENURE)
CC_SEGMENTATION$PURCHASES_TRX <-  as.numeric(CC_SEGMENTATION$PURCHASES_TRX)
CC_SEGMENTATION$CASH_ADVANCE_TRX <- as.numeric(CC_SEGMENTATION$CASH_ADVANCE_TRX)


#Exploring data

View(CC_SEGMENTATION)
str(CC_SEGMENTATION)
names(CC_SEGMENTATION)


# user written function for creating descriptive statistics

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

vars <- c( "BALANCE",	"BALANCE_FREQUENCY",	"PURCHASES"	,
           "ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES",	"CASH_ADVANCE",	
           "PURCHASES_FREQUENCY",	"ONEOFF_PURCHASES_FREQUENCY",	
           "PURCHASES_INSTALLMENTS_FREQUENCY",	"CASH_ADVANCE_FREQUENCY",	
           "CASH_ADVANCE_TRX",	"PURCHASES_TRX",	"CREDIT_LIMIT",	"PAYMENTS",	
           "MINIMUM_PAYMENTS",	"PRC_FULL_PAYMENT",	"TENURE")

diag_stats<-t(data.frame(apply(CC_SEGMENTATION[vars], 2, mystats)))

write.csv(diag_stats, "C:/Users/shashank/Documents/diag_stats.csv")


## Missing value treatment

CC_SEGMENTATION$CREDIT_LIMIT[is.na(CC_SEGMENTATION$CREDIT_LIMIT)] <- 4494.44945
CC_SEGMENTATION$MINIMUM_PAYMENTS[is.na(CC_SEGMENTATION$MINIMUM_PAYMENTS)] <- 864.2065423


#Outliers

CC_SEGMENTATION$BALANCE[CC_SEGMENTATION$BALANCE>5909.111808]<-5909.111808
CC_SEGMENTATION$BALANCE_FREQUENCY[CC_SEGMENTATION$BALANCE_FREQUENCY>1]<-1
CC_SEGMENTATION$PURCHASES[CC_SEGMENTATION$PURCHASES>3998.6195]<-3998.6195
CC_SEGMENTATION$ONEOFF_PURCHASES[CC_SEGMENTATION$ONEOFF_PURCHASES>2671.094]<-2671.094
CC_SEGMENTATION$INSTALLMENTS_PURCHASES[CC_SEGMENTATION$	INSTALLMENTS_PURCHASES>1750.0875]<-1750.0875
CC_SEGMENTATION$CASH_ADVANCE[CC_SEGMENTATION$CASH_ADVANCE>4647.169122]<-4647.169122
CC_SEGMENTATION$PURCHASES_FREQUENCY[CC_SEGMENTATION$PURCHASES_FREQUENCY	>1]<-1
CC_SEGMENTATION$ONEOFF_PURCHASES_FREQUENCY[CC_SEGMENTATION$ONEOFF_PURCHASES_FREQUENCY>1]<-1
CC_SEGMENTATION$PURCHASES_INSTALLMENTS_FREQUENCY[CC_SEGMENTATION$PURCHASES_INSTALLMENTS_FREQUENCY>1]<-1
CC_SEGMENTATION$CASH_ADVANCE_FREQUENCY[CC_SEGMENTATION$	CASH_ADVANCE_FREQUENCY>	0.583333]<-0.583333
CC_SEGMENTATION$CASH_ADVANCE_TRX[CC_SEGMENTATION$CASH_ADVANCE_TRX>15]<-15
CC_SEGMENTATION$PURCHASES_TRX[CC_SEGMENTATION$PURCHASES_TRX>57]<-57
CC_SEGMENTATION$CREDIT_LIMIT[CC_SEGMENTATION$CREDIT_LIMIT>12000]<-12000
CC_SEGMENTATION$PAYMENTS[CC_SEGMENTATION$PAYMENTS>6082.090595]<-6082.090595
CC_SEGMENTATION$MINIMUM_PAYMENTS[CC_SEGMENTATION$MINIMUM_PAYMENTS>2766.56331]<-2766.56331
CC_SEGMENTATION$PRC_FULL_PAYMENT[CC_SEGMENTATION$PRC_FULL_PAYMENT>1]<-1
CC_SEGMENTATION$TENURE[CC_SEGMENTATION$TENURE>12]<-12
CC_SEGMENTATION$BALANCE[CC_SEGMENTATION$BALANCE<8.81451835]<-8.81451835
CC_SEGMENTATION$BALANCE_FREQUENCY[CC_SEGMENTATION$BALANCE_FREQUENCY<0.272727]<-0.272727
CC_SEGMENTATION$CREDIT_LIMIT[CC_SEGMENTATION$CREDIT_LIMIT<1000]<-1000
CC_SEGMENTATION$PAYMENTS[CC_SEGMENTATION$PAYMENTS<89.98892395]<-89.98892395
CC_SEGMENTATION$MINIMUM_PAYMENTS[CC_SEGMENTATION$MINIMUM_PAYMENTS<73.2820058]<-73.2820058
CC_SEGMENTATION$TENURE[CC_SEGMENTATION$TENURE<8]<-8


#Creating New KPI's

CC_SEGMENTATION<-transform(CC_SEGMENTATION, MONTHLY_AVERAGE_PURCHASE = PURCHASES/12)
CC_SEGMENTATION<- transform(CC_SEGMENTATION, MONTHLY_AVG_CASH_ADV = CASH_ADVANCE/12)

CC_SEGMENTATION$AVG_AMOUNT_PER_PURCHASE <- with(CC_SEGMENTATION, ifelse(PURCHASES_TRX == 0, "AVG_AMOUNT_PER_PURCHASE" == 0, PURCHASES/PURCHASES_TRX))
CC_SEGMENTATION$AVG_AMOUNT_PER_CASH_ADV <- with(CC_SEGMENTATION, ifelse(CASH_ADVANCE_TRX == 0, "AVG_AMOUNT_PER_CASH_ADV" == 0, CASH_ADVANCE/CASH_ADVANCE_TRX ))

CC_SEGMENTATION<- transform(CC_SEGMENTATION, LIMIT_USAGE = BALANCE/CREDIT_LIMIT)
CC_SEGMENTATION<- transform(CC_SEGMENTATION, PAYMENT_TO_MIN_PAYMENT_RATIO = PAYMENTS/MINIMUM_PAYMENTS)

CC_SEGMENTATION$PURCHASE_TYPE <- with(CC_SEGMENTATION, ifelse(ONEOFF_PURCHASES == 0 & INSTALLMENTS_PURCHASES > 0, "Installment Purchase", "")) 
CC_SEGMENTATION$PURCHASE_TYPE <- with(CC_SEGMENTATION, ifelse(ONEOFF_PURCHASES > 0 & INSTALLMENTS_PURCHASES == 0, "OneOff Purchase", CC_SEGMENTATION$PURCHASE_TYPE))
CC_SEGMENTATION$PURCHASE_TYPE <- with(CC_SEGMENTATION, ifelse(ONEOFF_PURCHASES > 0 & INSTALLMENTS_PURCHASES > 0, "Both OneOff and Isntallment Purchase", CC_SEGMENTATION$PURCHASE_TYPE))
CC_SEGMENTATION$PURCHASE_TYPE <- with(CC_SEGMENTATION, ifelse(ONEOFF_PURCHASES == 0 & INSTALLMENTS_PURCHASES == 0, "Neither OneOff nor Installment Purchase", CC_SEGMENTATION$PURCHASE_TYPE))


#Creating Dummies for PURCHASE_TYPE and combining them in the CC_SEGMENTATION dataset.

library(caret)

dummies <- predict(dummyVars(~PURCHASE_TYPE, data=CC_SEGMENTATION), newdata = CC_SEGMENTATION)
head(dummies, n=4)


CC_SEGMENTATION<-cbind(CC_SEGMENTATION, dummies)


#Droping PURCHASE_TYPE and 4th Dummy variable (n-1 concept)

CC_SEGMENTATION <- CC_SEGMENTATION[ -c(24, 27) ]


#Renaming Dummy Variables

colnames(CC_SEGMENTATION)[24]<-"Both_OneOff_Install_Purchase"
colnames(CC_SEGMENTATION)[25]<-"Installment_Purchase"
colnames(CC_SEGMENTATION)[26]<-"OneOff_Purchase"


View(CC_SEGMENTATION)


vars <- c("BALANCE","BALANCE_FREQUENCY",
          "PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
          "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
          "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
          "PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
          "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")


####### prepare final data #########
final_data<- CC_SEGMENTATION[vars]     # Decided not to include the KPI's into my Solution.
#------------------------------------------------------------------------------------ 


## FACTOR ANALYSIS 
corrm<- cor(final_data)                                 ### CORRELATION MATRIX

require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "C:/Users/shashank/Documents/EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY


FA<-fa(r=corrm, nfactors = 6, rotate="varimax", fm="PA")               ### CONDUCTING FACTOR ANALYSIS

# fm = "minres" is giving the same loadings as fm="PA"
# fm = "ml" does not provide ideal Factor Loadings. Hence, used PA.


print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(final_data),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings, "C:/Users/shashank/Documents/loadings.csv") ### SAVING THE FILE


vars <- c( "BALANCE","ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES",	
           "CASH_ADVANCE",	"ONEOFF_PURCHASES_FREQUENCY",	"PURCHASES_INSTALLMENTS_FREQUENCY",	
           "CASH_ADVANCE_FREQUENCY",	"CREDIT_LIMIT",	"PAYMENTS",	"MINIMUM_PAYMENTS",	
           "PRC_FULL_PAYMENT",	"TENURE")

inputdata_final <-CC_SEGMENTATION[vars]

#Prepare final Data
#standardizing the data

inputdata_final = scale(inputdata_final)

#View(inputdata_final)
#building clusters using k-means clustering 

cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)


CC_SEGMENTATION_new<-cbind(CC_SEGMENTATION,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
#View(CC_SEGMENTATION_new)


###Profiling

#Converting into factors
CC_SEGMENTATION_new[,"km_clust_3"]=factor(CC_SEGMENTATION_new[,"km_clust_3"])
CC_SEGMENTATION_new[,"km_clust_4"]=factor(CC_SEGMENTATION_new[,"km_clust_4"])
CC_SEGMENTATION_new[,"km_clust_5"]=factor(CC_SEGMENTATION_new[,"km_clust_5"])
CC_SEGMENTATION_new[,"km_clust_6"]=factor(CC_SEGMENTATION_new[,"km_clust_6"])
CC_SEGMENTATION_new <- data.frame(CC_SEGMENTATION_new)


require(tables)
profiling <- tabular(1+BALANCE+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+
                       CASH_ADVANCE+ONEOFF_PURCHASES_FREQUENCY+
                       PURCHASES_INSTALLMENTS_FREQUENCY+CASH_ADVANCE_FREQUENCY+
                       CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE ~ mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6), 
                     data=CC_SEGMENTATION_new )
profile1<-as.matrix(profiling)
profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=CC_SEGMENTATION_new)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"C:/Users/shashank/Documents/Profile1.csv",row.names = F)
write.csv(profile2,"C:/Users/shashank/Documents/Profile2.csv",row.names = F)
#############################END OF k-Means Segmentation############################


