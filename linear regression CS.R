require(readxl)
install.packages('readxl')
#IMPORTING DATASET
credit_card<-readxl::read_xlsx(path='D:/BA/Linear Regression Case/Linear Regression Case.xlsx')
#DEPENDENT VARIABLE
credit_card$total_spent<-credit_card$card2spent+credit_card$cardspent

credit_card<-subset(credit_card,select = -c(carditems,card2items,cardspent,card2spent,birthmonth,custid,commutecar,
commutemotorcycle,commutecarpool,commutebus,commuterail,commutepublic,commutebike,commutewalk,commutenonmotor,telecommute))

#SPLITTING OUT CONTINUOUS AND CATEGORICAL VARIABLES
var_num=sapply(credit_card,is.numeric)
num_var= credit_card[,var_num]
Other_var=credit_card[,!var_num]

#USER DEFINE FUNCTION FOR CONTINUOUS VARIABLES
mystat=function(x){
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}
#APPLYING USER DEFINE FUNCTION ON MY CONTINUOUS VARIABLES
my_num_data<-t(data.frame(apply(num_var, 2, mystat)))

#MISSING VALUE IMPUTATION
num_var$lncreddebt[is.na(num_var$lncreddebt)]<-mean(num_var$lncreddebt,na.rm = T)
num_var$lnothdebt[is.na(num_var$lnothdebt)]<-mean(num_var$lnothdebt,na.rm = T)
num_var$commutetime[is.na(num_var$commutetime)]<-mean(num_var$commutetime,na.rm = T)
num_var$longten[is.na(num_var$longten)]<-mean(num_var$longten,na.rm = T)
num_var$lnlongten[is.na(num_var$lnlongten)]<-mean(num_var$lnlongten,na.rm = T)
num_var$cardten[is.na(num_var$cardten)]<-mean(num_var$cardten,na.rm = T)

#DELETING LOG VARIABLES WITH NULL VALUES AND RE-CREATING IT MANUALLY
num_var<-subset(num_var,select = -c(lntollmon,lntollten,lnequipmon,lnequipten,lncardmon,lncardten,
                                            lnwiremon,lnwireten))

require(dplyr)
num_var1<-dplyr::mutate(num_var,lntollmon=log(tollmon+1),lntollten=log(tollten+1),lnequipmon=log(equipmon+1),
                            lnequipten=log(equipten+1),lncardmon=log(cardmon+1),lncardten=log(cardten+1),lnwiremon=log(wiremon+1),lnwireten=log(wireten+1))

my_num_data<-t(data.frame(apply(num_var1, 2, mystat)))
rm(num_var)

#TREATING OUTLIERS
num_var1$income[num_var1$income>147.000000]<-147.000000
num_var1$employ[num_var1$employ>31.000000]<-31.000000
num_var1$debtinc[num_var1$debtinc>22.200000]<-22.200000
num_var1$creddebt[num_var1$creddebt>6.373010]<-6.373010
num_var1$othdebt[num_var1$othdebt>11.815981]<-11.815981
num_var1$carvalue[num_var1$carvalue>72.000000]<-72.000000
num_var1$longmon[num_var1$longmon>36.757500]<-36.757500
num_var1$longten[num_var1$longten>2567.650000]<-2567.650000
num_var1$tollmon[num_var1$tollmon>43.500000]<-43.500000
num_var1$tollten[num_var1$tollten>2620.212500]<-2620.212500
num_var1$equipmon[num_var1$equipmon>49.052500]<-49.052500
num_var1$equipten[num_var1$equipten>2600.990000]<-2600.990000
num_var1$cardmon[num_var1$cardmon>42.000000]<-42.000000
num_var1$cardten[num_var1$cardten>2455.750000]<-2455.750000
num_var1$wiremon[num_var1$wiremon>51.305000]<-51.305000
num_var1$wireten[num_var1$wireten>2687.922500]<-2687.922500
num_var1$total_spent[num_var1$total_spent>1145.146500]<-1145.146500

num_var1$age[num_var1$age<20.00000000]<-20.00000000
num_var1$ed[num_var1$ed<9.00000000]<-9.00000000
num_var1$income[num_var1$income<13.00000000]<-13.00000000
num_var1$lninc[num_var1$lninc<2.5649494]<-2.5649494
num_var1$debtinc[num_var1$debtinc<1.9000000]<-1.9000000
num_var1$creddebt[num_var1$creddebt<0.1010880]<-0.1010880
num_var1$lncreddebt[num_var1$lncreddebt< -2.2915947]<- -2.2915947
num_var1$othdebt[num_var1$othdebt<0.2876923]<-0.2876923
num_var1$lnothdebt[num_var1$lnothdebt< -1.2433585]<- -1.2433585
num_var1$tenure[num_var1$tenure<4.0000000]<-4.0000000
num_var1$longmon[num_var1$longmon<2.9000000]<-2.9000000
num_var1$lnlongmon[num_var1$lnlongmon<1.0647107]<-1.0647107
num_var1$longten[num_var1$longten<12.6425000]<-12.6425000
num_var1$lnlongten[num_var1$lnlongten<2.5370608]<-2.5370608
num_var1$commutetime[num_var1$commutetime<16.0000000]<-16.0000000
num_var1$cardtenure[num_var1$cardtenure<1.0000000]<-1.0000000
num_var1$card2tenure[num_var1$card2tenure<1.0000000]<-1.0000000
num_var1$hourstv[num_var1$hourstv<12.0000000]<-12.0000000
num_var1$total_spent[num_var1$total_spent<133.1060000]<-133.1060000



#USER DEFINE FUNCTION FOR CATEGORICAL VARIABLES

mystat1<-function(x){
  nmiss=sum(is.na(x))
  class=class(x)
  return(c(class=class,nmiss=nmiss))
}

#APPLYING USER DEFINE FUNCTION ON MY CATEGORICAL VARIABLES
my_cat_data<-t(data.frame(apply(Other_var,2,mystat1)))

#TREATING MISSING VALUES
Other_var$townsize[is.na(Other_var$townsize)]<-Other_var$townsize[which.max(prop.table(table(Other_var$townsize)))]

#DELETING VARIABLES AND KEEPING THERE LOG 
num_var1$income<-NULL
num_var1$creddebt<-NULL
num_var1$othdebt<-NULL
num_var1$longmon<-NULL
num_var1$longten<-NULL
num_var1$tollmon<-NULL
num_var1$tollten<-NULL
num_var1$equipmon<-NULL
num_var1$equipten<-NULL
num_var1$cardmon<-NULL
num_var1$cardten<-NULL
num_var1$wiremon<-NULL
num_var1$wireten<-NULL

file<-cbind(num_var1,Other_var)

#CHECKING FOR SKEWNESS
hist(file$total_spent)

#CHECKING MULTICOLLINEARITY
summary(aov(total_spent~region,data = file))
summary(aov(total_spent~townsize,data = file))
summary(aov(total_spent~gender,data = file))
summary(aov(total_spent~agecat,data = file))
summary(aov(total_spent~edcat,data = file))
summary(aov(total_spent~jobcat,data = file))
summary(aov(total_spent~union,data = file))
summary(aov(total_spent~empcat,data = file))
summary(aov(total_spent~retire,data = file))
summary(aov(total_spent~inccat,data = file))
summary(aov(total_spent~default,data = file))
summary(aov(total_spent~jobsat,data = file))
summary(aov(total_spent~marital,data = file))
summary(aov(total_spent~spousedcat,data = file))
summary(aov(total_spent~homeown,data = file))
summary(aov(total_spent~hometype,data = file))
summary(aov(total_spent~addresscat,data = file))
summary(aov(total_spent~cars,data = file))
summary(aov(total_spent~carown,data = file))
summary(aov(total_spent~cartype,data = file))
summary(aov(total_spent~carcatvalue,data = file))
summary(aov(total_spent~carbought,data = file))
summary(aov(total_spent~carbuy,data = file))
summary(aov(total_spent~commute,data = file))
summary(aov(total_spent~commutecat,data = file))
summary(aov(total_spent~reason,data = file))
summary(aov(total_spent~polview,data = file))
summary(aov(total_spent~polparty,data = file))
summary(aov(total_spent~polcontrib,data = file))
summary(aov(total_spent~vote,data = file))
summary(aov(total_spent~card,data = file))
summary(aov(total_spent~cardtype,data = file))
summary(aov(total_spent~cardbenefit,data = file))
summary(aov(total_spent~cardfee,data = file))
summary(aov(total_spent~cardtenurecat,data = file))
summary(aov(total_spent~card2,data = file))
summary(aov(total_spent~card2type,data = file))
summary(aov(total_spent~card2benefit,data = file))
summary(aov(total_spent~card2fee,data = file))
summary(aov(total_spent~card2tenurecat,data = file))
summary(aov(total_spent~active,data = file))
summary(aov(total_spent~bfast,data = file))
summary(aov(total_spent~churn,data = file))
summary(aov(total_spent~tollfree,data = file))
summary(aov(total_spent~equip,data = file))
summary(aov(total_spent~callcard,data = file))
summary(aov(total_spent~wireless,data = file))
summary(aov(total_spent~multline,data = file))
summary(aov(total_spent~voice,data = file))
summary(aov(total_spent~pager,data = file))
summary(aov(total_spent~internet,data = file))
summary(aov(total_spent~callid,data = file))
summary(aov(total_spent~callwait,data = file))
summary(aov(total_spent~forward,data = file))
summary(aov(total_spent~confer,data = file))
summary(aov(total_spent~ebill,data = file))
summary(aov(total_spent~owntv,data = file))
summary(aov(total_spent~ownvcr,data = file))
summary(aov(total_spent~owndvd,data = file))
summary(aov(total_spent~owncd,data = file))
summary(aov(total_spent~ownpda,data = file))
summary(aov(total_spent~ownpc,data = file))
summary(aov(total_spent~ownipod,data = file))
summary(aov(total_spent~owngame,data = file))
summary(aov(total_spent~ownfax,data = file))
summary(aov(total_spent~news,data = file))
summary(aov(total_spent~response_01,data = file))
summary(aov(total_spent~response_02,data = file))
summary(aov(total_spent~response_03,data = file))

#SPLITTING SAMPLE
set.seed(123)
train<-sample(1:nrow(file),size = floor(0.70*nrow(file)))
training<-file[train,]
testing<-file[-train,]

fit<-lm(total_spent~age+ed+employ+lninc+debtinc+lncreddebt+lnothdebt+spoused+reside+pets+
pets_cats+pets_dogs+pets_birds+pets_reptiles+pets_small+pets_saltfish+commutetime+cardtenure+
card2tenure+tenure+lnlongmon+lnlongten+
hourstv+lntollmon+lntollten+lnequipmon+lnequipten+lncardmon+lncardten+lnwiremon+lnwireten+region+townsize+gender+agecat
+edcat+jobcat+union+empcat+retire+inccat+default+jobsat+marital+homeown+hometype+address+addresscat+cars
+carbuy+commute+reason+polview+polparty+polcontrib+vote+card+cardtype+cardbenefit+cardfee+cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenurecat+active+bfast+churn
+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+forward+confer+ebill+owntv+ownvcr+owndvd+
owncd+ownpda+ownpc+ownipod+owngame+ownfax+news+response_01+response_02+response_03,data=training)
summary(fit)
library(car)
vif(fit)

require(MASS)
step<- stepAIC(fit,direction="both")

fit2<-lm(total_spent ~ age + lninc + pets + pets_reptiles + pets_saltfish + 
  tenure + hourstv + region + gender + edcat + inccat + address + 
  reason + card + card2 + card2fee + churn + voice + internet + 
  owntv + owngame,data=training)
summary(fit2)
vif(fit2)

layout(matrix(c(1,2,3,4),2,2))
plot(fit2)

#INFLUENTIAL OBSERVATION
cooksd<-cooks.distance(fit2)
sample_size<-nrow(training)
plot(cooksd,pch="*",cex=2,main="INFLUENTIAL OBS BY COOKS DISTANCE")

abline(h=4/sample_size,col="RED")
text(x=1:length(cooksd)+1,y=cooksd,labels = ifelse(cooksd>4/sample_size,names(cooksd),""),col="RED")

length(names(cooksd)[(cooksd>(4/sample_size))])

w<-abs(rstudent(fit2))<3 & abs(cooks.distance(fit2))<4/nrow(fit$model)
fit3<-update(fit2,weights=as.numeric(w))

#FINAL MODEL
summary(fit3)
vif(fit3)
b<-cbind(training, pred_spent =(predict(fit3)))

c<-cbind(testing, pred_spent =(predict(fit3,testing)))

#DECILING
#TRAINING SAMPLE
decLocations <- quantile(b$pred_spent, probs = seq(0.1,0.9,by=0.1))
b$decile <- findInterval(b$pred_spent,c(-Inf,decLocations, Inf))

require(sqldf)
b_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pred_spent,   
               avg(total_spent) as avg_Actual_spent,sum(pred_spent) as tot_pred_spent,sum(total_spent) as tot_Actual_spent
               from b
               group by decile
               order by decile desc")
write.csv(b_DA,"D:/linear/training.csv")
#TESTING SAMPLE
decLocations <- quantile(c$pred_spent, probs = seq(0.1,0.9,by=0.1))
c$decile <- findInterval(c$pred_spent,c(-Inf,decLocations, Inf))

c_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pred_spent,   
               avg(total_spent) as avg_Actual_spent,sum(pred_spent) as tot_pred_spent,sum(total_spent) as tot_Actual_spent
              from c
              group by decile
              order by decile desc")
write.csv(c_DA,"D:/linear/testing.csv")
