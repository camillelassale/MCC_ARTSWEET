#### Adjustment for HRT 

load("./DATA/mcc_aspartame_27042023.RData")
# Different N because of missing values for HRT (variable name: TSH)
mccbreast<-subset(mcc2,!is.na(mcc2$menopausia)&!is.na(mcc2$nulipara)&!is.na(mcc2$ACO)&!is.na(mcc2$casom)&!is.na(mcc2$TSH))
table(mccbreast$TSH,useNA="always")

# For analyses separately for premenopausal and menopausal
premenop<-subset(mccbreast,mccbreast$menopausia==1)
postmenop <-subset(mccbreast,mccbreast$menopausia== 0 )

# Numbers
dependent = "casom"
explanatory3 =  c("aspar_cat3","noaspar_cat3")

mccbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/breastHRT/hrt.csv",sep=";")

fitbreast<-glm(casom ~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) + as.factor(TSH) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/breastHRT/hrt.csv",sep=";",append=T)

fitbreast<-glm(casom ~ aspar_catnum + noaspar_catnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +as.factor(TSH) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/breastHRT/hrt.csv",sep=";",append=T)

# Premenop
premenop %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/breastHRT/premenop.csv",sep=";")
fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) + as.factor(TSH) +
                 as.factor(ccaa),family="binomial",data=premenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/breastHRT/premenop.csv",sep=";",append=T)

fitbreast<-glm(casom ~ aspar_catnum + noaspar_catnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) + as.factor(TSH) +
                 as.factor(ccaa),family="binomial",data=premenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/breastHRT/premenop.csv",sep=";",append=T)

# Postmenop
postmenop %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/breastHRT/postmenop.csv",sep=";")
fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) + as.factor(TSH) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/breastHRT/postmenop.csv",sep=";",append=T)

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum+as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) + as.factor(TSH) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/breastHRT/postmenop.csv",sep=";",append=T)
