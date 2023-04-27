library(binom)
library(questionr)
library(ordinal)       
library(nnet) 
library(oddsratio)
library(broom)
library(Hmisc)

load("./DATA/mcc_aspartame_27042023.RData")

# Separately for breast
mccbreast<-subset(mcc2,!is.na(mcc2$menopausia)&!is.na(mcc2$nulipara)&!is.na(mcc2$ACO)&!is.na(mcc2$casom))
################################
# For analysis Stratified by Diabetes
nodiab <-subset(mcc2,mcc2$diab==0)
diab <-subset(mcc2,mcc2$diab== 1 )
nodiabbreast <-subset(mccbreast,mccbreast$diab==0)
diabbreast <-subset(mccbreast,mccbreast$diab== 1 )
################################
# For analysis Stratified by MENOPAUSE
premenop<-subset(mccbreast,mccbreast$menopausia==1)
postmenop <-subset(mccbreast,mccbreast$menopausia== 0 )

###############################################
###############################################
# ASPARTAME AND NON-ASPARTAME in the same model
# Table 2 #
###############################################
###############################################
# Get the numbers
# exclude missing values
explanatory3 = c("aspar_cat3","noaspar_cat3")
dependent = "casoc"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_aspar_noaspar.csv",sep=";")
dependent = "casom"
mccbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/num_aspar_noaspar.csv",sep=";",append=T)
dependent = "casop"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_aspar_noaspar.csv",sep=";",append=T)
dependent = "casoe"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_aspar_noaspar.csv",sep=";",append=T)
dependent = "casol"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_aspar_noaspar.csv",sep=";",append=T)



fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ)  + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2  )
            
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar.csv",sep=";",append=T)




# the p-value for trend
#######################

fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~aspar_catnum + noaspar_catnum   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ)  + as.factor(smk) + 
            + score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2  )
            
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar.csv",sep=";",append=T)







################################
### STRATIFIED BY MENOPAUSE ##
# Premenop
dependent = "casom"
explanatory=c("aspar_cat3","noaspar_cat3")
premenop %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/num_aspar_noaspar_premenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar_premenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar_premenop.csv",sep=";")

# Postmenop
postmenop %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/num_aspar_noaspar_postmenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/aspar_noaspar_postmenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_aspar_noaspar_postmenop.csv",sep=";")



################################
# Stratified by Diabetes
################################


###############
# NON DIABETICS
###############
# get the numbers
dependent = "casoc"
explanatory3 = c("aspar_cat3","noaspar_cat3")
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_aspar_noaspar.csv",sep=";")
dependent = "casom"
nodiabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/numnodiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casop"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casoe"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casol"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_aspar_noaspar.csv",sep=";",append=T)

# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ)  + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar.csv",sep=";",append=T)


# the p-value for trend
#######################
fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum   +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) 
                 + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ)  + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)




### STRATIFIED BY MENOPAUSE ##
# Premenop
dependent = "casom"
explanatory=c("aspar_cat3","noaspar_cat3")
premenopnodiab %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/nodiab_num_aspar_noaspar_premenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenopnodiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar_premenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenopnodiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar_premenop.csv",sep=";")

# Postmenop
dependent = "casom"
explanatory=c("aspar_cat3","noaspar_cat3")
postmenopnodiab %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/nodiab_num_aspar_noaspar_postmenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenopnodiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_aspar_noaspar_postmenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenopnodiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_aspar_noaspar_postmenop.csv",sep=";")




####################
# DIABETES
####################
# get the numbers
dependent = "casoc"
explanatory3 = c("aspar_cat3","noaspar_cat3")
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_aspar_noaspar.csv",sep=";")
dependent = "casom"
diabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/numdiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casop"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casoe"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casol"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_aspar_noaspar.csv",sep=";",append=T)

# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar.csv",sep=";",append=T)



# Stomach with family history
fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHstomach) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar_stomach_FH.csv",sep=";")

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum  +as.factor(FHstomach)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar_stomach_FH.csv",sep=";",append=T)
# the p-value for trend
#######################
fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum   +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) 
                 + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo)  + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)


# STratified by menop status
   # Premenop
dependent = "casom"
explanatory=c("aspar_cat3","noaspar_cat3")
premenopdiab %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/diab_num_aspar_noaspar_premenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenopdiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar_premenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenopdiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar_premenop.csv",sep=";")

# Postmenop
dependent = "casom"
explanatory=c("aspar_cat3","noaspar_cat3")
postmenopdiab %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/diab_num_aspar_noaspar_postmenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenopdiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_aspar_noaspar_postmenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenopdiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_aspar_noaspar_postmenop.csv",sep=";")




######################################################

               # TABLE 3 #

### ALL ARTIFICIAL SWEETENERS and SUGAR #####

######################################################

# Get the numbers
explanatory3 = c("sugarartsweet")
dependent = "casoc"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_sugarartsweet.csv",sep=";")
dependent = "casom"
mccbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/num_sugarartsweet.csv",sep=";",append=T)
dependent = "casop"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_sugarartsweet.csv",sep=";",append=T)
dependent = "casoe"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_sugarartsweet.csv",sep=";",append=T)
dependent = "casol"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/num_sugarartsweet.csv",sep=";",append=T)

# p-trend
fitcolon<-glm(casoc~sugarartsweetnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet.csv",sep=";")

fitbreast<-glm(casom~sugarartsweetnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet.csv",sep=";",append=T)

fitprostate<-glm(casop~sugarartsweetnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet.csv",sep=";",append=T)

fitstomach<-glm(casoe~sugarartsweetnum + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet.csv",sep=";",append=T)

fitcll<-glm(casol~sugarartsweetnum   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet.csv",sep=";",append=T)

# Adjusted models

fitcolon<-glm(casoc~as.factor(sugarartsweet)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet.csv",sep=";")

fitbreast<-glm(casom~as.factor(sugarartsweet)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(sugarartsweet)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100  + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(sugarartsweet)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100  + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(sugarartsweet)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet.csv",sep=";",append=T)



################################
### STRATIFIED BY MENOPAUSE ##
# Premenop
dependent = "casom"
explanatory="sugarartsweet"
premenop %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/numpremenop_sugarartsweet.csv",sep=";")
fitbreast<-glm(casom~as.factor(sugarartsweet)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet_premenop.csv",sep=";")

fitbreast<-glm(casom~sugarartsweetnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet_premenop.csv",sep=";")

# Postmenop
postmenop %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/numpostmenop_sugarartsweet.csv",sep=";")
fitbreast<-glm(casom~as.factor(sugarartsweet)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/sugarartsweet_postmenop.csv",sep=";")

fitbreast<-glm(casom~sugarartsweetnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/pvalue_sugarartsweet_postmenop.csv",sep=";")

################################
# Stratified by Diabetes
################################


###############
# NON DIABETICS
###############
# get the numbers
dependent = "casoc"
explanatory3 = c("sugarartsweet")
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_sugarartsweet.csv",sep=";")
dependent = "casom"
nodiabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/numnodiab_sugarartsweet.csv",sep=";",append=T)
dependent = "casop"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_sugarartsweet.csv",sep=";",append=T)
dependent = "casoe"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_sugarartsweet.csv",sep=";",append=T)
dependent = "casol"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numnodiab_sugarartsweet.csv",sep=";",append=T)

# adjusted models
fitcolon<-glm(casoc~as.factor(sugarartsweet)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_sugarartsweet.csv",sep=";")

fitbreast<-glm(casom~as.factor(sugarartsweet)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_sugarartsweet.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(sugarartsweet)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100  + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_sugarartsweet.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(sugarartsweet)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100  + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_sugarartsweet.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(sugarartsweet)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/nodiab_sugarartsweet.csv",sep=";",append=T)


# p-trend
fitcolon<-glm(casoc~sugarartsweetnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_sugarartsweet.csv",sep=";")

fitbreast<-glm(casom~sugarartsweetnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_sugarartsweet.csv",sep=";",append=T)

fitprostate<-glm(casop~sugarartsweetnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_sugarartsweet.csv",sep=";",append=T)

fitstomach<-glm(casoe~sugarartsweetnum + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_sugarartsweet.csv",sep=";",append=T)

fitcll<-glm(casol~sugarartsweetnum   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/nodiab_pvalue_sugarartsweet.csv",sep=";",append=T)

####################
# DIABETES
####################
# get the numbers
dependent = "casoc"
explanatory3 = c("sugarartsweet")
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_sugarartsweet.csv",sep=";")
dependent = "casom"
diabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/numdiab_sugarartsweet.csv",sep=";",append=T)
dependent = "casop"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_sugarartsweet.csv",sep=";",append=T)
dependent = "casoe"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_sugarartsweet.csv",sep=";",append=T)
dependent = "casol"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/numdiab_sugarartsweet.csv",sep=";",append=T)

# Adjusted models
fitcolon<-glm(casoc~as.factor(sugarartsweet)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_sugarartsweet.csv",sep=";")

fitbreast<-glm(casom~as.factor(sugarartsweet)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_sugarartsweet.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(sugarartsweet)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100  + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_sugarartsweet.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(sugarartsweet)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100  + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_sugarartsweet.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(sugarartsweet)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + kcal100  + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/diab_sugarartsweet.csv",sep=";",append=T)


# p-trend
fitcolon<-glm(casoc~sugarartsweetnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_sugarartsweet.csv",sep=";")

fitbreast<-glm(casom~sugarartsweetnum +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100  + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_sugarartsweet.csv",sep=";",append=T)

fitprostate<-glm(casop~sugarartsweetnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_sugarartsweet.csv",sep=";",append=T)

fitstomach<-glm(casoe~sugarartsweetnum + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_sugarartsweet.csv",sep=";",append=T)

fitcll<-glm(casol~sugarartsweetnum   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + kcal100  + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/diab_pvalue_sugarartsweet.csv",sep=";",append=T)








