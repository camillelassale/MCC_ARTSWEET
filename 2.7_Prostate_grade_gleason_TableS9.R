load("./DATA/mcc_aspartame_27042023.RData")

# low grade prostate cancer
low<-subset(men,men$gleason7==0|men$gleason7==1)
# high grade prostate cancer
high<-subset(men,men$gleason7==0|men$gleason7==2)
################################
# For analysis Stratified by Diabetes
################################
nodiablow <-subset(low,low$diab==0)
nodiabhigh <-subset(high,high$diab==0)
diablow <-subset(low,low$diab==1)
diabhigh <-subset(high,high$diab==1)

###########
# OVerall #
###########

    # Low grade

dependent = "casop"
explanatory3 = c("aspar_cat3","noaspar_cat3")
low %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/gleason/lowp_aspar_noaspar.csv",sep=";")


fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=low  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/gleason/lowp_aspar_noaspar.csv",sep=";",append=T)
# the p-value for trend
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum   +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=low  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/gleason/lowp_aspar_noaspar.csv",sep=";",append=T)


    # High grade

dependent = "casop"
explanatory3 = c("aspar_cat3","noaspar_cat3")
high %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/gleason/highp_aspar_noaspar.csv",sep=";")

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),
                 family="binomial",data=high  )

summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/gleason/highp_aspar_noaspar.csv",sep=";",append=T)
# the p-value for trend
#######################
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),
                 family="binomial",data=high  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/gleason/highp_aspar_noaspar.csv",sep=";",append=T)

######################
    # NO DIABETES #
######################

    # LOW GRADE

dependent = "casop"
explanatory3 = c("aspar_cat3","noaspar_cat3")
nodiablow %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/gleason/nodiablowp_aspar_noaspar.csv",sep=";")
fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),
                 family="binomial",data=nodiablow  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/gleason/nodiablowp_aspar_noaspar.csv",sep=";",append=T)
# the p-value for trend
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiablow )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/gleason/nodiablowp_aspar_noaspar.csv",sep=";",append=T)


    # HIGH GRADE

nodiabhigh %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/gleason/nodiabhighp_aspar_noaspar.csv",sep=";")
fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiabhigh  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/gleason/nodiabhighp_aspar_noaspar.csv",sep=";",append=T)
# the p-value for trend
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiabhigh )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/gleason/nodiabhighp_aspar_noaspar.csv",sep=";",append=T)


######################
    # DIABETES #
######################

    # LOW GRADE #
diablow %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/gleason/diablowp_aspar_noaspar.csv",sep=";")
fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diablow  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/gleason/diablowp_aspar_noaspar.csv",sep=";",append=T)
# the p-value for trend
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diablow )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/gleason/diablowp_aspar_noaspar.csv",sep=";",append=T)

    # HIGH GRADE #

diabhigh %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/gleason/diabhighp_aspar_noaspar.csv",sep=";")

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diabhigh  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/gleason/diabhighp_aspar_noaspar.csv",sep=";",append=T)
# the p-value for trend
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diabhigh )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/gleason/diabhighp_aspar_noaspar.csv",sep=";",append=T)
