# Adjusted for dairy products
# only colon and prostate

load("./DATA/mcc_aspartame_27042023.RData")

################################
# For analysis Stratified by Diabetes
################################
nodiab <-subset(mcc2,mcc2$diab==0)
diab <-subset(mcc2,mcc2$diab== 1 )

# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10 + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL  + as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/adjdairy/aspar_noaspar.csv",sep=";")

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100  + METS10AÑOS_2TOTAL  + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/adjdairy/aspar_noaspar.csv",sep=";",append=T)

# the p-value for trend
fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/adjdairy/pvalue_aspar_noaspar.csv",sep=";")
fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL  + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/adjdairy/pvalue_aspar_noaspar.csv",sep=";",append=T)


##############
# NO DIABETES
##############
# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/adjdairy/nodiab_aspar_noaspar.csv",sep=";")


fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/adjdairy/nodiab_aspar_noaspar.csv",sep=";",append=T)

# p-trend
fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/adjdairy/nodiab_pvalue_aspar_noaspar.csv",sep=";")


fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/adjdairy/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

##############
# Diabetes
##############
# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/adjdairy/diab_aspar_noaspar.csv",sep=";")


fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/adjdairy/diab_aspar_noaspar.csv",sep=";",append=T)

# p-trend
fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/adjdairy/diab_pvalue_aspar_noaspar.csv",sep=";")


fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + dairy100 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/adjdairy/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)