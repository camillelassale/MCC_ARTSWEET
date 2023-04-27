load("./DATA/mcc_aspartame_27042023.RData")
mcc2<-subset(mcc2,!is.na(mcc2$METS10AÑOS_2TOTAL))

# Separately for breast
mccbreast<-subset(mcc2,!is.na(mcc2$menopausia)&!is.na(mcc2$nulipara)&!is.na(mcc2$ACO)&!is.na(mcc2$casom))
table(mccbreast$casom)
################################
# For analysis Stratified by Diabetes
################################
nodiab <-subset(mcc2,mcc2$diab==0)
diab <-subset(mcc2,mcc2$diab== 1 )

nodiabbreast <-subset(mccbreast,mccbreast$diab==0)
diabbreast <-subset(mccbreast,mccbreast$diab== 1 )

# For analyses separately for premenopausal and menopausal
premenop<-subset(mccbreast,mccbreast$menopausia==1)
postmenop <-subset(mccbreast,mccbreast$menopausia== 0 )


###############################################
###############################################
# ASPARTAME AND NON-ASPARTAME in the same model
###############################################
###############################################
# Get the numbers
# exclude missing values
dependent = "casoc"
explanatory3 = c("aspar_cat3","noaspar_cat3")
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar.csv",sep=";")
dependent = "casom"
mccbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar.csv",sep=";",append=T)
dependent = "casop"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar.csv",sep=";",append=T)
dependent = "casoe"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar.csv",sep=";",append=T)
dependent = "casol"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar.csv",sep=";",append=T)



fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10 + kcal100 + sugar10 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar.csv",sep=";",append=T)




# the p-value for trend
#######################

fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~aspar_catnum + noaspar_catnum  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~aspar_catnum + noaspar_catnum   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=mcc2  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar.csv",sep=";",append=T)




################################
### STRATIFIED BY MENOPAUSE ##
# Premenop
dependent = "casom"
explanatory=c("aspar_cat3","noaspar_cat3")
premenop %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar_premenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar_premenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=premenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar_premenop.csv",sep=";")

# Postmenop
postmenop %>%   finalfit(dependent, explanatory, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/supplPA/num_aspar_noaspar_postmenop.csv",sep=";")
fitbreast<-glm(casom~ as.factor(aspar_cat3) + as.factor(noaspar_cat3) +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:6,1],digits=2)," (",round(table1[2:6,2],digits=2),"-",round(table1[2:6,3],digits=2),", p=",round(table1[2:6,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/aspar_noaspar_postmenop.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst)  + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=postmenop  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/pvalue_aspar_noaspar_postmenop.csv",sep=";")



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
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numnodiab_aspar_noaspar.csv",sep=";")
dependent = "casom"
nodiabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/supplPA/numnodiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casop"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numnodiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casoe"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numnodiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casol"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numnodiab_aspar_noaspar.csv",sep=";",append=T)

# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/nodiab_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/nodiab_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/nodiab_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/nodiab_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/nodiab_aspar_noaspar.csv",sep=";",append=T)


# the p-value for trend
#######################
fitcolon<-glm(casoc~aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/nodiab_pvalue_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~aspar_catnum + noaspar_catnum   +as.factor(FHprostate) + edad + as.factor(radio) 
                 + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/nodiab_pvalue_aspar_noaspar.csv",sep=";",append=T)




####################
# DIABETES
####################
# get the numbers
dependent = "casoc"
explanatory3 = c("aspar_cat3","noaspar_cat3")
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numdiab_aspar_noaspar.csv",sep=";")
dependent = "casom"
diabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/supplPA/numdiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casop"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numdiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casoe"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numdiab_aspar_noaspar.csv",sep=";",append=T)
dependent = "casol"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/supplPA/numdiab_aspar_noaspar.csv",sep=";",append=T)

# Adjusted models
fitcolon<-glm(casoc~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/diab_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/diab_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/diab_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/diab_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(aspar_cat3) + as.factor(noaspar_cat3)  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/supplPA/diab_aspar_noaspar.csv",sep=";",append=T)


# the p-value for trend
#######################
fitcolon<-glm(casoc~ aspar_catnum + noaspar_catnum  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/diab_pvalue_aspar_noaspar.csv",sep=";")

fitbreast<-glm(casom~aspar_catnum + noaspar_catnum  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitprostate<-glm(casop~aspar_catnum + noaspar_catnum   +as.factor(FHprostate) + edad + as.factor(radio) 
                 + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitstomach<-glm(casoe~aspar_catnum + noaspar_catnum  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)

fitcll<-glm(casol~aspar_catnum + noaspar_catnum + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              bmi_complementario  + t_ethanol + t_fiber + redmeat10  + kcal100 + sugar10 + METS10AÑOS_2TOTAL + as.factor(ccaa),family="binomial",data=diab  )

summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/supplPA/diab_pvalue_aspar_noaspar.csv",sep=";",append=T)