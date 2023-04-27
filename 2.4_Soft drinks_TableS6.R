############################

### REFRESCOS LIGHT ###

############################


# Get the numbers
# exclude missing values
dependent = "casoc"
explanatory3 = c("refresc_cat","nosacc_cat")
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/num_refresc.csv",sep=";")
dependent = "casom"
mccbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/refrescos/num_refresc.csv",sep=";",append=T)
dependent = "casop"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/num_refresc.csv",sep=";",append=T)
dependent = "casoe"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/num_refresc.csv",sep=";",append=T)
dependent = "casol"
mcc2 %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/num_refresc.csv",sep=";",append=T)



# Adjusted
fitcolon<-glm(casoc~as.factor(refresc_cat) + as.factor(nosacc_cat)  + fre13210+ fre15100 + fre_calbeverage   +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=mcc2)
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/refresc.csv",sep=";")

fitbreast<-glm(casom~as.factor(refresc_cat) + as.factor(nosacc_cat) + fre13210+ fre15100 + fre_calbeverage  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/refresc.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(refresc_cat) + as.factor(nosacc_cat)+ fre13210 + fre15100 + fre_calbeverage  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/refresc.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(refresc_cat) + as.factor(nosacc_cat) + fre13210+ fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/refresc.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(refresc_cat) + as.factor(nosacc_cat) + fre13210+ fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/refresc.csv",sep=";",append=T)

# the p-value for trend
#######################

fitcolon<-glm(casoc~ refresc_catnum + nosacc_catnum + fre13210+ fre15100 + fre_calbeverage  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=mcc2)
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/pvalue_refresc.csv",sep=";")

fitbreast<-glm(casom~refresc_catnum + nosacc_catnum + fre13210+ fre15100 + fre_calbeverage  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=mcc2 )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/pvalue_refresc.csv",sep=";",append=T)

fitprostate<-glm(casop~refresc_catnum + nosacc_catnum+ fre13210 + fre15100 + fre_calbeverage  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/pvalue_refresc.csv",sep=";",append=T)

fitstomach<-glm(casoe~refresc_catnum + nosacc_catnum+ fre13210 + fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/pvalue_refresc.csv",sep=";",append=T)

fitcll<-glm(casol~refresc_catnum + nosacc_catnum + fre13210+ fre15100 + fre_calbeverage   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=mcc2 )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/pvalue_refresc.csv",sep=";",append=T)



################################
# Stratified by Diabetes
################################


###############
# NON DIABETICS
###############
# get the numbers
dependent = "casoc"
explanatory3 = c("refresc_cat","nosacc_cat")
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numnodiab_refresc.csv",sep=";")
dependent = "casom"
nodiabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/refrescos/numnodiab_refresc.csv",sep=";",append=T)
dependent = "casop"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numnodiab_refresc.csv",sep=";",append=T)
dependent = "casoe"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numnodiab_refresc.csv",sep=";",append=T)
dependent = "casol"
nodiab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numnodiab_refresc.csv",sep=";",append=T)

# adjusted models
fitcolon<-glm(casoc~as.factor(refresc_cat) + as.factor(nosacc_cat)+ fre13210+ fre15100 + fre_calbeverage  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/nodiab_refresc.csv",sep=";")

fitbreast<-glm(casom~as.factor(refresc_cat) + as.factor(nosacc_cat)+ fre13210+ fre15100 + fre_calbeverage  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/nodiab_refresc.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(refresc_cat) + as.factor(nosacc_cat)+ fre13210+ fre15100 + fre_calbeverage  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/nodiab_refresc.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(refresc_cat) + as.factor(nosacc_cat)+ fre13210+ fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/nodiab_refresc.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(refresc_cat) + as.factor(nosacc_cat)+ fre13210+ fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/nodiab_refresc.csv",sep=";",append=T)

#p-value for trend
fitcolon<-glm(casoc~ refresc_catnum + nosacc_catnum + fre15100 + fre13210+ fre_calbeverage  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=nodiab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/nodiab_pvalue_refresc.csv",sep=";")

fitbreast<-glm(casom~refresc_catnum  + nosacc_catnum + fre15100 + fre13210+ fre_calbeverage  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=nodiab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/nodiab_pvalue_refresc.csv",sep=";",append=T)

fitprostate<-glm(casop~refresc_catnum  + nosacc_catnum + fre15100 + fre13210+ fre_calbeverage  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/nodiab_pvalue_refresc.csv",sep=";",append=T)

fitstomach<-glm(casoe~refresc_catnum  + nosacc_catnum + fre15100 + fre13210+ fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/nodiab_pvalue_refresc.csv",sep=";",append=T)

fitcll<-glm(casol~refresc_catnum  + nosacc_catnum + fre15100 + fre13210+ fre_calbeverage   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=nodiab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/nodiab_pvalue_refresc.csv",sep=";",append=T)

####################
# DIABETES
####################
# get the numbers
dependent = "casoc"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numdiab_refresc.csv",sep=";")
dependent = "casom"
diabbreast %>%   finalfit(dependent, explanatory3, metrics=F) -> table4
write.table(table4,file="./ANALYSIS/OUTPUT/refrescos/numdiab_refresc.csv",sep=";",append=T)
dependent = "casop"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numdiab_refresc.csv",sep=";",append=T)
dependent = "casoe"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numdiab_refresc.csv",sep=";",append=T)
dependent = "casol"
diab %>%   finalfit(dependent, explanatory3, metrics=F) -> table3
write.table(table3,file="./ANALYSIS/OUTPUT/refrescos/numdiab_refresc.csv",sep=";",append=T)

# Adjusted models
fitcolon<-glm(casoc~as.factor(refresc_cat)+ as.factor(nosacc_cat) + fre13210 + fre15100 + fre_calbeverage  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(exp(cbind(OR = coef(fitcolon), confint(fitcolon))),digits=2),round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/diab_refresc.csv",sep=";")

fitbreast<-glm(casom~as.factor(refresc_cat)+ as.factor(nosacc_cat) + fre13210 + fre15100 + fre_calbeverage  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
summary(fitbreast)
table1<-cbind(round(exp(cbind(OR = coef(fitbreast), confint(fitbreast))),digits=2),round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/diab_refresc.csv",sep=";",append=T)

fitprostate<-glm(casop~as.factor(refresc_cat)+ as.factor(nosacc_cat) + fre13210 + fre15100 + fre_calbeverage  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(exp(cbind(OR = coef(fitprostate), confint(fitprostate))),digits=2),round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/diab_refresc.csv",sep=";",append=T)

fitstomach<-glm(casoe~as.factor(refresc_cat) + as.factor(nosacc_cat) + fre13210 + fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(exp(cbind(OR = coef(fitstomach), confint(fitstomach))),digits=2),round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/diab_refresc.csv",sep=";",append=T)

fitcll<-glm(casol~as.factor(refresc_cat) + as.factor(nosacc_cat) + fre13210 + fre15100 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(exp(cbind(OR = coef(fitcll), confint(fitcll))),digits=2),round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
try<-paste(round(table1[2:5,1],digits=2)," (",round(table1[2:5,2],digits=2),"-",round(table1[2:5,3],digits=2),", p=",round(table1[2:5,4],digits=3),")", sep="")
write.table(try,file="./ANALYSIS/OUTPUT/refrescos/diab_refresc.csv",sep=";",append=T)


#p-value for trend
fitcolon<-glm(casoc~ refresc_catnum +nosacc_catnum  + fre15100 + fre13210 + fre_calbeverage  +as.factor(FHcolon) + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                score_WCRF_simple + kcal100 + sugar10+ as.factor(ccaa),family="binomial",data=diab )
table1<-cbind(round(coef(summary(fitcolon))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/diab_pvalue_refresc.csv",sep=";")

fitbreast<-glm(casom~refresc_catnum +nosacc_catnum + fre15100 + fre13210 + fre_calbeverage  +as.factor(FHbreast) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) + as.factor(nightshift_) +
                 score_WCRF_simple + kcal100 + sugar10 + as.factor(agefirst) + as.factor(menopausia) + as.factor(nulipara) + as.factor(ACO) +
                 as.factor(ccaa),family="binomial",data=diab  )
table1<-cbind(round(coef(summary(fitbreast))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/diab_pvalue_refresc.csv",sep=";",append=T)

fitprostate<-glm(casop~refresc_catnum +nosacc_catnum + fre15100 + fre13210 + fre_calbeverage  +as.factor(FHprostate) + edad + as.factor(radio) + as.factor(educ) + as.factor(smk) +  as.factor(nightshift_) +
                   score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitprostate)
table1<-cbind(round(coef(summary(fitprostate))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/diab_pvalue_refresc.csv",sep=";",append=T)

fitstomach<-glm(casoe~refresc_catnum +nosacc_catnum + fre15100 + fre13210 + fre_calbeverage  + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
                  score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitstomach)
table1<-cbind(round(coef(summary(fitstomach))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/diab_pvalue_refresc.csv",sep=";",append=T)

fitcll<-glm(casol~refresc_catnum +nosacc_catnum + fre15100 + fre13210 + fre_calbeverage   + edad + as.factor(radio) + as.factor(sexo) + as.factor(educ) + as.factor(smk) + 
              score_WCRF_simple + kcal100 + sugar10 + as.factor(ccaa),family="binomial",data=diab  )
summary(fitcll)
table1<-cbind(round(coef(summary(fitcll))[,'Pr(>|z|)'],digits=3))
table1<-table1[2:3,1]
write.table(table1,file="./ANALYSIS/OUTPUT/refrescos/diab_pvalue_refresc.csv",sep=";",append=T)
