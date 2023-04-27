library(data.table)
library(tableone)
library(tidyverse)
library(finalfit)

load("./DATA/mcc_aspartame_27042023.RData")
############### 1. DESCRIPTION ################

vardesc<-c("edad", "sexo","educ","smk","bmi_complementario","obese","diab","score_WCRF_simple","nightshift","radio",
            "t_energy","gra_redmeat","fv","dairy","t_ethanol","t_fiber","t_sugars_dig",
            "aspartame","noaspar","artsweet",
            "menopausia","nulipara","ACO","TSH",
            "casoc","casom","casop","casoe","casol")
nonorm<-c("fre14400","fre15100","fre13220","fre13210","aspartame","noaspar","artsweet")
# Overall
taball <-CreateTableOne(vardesc,data=mcc2,includeNA =F)
taball<-print(taball, nonnormal = nonorm, noSpaces = TRUE, varLabels=T, formatOptions = list(big.mark = ","))
write.table(taball,file="./ANALYSIS/OUTPUT/Table1_all_noNA_8452.csv",sep=";")

taball <-CreateTableOne(vardesc,data=mcc2,includeNA =F)
taball<-print(taball, noSpaces = TRUE, varLabels=T, formatOptions = list(big.mark = ","))
write.table(taball,file="./ANALYSIS/OUTPUT/Table1_all_noNA_8452meanSD.csv",sep=";")

# By sweeteners use
tabsweet <-CreateTableOne(vardesc,strata="artsweet_cat3",data=mcc2,includeNA =F)
trysweet<-print(tabsweet,  nonnormal = nonorm, noSpaces = TRUE, varLabels=T, formatOptions = list(big.mark = ","))
write.table(trysweet,file="./ANALYSIS/OUTPUT/Table1_by_artsweet_8452.csv",sep=";")
# the means for aspartame, noaspartame and total AS
vardesc2<-c("aspartame","noaspar","artsweet")
tabsweet <-CreateTableOne(vardesc,strata="artsweet_cat3",data=mcc2,includeNA =F)
trysweet<-print(tabsweet,  noSpaces = TRUE, varLabels=T, formatOptions = list(big.mark = ","))

write.table(trysweet,file="./ANALYSIS/OUTPUT/Table1_by_artsweet_8452means.csv",sep=";")

# By Diabetes
vardesc2<-c("edad", "sexo","educ","smk","bmi_complementario","obese","score_WCRF_simple",
            "t_energy","gra_redmeat","fv","dairy","t_ethanol","t_fiber","t_sugars_dig",
            "aspartame","noaspar","artsweet",
            "casoc","casom","casop","casoe","casol")
tabsweet <-CreateTableOne(vardesc2,strata="diab",data=mcc2,includeNA =F)
trysweet<-print(tabsweet,  nonnormal = nonorm, noSpaces = TRUE, varLabels=T, formatOptions = list(big.mark = ","))

write.table(trysweet,file="./ANALYSIS/OUTPUT/Table1_by_diab_8452.csv",sep=";")

########## IN CONTROLS ONLY ###########

controls<-subset(mcc2,mcc2$caseall==0)

# By sweeteners use
tabsweet <-CreateTableOne(vardesc,strata="artsweet_cat3",data=controls,includeNA =F)
trysweet<-print(tabsweet,  nonnormal = nonorm, noSpaces = TRUE, varLabels=T, formatOptions = list(big.mark = ","))

write.table(trysweet,file="./ANALYSIS/OUTPUT/Table1_by_artsweet_CONTROLS.csv",sep=";")
