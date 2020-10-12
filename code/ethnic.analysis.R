##########################################################
###                   ethnic anaysis                   ###
##########################################################
source("code/load_data.R")
source("code/function.R")

### ----------------  read data  ---------------- ###
data <- load.patient()


### ----------------  outcome  ---------------- ###
# icu
icu <- chisq.table.level(data, var = "ETHNIC.LANG", obs = "ICU", level = "AGE.GROUP")
write.excel(icu, sheet = "ICU", path = "result/Ethnic.Analysis.outcome.xlsx")
# death
death <- chisq.table.level(data, var = "ETHNIC.LANG", obs = "MORTALITY", level = "AGE.GROUP")
write.excel(death, sheet = "MORTALITY", path = "result/Ethnic.Analysis.outcome.xlsx")
# vent
vent <- chisq.table.level(data, var = "ETHNIC.LANG", obs = "VENT", level = "AGE.GROUP")
write.excel(vent, sheet = "VENT", path = "result/Ethnic.Analysis.outcome.xlsx")
# plot
pdf("result/barplot.outcome.pdf", width = 5, height = 5)
plot.bar.ratio(data, data[data$ICU == "Yes",], 
               var = "ETHNIC.LANG", obs = "AGE.GROUP", main = "ICU Rate")
plot.bar.ratio(data, data[data$MORTALITY == "Yes",], 
               var = "ETHNIC.LANG", obs = "AGE.GROUP", main = "Death Rate")
plot.bar.ratio(data, data[data$VENT == "Yes",], 
               var = "ETHNIC.LANG", obs = "AGE.GROUP", main = "Ventilator Rate")
dev.off()


### ----------------  demographic  ---------------- ###
# age
age <- chisq.table(data, var = "ETHNIC.LANG", obs = "AGE.GROUP", ratio = FALSE)
pdf("result/pie.age.group.pdf", width = 10, height = 3)
plot.pie(data, var = "ETHNIC.LANG", obs = "AGE.GROUP")
dev.off()
# sex
sex <- chisq.table(data, var = "ETHNIC.LANG", obs = "SEX")
# year barplot
pdf("result/barplot.year.group.pdf", width = 5, height = 5)
plot.bar(data, var = "ETHNIC.LANG", obs = "YEAR.GROUP", main = "New patient enrollment per month", norm = c(15 * 12, 9))
# legend("topleft", legend = levels(data$ETHNIC.LANG), fill = c("red", "yellow", "green", "blue"), inset = 0.02)
dev.off()
pdf("result/barplot.year.pdf", width = 8, height = 4)
plot.bar(data, var = "ETHNIC.LANG", obs = "YEAR", main = "New patient enrollment")
dev.off()
# year pir plot
pdf("result/pie.year.pdf", width = 11, height = 4)
plot.pie(data, var = "YEAR.GROUP", obs = "ETHNIC.LANG")
dev.off()

### ----------------  treatment  ---------------- ###
treatment <- c("STEROID", "REMDESIVIR", "TOCILIZUMAB",
               "AZITHROMYCIN", "ZINC_SULFATE", "HYDROXYCHLOROQUINE", "SOLUMEDROL", 
               "ARB", "STATIN", "AICE")
for (t in treatment) {
    print(t)
    tmp <- chisq.table.level(data, var = "ETHNIC.LANG", obs = t, level = "AGE.GROUP")
    write.excel(tmp, sheet = t, path = "result/Ethnic.Analysis.treatment.xlsx")
}


### ----------------  comorbidity  ---------------- ###
coms <- c("DIABETES", "HYPERTENSION", "OBESITY",
          "COPD", "ASTHMA", "HEART_FAILURE", "CANCER", "KIDNEY_WO_ESRD", "ESRD",
          "LIVER", "SMOKER", "LUNG_OTHER")
for (com in coms) {
    print(com)
    tmp <- chisq.table.level(data, var = "ETHNIC.LANG", obs = com, level = "AGE.GROUP")
    write.excel(tmp, sheet = com, path = "result/Ethnic.Analysis.comorbidity.xlsx")
}
# plot
pdf("result/barplot.comorbidity.pdf", width = 5, height = 5)
for (com in coms) {
    print(com)
    plot.bar.ratio(data, data[data[,com] == "Yes",], 
                   var = "ETHNIC.LANG", obs = "AGE.GROUP", main = paste(com, "Rate"))
}
dev.off()


### ----------------  healthcare  ---------------- ###
# pcp
health <- chisq.table.level(data, var = "ETHNIC.LANG", obs = "PCP.GROUP", level = "AGE.GROUP")
write.excel(health, sheet = "PCP", path = "result/Ethnic.Analysis.pcp.xlsx")
pdf("result/pie.pcp.pdf", width = 11, height = 4)
plot.pie(data, var = "PCP.GROUP", obs = "ETHNIC.LANG")
dev.off()
# insurance
pdf("result/barplot.insurance.pdf", width = 8, height = 4)
plot.bar(data, var = "ETHNIC.LANG", obs = "PAYOR", drop.obs = c("NULL"))
dev.off()
pdf("result/pie.insurance.pdf", width = 12, height = 3)
plot.pie(data, var = "ETHNIC.LANG", obs = "PAYOR", drop.obs = c("NULL"))
dev.off()

### ----------------  baseline  ---------------- ###
bl.m <- c("MSOFA", "DI", "A1c", "Creatinine", "LDH", "D.dimer", "CRP", "Fibrinogen",
          "Ferritin", "Troponin", "WBC", "Lymphocyte", "Platelet")
# chisquare table
for (m in paste0(bl.m, ".GROUP")) {
    print(m)
    tmp <- chisq.table.level(data, var = "ETHNIC.LANG", obs = m, level = "AGE.GROUP")
    write.excel(tmp, sheet = m, path = "result/Ethnic.Analysis.baseline.xlsx")
}
# plot
pdf("result/pie.msofa.pdf", width = 10, height = 4)
plot.pie(data, var = "ETHNIC.LANG", obs = "MSOFA", drop.obs = c())
dev.off()
pdf("result/density.baseline.pdf", width = 8, height = 4)
plot.density(data, var = "ETHNIC.LANG", obs = "DI", main = "DI")
plot.density(data, var = "ETHNIC.LANG", obs = "A1c", main = "A1c")
plot.density(data, var = "ETHNIC.LANG", obs = "Creatinine", main = "Creatinine", cut = c(0, 3))
plot.density(data, var = "ETHNIC.LANG", obs = "LDH", main = "LDH", cut = c(0, 1500))
plot.density(data, var = "ETHNIC.LANG", obs = "D.dimer", main = "D.dimer", cut = c(0, 7))
plot.density(data, var = "ETHNIC.LANG", obs = "CRP", main = "CRP", cut = c(0, 60))
plot.density(data, var = "ETHNIC.LANG", obs = "Fibrinogen", main = "Fibrinogen")
plot.density(data, var = "ETHNIC.LANG", obs = "Ferritin", main = "Ferritin", cut = c(0, 5000))
plot.density(data, var = "ETHNIC.LANG", obs = "Troponin", main = "Troponin", cut = c(0, 0.5))
plot.density(data, var = "ETHNIC.LANG", obs = "WBC", main = "WBC", cut = c(0, 30))
plot.density(data, var = "ETHNIC.LANG", obs = "Lymphocyte", main = "Lymphocyte", cut = c(0, 5))
plot.density(data, var = "ETHNIC.LANG", obs = "Platelet", main = "Platelet", cut = c(0, 700))
plot.density(data, var = "ETHNIC.LANG", obs = "SpO2.FiO2", main = "SpO2/FiO2", cut = c(0, 600))
plot.density(data, var = "ETHNIC.LANG", obs = "MSOFA.score", main = "MSOFA.score")
dev.off()

### ----------------  logistic regression  ---------------- ###
# icu
icu.fit <- glm(ICU ~ AGE.GROUP + SEX + ETHNIC.LANG 
               + DIABETES + HYPERTENSION + OBESITY
               + COPD + ASTHMA + HEART_FAILURE + CANCER
               + KIDNEY_WO_ESRD + ESRD + LIVER + SMOKER + LUNG_OTHER
               + MSOFA + DI + A1c + Creatinine
               + LDH + D.dimer + CRP + Ferritin
               + WBC + Lymphocyte + Platelet
               , data = data, family = binomial(link = "logit"))
write.reg(icu.fit, "result/Ethnic.Analysis.LogisticReg.ICU.csv")
# mortality
death.fit <- glm(MORTALITY ~ AGE.GROUP + SEX + ETHNIC.LANG 
               + DIABETES + HYPERTENSION + OBESITY
               + COPD + ASTHMA + HEART_FAILURE + CANCER
               + KIDNEY_WO_ESRD + ESRD + LIVER + SMOKER + LUNG_OTHER
               + MSOFA + DI + A1c + Creatinine
               + LDH + D.dimer + CRP + Ferritin
               + WBC + Lymphocyte + Platelet
               , data = data, family = binomial(link = "logit"))
write.reg(death.fit, "result/Ethnic.Analysis.LogisticReg.death.csv")


