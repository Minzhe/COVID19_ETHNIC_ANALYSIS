##########################################################
###                     load_data.R                    ###
##########################################################


### ----------------  read data  ---------------- ###
load.patient <- function() {
    cat("Reading data ...\n")
    data <- read.csv("data/THR/COVID_PATIENT_REGISTRY_GRAIN.csv", stringsAsFactors = FALSE)
    data <- data[data$DETECTED_PATIENTS == 1,]    # filter covid patient
    data <- data[!duplicated(data$PAT_ID),]
    cols <- c("PAT_ID", "AGE_AT_ADMISSION", "SEX_NAME", "EHR_CREATE_YEAR", "ETHNIC_GROUP_NAME", "LANGUAGE_NAME", "CUR_PCP_NAME", "VENT", "MORTALITY_CSN", "COVID_ICU")
    data <- data[,cols]
    colnames(data) <- c("PAT_ID", "AGE", "SEX", "YEAR", "ETHNIC", "LANG", "PCP", "VENT", "MORTALITY", "ICU")
    # filter patient
    pat <- filter.hosp()
    data <- data[data$PAT_ID %in% pat,]
    # categorize group
    data <- categorize.group(data)
    # comorbidity
    com <- read.comorbidity()
    data <- merge(data, com, by = "PAT_ID", all.x = TRUE, all.y = FALSE)
    # marker
    marker <- read.marker(pat.id = data$PAT_ID)
    data <- merge(data, marker, by = "PAT_ID", all.x = TRUE, all.y = FALSE)
    # msofa
    msofa <- read.msofa()
    data <- merge(data, msofa, by = "PAT_ID", all.x = TRUE, all.y = FALSE)
    # treatment
    trt <- read.treatment()
    data <- merge(data, trt, by = "PAT_ID", all.x = TRUE, all.y = FALSE)
    # insuranc, PCP
    insur <- read.insurance()
    data <- merge(data, insur, by = "PAT_ID", all.x = TRUE, all.y = FALSE)
    return(data)
}

filter.hosp <- function() {
    cat("Filtering patients.\n")
    addon <- read.csv("data/THR/COVID_PATIENT_REGISTRY_GRAIN_addon_update20201006.csv", stringsAsFactors = FALSE)
    cols <- c("PAT_ID", "khe_THR_ABBR")
    addon <- addon[, cols]
    colnames(addon)[2] <- "THR_ABBR"
    addon <- addon[addon$THR_ABBR != "NULL",]
    hosp <- names(which(table(addon$THR_ABBR) > 25))
    addon <- addon[addon$THR_ABBR %in% hosp,]
    return(addon$PAT_ID)
}

categorize.group <- function(data) {
    # age
    cat("Categorize age, ")
    data$AGE <- as.numeric(data$AGE)
    data <- data[!is.na(data$AGE),]
    data$AGE.GROUP <- "< 40"
    data$AGE.GROUP[(data$AGE >= 40) & (data$AGE < 70)] <- "40 ~ 70"
    data$AGE.GROUP[data$AGE >= 70] <- ">= 70"
    data$AGE.GROUP <- factor(data$AGE.GROUP, levels = c("< 40", "40 ~ 70", ">= 70"))
    # ethnic
    cat("ethnic, ")
    data$ETHNIC.LANG <- data$ETHNIC
    data <- data[data$ETHNIC.LANG != "UNKNOWN",]
    data$ETHNIC.LANG[(data$ETHNIC == "HISPANIC") & (data$LANG == "English")] <- "HISPANIC.EN"
    data$ETHNIC.LANG[(data$ETHNIC == "HISPANIC") & (data$LANG != "English")] <- "HISPANIC.NON-EN"
    data$ETHNIC.LANG[(data$ETHNIC == "NON-HISPANIC") & (data$LANG == "English")] <- "NON-HISPANIC.EN"
    data$ETHNIC.LANG[(data$ETHNIC == "NON-HISPANIC") & (data$LANG != "English")] <- "NON-HISPANIC.NON-EN"
    data$ETHNIC.LANG <- factor(data$ETHNIC.LANG, levels = c("NON-HISPANIC.EN", "NON-HISPANIC.NON-EN", "HISPANIC.EN", "HISPANIC.NON-EN"))
    # sex
    cat("sex, ")
    data$SEX <- factor(data$SEX, levels = c("Male", "Female"))
    # year
    cat("year, ")
    data$YEAR <- as.numeric(data$YEAR)
    data$YEAR.GROUP <- NA
    data$YEAR.GROUP[data$YEAR < 2020] <- "2005 ~ 2019"
    data$YEAR.GROUP[data$YEAR == 2020] <- "2020"
    # pcp
    cat("pcp, ")
    data$PCP.GROUP <- NA
    data$PCP.GROUP[data$PCP == "NO PCP"] <- 0
    data$PCP.GROUP[data$PCP != "NO PCP"] <- 1
    data$PCP.GROUP <- factor(data$PCP.GROUP, levels = c(0, 1), labels = c("No", "Yes"))
    # vent
    cat("vent, ")
    data$VENT <- as.numeric(data$VENT)
    data$VENT <- factor(data$VENT, levels = c(0, 1), labels = c("No", "Yes"))
    # death
    cat("death, ")
    data$MORTALITY <- as.numeric(data$MORTALITY)
    data$MORTALITY[!is.na(data$MORTALITY)] <- 1
    data$MORTALITY[is.na(data$MORTALITY)] <- 0
    data$MORTALITY <- factor(data$MORTALITY, levels = c(0, 1), labels = c("No", "Yes"))
    cat("icu.\n")
    # icu
    data$ICU <- as.numeric(data$ICU)
    data$ICU[is.na(data$ICU)] <- NA
    data$ICU <- factor(data$ICU, levels = c(0, 1), labels = c("No", "Yes"))
    return(data)
}

read.comorbidity <- function() {
    cat("Reading comorbidity ...\n")
    data <- read.csv("data/THR/COVID_PATIENT_REGISTRY_GRAIN.csv", stringsAsFactors = FALSE)
    data <- data[data$DETECTED_PATIENTS == 1,]    # filter covid patient
    data <- data[!duplicated(data$PAT_ID),]
    cols <- c("PAT_ID", "DIABETES", "HYPERTENSION", 
              "COPD", "ASTHMA", "HEART_FAILURE", "CANCER", "KIDNEY_WO_ESRD", "ESRD",
              "LIVER", "SMOKER", "LUNG_OTHER")
    data <- data[,cols]
    data[,2:12] <- apply(data[,2:12], 2, as.numeric)
    data[,2:12] <- apply(data[,2:12], 2, FUN = function(x) factor(x, levels = c(0, 1), labels = c("No", "Yes")))
    return(data)
}

read.marker <- function(pat.id = c()) {
    cat("Reading markers ...\n")
    addon <- read.csv("data/THR/COVID_PATIENT_REGISTRY_GRAIN_addon_update20201006.csv", stringsAsFactors = FALSE)
    cols <- c("PAT_ID", "khe_bl_BMI", "khe_bl_A1c", "khe_bl_Creatinine", "khe_bl_LDH", 
              "khe_bl_D.dimer", "khe_bl_CRP", "khe_bl_Fibrinogen", "khe_bl_Ferritin", "khe_bl_Troponin", 
              "khe_bl_WBC", "khe_bl_Lymphocyte", "khe_bl_Platelet", "khe_bl_SpO2", "khe_bl_SF.ratio")
    addon <- addon[, cols]
    addon <- addon[!duplicated(addon$PAT_ID),]
    if(length(pat.id) > 0) {addon <- addon[addon$PAT_ID %in% pat.id,]}
    colnames(addon)[2:15] <- sapply(strsplit(colnames(addon)[2:15], "khe_bl_"), "[[", 2)
    addon[2:15] <- apply(addon[2:15], 2, as.numeric)
    # categorize
    addon <- categorize.marker(addon, m = "BMI", th = 30)
    addon$OBESITY <- factor(addon$BMI.GROUP, labels = c("No", "Yes"))
    addon <- categorize.marker(addon, m = "A1c", th = 5.6)
    addon <- categorize.marker(addon, m = "Creatinine", th = 1.3)
    addon <- categorize.marker(addon, m = "LDH", th = 243)
    addon <- categorize.marker(addon, m = "D.dimer", th = 0.5)
    addon <- categorize.marker(addon, m = "CRP", th = 20)
    addon <- categorize.marker(addon, m = "Fibrinogen", th = 488)
    addon <- categorize.marker(addon, m = "Ferritin", th = 322)
    addon <- categorize.marker(addon, m = "Troponin", th = 0.032)
    addon <- categorize.marker(addon, m = "WBC", th = 11)
    addon <- categorize.marker(addon, m = "Lymphocyte", th = 3.2)
    addon <- categorize.marker(addon, m = "Platelet", th = 416)
    return(addon)
}

categorize.marker <- function(data, m, th) {
    m.g <- paste0(m, ".GROUP")
    val <- data[,m]
    data[,m.g] <- NA
    data[,m.g][val <= th] <- "Normal"
    data[,m.g][val > th] <- "High"
    data[,m.g] <- factor(data[,m.g], levels = c("Normal", "High"))
    return(data)
}

read.msofa <- function() {
    cat("Reading msofa ...\n")
    msofa <- read.csv("data/THR/THR_processed.csv", stringsAsFactors = FALSE)
    msofa <- msofa[,c("PAT_ID", "thr.det.ind.first12", "msofa.resp.first12", "sf.ratio.first12", "msofa.first12")]
    names(msofa)[2:5] <- c("DI", "MSOFA", "SpO2.FiO2", "MSOFA.score")
    msofa <- categorize.marker(msofa, m = "DI", th = 45)
    msofa <- categorize.marker(msofa, m = "MSOFA", th = 3.5)
    return(msofa)
}

read.treatment <- function() {
    cat("Reading treatment ...\n")
    data <- read.csv("data/THR/COVID_PATIENT_REGISTRY_GRAIN.csv", stringsAsFactors = FALSE)
    data <- data[data$DETECTED_PATIENTS == 1,]    # filter covid patient
    data <- data[!duplicated(data$PAT_ID),]
    cols <- colnames(data)[which(colnames(data) == "AZITHROMYCIN_CNT"):which(colnames(data) == "ANTICOAGULANT_FIRST_TAKEN")]
    cols <- cols[seq(1, length(cols), 4)]
    data <- data[,c("PAT_ID", cols)]
    colnames(data)[2:12] <- sapply(strsplit(colnames(data)[2:12], "_CNT"), "[[", 1)
    data[,2:12] <- apply(data[,2:12], 2, as.numeric)
    data[,2:12][data[,2:12] != 0] <- 1
    data[,2:12] <- apply(data[,2:12], 2, FUN = function(x) factor(x, levels = c(0, 1), labels = c("No", "Yes")))
    return(data)
}

read.insurance <- function() {
    cat("Reading insurance.\n")
    addon <- read.csv("data/THR/COVID_PATIENT_REGISTRY_GRAIN_addon_update20201006.csv", stringsAsFactors = FALSE)
    addon <- addon[, c("PAT_ID", "payor_class")]
    addon <- addon[!duplicated(addon$PAT_ID),]
    colnames(addon)[2] <- "PAYOR"
    return(addon)
}
