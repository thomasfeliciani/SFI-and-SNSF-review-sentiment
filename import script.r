# We wrote this script to import, combine and explore raw review data from the
# two sources: SNSF and SFI. Raw data are not included in the repository.
#
# Runs on R-4.0.3

# Clearing environment and loading libraries
rm(list=ls())
library(readxl)
#require(psych)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(writexl)



# Importing SNSF _______________________________________________________________
#
#
#
manualCoders <- data.frame(rbind(
  c("Junwen", "English"),
  c("Vineeth", "English"),
  c("Judith","German"),
  c("Kalpana","French"),
  c("Thomas","French")
))
names(manualCoders) <- c("coder", "language")



raw <- as.data.frame(read_excel(
  "./data/SNSF SA Sections and Languages.xlsx",
  sheet = "All-stru",
  na = ""
))
#class(raw[,2])
#class(raw[,4])
#class(raw[,5])
#class(raw[,7])
#table(raw[,4])
#table(raw[,5])
r <- data.frame(
  revID = as.character(raw[,1]),
  propID = as.numeric(raw[,2]),
  panel = as.character(raw[,4]),
  language = as.character(raw[,5]),
  fundingDecision = as.character(raw[,6]),
  trueScore = as.numeric(raw[,7]),
  trueRank = as.numeric(raw[,10]),
  S1m1 = rep(NA, times = nrow(raw)),
  S1m2 = rep(NA, times = nrow(raw)),
  S1ma = rep(NA, times = nrow(raw)),
  S1t = rep(NA, times = nrow(raw)),
  S1v = rep(NA, times = nrow(raw)),
  S1score = rep(NA, times = nrow(raw)),
  
  S2m1 = rep(NA, times = nrow(raw)),
  S2m2 = rep(NA, times = nrow(raw)),
  S2ma = rep(NA, times = nrow(raw)),
  S2t = rep(NA, times = nrow(raw)),
  S2v = rep(NA, times = nrow(raw)),
  S2score = rep(NA, times = nrow(raw)),
  
  S3m1 = rep(NA, times = nrow(raw)),
  S3m2 = rep(NA, times = nrow(raw)),
  S3ma = rep(NA, times = nrow(raw)),
  S3t = rep(NA, times = nrow(raw)),
  S3v = rep(NA, times = nrow(raw)),
  S3score = rep(NA, times = nrow(raw)),
  
  S4m1 = rep(NA, times = nrow(raw)),
  S4m2 = rep(NA, times = nrow(raw)),
  S4ma = rep(NA, times = nrow(raw)),
  S4t = rep(NA, times = nrow(raw)),
  S4v = rep(NA, times = nrow(raw)),
  S4score = rep(NA, times = nrow(raw)),
  
  S5m1 = rep(NA, times = nrow(raw)),
  S5m2 = rep(NA, times = nrow(raw)),
  S5ma = rep(NA, times = nrow(raw)),
  S5t = rep(NA, times = nrow(raw)),
  S5v = rep(NA, times = nrow(raw)),
  S5score = rep(NA, times = nrow(raw)),
  
  S6m1 = rep(NA, times = nrow(raw)),
  S6m2 = rep(NA, times = nrow(raw)),
  S6ma = rep(NA, times = nrow(raw)),
  S6t = rep(NA, times = nrow(raw)),
  S6v = rep(NA, times = nrow(raw)),
  S6score = rep(NA, times = nrow(raw)),
  
  
  S7m1 = rep(NA, times = nrow(raw)),
  S7m2 = rep(NA, times = nrow(raw)),
  S7ma = rep(NA, times = nrow(raw)),
  S7t = rep(NA, times = nrow(raw)),
  S7v = rep(NA, times = nrow(raw)),
  S7score = rep(NA, times = nrow(raw))
)
#plot(r$trueScore,r$trueRank)



# Loading SNSF section level data for Ensligh reviews___________________________
rawEN <- as.data.frame(read_excel(
  "./data/SNSF SA Sections and Languages.xlsx",
  sheet = "English_stru",
  na = c("", "n", "NA")
))
#rawEN[,1] %in% r$revID

for(rev in 1:nrow(rawEN)){
  if(!(rawEN[rev,1] %in% r$revID)){warning(
    paste(rawEN[rev,1], "not in the data.frame 'r'"))}
  key <- which(r$revID == rawEN[rev,1])
  
  r$S1m1[key] <- rawEN$`S1 Junwen`[rev]
  r$S1m2[key] <- rawEN$`S1 Vineeth`[rev]
  r$S1t[key] <- rawEN$`S1 TextBlob`[rev]
  r$S1v[key] <- rawEN$`S1 Vader`[rev]
  r$S1score[key] <- rawEN$`S1 Score`[rev]
  
  r$S2m1[key] <- rawEN$`S2 Junwen`[rev]
  r$S2m2[key] <- rawEN$`S2 Vineeth`[rev]
  r$S2t[key] <- rawEN$`S2 TextBlob`[rev]
  r$S2v[key] <- rawEN$`S2 Vader`[rev]
  r$S2score[key] <- rawEN$`S2 Score`[rev]
  
  r$S3m1[key] <- rawEN$`S3 Junwen`[rev]
  r$S3m2[key] <- rawEN$`S3 Vineeth`[rev]
  r$S3t[key] <- rawEN$`S3 TextBlob`[rev]
  r$S3v[key] <- rawEN$`S3 Vader`[rev]
  r$S3score[key] <- rawEN$`S3 Score`[rev]
  
  r$S4m1[key] <- rawEN$`S4 Junwen`[rev]
  r$S4m2[key] <- rawEN$`S4 Vineeth`[rev]
  r$S4t[key] <- rawEN$`S4 TextBlob`[rev]
  r$S4v[key] <- rawEN$`S4 Vader`[rev]
  r$S4score[key] <- rawEN$`S4 Score`[rev]
  
  r$S5m1[key] <- rawEN$`S5 Junwen`[rev]
  r$S5m2[key] <- rawEN$`S5 Vineeth`[rev]
  r$S5t[key] <- rawEN$`S5 TextBlob`[rev]
  r$S5v[key] <- rawEN$`S5 Vader`[rev]
  r$S5score[key] <- rawEN$`S5 Score`[rev]
  
  r$S6m1[key] <- rawEN$`S6 Junwen`[rev]
  r$S6m2[key] <- rawEN$`S6 Vineeth`[rev]
  r$S6t[key] <- rawEN$`S6 TextBlob`[rev]
  r$S6v[key] <- rawEN$`S6 Vader`[rev]
  r$S6score[key] <- rawEN$`S6 Score`[rev]
  
  r$S7m1[key] <- rawEN$`S7 Junwen`[rev]
  r$S7m2[key] <- rawEN$`S7 Vineeth`[rev]
  r$S7t[key] <- rawEN$`S7 TextBlob`[rev]
  r$S7v[key] <- rawEN$`S7 Vader`[rev]
  r$S7score[key] <- rawEN$`S7 Score`[rev]
}


# Loading SNSF section level data for German reviews____________________________
rawDE <- as.data.frame(read_excel(
  "./data/SNSF SA Sections and Languages.xlsx",
  sheet = "German_stru",
  na = c("", "n", "NA")
))
for(rev in 1:nrow(rawDE)){
  if(!(rawDE[rev,1] %in% r$revID)){warning(
    paste(rawDE[rev,1], "not in the data.frame 'r'"))}
  key <- which(r$revID == rawDE[rev,1])
  
  r$S1m1[key] <- rawDE$`S1 Judith's manual coding`[rev]
  r$S1t[key] <- rawDE$`S1 TextBlob`[rev]
  r$S1score[key] <- rawDE$`S1 Score`[rev]
  
  r$S2m1[key] <- rawDE$`S2 Judith's manual coding`[rev]
  r$S2t[key] <- rawDE$`S2 TextBlob`[rev]
  r$S2score[key] <- rawDE$`S2 Score`[rev]
  
  r$S3m1[key] <- rawDE$`S3 Judith's manual coding`[rev]
  r$S3t[key] <- rawDE$`S3 TextBlob`[rev]
  r$S3score[key] <- rawDE$`S3 Score`[rev]
  
  r$S4m1[key] <- rawDE$`S4 Judith's manual coding`[rev]
  r$S4t[key] <- rawDE$`S4 TextBlob`[rev]
  r$S4score[key] <- rawDE$`S4 Score`[rev]
  
  r$S5m1[key] <- rawDE$`S5 Judith's manual coding`[rev]
  r$S5t[key] <- rawDE$`S5 TextBlob`[rev]
  r$S5score[key] <- rawDE$`S5 Score`[rev]
  
  r$S6m1[key] <- rawDE$`S6 Judith's manual coding`[rev]
  r$S6t[key] <- rawDE$`S6 TextBlob`[rev]
  r$S6score[key] <- rawDE$`S6 Score`[rev]
  
  r$S7m1[key] <- rawDE$`S7 Judith's manual coding`[rev]
  r$S7t[key] <- rawDE$`S7 TextBlob`[rev]
  r$S7score[key] <- rawDE$`S7 Score`[rev]
}


# Loading SNSF section level data for French reviews____________________________
rawFR <- as.data.frame(read_excel(
  "./data/SNSF SA Sections and Languages.xlsx",
  sheet = "French_stru",
  na = c("", "n", "NA", "4???")
))
for(rev in 1:nrow(rawFR)){
  if(!(rawFR[rev,1] %in% r$revID)){warning(
    paste(rawFR[rev,1], "not in the data.frame 'r'"))}
  key <- which(r$revID == rawFR[rev,1])
  
  r$S1m1[key] <- rawFR$`S1 KS`[rev]
  r$S1m2[key] <- rawFR$`S1 TF`[rev]
  r$S1t[key] <- rawFR$`S1 TextBlob`[rev]
  r$S1score[key] <- rawFR$`S1 Score`[rev]
  
  r$S2m1[key] <- rawFR$`S2 KS`[rev]
  r$S2m2[key] <- rawFR$`S2 TF`[rev]
  r$S2t[key] <- rawFR$`S2 TextBlob`[rev]
  r$S2score[key] <- rawFR$`S2 Score`[rev]
  
  r$S3m1[key] <- rawFR$`S3 KS`[rev]
  r$S3m2[key] <- rawFR$`S3 TF`[rev]
  r$S3t[key] <- rawFR$`S3 TextBlob`[rev]
  r$S3score[key] <- rawFR$`S3 Score`[rev]
  
  r$S4m1[key] <- rawFR$`S4 KS`[rev]
  r$S4m2[key] <- rawFR$`S4 TF`[rev]
  r$S4t[key] <- rawFR$`S4 TextBlob`[rev]
  r$S4score[key] <- rawFR$`S4 Score`[rev]
  
  r$S5m1[key] <- rawFR$`S5 KS`[rev]
  r$S5m2[key] <- rawFR$`S5 TF`[rev]
  r$S5t[key] <- rawFR$`S5 TextBlob`[rev]
  r$S5score[key] <- rawFR$`S5 Score`[rev]
  
  r$S6m1[key] <- rawFR$`S6 KS`[rev]
  r$S6m2[key] <- rawFR$`S6 TF`[rev]
  r$S6t[key] <- rawFR$`S6 TextBlob`[rev]
  r$S6score[key] <- rawFR$`S6 Score`[rev]
  
  r$S7m1[key] <- rawFR$`S7 KS`[rev]
  r$S7m2[key] <- rawFR$`S7 TF`[rev]
  r$S7t[key] <- rawFR$`S7 TextBlob`[rev]
  r$S7score[key] <- rawFR$`S7 Score`[rev]
}



# Normalizing scores
r$trueScore <- r$trueScore / 6

r$S1m1 <- (r$S1m1 - 1) / (5 - 1)
r$S1m2 <- (r$S1m2 - 1) / (5 - 1)
r$S1score <- (r$S1score - 1) / (6 - 1)
r$S1t <- (r$S1t + 1) / 2
r$S1v <- (r$S1v + 1) / 2

r$S2m1 <- (r$S2m1 - 1) / (5 - 1)
r$S2m2 <- (r$S2m2 - 1) / (5 - 1)
r$S2score <- (r$S2score - 1) / (6 - 1)
r$S2t <- (r$S2t + 1) / 2
r$S2v <- (r$S2v + 1) / 2

r$S3m1 <- (r$S3m1 - 1) / (5 - 1)
r$S3m2 <- (r$S3m2 - 1) / (5 - 1)
r$S3score <- (r$S3score - 1) / (6 - 1)
r$S3t <- (r$S3t + 1) / 2
r$S3v <- (r$S3v + 1) / 2

r$S4m1 <- (r$S4m1 - 1) / (5 - 1)
r$S4m2 <- (r$S4m2 - 1) / (5 - 1)
r$S4score <- (r$S4score - 1) / (6 - 1)
r$S4t <- (r$S4t + 1) / 2
r$S4v <- (r$S4v + 1) / 2

r$S5m1 <- (r$S5m1 - 1) / (5 - 1)
r$S5m2 <- (r$S5m2 - 1) / (5 - 1)
r$S5score <- (r$S5score - 1) / (6 - 1)
r$S5t <- (r$S5t + 1) / 2
r$S5v <- (r$S5v + 1) / 2

r$S6m1 <- (r$S6m1 - 1) / (5 - 1)
r$S6m2 <- (r$S6m2 - 1) / (5 - 1)
r$S6score <- (r$S6score - 1) / (6 - 1)
r$S6t <- (r$S6t + 1) / 2
r$S6v <- (r$S6v + 1) / 2

r$S7m1 <- (r$S7m1 - 1) / (5 - 1)
r$S7m2 <- (r$S7m2 - 1) / (5 - 1)
r$S7score <- (r$S7score - 1) / (6 - 1)
r$S7t <- (r$S7t + 1) / 2
r$S7v <- (r$S7v + 1) / 2




# Aggregation across reviewers
aggr <- function(scores) {
  aggregatedScores <- c()
  for (i in 1:nrow(scores)){
    aggregatedScores[i] <- mean(scores[i,], na.rm = TRUE)}
  return(aggregatedScores)
}

r$S1ma <- aggr(cbind(r$S1m1, r$S1m2))
r$S2ma <- aggr(cbind(r$S2m1, r$S2m2))
r$S3ma <- aggr(cbind(r$S3m1, r$S3m2))
r$S4ma <- aggr(cbind(r$S4m1, r$S4m2))
r$S5ma <- aggr(cbind(r$S5m1, r$S5m2))
r$S6ma <- aggr(cbind(r$S6m1, r$S6m2))
r$S7ma <- aggr(cbind(r$S7m1, r$S7m2))

r$S1ma[is.nan(r$S1ma)] <- NA
r$S2ma[is.nan(r$S2ma)] <- NA
r$S3ma[is.nan(r$S3ma)] <- NA
r$S4ma[is.nan(r$S4ma)] <- NA
r$S5ma[is.nan(r$S5ma)] <- NA
r$S6ma[is.nan(r$S6ma)] <- NA
r$S7ma[is.nan(r$S7ma)] <- NA



# Aggregate across criteria
r$aggrManual <- aggr(cbind(
  r$S1ma, r$S2ma, r$S3ma, r$S4ma, r$S5ma, r$S6ma, r$S7ma  
))
r$aggrManual[is.nan(r$aggrManual)] <- NA

r$aggrT <- aggr(cbind(
  r$S1t, r$S2t, r$S3t, r$S4t, r$S5t, r$S6t, r$S7t  
))
r$aggrT[is.nan(r$aggrT)] <- NA

r$aggrV <- aggr(cbind(
  r$S1v, r$S2v, r$S3v, r$S4v, r$S5v, r$S6v, r$S7v  
))
r$aggrV[is.nan(r$aggrV)] <- NA

r$aggrScore <- aggr(cbind(
  r$S1score, r$S2score, r$S3score, r$S4score, r$S5score, r$S6score, r$S7score  
))
r$aggrScore[is.nan(r$aggrScore)] <- NA


# Add program info:
r$funder <- "SNSF"
r$program <- "SNSF"

rm(raw, rawDE, rawEN, rawFR)


# Loading SNSF sentence level data _____________________________________________
rawS <- as.data.frame(read_excel(
  "./data/SNSF Individual Statements.xlsx",
  sheet = "All individual statements",
  na = ""
))
#max(table(rawS[,1]))
#length(unique(rawS[,3]))
#length(unique(rawS[,1]))
#nrow(rawS)

exclude <- as.data.frame(read_excel(
  "./data/SNSF Individual Statements.xlsx",
  sheet = "Exclude",
  na = ""
))


s <- rawS
for (i in 1:nrow(rawS)){
  if (rawS[i,1] %in% exclude[,1]) {
    print(paste("sentence ID", rawS[i,1], "is removed."))
    rawS[i,] <- NULL
  } 
}

temp <- as.character(s[,15]) # manual scores to be cleaned later
s <- data.frame(
  sentID = as.numeric(s[,1]),
  revID = as.character(paste0(s[,3], ".txt")),
  propID = as.numeric(s[,4]),
  panel = as.character(s[,5]),
  language = as.character(s[,20]),
  fundingDecision = as.character(s[,6]),
  manual = rep(NA, times = nrow(s)),
  TextBlob = as.numeric(s[,21]),
  Vader = as.numeric(s[,22]),
  text = as.character(s[,16]),
  wordCount = as.numeric(s[,17]),
  messycode = as.character(s$`Code Name_new`),
  code = rep(NA, times = nrow(s))
)

for(i in 1:length(temp)){
  if(temp[i] == "1") {s$manual[i] <- 1}
  if(temp[i] == "2") {s$manual[i] <- 2}
  if(temp[i] == "3") {s$manual[i] <- 3}
  if(temp[i] == "4") {s$manual[i] <- 4}
  if(temp[i] == "5") {s$manual[i] <- 5}
}
rm(temp)


# Removing a broken data point (its TextBlob value is far outside the
# theoretical range, which indicates that this data entry was corrupted:
table(s$TextBlob > 1)
s[s$TextBlob == max(s$TextBlob),]
s$TextBlob[which(s$TextBlob == max(s$TextBlob))] <- NA


# Transforming to the correct range:
s$manual <- (s$manual - 1) / (5 - 1)
s$TextBlob <- (s$TextBlob + 1) / 2
s$Vader <- (s$Vader + 1) / 2


# Fixing the messy code names, which contain all kinds of errors: typos,
# inconsistent use of upper- and lower-case, and were written in different
# languages. We do this cleaning by manually specifying each (messy) source
# label that corresponds to each code.
codes <- names(table(s$messycode))

#s[s$code %in% c("achievements to date","Leistungen bisher"),
#  "code"] <- "achievements"

s[s$messycode %in% c("Aktualit?t", "topicality", "Topicality"),
  "code"] <- "topicality"

s[s$messycode %in% c(
  "Allgemeiner Eindruck","general impression","General Impression"),
  "code"] <- "general impression"

s[s$messycode %in% c("Priorit?t","priority","Priority"),
  "code"] <- "priority"

s[s$messycode %in% c("Methoden","methods","Methods"),
  "code"] <- "methods"

s[s$messycode %in% c("Originalit?t","originality","Originality"),
  "code"] <- "originality"

s[s$messycode %in% c("project general","Projekt allgemein","project"),
  "code"] <- "general project"

s[s$messycode %in% c("research plan","Research Plan","Forschungsplan"),
  "code"] <- "research plan"

s[s$messycode %in% c("feasibility","Feasibility","Machbarkeit"),
  "code"] <- "feasibility"

s[s$messycode %in% c(
  "theoretical Relevance","Theoretical Relevance","Theoretische Relevanz"),
  "code"] <- "theoretical relevance"

s[s$messycode %in% c("qualification","Qualification","Qualifikation"),
  "code"] <- "qualification"

s[s$messycode %in% c(
  "Stand der Forschung","state of research", "StateOfKnowledge"),
  "code"] <- "state of research"

s[s$messycode %in% c("Previous Work"),
  "code"] <- "previous work"


s[s$messycode %in% c("Reputation"),
  "code"] <- "reputation"

s[s$messycode %in% c(
  "practical Relevance","Practical Relevance","Praktische Relevanz"),
  "code"] <- "practical relevance"

s[s$messycode %in% c("Pr?sentation","presentation","Presentation"),
  "code"] <- "presentation"


s[s$messycode %in% c("CoApplicant","Mitgesuchsteller"),
  "code"] <- "co-applicant"

s[s$messycode %in% c("Rest"),
  "code"] <- "rest"

s[s$messycode %in% 
    c("Institution / environment","Institution / environment so far",
      "Institution/Umfeld","Institution/Umfeld bisher", "Present Affiliation"),
  "code"] <- "institution/environment"

s[s$messycode %in% c("Kosten","costs","Costs"),
  "code"] <- "costs"

s[s$messycode %in% c("Gutachter","Reviewer"),
  "code"] <- "reviewer"

s[s$messycode %in% 
    c("Previous Affiliation","Institution/Umfeld bisher",
      "Institution / environment so far"),
  "code"] <- "previous affiliation"



s$pseudoSection <- NA

for (i in 1:nrow(s)){
  if (s$code[i] %in% c(
    "qualification",
    "reputation",
    "institution/environment",
    "co-applicant",
    "previous affiliation",
    "previous work"
  )) {s$pseudoSection[i] <- 1} # Applicants
  
  if (s$code[i] %in% c(
    "feasibility",
    "originality",
    "general project",
    "rest",
    "theoretical relevance",
    "costs",
    "methods",
    "priority",
    "state of research",
    "research plan",
    "presentation",
    "general impression",
    "reviewer"
  )) {s$pseudoSection[i] <- 2} # Research programme
  
  if (s$code[i] %in% c(
    "practical relevance",
    "topicality"
  )) {s$pseudoSection[i] <- 3} # Potential for impact

}



# Exploring codes from statement-level data_____________________________________
if(FALSE){############################################
summary(lm(
  s$manual ~ s$TextBlob
))
summary(lm(
  s$manual ~ s$TextBlob + s$code
))

codes <- unique(s$code)

# overall
for (cod in codes){
  #print(cod)
  if(!is.na(cod)){
    temp <- subset(s, s$code == cod)
    if (!is.na(cor(temp$manual, temp$TextBlob))){
      t <- cor.test(temp$manual, temp$TextBlob)
    #t$estimate
    stars <- ""
    if (t$p.value <= 0.05) {stars <- "*"}
    if (t$p.value <= 0.01) {stars <- "**"}
    if (t$p.value <= 0.001) {stars <- "***"}
    cat(paste0(
      cod, ":  ",
      round(t$estimate, digits = 3),
      stars, "\n"
    ))
    }
    
  }
}

# funded only
for (cod in codes){
  #print(cod)
  if(!is.na(cod)){
    temp <- subset(s, s$code == cod & s$fundingDecision == "yes")
    if (!is.na(cor(temp$manual, temp$TextBlob))){
      t <- cor.test(temp$manual, temp$TextBlob)
      #t$estimate
      stars <- ""
      if (t$p.value <= 0.05) {stars <- "*"}
      if (t$p.value <= 0.01) {stars <- "**"}
      if (t$p.value <= 0.001) {stars <- "***"}
      cat(paste0(
        cod, ":  ",
        round(t$estimate, digits = 3),
        stars, "\n"
      ))
    }
    
  }
}
# declined only
for (cod in codes){
  #print(cod)
  if(!is.na(cod)){
    temp <- subset(s, s$code == cod & s$fundingDecision == "no")
    if (!is.na(cor(temp$manual, temp$TextBlob))){
      t <- cor.test(temp$manual, temp$TextBlob)
      #t$estimate
      stars <- ""
      if (t$p.value <= 0.05) {stars <- "*"}
      if (t$p.value <= 0.01) {stars <- "**"}
      if (t$p.value <= 0.001) {stars <- "***"}
      cat(paste0(
        cod, ":  ",
        round(t$estimate, digits = 3),
        stars, "\n"
      ))
    }
    
  }
}

# % funded
for (cod in codes){
  if(!is.na(cod)){
    temp <- subset(s, s$code == cod)
    
    cat(paste0(
      cod, ":  ",
      round(sum(temp$fundingDecision == "yes") /  nrow(temp), digits = 2) * 100,
      "% funded", "\n"
    ))
  }
}


si <- melt(s, id=c(
  "sentID", "revID", "propID", "panel", "language", "fundingDecision",
  "text", "wordCount"))
si$text <- NULL
si$panel[si$panel == "humanities and social sciences"] <-
  "humanities and\nsocial sciences"


si$fundingDecision[si$fundingDecision == "yes" ] <- "funded"
si$fundingDecision[si$fundingDecision == "no" ] <- "not funded"



png(
  "./output/sentences by funding decision.png",
  width = 1600,
  height = 1100,
  units = "px",
  res = 300
)
ggplot(data = si, 
       aes(x = si$fundingDecision, y = si$value, fill=si$variable)) +
  geom_boxplot() +
  ggtitle("review sentences") +
  scale_fill_manual(values = rev(viridis::viridis(4))) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("most negative", "neutral", "most positive")) +
  theme(
    axis.title = element_blank(),
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_line(colour = "black")
  )
dev.off()


} ##############################




# Importing SFI data____________________________________________________________

rawIF <- as.data.frame(read_excel(
  "./data/SFI SA_cleaned.xlsx",
  sheet = "IF",
  na = c("", "NA")
))
rawIvP <- as.data.frame(read_excel(
  "./data/SFI SA_cleaned.xlsx",
  sheet = "IvP",
  na = c("", "NA")
))

aggr <- function(scores) {
  aggregatedScores <- c()
  for (i in 1:nrow(scores)){
    aggregatedScores[i] <- mean(scores[i,], na.rm = TRUE)}
  aggregatedScores[is.nan(aggregatedScores)] <- NA
  return(aggregatedScores)
}


IF <- data.frame(
  revID = as.character(rawIF$`Review ID`),
  propID = rep(NA, times = nrow(rawIF)),
  panel =  rep(NA, times = nrow(rawIF)),
  language =  rep("English", times = nrow(rawIF)),
  fundingDecision =  as.character(rawIF$`Application Status`),
  trueScore = rep(NA, times = nrow(rawIF)),
  trueRank = as.numeric(rawIF$`Actual Rank`)
)
  
IF$S1m1 = (as.numeric(rawIF$`C1 Manual`) + 1) / 2
IF$S1m2 = rep(NA, times = nrow(rawIF))
IF$S1ma = (as.numeric(rawIF$`C1 Manual`) + 1) / 2
IF$S1t = as.numeric(rawIF$`C1 Textblob`)
IF$S1v = as.numeric(rawIF$`C1 Vader`)
IF$S1score = rep(NA, times = nrow(rawIF))
  
IF$S2m1 = (as.numeric(rawIF$`C2 Manual`) + 1) / 2
IF$S2m2 = rep(NA, times = nrow(rawIF))
IF$S2ma = (as.numeric(rawIF$`C2 Manual`) + 1) / 2
IF$S2t = as.numeric(rawIF$`C2 Textblob`)
IF$S2v = as.numeric(rawIF$`C2 Vader`)
IF$S2score = rep(NA, times = nrow(rawIF))
  
IF$S3m1 = (as.numeric(rawIF$`C3 Manual`) + 1) / 2
IF$S3m2 = rep(NA, times = nrow(rawIF))
IF$S3ma = (as.numeric(rawIF$`C3 Manual`) + 1) / 2
IF$S3t = as.numeric(rawIF$`C3 Textblob`)
IF$S3v = as.numeric(rawIF$`C3 Vader`)
IF$S3score = rep(NA, times = nrow(rawIF))
  
IF$S4m1 <- IF$S4m2 <- IF$S4ma <- IF$S4t <- IF$S4v <- IF$S4score <-
IF$S5m1 <- IF$S5m2 <- IF$S5ma <- IF$S5t <- IF$S5v <- IF$S5score <-
IF$S6m1 <- IF$S6m2 <- IF$S6ma <- IF$S6t <- IF$S6v <- IF$S6score <-
IF$S7m1 <- IF$S7m2 <- IF$S7ma <- IF$S7t <- IF$S7v <- IF$S7score <-
  rep(NA, times = nrow(rawIF))
  
IF$aggrManual = aggr(cbind(IF$S1ma, IF$S2ma, IF$S3ma))
IF$aggrT = aggr(cbind(IF$S1t, IF$S2t, IF$S3t))
IF$aggrV = aggr(cbind(IF$S1v, IF$S2v, IF$S3v))
IF$aggrScore = aggr(cbind(IF$S1score, IF$S2score, IF$S3score))
IF$funder = rep("SFI", times = nrow(IF))
IF$program = rep("IF", times = nrow(IF))



IvP <- data.frame(
  revID = as.character(rawIvP$`Review ID`),
  propID = rep(NA, times = nrow(rawIvP)),
  panel =  as.character(rawIvP$Panel),
  language =  rep("English", times = nrow(rawIvP)),
  fundingDecision =  as.character(rawIvP$`Funding decision`),
  trueScore = rep(NA, times = nrow(rawIvP)),
  trueRank = as.numeric(rawIvP$"Actual rank stage 1")
)

IvP$S1m1 = (as.numeric(rawIvP$`C1 Manual`) + 1) / 2
IvP$S1m2 = rep(NA, times = nrow(rawIvP))
IvP$S1ma = (as.numeric(rawIvP$`C1 Manual`) + 1) / 2
IvP$S1t = as.numeric(rawIvP$`C1 Textblob`)
IvP$S1v = as.numeric(rawIvP$`C1 Vader`)
IvP$S1score = rep(NA, times = nrow(rawIvP))

IvP$S2m1 = (as.numeric(rawIvP$`C2 Manual`) + 1) / 2
IvP$S2m2 = rep(NA, times = nrow(rawIvP))
IvP$S2ma = (as.numeric(rawIvP$`C2 Manual`) + 1) / 2
IvP$S2t = as.numeric(rawIvP$`C2 Textblob`)
IvP$S2v = as.numeric(rawIvP$`C2 Vader`)
IvP$S2score = rep(NA, times = nrow(rawIvP))

IvP$S3m1 = (as.numeric(rawIvP$`C3 Manual`) + 1) / 2
IvP$S3m2 = rep(NA, times = nrow(rawIvP))
IvP$S3ma = (as.numeric(rawIvP$`C3 Manual`) + 1) / 2
IvP$S3t = as.numeric(rawIvP$`C3 Textblob`)
IvP$S3v = as.numeric(rawIvP$`C3 Vader`)
IvP$S3score = rep(NA, times = nrow(rawIvP))

IvP$S4m1 <- IvP$S4m2 <- IvP$S4ma <- IvP$S4t <- IvP$S4v <- IvP$S4score <-
  IvP$S5m1 <- IvP$S5m2 <- IvP$S5ma <- IvP$S5t <- IvP$S5v <- IvP$S5score <-
  IvP$S6m1 <- IvP$S6m2 <- IvP$S6ma <- IvP$S6t <- IvP$S6v <- IvP$S6score <-
  IvP$S7m1 <- IvP$S7m2 <- IvP$S7ma <- IvP$S7t <- IvP$S7v <- IvP$S7score <-
  rep(NA, times = nrow(rawIvP))

IvP$aggrManual = aggr(cbind(IvP$S1ma, IvP$S2ma, IvP$S3ma))
IvP$aggrT = aggr(cbind(IvP$S1t, IvP$S2t, IvP$S3t))
IvP$aggrV = aggr(cbind(IvP$S1v, IvP$S2v, IvP$S3v))
IvP$aggrScore = aggr(cbind(IvP$S1score, IvP$S2score, IvP$S3score))
IvP$funder = rep("SFI", times = nrow(IvP))
IvP$program = rep("IvP", times = nrow(IvP))



# Merging SNSF and SFI datasets_________________________________________________
r <- rbind(r, IF, IvP)
r$fundingDecision[r$fundingDecision %in% c(
  "Awarded", "awarded", "offered", "Closed", "Refused")] <- "yes"
r$fundingDecision[r$fundingDecision != "yes"] <- "no"



save (r, s, manualCoders, file = "./data/sfi+snsf review data.RDATA")



# Loading SNSF and SFI datasets_________________________________________________

# Clearing environment, loading libraries and the review data.
rm(list=ls())
library(Hmisc)
library(reshape2)
library(ggplot2)
library(writexl)

load ("./data/sfi+snsf review data.RDATA")


# Printing SFI correlation matrices_____________________________________________
for(f in c("yes", "no", "overall")) {
  print(paste("SFI proposals' funding:", f))
  temp <- subset(r, r$funder == "SFI")
  
  if (f %in% c("yes", "no")){
    temp <- subset(temp, r$fundingDecision == f)
  }
  
  # creating correlation matrix:
  corr <- rcorr(
    as.matrix(cbind(temp$aggrManual, temp$aggrT, temp$aggrV)),
    type = "spearman"
  )
  
  c <- as.data.frame(round(corr[[1]], digits = 3))
  names(c) <- c("manual", "TextBlob","Vader")
  
  c <- cbind(names(c), c)
  names(c)[1] <- ""
  
  for (row in 1:nrow(c)){ for (col in 1:nrow(c)) {
    stars <- ""
    p_value <- corr[[3]][row,col]
    if (!is.na(p_value) & p_value <= 0.05) {stars <- "*"}
    if (!is.na(p_value) & p_value <= 0.01) {stars <- "**"}
    if (!is.na(p_value) & p_value <= 0.001) {stars <- "***"}
    c[row,col + 1] <- paste0(c[row,col + 1], stars)
  }}
  
  print(c)
}
  

# Comparing manual coding in SNSF vs SFI________________________________________
rm(list=ls())
#library(Hmisc)
#library(reshape2)
library(ggplot2)
#library(writexl)

load ("./data/sfi+snsf review data.RDATA")



d <- data.frame(
  program = r$program,
  funding = r$fundingDecision,
  c1 = r$S1ma,
  c2 = r$S2ma,
  c3 = r$S3ma,
  c4 = r$S4ma,
  c5 = r$S5ma,
  c6 = r$S6ma,
  c7 = r$S7ma,
  aggregated = r$aggrManual
)
for (i in 1:nrow(d)) {
  if(d$program[i] == "IF"){d$program[i] <- "SFI-IF"}
  if(d$program[i] == "IvP"){d$program[i] <- "SFI-IvP"}
  ifelse(
    d$funding[i]  == "yes",
    d$funding[i] <- "awarded",
    d$funding[i] <- "rejected"
  )
}

temp <- d
temp$funding <- "overall"
d <- rbind(d, temp)


ggplot(data = d, aes(x = aggregated, y = funding, fill = program)) +
  #geom_violin(color = NA, bw = 0.05) + 
  #geom_boxplot(color = rgb(0, 0, 0, 0.2), alpha = 0.3) +
  geom_boxplot() +
  ggtitle("Overall sentiment score\n(manual - aggregated across criteria/sections)") +
  scale_x_continuous(
    breaks = c(0,1), labels = c("most negative", "most positive")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank()#,
    #axis.text.y = element_blank()
  )


# Now the same at the sentence level (thus only SNSF data):
dd <- data.frame(
  TextBlob = s$TextBlob,
  manual = s$manual,
  funding = s$fundingDecision
)
for (i in 1:nrow(dd)){
  ifelse(
    dd$funding[i]  == "yes",
    dd$funding[i] <- "awarded",
    dd$funding[i] <- "rejected"
  )
}
temp <- dd
temp$funding <- "overall"
dd <- rbind(dd, temp)

ggplot(data = dd, aes(x = manual, y = as.factor(funding))) +
  #geom_violin(color = NA, fill = "blue", bw = 0.05) + 
  #geom_boxplot(color = rgb(0, 0, 0, 0.2), alpha = 0.3) +
  geom_boxplot(fill = 3) +
  ggtitle("Sentiment score\n(manual - statement level)") +
  scale_x_continuous(
    limits = c(0,1),
    breaks = c(0,1),
    labels = c("most negative", "most positive")
  ) +
  #guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank()
  )



# selecting a few idealtypes of review _________________________________________
#table(r$fundingDecision)
#hist(r$aggrManual)
rm(list=ls())
load ("./data/sfi+snsf review data.RDATA")

for (i in 1:nrow(r)) {
  ifelse(
    r$funding[i]  == "yes",
    r$funding[i] <- "awarded",
    r$funding[i] <- "rejected"
  )
}

# manual coding:
for (f in c("awarded", "rejected")){
  for (s in c("extr. positive", "positive", "negative", "extr. negative")) {
    reviews <- r
    
    if (s == "extr.positive") {
      thr <- sort(r$aggrManual[r$funding == f], decreasing = TRUE)[10]
      reviews <- subset(r, r$funding == f & r$aggrManual >= thr)
    }
    
    if (s == "positive") {
      b <- c(
        sort(r$aggrManual[r$funding == f], decreasing = TRUE)[11],
        sort(r$aggrManual[r$funding == f], decreasing = TRUE)[20]
      ) 
      reviews <- subset(
        r, r$funding == f & r$aggrManual <= b[1] & r$aggrManual >= b[2])
    }
    
    if (s == "negative") {
      b <- c(
        sort(r$aggrManual[r$funding == f], decreasing = FALSE)[11],
        sort(r$aggrManual[r$funding == f], decreasing = FALSE)[20]
      ) 
      reviews <- subset(
        r, r$funding == f & r$aggrManual >= b[1] & r$aggrManual <= b[2])
    }
    
    if (s == "extr. negative") {
      thr <- sort(r$aggrManual[r$funding == f], decreasing = FALSE)[10]
      reviews <- subset(r, r$funding == f & r$aggrManual <= thr)
    }
    
    if (nrow(reviews) > 10) {
      reviews <- reviews[sample(1:nrow(reviews), size = 10),]
    }
    
    cat(paste(
      f, "proposals with ", s, "reviews according to manual coders:"
    ), fill = 1)
    cat(reviews$revID, fill = 1)
  }
}


# textblob
# manual coding:
for (f in c("awarded", "rejected")){
  for (s in c("extr. positive", "positive", "negative", "extr. negative")) {
    
    if (s == "extr.positive") {
      thr <- sort(r$aggrT[r$funding == f], decreasing = TRUE)[10]
      reviews <- subset(r, r$funding == f & r$aggrT >= thr)
    }
    
    if (s == "positive") {
      b <- c(
        sort(r$aggrT[r$funding == f], decreasing = TRUE)[11],
        sort(r$aggrT[r$funding == f], decreasing = TRUE)[20]
      ) 
      reviews <- subset(
        r, r$funding == f & r$aggrT <= b[1] & r$aggrT >= b[2])
    }
    
    if (s == "negative") {
      b <- c(
        sort(r$aggrT[r$funding == f], decreasing = FALSE)[11],
        sort(r$aggrT[r$funding == f], decreasing = FALSE)[20]
      ) 
      reviews <- subset(
        r, r$funding == f & r$aggrT >= b[1] & r$aggrT <= b[2])
    }
    
    if (s == "extr. negative") {
      thr <- sort(r$aggrT[r$funding == f], decreasing = FALSE)[10]
      reviews <- subset(r, r$funding == f & r$aggrT <= thr)
    }
    
    if (nrow(reviews) > 10) {
      reviews <- reviews[sample(1:nrow(reviews), size = 10),]
    }
    
    cat(paste(
      f, "proposals with ", s, "reviews according to TextBlob:"
    ), fill = 1)
    cat(reviews$revID, fill = 1)
  }
}




# SNSF sentence level 1/3 ______________________________________________________
#
# SA vs funding decision
rm(list=ls())
load ("./data/sfi+snsf review data.RDATA")

# Comparing sentiment and funding decision
 
t.test(s$TextBlob ~ s$fundingDecision) # Not significant
t.test(s$Vader ~ s$fundingDecision) # Significant


#x = summary(aov(s$TextBlob ~ s$fundingDecision * s$pseudoSection))


# Comparing the correlation between the 3 review languages:
languages <- unique(s$language)[!is.na(unique(s$language))]
results <- data.frame(
  language = languages,
  averageSentimentFunded = rep(NA, times = length(languages)),
  averageSentimentDeclined = rep(NA, times = length(languages)),
  pValue = rep(NA, times = length(languages))
)
for (c in 1:length(languages)){
  r <- s[s$language == languages[c],]
  x <- t.test(r$TextBlob ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)

# Comparing the correlation between Martin's 21 codes:
codes <- unique(s$code)[!is.na(unique(s$code))]
results <- data.frame(
  code = codes,
  averageSentimentFunded = rep(NA, times = length(codes)),
  averageSentimentDeclined = rep(NA, times = length(codes)),
  pValue = rep(NA, times = length(codes))
)
for (c in 1:length(codes)){
  r <- s[s$code == codes[c],]
  x <- t.test(r$Vader ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)

# Comparing the correlation between the 3 SFI-style review sections:
criteria <- 1:3
results <- data.frame(
  SFIcrit = criteria,
  averageSentimentFunded = rep(NA, times = length(criteria)),
  averageSentimentDeclined = rep(NA, times = length(criteria)),
  pValue = rep(NA, times = length(criteria))
)
for (c in 1:length(criteria)){
  r <- s[s$pseudoSection == criteria[c],]
  x <- t.test(r$Vader ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)


# Comparing the correlation between the 3 disciplinary panels:
panels <- unique(s$panel)
results <- data.frame(
  panel = panels,
  averageSentimentFunded = rep(NA, times = length(panels)),
  averageSentimentDeclined = rep(NA, times = length(panels)),
  pValue = rep(NA, times = length(panels))
)
for (c in 1:length(panels)){
  r <- s[s$panel == panels[c],]
  x <- t.test(r$Vader ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)


# SNSF sentence level 2/3 ______________________________________________________
#
# Martin's manual coding vs funding decision

# Comparing manual codes and funding decision

t.test(s$manual ~ s$fundingDecision) # Not significant

# Cross-tabs:
round(prop.table(table(s$manual, s$fundingDecision)),digits = 3)


#x = summary(aov(s$TextBlob ~ s$fundingDecision * s$pseudoSection))


# Comparing the correlation between the 3 review languages:
languages <- unique(s$language)[!is.na(unique(s$language))]
results <- data.frame(
  language = languages,
  averageSentimentFunded = rep(NA, times = length(languages)),
  averageSentimentDeclined = rep(NA, times = length(languages)),
  pValue = rep(NA, times = length(languages))
)
for (c in 1:length(languages)){
  r <- s[s$language == languages[c],]
  x <- t.test(r$manual ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)

# Comparing the correlation between Martin's 21 codes:
codes <- unique(s$code)[!is.na(unique(s$code))]
results <- data.frame(
  code = codes,
  averageSentimentFunded = rep(NA, times = length(codes)),
  averageSentimentDeclined = rep(NA, times = length(codes)),
  pValue = rep(NA, times = length(codes))
)
for (c in 1:length(codes)){
  r <- s[s$code == codes[c],]
  if (any(!is.na(r$manual))){
    x <- t.test(r$manual ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
  }
}
print(results)

# Comparing the correlation between the 3 SFI-style review sections:
criteria <- 1:3
results <- data.frame(
  SFIcrit = criteria,
  averageSentimentFunded = rep(NA, times = length(criteria)),
  averageSentimentDeclined = rep(NA, times = length(criteria)),
  pValue = rep(NA, times = length(criteria))
)
for (c in 1:length(criteria)){
  r <- s[s$pseudoSection == criteria[c],]
  x <- t.test(r$manual ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)


# Comparing the correlation between the 3 disciplinary panels:
panels <- unique(s$panel)
results <- data.frame(
  panel = panels,
  averageSentimentFunded = rep(NA, times = length(panels)),
  averageSentimentDeclined = rep(NA, times = length(panels)),
  pValue = rep(NA, times = length(panels))
)
for (c in 1:length(panels)){
  r <- s[s$panel == panels[c],]
  x <- t.test(r$manual ~ r$fundingDecision)
  results$averageSentimentFunded[c] <- x$estimate[2]
  results$averageSentimentDeclined[c] <- x$estimate[1]
  results$pValue[c] <- x$p.value
}
print(results)



#
# SNSF sentence level 3/3 ______________________________________________________
#
# scenarios: true/false positive/negative


threshold <- sum(s$fundingDecision == "yes") / nrow(s)

results <- data.frame(
  truePos_manual = NA,
  truePos_TextBlob = NA,
  truePos_Vader = NA,
  trueNeg_manual = NA,
  trueNeg_TextBlob = NA,
  trueNeg_Vader = NA
)

# Manual
d <- data.frame(cbind(
  funded = s$fundingDecision == "yes",
  sentiment = s$manual
))
d <- d[!is.na(d$sentiment),]

th = round(threshold * nrow(d))
d$relativPos <- rank(d$sentiment, ties.method = "max") > th
N = nrow(d)

d$truePos <- d$funded & d$relativPos
d$trueNeg <- !d$funded & !d$relativPos


results$truePos_manual <- sum(d$truePos) / N * 100
results$trueNeg_manual <- sum(d$trueNeg) / N * 100

# Textblob
d <- data.frame(cbind(
  funded = s$fundingDecision == "yes",
  sentiment = s$TextBlob
))
d <- d[!is.na(d$sentiment),]
th = round(threshold * nrow(d))
d$relativPos <- rank(d$sentiment, ties.method = "max") > th
d$truePos <- d$funded & d$relativPos
d$trueNeg <- !d$funded & !d$relativPos

results$truePos_TextBlob <- sum(d$truePos) / N * 100
results$trueNeg_TextBlob <- sum(d$trueNeg) / N * 100

# Vader
d <- data.frame(cbind(
  funded = s$fundingDecision == "yes",
  sentiment = s$Vader
))
d <- d[!is.na(d$sentiment),]
th = round(threshold * nrow(d))
d$relativPos <- rank(d$sentiment, ties.method = "max") > th
d$truePos <- d$funded & d$relativPos
d$trueNeg <- !d$funded & !d$relativPos

results$truePos_Vader <- sum(d$truePos) / N * 100
results$trueNeg_Vader <- sum(d$trueNeg) / N * 100

print(results)
