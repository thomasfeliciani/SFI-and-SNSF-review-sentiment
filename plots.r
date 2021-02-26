# Runs on R-4.0.3

# Clearing environment, loading libraries and dataset (not included in the
# repository data).
rm(list = ls())
library(reshape2)
library(ggplot2)
load("./data/sfi+snsf review data.RDATA")
r <- r[r$language != "Italian",]


# Specify the file format: png or tiff?_________________________________________
figureFormat <- "png"


# Figure 1______________________________________________________________________
SNSFsectionLabels <- c(
  "scientific impact",
  "originality",
  "suitability of\nmethods",
  "feasibility",
  "experience and\npast performance",
  "specific abilities",
  "other comments"
)
SFIsectionLabels <- 
  c("applicant", "proposed\nresearch", "potential for\nimpact")

d <- r[r$funder == "SNSF",]
for (sec in 1:7){
  temp <- data.frame(
    revID = d$revID,
    funder = d$funder,
    program = d$program,
    fundingDecision = d$fundingDecision,
    trueScore = d$trueScore,
    manual = d[,paste0("S", sec, "ma")],
    TextBlob = d[,paste0("S", sec, "t")],
    VADER = d[,paste0("S", sec, "v")],
    Sscore = d[,paste0("S", sec, "score")],
    section = rep(SNSFsectionLabels[sec], times = nrow(d))
  )
  ifelse(
    sec == 1,
    ddSNSF <- temp,
    ddSNSF <- rbind(ddSNSF, temp)
  )
}
d <- r[r$funder == "SFI",]
for (sec in 1:3){
  temp <- data.frame(
    revID = d$revID,
    funder = d$funder,
    program = d$program,
    fundingDecision = d$fundingDecision,
    trueScore = d$trueScore,
    manual = d[,paste0("S", sec, "ma")],
    TextBlob = d[,paste0("S", sec, "t")],
    VADER = d[,paste0("S", sec, "v")],
    Sscore = d[,paste0("S", sec, "score")],
    section = rep(SFIsectionLabels[sec], times = nrow(d))
  )
  ifelse(
    sec == 1,
    ddSFI <- temp,
    ddSFI <- rbind(ddSFI, temp)
  )
}

dd <- rbind(ddSNSF, ddSFI)
dd$section <- factor(
  dd$section,
  levels = c(SNSFsectionLabels, SFIsectionLabels)
)

# Transforming SA scores in [-1,1] to match the manual scale [0,1]:
dd$VADER <- (dd$VADER + 1) / 2
dd$TextBlob <- (dd$TextBlob + 1) / 2

dd <- melt(
  data = dd,
  id.vars = c(
    "revID",
    "funder",
    "program",
    "fundingDecision",
    "trueScore",
    "section"
  )
)

# Reconding variables
dd$program[dd$program == "IF"] <- "SFI:IF"
dd$program[dd$program == "IvP"] <- "SFI:IvP"
#dd <- dd[!is.na(dd$manual) & dd$section != "other comments",]
#dd$Sscore <- (dd$Sscore * 5) + 1
dd$variable <- as.character(dd$variable)
dd$variable[dd$variable == "Sscore"] <- "review score"
levs <- c("review score", "manual", "TextBlob", "VADER")
dd$variabe <-
  factor(dd$variable, levels = levs)


# Defining color palette
bpcl <- viridisLite::viridis(begin = 0.1, n = 4)
bpcl[1] <- "#762525"#"#1f1230FF"





figurePar <- list(
  filename = paste0("./plots/Figure 1.", figureFormat),
  height = 2000,
  width = 1200,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd, aes(x = section, y = value)) +
  geom_boxplot(aes(
    color = factor(variable, levels = levs),
    fill = factor(variable, levels = levs)
  )) +
  geom_vline(
    xintercept = seq(0, length(unique(dd$section))) + .5,
    color="#f5f5f5"
  ) +
  facet_wrap(~ program, ncol=1, scales = "free") +
  #facet_grid(cols = vars(program), space = "free", scales = "free") +
  xlab("review section") +
  ylab("(sentiment) score") +
  ggtitle("sentiment:\nSFI and SNSF review texts") +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl) +
  scale_fill_manual(values = alpha(bpcl, 0.5)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.x = element_line(color = "#f5f5f5", size = 30),
    panel.border = element_blank(),
    legend.position = "top",
    legend.key.size = unit(10, "pt"),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dev.off()



# Figure 2______________________________________________________________________
sectionLabels <- c(
  "scientific impact",
  "originality",
  "suitability of\nmethods",
  "feasibility",
  "experience and\npast performance",
  "specific abilities",
  "other comments"
)

d <- r[r$funder == "SNSF",]
for (sec in 1:7){
  temp <- data.frame(
    revID = d$revID,
    funder = d$funder,
    program = d$program,
    fundingDecision = d$fundingDecision,
    trueScore = d$trueScore,
    manual = d[,paste0("S", sec, "ma")],
    TextBlob = d[,paste0("S", sec, "t")],
    VADER = d[,paste0("S", sec, "v")],
    Sscore = d[,paste0("S", sec, "score")],
    section = rep(SNSFsectionLabels[sec], times = nrow(d))
  )
  ifelse(
    sec == 1,
    dd <- temp,
    dd <- rbind(dd, temp)
  )
}
dd$section <- factor(dd$section, levels = sectionLabels)#

# Transforming SA scores in [-1,1] to match the manual scale [0,1]:
dd$VADER <- (dd$VADER + 1) / 2
dd$TextBlob <- (dd$TextBlob + 1) / 2

dd <- melt(
  data = dd,
  id.vars = c(
    "revID", "funder", "program", "fundingDecision", "trueScore",
    "Sscore", "section")
)
dd <- dd[!is.na(dd$Sscore) & dd$section != "other comments",]
#dd$Sscore <- as.character((dd$Sscore * 5) + 1)

dd$Sscore <- (dd$Sscore * 5) + 1
dd$Sscore[dd$Sscore <= 3] <- "1 to 3      "



figurePar <- list(
  filename = paste0("./plots/Figure 2.", figureFormat),
  height = 1150,
  width = 1800,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd, aes(x = Sscore, y = value)) +
  geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~section, nrow = 2) +
  xlab("review score\n(Likert scale from 1 to 6)") +
  ylab("sentiment") +
  ggtitle("SNSF reviews") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$trueScore))) + .5,
    color="#f5f5f5"
  ) +
  scale_y_continuous(
    limits = c(0,1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  #scale_x_discrete(limits=c(1, 3,4,5)) +
  scale_color_viridis_d(begin = 0.4, option = "D") +
  scale_fill_viridis_d(begin = 0.4, option = "D", alpha = 0.3) +
  theme(
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    #strip.background = element_rect(fill = "#ebebeb"),
    axis.line = element_line()#,
  )

dev.off()






# Figure 3______________________________________________________________________

d <- data.frame(
  revID = r$revID,
  funder = r$funder,
  program = r$program,
  fundingDecision = r$fundingDecision,
  trueScore = r$trueScore,
  manual = r$aggrManual,
  TextBlob = r$aggrT,
  VADER = r$aggrV
)

# Transforming SA scores in [-1,1] to match the manual scale [0,1]:
d$VADER <- (d$VADER + 1) / 2
d$TextBlob <- (d$TextBlob + 1) / 2

dd <- melt(
  data = d,
  id.vars = c(
    "revID",
    "funder",
    "program",
    "fundingDecision",
    "trueScore",
    "manual"
  )
)

# Reconding variables
dd$program[dd$program == "IF"] <- "SFI:IF"
dd$program[dd$program == "IvP"] <- "SFI:IvP"

# Discretizing manual scores into a standard number of bins:
manual <- c()
for (i in 1:nrow(dd)){
  x <- dd$manual[i]
  if(!is.na(x)){
    if(x >= 0 & x < 0.25) manual[i] <- "most negative"#"[0,0.125]"
    if(x >= 0.25 & x < 0.5) manual[i] <- "moderate negative"#"(0.125,0.375]"
    if(x >= 0.5 & x < 0.75) manual[i] <- "neutral"#"(0.375,0.625]"
    if(x >= 0.75 & x < 1) manual[i] <- "moderate positive"#"(0.625,0.875]"
    if(x == 1) manual[i] <- "most positive"#"(0.875,1]"
  }
}
manual <- factor(
  manual,
  levels = c(
    "most negative", "moderate negative", "neutral",
    "moderate positive", "most positive")
  #"[0,0.125]","(0.125,0.375]","(0.375,0.625]","(0.625,0.875]","(0.875,1]")
)
dd$manual <- manual
dd <- dd[!is.na(dd$manual),]



figurePar <- list(
  filename = paste0("./plots/Figure 3.", figureFormat),
  height = 1500,
  width = 950,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd, aes(x = manual, y = value)) +
  geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~program, nrow = 3) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  ggtitle("sentiment:\nSFI and SNSF reviews") +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_viridis_d(begin = 0.7, option = "D") +
  scale_fill_viridis_d(begin = 0.7, option = "D", alpha = 0.3) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

dev.off()








# Figure 4______________________________________________________________________
d <- data.frame(
  program = r$program,
  funding = r$fundingDecision,
  TextBlob = r$aggrT,
  VADER = r$aggrV,
  manual = r$aggrManual
)
for (i in 1:nrow(d)) {
  if(d$program[i] == "IF"){d$program[i] <- "SFI:IF"}
  if(d$program[i] == "IvP"){d$program[i] <- "SFI:IvP"}
  ifelse(
    d$funding[i]  == "yes",
    d$funding[i] <- "awarded",
    d$funding[i] <- "rejected"
  )
}

# Transforming SA scores in [-1,1] to match the manual scale [0,1]:
d$VADER <- (d$VADER + 1) / 2
d$TextBlob <- (d$TextBlob + 1) / 2

dd <- melt(
  data = d,
  id.vars = c("program", "funding")
)
dd$variable <-
  factor(as.character(dd$variable), levels = c("manual", "TextBlob", "VADER"))




figurePar <- list(
  filename = paste0("./plots/Figure 4.", figureFormat),
  height = 1400,
  width = 1500,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(
  data = dd,
  aes(x = funding, y = value, color = variable, fill = variable)
) +
  geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(program)) +
  ggtitle("Sentiment score and funding decision") +
  ylab("sentiment") +
  scale_y_continuous(
    breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0,0),
    labels = c("most negative", "neutral", "most positive")
  ) +
  scale_color_viridis_d(begin = 0.4, option = "D") +
  scale_fill_viridis_d(begin = 0.4, option = "D", alpha = 0.3) +
  theme(
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    panel.spacing.y = unit(1, "lines"),
    strip.background = element_rect(fill = "#ebebeb"),
    strip.text.y = element_text(angle = 0),
    axis.line = element_line(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

dev.off()





# Figure 5______________________________________________________________________
# RQ3 at the statement level (thus only SNSF data).
#
#

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
color <- viridisLite::viridis(n = 1, begin = 0.4, option = "D")
#temp <- dd
#temp$funding <- "overall"
#dd <- rbind(dd, temp)



figurePar <- list(
  filename = paste0("./plots/Figure 5.", figureFormat),
  height = 750,
  width = 900,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd, aes(x = funding, y = manual)) +
  geom_boxplot(fill = color, color = color, alpha = 0.3) +
  ggtitle("Statement level") +
  ylab("manually coded sentiment") +
  scale_y_continuous(
    limits = c(0,1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most negative", "neutral", "most positive")
  ) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_blank()
  )

dev.off()



# Figure A1_____________________________________________________________________
d <- r[r$funder == "SNSF",]
for (sec in 1:7){
  temp <- data.frame(
    revID = d$revID,
    funder = d$funder,
    program = d$program,
    panel = d$panel,
    language = d$language,
    fundingDecision = d$fundingDecision,
    trueScore = d$trueScore,
    manual = d[,paste0("S", sec, "ma")],
    TextBlob = d[,paste0("S", sec, "t")],
    VADER = d[,paste0("S", sec, "v")],
    Sscore = d[,paste0("S", sec, "score")],
    section = rep(SNSFsectionLabels[sec], times = nrow(d))
  )
  ifelse(
    sec == 1,
    dd <- temp,
    dd <- rbind(dd, temp)
  )
}


# Transforming SA scores in [-1,1] to match the manual scale [0,1]:
dd$VADER <- (dd$VADER + 1) / 2
dd$TextBlob <- (dd$TextBlob + 1) / 2

dd <- melt(
  data = dd,
  id.vars = c(
    "revID",
    "funder",
    "program",
    "panel",
    "language",
    "fundingDecision",
    "trueScore",
    "section"
  )
)

# Reconding variables
dd$program[dd$program == "IF"] <- "SFI:IF"
dd$program[dd$program == "IvP"] <- "SFI:IvP"
dd$panel[dd$panel == "humanities and social sciences"] <-
  "humanities and\nsocial sciences"
#dd <- dd[!is.na(dd$manual) & dd$section != "other comments",]
#dd$Sscore <- (dd$Sscore * 5) + 1
dd$variable <- as.character(dd$variable)
dd$variable[dd$variable == "Sscore"] <- "review score"
levs <- c("review score", "manual", "TextBlob", "VADER")
dd$variable <-
  factor(dd$variable, levels = levs)


bpcl <- viridisLite::viridis(begin = 0.1, n = 4)
bpcl[1] <- "#762525"#"#1f1230FF"
#bpcl <- substr(bpcl, start = 1, stop = 7)



figurePar <- list(
  filename = paste0("./plots/Figure A1.", figureFormat),
  height = 1000,
  width = 1000,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd, aes(y = value)) +
  geom_boxplot(aes(
    color = factor(variable, levels = levs),
    fill = factor(variable, levels = levs)
  )) +
  facet_wrap(~ panel, nrow=1) +
  ylab("(sentiment) score") +
  ggtitle("SNSF review sections by panel") +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most negative", "neutral", "most positive")
  ) +
  scale_color_manual(values = bpcl) +
  scale_fill_manual(values = alpha(bpcl, 0.5)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y =element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size = unit(10, "pt"),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    axis.line = element_line(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()


# Figure A2_____________________________________________________________________
figurePar <- list(
  filename = paste0("./plots/Figure A2.", figureFormat),
  height = 1000,
  width = 1300,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)
ggplot(data = dd, aes(y = value)) +
  geom_boxplot(aes(
    color = factor(variable, levels = levs),
    fill = factor(variable, levels = levs)
  )) +
  facet_wrap(~ language, nrow=1) +
  ylab("(sentiment) score") +
  ggtitle("SNSF review sections by language") +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most negative", "neutral", "most positive")
  ) +
  scale_color_manual(values = bpcl) +
  scale_fill_manual(values = alpha(bpcl, 0.5)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y =element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size = unit(10, "pt"),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    axis.line = element_line(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()


# Figure A3_____________________________________________________________________
SNSFsectionLabels <- c(
  "scientific impact",
  "originality",
  "suitability of\nmethods",
  "feasibility",
  "experience and\npast performance",
  "specific abilities",
  "other comments"
)
SFIsectionLabels <- 
  c("applicant", "proposed\nresearch", "potential for\nimpact")

d <- r[r$funder == "SNSF",]
for (sec in 1:7){
  temp <- data.frame(
    revID = d$revID,
    funder = d$funder,
    program = d$program,
    fundingDecision = d$fundingDecision,
    trueScore = d$trueScore,
    manual = d[,paste0("S", sec, "ma")],
    TextBlob = d[,paste0("S", sec, "t")],
    VADER = d[,paste0("S", sec, "v")],
    Sscore = d[,paste0("S", sec, "score")],
    section = rep(SNSFsectionLabels[sec], times = nrow(d))
  )
  ifelse(
    sec == 1,
    ddSNSF <- temp,
    ddSNSF <- rbind(ddSNSF, temp)
  )
}
d <- r[r$funder == "SFI",]
for (sec in 1:3){
  temp <- data.frame(
    revID = d$revID,
    funder = d$funder,
    program = d$program,
    fundingDecision = d$fundingDecision,
    trueScore = d$trueScore,
    manual = d[,paste0("S", sec, "ma")],
    TextBlob = d[,paste0("S", sec, "t")],
    VADER = d[,paste0("S", sec, "v")],
    Sscore = d[,paste0("S", sec, "score")],
    section = rep(SFIsectionLabels[sec], times = nrow(d))
  )
  ifelse(
    sec == 1,
    ddSFI <- temp,
    ddSFI <- rbind(ddSFI, temp)
  )
}

dd <- rbind(ddSNSF, ddSFI)
dd$section <- factor(
  dd$section,
  levels = c(SNSFsectionLabels, SFIsectionLabels)
)



# Transforming SA scores in [-1,1] to match the manual scale [0,1]:
dd$VADER <- (dd$VADER + 1) / 2
dd$TextBlob <- (dd$TextBlob + 1) / 2

dd <- melt(
  data = dd,
  id.vars = c(
    "revID",
    "funder",
    "program",
    "fundingDecision",
    "trueScore",
    "manual",
    "Sscore", #
    "section" #
  )
)

# Reconding variables
dd$program[dd$program == "IF"] <- "SFI:IF"
dd$program[dd$program == "IvP"] <- "SFI:IvP"
#dd <- dd[!is.na(dd$manual) & dd$section != "other comments",]
#dd$Sscore <- (dd$Sscore * 5) + 1

# Discretizing manual scores into a standard number of bins:
manual <- c()
for (i in 1:nrow(dd)){
  x <- dd$manual[i]
  if(!is.na(x)){
    if(x >= 0 & x < 0.25) manual[i] <- "most negative"#"[0,0.125]"
    if(x >= 0.25 & x < 0.5) manual[i] <- "moderate negative"#"(0.125,0.375]"
    if(x >= 0.5 & x < 0.75) manual[i] <- "neutral"#"(0.375,0.625]"
    if(x >= 0.75 & x < 1) manual[i] <- "moderate positive"#"(0.625,0.875]"
    if(x == 1) manual[i] <- "most positive"#"(0.875,1]"
  }
}
manual <- factor(
  manual,
  levels = c(
    "most negative", "moderate negative", "neutral",
    "moderate positive", "most positive")
  #"[0,0.125]","(0.125,0.375]","(0.375,0.625]","(0.625,0.875]","(0.875,1]")
)
dd$manual <- manual
dd <- dd[!is.na(dd$manual),]




figurePar <- list(
  filename = paste0("./plots/Figure A3a.", figureFormat),
  height = 1500,
  width = 1600,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd[dd$program == "SNSF",], aes(x = manual, y = value)) +
  geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~ section, nrow = 2) +
  #facet_grid(cols = vars(section)) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  ggtitle("A. sentiment in SNSF review sections") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_viridis_d(begin = 0.7, option = "D") +
  scale_fill_viridis_d(begin = 0.7, option = "D", alpha = 0.3) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 75, hjust = 1)
  )
dev.off()




figurePar <- list(
  filename = paste0("./plots/Figure A3b.", figureFormat),
  height = 1000,
  width = 1300,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd[dd$program == "SFI:IvP",], aes(x = manual, y = value)) +
  geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~ section, nrow = 1) +
  #facet_grid(cols = vars(section)) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  ggtitle("B. sentiment in SFI:IvP review sections") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_viridis_d(begin = 0.7, option = "D") +
  scale_fill_viridis_d(begin = 0.7, option = "D", alpha = 0.3) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    #strip.text.x = element_text(angle = 90),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()




figurePar <- list(
  filename = paste0("./plots/Figure A3c.", figureFormat),
  height = 1000,
  width = 1300,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(data = dd[dd$program == "SFI:IF",], aes(x = manual, y = value)) +
  geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~ section, nrow = 1) +
  #facet_grid(cols = vars(section)) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  ggtitle("C. sentiment in SFI:IF review sections") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_viridis_d(begin = 0.7, option = "D") +
  scale_fill_viridis_d(begin = 0.7, option = "D", alpha = 0.3) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#ebebeb"),
    #strip.text.x = element_text(angle = 90),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()

