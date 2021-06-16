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


ggplot(
  data = dd,
  aes(
    x = section,
    y = value,
    #color = factor(variable, levels = levs),
    fill = factor(variable, levels = levs)
  )
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    scale = "width",
    color = NA
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    color = "black", alpha = 0, width = 0.2
  ) +
  geom_vline(
    xintercept = seq(0, length(unique(dd$section))) + .5,
    color="#f5f5f5"
  ) +
  facet_wrap(~ program, ncol = 1, scales = "free") +
  #facet_grid(cols = vars(program), space = "free", scales = "free") +
  xlab("review section") +
  ylab("(sentiment) score") +
  ggtitle("sentiment:\nSFI and SNSF review texts") +
  scale_y_continuous(
    limits = c(0, 1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl) +
  scale_fill_manual(values = alpha(bpcl, 0.8)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.x = element_line(color = "#f5f5f5", size = 30),
    panel.border = element_blank(),
    legend.position = "top",
    legend.key.size = unit(10, "pt"),
    legend.background = element_blank(),
    legend.key = element_blank(),
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

dd <- dd[!is.na(dd$value),]


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

ggplot(
  data = dd,
  aes(x = Sscore, y = value, color = variable, fill = variable)
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    scale = "width"#, color = NA
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    color = "black", alpha = 0.8, width = 0.2
  ) +
  #geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~section, nrow = 2) +
  xlab("review score\n(Likert scale from 1 to 6)") +
  ylab("sentiment") +
  ggtitle("SNSF reviews") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$trueScore))) + .5,
    color="#f5f5f5"
  ) +
  scale_y_continuous(
    limits = c(0, 1.03), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-1]) +
  scale_fill_manual(values = alpha(bpcl[-1], 0.8)) +
  #scale_color_viridis_d(begin = 0.4, option = "D") +
  #scale_fill_viridis_d(begin = 0.4, option = "D", alpha = 0.3) +
  theme(
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
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







dd$Sscore[dd$Sscore == "1 to 3      "] <- 3
dd$Sscore <- as.numeric(dd$Sscore)

figurePar <- list(
  filename = paste0("./plots/Figure 2b.", figureFormat),
  height = 1300,
  width = 1800,
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
  aes(x = Sscore, y = value, color = variable, fill = variable)
) +
  geom_smooth(method = 'lm') +
  facet_wrap(~section, nrow = 2, scales = "free_x") +
  xlab("review score\n(Likert scale from 1 to 6)") +
  ylab("sentiment") +
  ggtitle("SNSF reviews") +
  scale_y_continuous(
    limits = c(0, 1.03), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-1]) +
  scale_fill_manual(values = alpha(bpcl[-1], 0.8)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_line(color = "#f5f5f5"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    legend.background=element_blank(),
    legend.key=element_blank(),
    legend.title = element_blank(),
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
dd$manual_raw <- dd$manual
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

ggplot(
  data = dd,
  aes(x = manual, y = value, color = variable, fill = variable)
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    scale = "width"#, color = NA
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    color = "black", alpha = 0.8, width = 0.2
  ) +
  #geom_boxplot(aes(color = variable, fill = variable)) +
  facet_wrap(~program, nrow = 3) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  ggtitle("sentiment:\nSFI and SNSF reviews") +
  scale_y_continuous(
    limits = c(0, 1.03), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-c(1:2)]) +
  scale_fill_manual(values = alpha(bpcl[-c(1:2)], 0.8)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
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



figurePar <- list(
  filename = paste0("./plots/Figure 3b.", figureFormat),
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

ggplot(
  data = dd,
  aes(x = manual_raw, y = value, color = variable, fill = variable)
) +
  geom_smooth(method = 'lm') +
  facet_wrap(~program, nrow = 3) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  ggtitle("sentiment:\nSFI and SNSF reviews") +
  scale_x_continuous(
    limits = c(0,1), breaks = 0:4/4,
    labels = c(
      "most negative", "moderate negrative", "neutral",
      "moderate positive", "most positive")
  ) +
  scale_y_continuous(
    limits = c(0, 1.03), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-c(1:2)]) +
  scale_fill_manual(values = alpha(bpcl[-c(1:2)], 0.8)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_line(color = "#f5f5f5"),
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
  geom_violin(
    position = position_dodge(width = 0.7),
    scale = "width"#, color = NA
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    color = "black", alpha = 0.8, width = 0.2
  ) +
  #geom_boxplot() +
  facet_grid(cols = vars(variable), rows = vars(program)) +
  ggtitle("Sentiment score and funding decision") +
  ylab("sentiment") +
  scale_y_continuous(
    limits = c(0,1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most negative", "neutral", "most positive")
  ) +
  scale_color_manual(values = bpcl[-1]) +
  scale_fill_manual(values = alpha(bpcl[-1], 0.8)) +
  theme(
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
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

dd <- melt(
  data = dd,
  id.vars = c("funding")
)
dd$variable <-
  factor(as.character(dd$variable), levels = c("manual", "TextBlob"))


#olor <- viridisLite::viridis(n = 1, begin = 0.4, option = "D")



figurePar <- list(
  filename = paste0("./plots/Figure 5.", figureFormat),
  height = 850,
  width = 1000,
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
  aes(x = funding, y = value, fill = variable, color = variable)
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    na.rm = TRUE, scale = "area", bw = 0.1#,
    #color = alpha(bpcl[2], 0.8)#, alpha = 0.5
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    na.rm = TRUE, color = "black", width = 0.15#, alpha = 0
  ) +
  ggtitle("Statement level") +
  ylab("manually coded sentiment") +
  scale_y_continuous(
    limits = c(0,1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most negative", "neutral", "most positive")
  ) +
  scale_color_manual(values = bpcl[2:3]) +
  scale_fill_manual(values = alpha(bpcl[2:3], 0.8)) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid = element_line(color = "#f5f5f5"),
    #panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.key = element_blank(),
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
  "Humanities and\nSocial Sciences"
dd$panel[dd$panel == "natural sciences"] <-
  "Mathematics, Natural\nand Engineering Sciences"
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
  width = 1250,
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
  aes(
    x = funder,
    y = value,
    color = factor(variable, levels = levs),
    fill = factor(variable, levels = levs)
  )
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    scale = "width"#, color = NA
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    color = "black", alpha = 0, width = 0.2
  ) +
  facet_wrap(~ panel, nrow=1) +
  ylab("(sentiment) score") +
  ggtitle("SNSF review sentiments by panel") +
  scale_y_continuous(
    limits = c(0, 1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl) +
  scale_fill_manual(values = alpha(bpcl, 0.8)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
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

ggplot(
  data = dd,
  aes(
    x = funder,
    y = value,
    color = factor(variable, levels = levs),
    fill = factor(variable, levels = levs)
  )
) +
  geom_violin(
    position = position_dodge(width = 0.7),
    scale = "width"#, color = NA
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    color = "black", alpha = 0, width = 0.2
  ) +
  facet_wrap(~ language, nrow=1) +
  ylab("(sentiment) score") +
  ggtitle("SNSF review sentiments by language") +
  scale_y_continuous(
    limits = c(0, 1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl) +
  scale_fill_manual(values = alpha(bpcl, 0.8)) +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
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
dd$manual_raw <- dd$manual
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

ggplot(
  data = dd[dd$program == "SNSF",],
  aes(x = manual_raw, y = value, color = variable, fill = variable)
) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ section, nrow = 2) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  ggtitle("A. sentiment in SNSF review sections") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  scale_x_continuous(
    limits = c(0,1), breaks = 0:4/4,
    labels = c(
      "most negative", "moderate negrative", "neutral",
      "moderate positive", "most positive")
  ) +
  scale_y_continuous(
    limits = c(0, 1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-c(1,2)]) +
  scale_fill_manual(values = alpha(bpcl[-c(1,2)], 0.8)) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_line(color = "#f5f5f5"),
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
  height = 1100,
  width = 1300,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(
  data = dd[dd$program == "SFI:IvP",],
  aes(x = manual_raw, y = value, color = variable, fill = variable)
) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ section, nrow = 1) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  ggtitle("B. sentiment in SFI:IvP review sections") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  scale_x_continuous(
    limits = c(0,1), breaks = 0:4/4,
    labels = c(
      "most negative", "moderate negrative", "neutral",
      "moderate positive", "most positive")
  ) +
  scale_y_continuous(
    limits = c(0, 1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-c(1,2)]) +
  scale_fill_manual(values = alpha(bpcl[-c(1,2)], 0.8)) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_line(color = "#f5f5f5"),
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
  filename = paste0("./plots/Figure A3c.", figureFormat),
  height = 1100,
  width = 1300,
  res = 300,
  units = "px"
)
ifelse(
  figureFormat == "png",
  do.call(png, figurePar),
  do.call(tiff, figurePar)
)

ggplot(
  data = dd[dd$program == "SFI:IF",],
  aes(x = manual_raw, y = value, color = variable, fill = variable)
) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ section, nrow = 1) +
  xlab("manually coded sentiment") +
  ylab("algorithmic SA") +
  ggtitle("C. sentiment in SFI:IF review sections") +
  geom_vline(
    xintercept = seq(0, length(unique(dd$manual))) + .5,
    color="#f5f5f5"
  ) +
  scale_x_continuous(
    limits = c(0,1), breaks = 0:4/4,
    labels = c(
      "most negative", "moderate negrative", "neutral",
      "moderate positive", "most positive")
  ) +
  scale_y_continuous(
    limits = c(0, 1.05), expand = c(0, 0),
    breaks = c(0, 0.5, 1), minor_breaks = c(0.25, 0.75),
    labels = c("most\nnegative", "neutral", "most\npositive")
  ) +
  scale_color_manual(values = bpcl[-c(1,2)]) +
  scale_fill_manual(values = alpha(bpcl[-c(1,2)], 0.8)) +
  theme(
    #plot.title.position = "plot",
    panel.background = element_blank(),#element_rect(fill = "#f5f5f5"),
    panel.grid.major.y = element_line(color = "#f5f5f5"),
    panel.grid.minor.y = element_line(color = "#f5f5f5"),
    panel.grid.major.x = element_line(color = "#f5f5f5"),
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



# Tables
# New Table 5___________________________________________________________________
rm(list = ls())
library("writexl")
load("./data/sfi+snsf review data.RDATA")
#rr <- r[r$program %in% c("IF", "IvP"),]
#IF <- r[r$program == "IF",]


# Calculating proposal-level average scores:
for(i in 1:nrow(r)) {
  if (is.na(r$propID[i])) {
    ifelse(
      substr(
        r$revID[i],
        start = nchar(r$revID[i]) - 3,
        stop = nchar(r$revID[i]) - 3
      ) == " ",
      r$propID[i] <- substr(r$revID[i], start=1, stop = nchar(r$revID[i]) -3),
      r$propID[i] <- substr(r$revID[i], start=1, stop = nchar(r$revID[i]) -2)
    )
    if(substr(
      r$propID[i],
      start = nchar(r$propID[i]),
      stop = nchar(r$propID[i])) == " ") {
      r$propID[i] <- substr(r$propID[i], 1, stop = nchar(r$propID[i]) - 1)
    }
    
    #r$propID[i] <- substr(r$revID[i], start = 1, stop = nchar(r$revID[i]) - 3)
  }
}
# Fixing 2 typos in the IDs:
r$propID[r$propID == "Ivp2016:2klrz"] <- "IvP2016:2klrz"
r$propID[r$propID == "IF2016: 7Z9Ie"] <- "IF2016: 7Z9le"
#table(r$propID)
#View(cbind(substr(r$revID, nchar(r$revID), nchar(r$revID)) == "a", r))
#sum(substr(r$revID, nchar(r$revID), nchar(r$revID)) == "a")
#length(unique(subset(r, r$program != "SNSF")$propID))
r$Ma <- r$Te <- r$Va <- NA
pr <- c()
for (i in 1:nrow(r)) {
  if (substr(r$revID[i], nchar(r$revID[i]), nchar(r$revID[i])) == "a") {
    
    # Note that the sentiment score is reverse-coded - this is in order to
    # procude a ranking where, like in the original ranking data, low ranking
    # positions (e.g. 1st, 2nd etc.) mean good.
    r$Ma[i] <- 2 - (1 + mean(subset(r, r$propID == r$propID[i])$aggrManual, na.rm = TRUE))
    r$Te[i] <- 2 - (1 + mean(subset(r, r$propID == r$propID[i])$aggrT, na.rm = TRUE))
    r$Va[i] <- 2 - (1 + mean(subset(r, r$propID == r$propID[i])$aggrV, na.rm = TRUE))
    pr <- c(pr, i)
  }
}
pr <- r[pr,]
#View(r[,c(1:5, 49:ncol(r))])

#test <- pr[pr$program == "IvP" & pr$panel == "A",]
#test <- data.frame(
#  propID = test$propID,
#  trueRank = test$trueRank,
#  manual = (2 - test$Ma) - 1,
#  rankManual = rank(test$Ma, ties.method = "min"),
#  TextBlob = (2 - test$Te) - 1,
#  rankTextBlob = rank(test$Te, ties.method = "min"),
#  VADER = (2 - test$Va) - 1,
#  rankVADER = rank(test$Va, ties.method = "min")
#)
#writexl::write_xlsx(test, "./output/IvP_panel_A.xlsx")


rr <- list() 
rr$IF2014 <- pr[pr$program == "IF" & grepl("2014", pr$revID, fixed = TRUE),]
rr$IF2015 <- pr[pr$program == "IF" & grepl("2015", pr$revID, fixed = TRUE),]
rr$IF2016 <- pr[pr$program == "IF" & grepl("2016", pr$revID, fixed = TRUE),]
#rr$IF2017 <- pr[pr$program == "IF" & grepl("2017", pr$revID, fixed = TRUE),]
rr$IvPa <- pr[pr$program == "IvP" & pr$panel == "A",]
rr$IvPb <- pr[pr$program == "IvP" & pr$panel == "B",]
rr$IvPc <- pr[pr$program == "IvP" & pr$panel == "C",]
rr$IvPd <- pr[pr$program == "IvP" & pr$panel == "D",]
rr$SNSF_hss <- r[r$program == "SNSF" & r$panel == "humanities and social sciences",]
rr$SNSF_ns <- r[r$program == "SNSF" & r$panel == "natural sciences",]

programs <- c("IvPa","IvPb","IvPc","IvPd","IF2014","IF2015","IF2016")#,"IF2017")
SAmethod <- c("Ma", "Te", "Va")

corrTable <- matrix(NA, nrow = 3, ncol = length(programs))

for(s in 1:3) for (p in 1:length(programs)) {
  t <- cor.test(
    x = rr[[programs[p]]][,SAmethod[s]],
    y = rr[[programs[p]]]$trueRank,
    method = "spearman"
  )
  stars <- ""
  if (!is.na(t$p.value) & t$p.value <= 0.05) {stars <- "*"}
  if (!is.na(t$p.value) & t$p.value <= 0.01) {stars <- "**"}
  if (!is.na(t$p.value) & t$p.value <= 0.001) {stars <- "***"}
  
  corrTable[s, p] <- paste0(round(t$estimate, digits = 3),stars)
}

corrTable <- as.data.frame(cbind(
  c("Manual","TextBlob","VADER"), corrTable
))
names(corrTable) <- c("", programs)

writexl::write_xlsx(corrTable, "./output/SFIreviews_rankcorr_table5a.xlsx")




#### We do the same for SNSF data (unit of analysis: structured review)
programs <- c("SNSF_hss","SNSF_ns")
SAmethod <- c("aggrManual", "aggrT", "aggrV")

corrTable <- matrix(NA, nrow = 3, ncol = length(programs))

for(s in 1:3) for (p in 1:length(programs)) {
  t <- cor.test(
    x = rr[[programs[p]]][,SAmethod[s]],
    y = rr[[programs[p]]]$trueScore,
    method = "spearman"
  )
  stars <- ""
  if (!is.na(t$p.value) & t$p.value <= 0.05) {stars <- "*"}
  if (!is.na(t$p.value) & t$p.value <= 0.01) {stars <- "**"}
  if (!is.na(t$p.value) & t$p.value <= 0.001) {stars <- "***"}
  
  corrTable[s, p] <- paste0(round(t$estimate, digits = 3),stars)
}

corrTable <- as.data.frame(cbind(
  c("Manual","TextBlob","VADER"), corrTable
))
names(corrTable) <- c("", programs)

write_xlsx(corrTable, "./output/SNSFreviews_rankcorr_table4.xlsx")




# next we look into all bivariate correlations
pr <- pr[!grepl("2017", pr$revID, fixed = TRUE),]

corr <- Hmisc::rcorr(
  as.matrix(cbind(pr$trueRank, pr$Ma, pr$Te, pr$Va)),
  type = "spearman"
)

c <- as.data.frame(round(corr[[1]], digits = 3))
names(c) <- c("actual ranking", "manual", "TextBlob","Vader")

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

write_xlsx(c, "./output/SFIreviews_rankcorr_table5b.xlsx")

