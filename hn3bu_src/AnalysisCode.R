######################################## Analyse Meaning Coherence Judgement Task ########################################

# Project: 
# (Blott, L M, Rodd, J M, Ferreira, F, & Warren, J E, 2019)

# Author: Lena M Blott
# Last updated: 20/03/2019

# Notes:
# Plaus = plausible, effect coded as -0.5 [this is the "coherent" condition in the Blott et al. paper]
# Anom = anomalous, effect coded as 0.5
# UA = unambiguous, effect coded as 0.5
# Amb = ambiguous, effect coded as -0.5

#### install and load relevant packages ####
# install.packages("lme4")
# install.packages("ggplot2")
library(lme4)
library(ggplot2)

#### set working directory ######
#setwd()
setwd("~/PROJECTS/2018Exp_EyetrackingUS/Eyetracking_PlausJudg/Report/Preprint")

###################### Preparing the data #############################
# read in the original data file that contains all trials for all participants and items
Data.AllTrials <- read.csv("~/Data_AllTrials_NA.csv")

## Participant exclusions ##
# Exclude the following participants (reasons: non-native speaker, uncorrected visual impairment, 
# technical issues w/ data collection):
# s018
# s046
# s095
# s055
# s097

# Subset the data file excluding rows for the above participants
Data.Exclude5Subj <- subset(Data.AllTrials, !RECORDING_SESSION_LABEL =="s018" &
                            !RECORDING_SESSION_LABEL =="s046" &
                            !RECORDING_SESSION_LABEL =="s095" &
                            !RECORDING_SESSION_LABEL =="s055" &
                            !RECORDING_SESSION_LABEL =="s097" )

## Item exclusions ##
# Exclude the following items (reason: participants did not seem to know subordinate meaning):
# bridge 

# Subset the data file excluding rows for the above item
Data.Exclude1Item <- subset(Data.Exclude5Subj, !item_label =="bridge")


# Exclude practice trials (condition = "99")
Data.ExpTrials <- subset(Data.Exclude1Item, !condition =="99")

# save as new file
write.csv(Data.ExpTrials, file="Data_ExpTrials.csv")


###################################################
########## SCATTERPLOTS WITH REGR LINES ###########
###################################################

# install and load easy ggplot2 package
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
# install.packages("ggpubr")
library("ggpubr")
# scatterplot code from http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/78-perfect-scatter-plots-with-correlation-and-marginal-histograms/


#### Comparisons of Amb and UA conditions ####
# read in the file that contains summary measures (means) for each participant for each condition
ScatterData <- read.csv("~/PROJECTS/2018Exp_EyetrackingUS/Eyetracking_PlausJudg/Report/Preprint/BehaviouralData_ByCondition.csv")

# Convert Ambiguity as a grouping variable
ScatterData$Ambiguity <- as.factor(ScatterData$Ambiguity)


      #### _Key word region (Key AOI): Gaze duration, go-past time, regressions out ####
# plotting PRINT EXPOSURE against GAZE DURATION (IA_FIRST_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "KEY_GazeDur",
          color = "Ambiguity", 
          palette = "jco",   
          shape = "Ambiguity",
          add = "reg.line",  
          conf.int = TRUE,
          fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
            #title = "Keyword",
            #subtitle = "Relationship between Print Exposure Scores and Gaze Duration",
            #caption = "Source: ggpubr",
            xlab ="Print Exposure score", 
            ylab = "Gaze duration (ms)",
            font.x  = list(size = 28, face = "plain"),
            font.y = list(size = 28, face = "plain"),
            #legend.title = "Ambiguity",
            #legend = "right",
            #font.legend = list(size = 28, face = "plain"),
            xlim = c(0, 40),
            font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting PRINT EXPOSURE against GO-PAST TIME (Regr Path Dur)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "KEY_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Keyword",
                     #subtitle = "Relationship between Print Exposure Scores and Go-past Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Go-past time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                     #legend = "right",
                     #font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2

# plotting PRINT EXPOSURE against REGR OUT
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "KEY_ProbRegrOut",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Keyword",
                     #subtitle = "Relationship between Print Exposure Scores and Regressions Out",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Probability of regression out",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                     #legend.title = "Ambiguity",
                     #legend = "right",
                     #font.legend = list(size = 22, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against REGR OUT
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "KEY_ProbRegrOut",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Keyword",
                     #subtitle = "Relationship between Print Exposure Scores and Regressions Out",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Probability of regression out",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                     #legend.title = "Ambiguity",
                     #legend = "right",
                     #font.legend = list(size = 22, face = "plain"),
                     xlim = c(40, 80),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


#### Comparisons of CohAmb and CohUA conditions ####
# read in the file that contains summary measures (means) for each participant for each condition
ScatterData <- read.csv("~/PROJECTS/2018Exp_EyetrackingUS/Eyetracking_PlausJudg/Report/Preprint/BehaviouralData_ByCondition_CohAmbCohUAOnly.csv")

# Convert Ambiguity as a grouping variable
ScatterData$Ambiguity <- as.factor(ScatterData$Ambiguity)


      #### _Key word region (Key AOI): 2nd pass reading time ####
# plotting PRINT EXPOSURE against 2nd pass reading time (IA_SECOND_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "KEY_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Keyword",
                     #subtitle = "Relationship between Print Exposure Scores and Second-Pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Second-pass reading time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against 2nd pass reading time (IA_SECOND_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "KEY_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Keyword",
                     #subtitle = "Relationship between Vocabulary Scores and Second-Pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Second-pass reading time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(40, 80),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2



     #### _Coherence cue region (Cohcue AOI): First-fix duration, gaze duration, regressions out, go-past time, 2nd pass reading time  ####

# plotting VOCAB against FF 
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "COHCUE_FF",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Coherence Cue",
                     #subtitle = "Relationship between Vocabulary Scores and First-fixation duration",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "First-fixation duration (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                     #legend.title = "Ambiguity",
                     #legend = "right",
                     #font.legend = list(size = 28, face = "plain"),
                     xlim = c(40, 80),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2

# plotting PRINT EXPOSURE against GAZE DUR (Gaze Dur)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "COHCUE_GazeDur",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Coherence Cue",
                     #subtitle = "Relationship between Print Exposure Scores and Gaze Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Gaze duration (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting PRINT EXPOSURE against GO-PAST TIME (Regr Path Dur)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "COHCUE_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Coherence Cue",
                     #subtitle = "Relationship between Print Exposure Scores and Go-past Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Go-past time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against GO-PAST TIME (Regr Path Dur)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "COHCUE_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Coherence Cue",
                     #subtitle = "Relationship between Vocabulary Scores and Go-past Time",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Go-past time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(40, 80),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2

# Scatterplot with CohAmb/CohUA as different variables
# plotting PRINT EXPOSURE against 2nd pass reading time (IA_SECOND_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "COHCUE_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Coherence Cue",
                     #subtitle = "Relationship between Print Exposure Scores and Second-Pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Second-pass reading time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                     #font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against 2nd pass reading time (IA_SECOND_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "COHCUE_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                    # title = "Coherence Cue",
                     # subtitle = "Relationship between Vocabulary Scores and Second-Pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Second-pass reading time (ms)",
                    font.x  = list(size = 28, face = "plain"),
                    font.y = list(size = 28, face = "plain"),
                   # legend.title = "Ambiguity",
                  #  legend = "right",
                  #  font.legend = list(size = 28, face = "plain"),
                    xlim = c(40, 80),
                    font.tickslab = list(size=22,face="plain"))
prettyplot2


          #### _Spill-over region (Spillover AOI): Gaze duration, regressions out, go-past time, 2nd pass reading time  ####

# plotting PRINT EXPOSURE against GAZE DURATION (IA_FIRST_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_GazeDur",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Spill-over region",
                     #subtitle = "Relationship between Print Exposure Scores and Gaze Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Gaze duration (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against GAZE DURATION (IA_FIRST_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "SPILLOVER_GazeDur",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Spill-over region",
                     #subtitle = "Relationship between Vocabulary Scores and Gaze Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Gaze duration (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                   #  legend.title = "Ambiguity",
                  #   legend = "right",
                  #   font.legend = list(size = 28, face = "plain"),
                     xlim = c(40, 80),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against REGR OUT (IA_REGRESSIONS_OUT)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "SPILLOVER_ProbRegrOut",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Spill-over region",
                     #subtitle = "Relationship between Vocabulary Scores and Regressions Out",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Probability of regression out",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(40, 80),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


#plotting PRINT EXP against REGR OUT (IA_REGRESSIONS_OUT)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_ProbRegrOut",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Spill-over region",
                     #subtitle = "Relationship between Vocabulary Scores and Regressions Out",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score score", 
                     ylab = "Probability of regression out",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                     # legend.title = "Ambiguity",
                     # legend = "right",
                     # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting PRINT EXPOSURE against GO-PAST TIME (Regr Path Dur)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Spill-over region",
                     #subtitle = "Relationship between Print Exposure Scores and Go-past Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Go-past time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting VOCAB against GO-PAST TIME (Regr Path Dur)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "SPILLOVER_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                    # title = "Spill-over region",
                    # subtitle = "Relationship between Vocabulary Scores and Go-past Time",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "Go-past time (ms)",
                    font.x  = list(size = 28, face = "plain"),
                    font.y = list(size = 28, face = "plain"),
                   # legend.title = "Ambiguity",
                  #  legend = "right",
                  #  font.legend = list(size = 28, face = "plain"),
                    xlim = c(40, 80),
                    font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting PRINT EXPOSURE against SECOND PASS READING TIME
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     #title = "Spill-over region",
                     #subtitle = "Relationship between Print Exposure Scores and Second-pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Second-pass reading time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                   #  legend.title = "Ambiguity",
                  #   legend = "right",
                   #  font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2



#### Comparisons of CohAmb and AnomUA conditions ####
# read in the file that contains summary measures (means) for each participant for each condition
ScatterData <- read.csv("~/PROJECTS/2018Exp_EyetrackingUS/Eyetracking_PlausJudg/Report/Preprint/BehaviouralData_ByCondition_CohAmbAnomUAOnly.csv")

# Convert Ambiguity as a grouping variable
ScatterData$Ambiguity <- as.factor(ScatterData$Ambiguity)


      #### _Keyword region (KEY AOI): 2nd pass reading time ####

# plotting PRINT EXPOSURE against SECOND PASS READING TIME 
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "KEY_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     # title = "Spill-over region",
                     # subtitle = "Relationship between Print Exposure Scores and Second-pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Second pass reading time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                     #legend.title = "Ambiguity",
                     #legend = "right",
                     #font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


     #### _Coherence cue region (Cohcue AOI): First-fix duration, gaze duration, go-past time  ####

# plotting VOCAB against FF DURATION (IA_FIRST_FIXATION_DURATION)
prettyplot <- ggscatter(ScatterData, x = "Vocab", y = "COHCUE_FF",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                    # title = "Coherence Cue",
                    # subtitle = "Relationship between Vocabulary Scores and First-fixation Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Vocabulary score", 
                     ylab = "First-fixation duration (ms)",
                    font.x  = list(size = 28, face = "plain"),
                    font.y = list(size = 28, face = "plain"),
                   # legend.title = "Ambiguity",
                  #  legend = "right",
                  #  font.legend = list(size = 28, face = "plain"),
                    xlim = c(40, 80),
                    font.tickslab = list(size=22,face="plain"))
prettyplot2

# plotting PRINT EXPOSURE against GAZE DURATION (IA_FIRST_RUN_DWELL_TIME)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "COHCUE_GazeDur",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                    # title = "Coherence Cue",
                    # subtitle = "Relationship between Print Exposure Scores and Gaze Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Gaze duration (ms)",
                    font.x  = list(size = 28, face = "plain"),
                    font.y = list(size = 28, face = "plain"),
                   # legend.title = "Ambiguity",
                  #  legend = "right",
                  #  font.legend = list(size = 28, face = "plain"),
                    xlim = c(0, 40),
                    font.tickslab = list(size=22,face="plain"))
prettyplot2

# plotting PRINT EXPOSURE against GO-PAST TIME (Regr Path Dur)
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "COHCUE_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     # title = "Spill-over region",
                     # subtitle = "Relationship between Print Exposure Scores and Go-past Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Go-past time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                  #   legend.title = "Ambiguity",
                  #   legend = "right",
                  #   font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2



        #### _Spill-over region (Spillover AOI): Gaze duration, 2nd pass reading time ####


# plotting PRINT EXPOSURE against GAZE DURATION 
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_GazeDur",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     # title = "Spill-over region",
                     # subtitle = "Relationship between Print Exposure Scores and Gaze Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Gaze duration (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                   #  legend.title = "Ambiguity",
                  #   legend = "right",
                  #   font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2

# plotting PRINT EXPOSURE against GO-PAST TIME
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_RegrPath",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     # title = "Spill-over region",
                     # subtitle = "Relationship between Print Exposure Scores and Gaze Duration",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Go-past time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                     #  legend.title = "Ambiguity",
                     #   legend = "right",
                     #   font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


# plotting PRINT EXPOSURE against SECOND PASS READING TIME 
prettyplot <- ggscatter(ScatterData, x = "ART.Hit.FalseAl.", y = "SPILLOVER_SecPass",
                        color = "Ambiguity", 
                        palette = "jco",   
                        shape = "Ambiguity",
                        add = "reg.line",  
                        conf.int = TRUE,
                        fullrange = TRUE)
prettyplot2 <- ggpar(prettyplot, 
                     # title = "Spill-over region",
                     # subtitle = "Relationship between Print Exposure Scores and Second-pass Reading Time",
                     #caption = "Source: ggpubr",
                     xlab ="Print Exposure score", 
                     ylab = "Second pass reading time (ms)",
                     font.x  = list(size = 28, face = "plain"),
                     font.y = list(size = 28, face = "plain"),
                    # legend.title = "Ambiguity",
                    # legend = "right",
                    # font.legend = list(size = 28, face = "plain"),
                     xlim = c(0, 40),
                     font.tickslab = list(size=22,face="plain"))
prettyplot2


###################################################################################################
####################### ACCURACY OF MEANING COHERENCE JUDGEMENT TASK ##############################
###################################################################################################

              #### CohAmb and CohUA trials only: Main accuracy model ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove anomalous trials
Data.CohOnly <-subset(Data.ExpTrials, subset=(Plausibility.code=="-0.5"))

# centre & scale individual differences variables
Data.CohOnly$Vocab.Cent <- scale(Data.CohOnly$Vocab,center=TRUE, scale=TRUE)
Data.CohOnly$ART.Cent <- scale(Data.CohOnly$ART,center=TRUE, scale=TRUE)

# construct a maximal glmer() model 
# This model contains a fixed within-subjects effect of Ambiguity (effect-coded with -0.5 amb), 
# and between-subjects fixed effects for Vocabulary and ART test scores,
# plus random effects by participants and items.
Acc.max <- glmer(accuracy ~ 1 + 
                   Ambiguity.code  +
                   Vocab.Cent +
                   ART.Cent +
                   Ambiguity.code : Vocab.Cent +
                   Ambiguity.code : ART.Cent +
                   (1 
                    + Ambiguity.code  
                    | RECORDING_SESSION_LABEL) +
                   (1  
                    + Vocab.Cent
                    + ART.Cent
                    | item),
                 data = Data.CohOnly,
                 family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# get model summary
summary(Acc.max)

# main effect of Ambiguity
Acc.noAmb <- glmer(accuracy ~ 1 + 
                   #Ambiguity.code  +
                   Vocab.Cent +
                   ART.Cent +
                   Ambiguity.code : Vocab.Cent +
                   Ambiguity.code : ART.Cent +
                   (1 
                    + Ambiguity.code  
                    | RECORDING_SESSION_LABEL) +
                   (1  
                    + Vocab.Cent
                    + ART.Cent
                    | item),
                 data = Data.CohOnly,
                 family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(Acc.max, Acc.noAmb)

# main effect of vocab
Acc.noVocab <- glmer(accuracy ~ 1 + 
                         Ambiguity.code  +
                         #Vocab.Cent +
                         ART.Cent +
                         Ambiguity.code : Vocab.Cent +
                         Ambiguity.code : ART.Cent +
                         (1 
                          + Ambiguity.code  
                          | RECORDING_SESSION_LABEL) +
                         (1   
                          + Vocab.Cent
                          + ART.Cent
                          | item),
                       data = Data.CohOnly,
                       family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(Acc.max, Acc.noVocab)

# main effect of Print exposure
Acc.noART <- glmer(accuracy ~ 1 + 
                           Ambiguity.code  +
                           Vocab.Cent +
                           #ART.Cent +
                           Ambiguity.code : Vocab.Cent +
                           Ambiguity.code : ART.Cent +
                           (1 
                            + Ambiguity.code  
                            | RECORDING_SESSION_LABEL) +
                           (1   
                            + Vocab.Cent
                            + ART.Cent
                            | item),
                         data = Data.CohOnly,
                         family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(Acc.max, Acc.noART)

# Vocab x Amb interaction
Acc.noVocabAmb <- glmer(accuracy ~ 1 + 
                         Ambiguity.code  +
                         Vocab.Cent +
                         ART.Cent +
                         #Ambiguity.code : Vocab.Cent +
                         Ambiguity.code : ART.Cent +
                         (1 
                          + Ambiguity.code  
                          | RECORDING_SESSION_LABEL) +
                         (1   
                          + Vocab.Cent
                          + ART.Cent
                          | item),
                       data = Data.CohOnly,
                       family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(Acc.max, Acc.noVocabAmb)

# ART x Amb interaction
Acc.noARTAmb <- glmer(accuracy ~ 1 + 
                              Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              #Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code  
                               | RECORDING_SESSION_LABEL) +
                              (1    
                               + Vocab.Cent
                               + ART.Cent
                               | item),
                            data = Data.CohOnly,
                            family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(Acc.max, Acc.noARTAmb)



###################################################################################################
#### READING TIME ANALYSES: processing temporarily ambiguous vs coherent unambiguous sentences ####
###################################################################################################


          #### Total sentence reading time on correct Coherent (Coh) trials only ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove anomalous trials
Data.CohOnly <-subset(Data.ExpTrials, subset=(Plausibility.code=="-0.5"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logTrialDwellTime <- log10(Data.CorrTrials$TRIAL_DWELL_TIME)



# construct a maximal lmer() model 
# This model contains a fixed within-subjects effect of Ambiguity (effect-coded with -0.5 amb), 
# and between-subjects fixed effects for Vocabulary and ART test scores,
# plus random effects by participants and items.

# Maximal model with interactions
TrialDwellTime.max <- lmer(logTrialDwellTime ~ 1 + 
                             Ambiguity.code  +
                             Vocab.Cent +
                             ART.Cent +
                             Ambiguity.code : Vocab.Cent +
                             Ambiguity.code : ART.Cent +
                             (1 
                              + Ambiguity.code  
                              | RECORDING_SESSION_LABEL) +
                             (1  
                              + Vocab.Cent
                              + ART.Cent
                              | item),
                           data = Data.CorrTrials,
                           REML=FALSE)

# main effect of Ambiguity
TrialDwellTime.noAmb <- lmer(logTrialDwellTime ~ 1 + 
                            # Ambiguity.code  +
                             Vocab.Cent +
                             ART.Cent +
                             Ambiguity.code : Vocab.Cent +
                             Ambiguity.code : ART.Cent +
                             (1 
                              + Ambiguity.code  
                              | RECORDING_SESSION_LABEL) +
                             (1  
                              + Vocab.Cent
                              + ART.Cent
                              | item),
                           data = Data.CorrTrials,
                           REML=FALSE)
# model comparison
anova(TrialDwellTime.max, TrialDwellTime.noAmb)

# main effect of Vocab
max.TrialDwellTime.noVocab <- lmer(logTrialDwellTime ~ 1 + 
                             Ambiguity.code  +
                             #Vocab.Cent +
                             ART.Cent +
                             Ambiguity.code : Vocab.Cent +
                             Ambiguity.code : ART.Cent +
                             (1 
                              + Ambiguity.code  
                              | RECORDING_SESSION_LABEL) +
                             (1  
                              + Vocab.Cent
                              + ART.Cent
                              | item),
                           data = Data.CorrTrials,
                           REML=FALSE)
# model comparison
anova(TrialDwellTime.max, TrialDwellTime.noVocab)

# main effect of Print exposure
max.TrialDwellTime.noART <- lmer(logTrialDwellTime ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                     #ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1 
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL) +
                                     (1  
                                      + Vocab.Cent
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=FALSE)
# model comparison
anova(TrialDwellTime.max, TrialDwellTime.noART)

# Vocab x Ambiguity interaction
max.TrialDwellTime.noVocabAmb <- lmer(logTrialDwellTime ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   #Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1 
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(TrialDwellTime.max, TrialDwellTime.noVocabAmb)

# Print exposure x Ambiguity interaction
max.TrialDwellTime.noARTAmb <- lmer(logTrialDwellTime ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   #Ambiguity.code : ART.Cent +
                                   (1 
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(TrialDwellTime.max, TrialDwellTime.noARTAmb)

# rerun maximal model with reml
TrialDwellTime.REML <- lmer(logTrialDwellTime ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1 
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1 
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=TRUE)
# call model summary
summary(TrialDwellTime.REML)




#################################################################
################# AREA OF INTEREST: KEYWORD #####################
#################################################################

#### _FIRST-PASS MEASURES ####

# In this region, models for first-pass measures include all types of trials 
# (both coherent and anomalous)

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="2")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)


#### _first-fixation duration ####
# Construct a maximal lmer() model
# Because not all the necessary models converge, remove correlations between 
# random effects for all models for the first-fixation duration measure:
AOIKey.FF.reduced <- lmer(logFirstFixDur ~ 1 + 
                        Ambiguity.code  +
                        Vocab.Cent +
                        ART.Cent +
                        Ambiguity.code : Vocab.Cent +
                        Ambiguity.code : ART.Cent +
                        (1   
                         | RECORDING_SESSION_LABEL) +
                          (0
                           + Ambiguity.code   
                           | RECORDING_SESSION_LABEL) +
                          (1   
                         | item) +
                          (0 
                           + Vocab.Cent 
                           | item) +
                          (0
                           + ART.Cent  
                           | item),
                      data = Data.CorrTrials,
                      REML=FALSE)

# main effect of Ambiguity
AOIKey.FF.noAmb <- lmer(logFirstFixDur ~ 1 + 
                           #  Ambiguity.code  +
                            Vocab.Cent +
                            ART.Cent +
                            Ambiguity.code : Vocab.Cent +
                            Ambiguity.code : ART.Cent +
                            (1   
                             | RECORDING_SESSION_LABEL) +
                            (0
                             + Ambiguity.code   
                             | RECORDING_SESSION_LABEL) +
                            (1   
                             | item) +
                            (0 
                             + Vocab.Cent 
                             | item) +
                            (0
                             + ART.Cent  
                             | item),
                          data = Data.CorrTrials,
                          REML=FALSE)
# model comparison
anova(AOIKey.FF.reduced, AOIKey.FF.noAmb)

# main effect of Vocab
AOIKey.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                            Ambiguity.code  +
                            # Vocab.Cent +
                            ART.Cent +
                            Ambiguity.code : Vocab.Cent +
                            Ambiguity.code : ART.Cent +
                            (1   
                             | RECORDING_SESSION_LABEL) +
                            (0
                             + Ambiguity.code   
                             | RECORDING_SESSION_LABEL) +
                            (1   
                             | item) +
                            (0 
                             + Vocab.Cent 
                             | item) +
                            (0
                             + ART.Cent  
                             | item),
                          data = Data.CorrTrials,
                          REML=FALSE)
# model comparison
anova(AOIKey.FF.reduced, AOIKey.FF.noVocab)

# main effect of Print exposure
reduced.AOIKey.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                            Ambiguity.code  +
                            Vocab.Cent +
                           #  ART.Cent +
                            Ambiguity.code : Vocab.Cent +
                            Ambiguity.code : ART.Cent +
                            (1   
                             | RECORDING_SESSION_LABEL) +
                            (0
                             + Ambiguity.code   
                             | RECORDING_SESSION_LABEL) +
                            (1   
                             | item) +
                            (0 
                             + Vocab.Cent 
                             | item) +
                            (0
                             + ART.Cent  
                             | item),
                          data = Data.CorrTrials,
                          REML=FALSE)
# model comparison
anova(AOIKey.FF.reduced, AOIKey.FF.noART)

# Vocab x Ambiguity interaction
reduced.AOIKey.FF.noVocabAmb <- lmer(logFirstFixDur ~ 1 + 
                            Ambiguity.code  +
                            Vocab.Cent +
                            ART.Cent +
                          #  Ambiguity.code : Vocab.Cent +
                            Ambiguity.code : ART.Cent +
                            (1   
                             | RECORDING_SESSION_LABEL) +
                            (0
                             + Ambiguity.code   
                             | RECORDING_SESSION_LABEL) +
                            (1   
                             | item) +
                            (0 
                             + Vocab.Cent 
                             | item) +
                            (0
                             + ART.Cent  
                             | item),
                          data = Data.CorrTrials,
                          REML=FALSE)
# model comparison
anova(AOIKey.FF.reduced, AOIKey.FF.noVocabAmb)


# Print exposure x Ambiguity interaction
reduced.AOIKey.FF.noARTAmb <- lmer(logFirstFixDur ~ 1 + 
                            Ambiguity.code  +
                            Vocab.Cent +
                            ART.Cent +
                            Ambiguity.code : Vocab.Cent +
                         #   Ambiguity.code : ART.Cent +
                            (1   
                             | RECORDING_SESSION_LABEL) +
                            (0
                             + Ambiguity.code   
                             | RECORDING_SESSION_LABEL) +
                            (1   
                             | item) +
                            (0 
                             + Vocab.Cent 
                             | item) +
                            (0
                             + ART.Cent  
                             | item),
                          data = Data.CorrTrials,
                          REML=FALSE)
# model comparison
anova(AOIKey.FF.reduced, AOIKey.FF.noARTAmb)

# re-run maximal model with REML
reduced.AOIKey.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                            Ambiguity.code  +
                            Vocab.Cent +
                            ART.Cent +
                            Ambiguity.code : Vocab.Cent +
                            Ambiguity.code : ART.Cent +
                            (1   
                             | RECORDING_SESSION_LABEL) +
                            (0
                             + Ambiguity.code   
                             | RECORDING_SESSION_LABEL) +
                            (1   
                             | item) +
                            (0 
                             + Vocab.Cent 
                             | item) +
                            (0
                             + ART.Cent  
                             | item),
                          data = Data.CorrTrials,
                          REML=TRUE)
# call model summary
summary(reduced.AOIKey.FF.REML)



#### _gaze duration ####
# Construct a maximal lmer() model
AOIKey.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                           Ambiguity.code  +
                           Vocab.Cent +
                           ART.Cent +
                           Ambiguity.code : Vocab.Cent +
                           Ambiguity.code : ART.Cent +
                           (1  
                            + Ambiguity.code  
                            | RECORDING_SESSION_LABEL) +
                           (1  
                            + Vocab.Cent
                            + ART.Cent
                            | item),
                         data = Data.CorrTrials,
                         REML=FALSE)

# main effect of Ambiguity
AOIKey.GazeDur.noAmb <- lmer(logGazeDur ~ 1 + 
                             #Ambiguity.code  +
                             Vocab.Cent +
                             ART.Cent +
                             Ambiguity.code : Vocab.Cent +
                             Ambiguity.code : ART.Cent +
                             (1  
                              + Ambiguity.code  
                              | RECORDING_SESSION_LABEL) +
                             (1  
                              + Vocab.Cent
                              + ART.Cent
                              | item),
                           data = Data.CorrTrials,
                           REML=FALSE)
# model comparison
anova(AOIKey.GazeDur.max, AOIKey.GazeDur.noAmb)

# main effect of Vocab
AOIKey.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                  # Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1 
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOIKey.GazeDur.max, AOIKey.GazeDur.noVocab)

# main effect of Print exposure
AOIKey.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   #ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOIKey.GazeDur.max, AOIKey.GazeDur.noART)

# Vocab x Ambiguity interaction
AOIKey.GazeDur.noVocabAmb <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   #Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOIKey.GazeDur.max, AOIKey.GazeDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIKey.GazeDur.noARTAmb <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   #Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOIKey.GazeDur.max, AOIKey.GazeDur.noARTAmb)

# re-run maximal model with reml
AOIKey.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=TRUE)
# call model summary
summary(AOIKey.GazeDur.REML)



#### _regressions out ####
# Construct a maximal glmer() model
AOIKey.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                              Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code  
                               | RECORDING_SESSION_LABEL) +
                              (1  
                               + Vocab.Cent
                               + ART.Cent
                               | item),
                            data = Data.CorrTrials,
                            family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOIKey.RegrOut.max)

# main effect of Ambiguity
AOIKey.RegrOut.noAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                             # Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code  
                               | RECORDING_SESSION_LABEL) +
                               (1  
                                + Vocab.Cent
                                + ART.Cent
                                | item),
                            data = Data.CorrTrials,
                            family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut.max, AOIKey.RegrOut.noAmb)

# main effect of Vocab
AOIKey.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                     Ambiguity.code  +
                                    #Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut.max, AOIKey.RegrOut.noVocab)

# main effect of Print Exposure
AOIKey.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                     Ambiguity.code  +
                                    Vocab.Cent +
                                    #ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut.max, AOIKey.RegrOut.noART)

# Vocab x Ambiguity interaction
AOIKey.RegrOut.noVocabAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                     Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    #Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut.max, AOIKey.RegrOut.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIKey.RegrOut.noARTAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                     Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    #Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut.max, AOIKey.RegrOut.noARTAmb)



#### __ pairwise comparison: regressions out in Amb vs UA trials ####

        ### ____ Amb trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="2")

# subset only rows for Amb
Data.CorrTrials <- subset(Data.CorrTrials, amb =="Amb")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)


# Construct a maximal glmer() model
AOIKey.RegrOut <- glmer(IA_REGRESSION_OUT ~ 1 + 
                              Vocab.Cent +
                              ART.Cent +
                              (1  | RECORDING_SESSION_LABEL) +
                              (1 
                               + Vocab.Cent
                               + ART.Cent
                               | item),
                            data = Data.CorrTrials,
                            family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOIKey.RegrOut)

# main effect of Vocab
AOIKey.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                      #   Vocab.Cent +
                                      ART.Cent +
                                      (1  | RECORDING_SESSION_LABEL) +
                                      (1 
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut, AOIKey.RegrOut.noVocab)

# main effect of Print exposure
AOIKey.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                    Vocab.Cent +
                                    #ART.Cent +
                                    (1  | RECORDING_SESSION_LABEL) +
                                    (1 
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut, AOIKey.RegrOut.noART)


        ### ____ UA trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="2")

# subset only rows for Amb
Data.CorrTrials <- subset(Data.CorrTrials, amb =="UA")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)


# Construct a maximal glmer() model
AOIKey.RegrOut <- glmer(IA_REGRESSION_OUT ~ 1 + 
                          Vocab.Cent +
                          ART.Cent +
                          (1  | RECORDING_SESSION_LABEL) +
                          (1 
                           + Vocab.Cent
                           + ART.Cent
                           | item),
                        data = Data.CorrTrials,
                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOIKey.RegrOut)

# main effect of Vocab
AOIKey.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                  #   Vocab.Cent +
                                  ART.Cent +
                                  (1  | RECORDING_SESSION_LABEL) +
                                  (1 
                                   + Vocab.Cent
                                   + ART.Cent
                                   | item),
                                data = Data.CorrTrials,
                                family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut, AOIKey.RegrOut.noVocab)

# main effect of Print exposure
AOIKey.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                Vocab.Cent +
                                #ART.Cent +
                                (1  | RECORDING_SESSION_LABEL) +
                                (1 
                                 + Vocab.Cent
                                 + ART.Cent
                                 | item),
                              data = Data.CorrTrials,
                              family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIKey.RegrOut, AOIKey.RegrOut.noART) 




#### _go-past time ####
# Construct a maximal lmer() model
# Because not all the necessary models converge, remove correlations between 
# random effects for all models for the go-past time measure:
AOIKey.RegrPathDur.reduced <- lmer(logRegrPathDur ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                 (1  
                                  | RECORDING_SESSION_LABEL) +
                                   (0  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL)+
                                   (1  
                                  | item) +
                                 (0
                                   + Vocab.Cent
                                   | item) +
                                   (0 
                                    + ART.Cent
                                    | item),
                               data = Data.CorrTrials,
                               REML=FALSE)

# main effect of Ambiguity
AOIKey.RegrPathDur.noAmb  <- lmer(logRegrPathDur ~ 1 + 
                                    #  Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1  
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL)+
                                     (1  
                                      | item) +
                                     (0
                                      + Vocab.Cent
                                      | item) +
                                     (0 
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=FALSE)
# model comparison
anova(AOIKey.RegrPathDur.reduced, AOIKey.RegrPathDur.noAmb)

# main effect of Vocab
AOIKey.RegrPathDur.noVocab <- lmer(logRegrPathDur ~ 1 + 
                                     Ambiguity.code  +
                                    # Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1  
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL)+
                                     (1  
                                      | item) +
                                     (0
                                      + Vocab.Cent
                                      | item) +
                                     (0 
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=FALSE)
# model comparison
anova(AOIKey.RegrPathDur.reduced, AOIKey.RegrPathDur.noVocab)

# main effect of Print exposure
AOIKey.RegrPathDur.noART  <- lmer(logRegrPathDur ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                   #  ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1  
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL)+
                                     (1  
                                      | item) +
                                     (0
                                      + Vocab.Cent
                                      | item) +
                                     (0 
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=FALSE)
# model comparison
anova(AOIKey.RegrPathDur.reduced, AOIKey.RegrPathDur.noART)

# Vocab x Ambiguity interaction
AOIKey.RegrPathDur.noVocabAmb  <- lmer(logRegrPathDur ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                  #   Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1  
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL)+
                                     (1  
                                      | item) +
                                     (0
                                      + Vocab.Cent
                                      | item) +
                                     (0 
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=FALSE)
# model comparison
anova(AOIKey.RegrPathDur.reduced, AOIKey.RegrPathDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIKey.RegrPathDur.noARTAmb  <- lmer(logRegrPathDur ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                   #  Ambiguity.code : ART.Cent +
                                     (1  
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL)+
                                     (1  
                                      | item) +
                                     (0
                                      + Vocab.Cent
                                      | item) +
                                     (0 
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=FALSE)
# model comparison
anova(AOIKey.RegrPathDur.reduced, AOIKey.RegrPathDur.noARTAmb)

# re-run maximal model with reml
AOIKey.RegrPathDur.REML <- lmer(logRegrPathDur ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1  
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL)+
                                     (1  
                                      | item) +
                                     (0
                                      + Vocab.Cent
                                      | item) +
                                     (0 
                                      + ART.Cent
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=TRUE)
# call model summary
summary(AOIKey.RegrPathDur.REML)



#### _SECOND-PASS MEASURES ####

# In this region, models for second-pass measures include only coherent trials

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove anomalous trials to retain only coherent trials
Data.CohOnly <-subset(Data.ExpTrials, subset=(Plausibility.code=="-0.5"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="2")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)


#### _second pass reading time ####
# Construct a maximal lmer() model
# Because not all the necessary models converge, remove correlations between 
# random effects for all models for the second-pass reading time measure:
AOIKey.log2ndPassTime.reduced <- lmer(log2ndPassTime ~ 1 + 
                                        Ambiguity.code  +
                                        Vocab.Cent +
                                        ART.Cent +
                                        Ambiguity.code : Vocab.Cent +
                                        Ambiguity.code : ART.Cent +
                                        (1   
                                         | RECORDING_SESSION_LABEL) +
                                        (0  
                                         + Ambiguity.code  
                                         | RECORDING_SESSION_LABEL) +
                                        (1 
                                         | item)+
                                        (0 
                                         + Vocab.Cent
                                         | item)+
                                        (0 
                                         + ART.Cent | item),
                                      data = Data.CorrTrials,
                                      REML=FALSE)

# main effect of Ambiguity
AOIKey.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                      #Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1   
                                       | RECORDING_SESSION_LABEL) +
                                      (0  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                      (1 
                                       | item)+
                                      (0 
                                       + Vocab.Cent
                                       | item)+
                                      (0 
                                       + ART.Cent | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOIKey.log2ndPassTime.reduced, AOIKey.log2ndPassTime.noAmb)

# main effect of Vocab
AOIKey.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                        Ambiguity.code  +
                                        #Vocab.Cent +
                                        ART.Cent +
                                        Ambiguity.code : Vocab.Cent +
                                        Ambiguity.code : ART.Cent +
                                        (1   
                                         | RECORDING_SESSION_LABEL) +
                                        (0  
                                         + Ambiguity.code  
                                         | RECORDING_SESSION_LABEL) +
                                        (1 
                                         | item)+
                                        (0 
                                         + Vocab.Cent
                                         | item)+
                                        (0 
                                         + ART.Cent | item),
                                      data = Data.CorrTrials,
                                      REML=FALSE)
# model comparison
anova(AOIKey.log2ndPassTime.reduced, AOIKey.log2ndPassTime.noVocab)

# main effect of Print exposure
AOIKey.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      #ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1   
                                       | RECORDING_SESSION_LABEL) +
                                      (0  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                      (1 
                                       | item)+
                                      (0 
                                       + Vocab.Cent
                                       | item)+
                                      (0 
                                       + ART.Cent | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOIKey.log2ndPassTime.reduced, AOIKey.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOIkey.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                           Ambiguity.code  +
                                           Vocab.Cent +
                                           ART.Cent +
                                           #Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1   
                                            | RECORDING_SESSION_LABEL) +
                                           (0  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1 
                                            | item)+
                                           (0 
                                            + Vocab.Cent
                                            | item)+
                                           (0 
                                            + ART.Cent | item),
                                         data = Data.CorrTrials,
                                         REML=FALSE)
# model comparison
anova(AOIKey.log2ndPassTime.reduced, AOIKey.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIKey.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         #Ambiguity.code : ART.Cent +
                                         (1   
                                          | RECORDING_SESSION_LABEL) +
                                         (0  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1 
                                          | item)+
                                         (0 
                                          + Vocab.Cent
                                          | item)+
                                         (0 
                                          + ART.Cent | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOIKey.log2ndPassTime.reduced, AOIKey.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOIKey.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1   
                                      | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL) +
                                     (1 
                                      | item)+
                                     (0 
                                      + Vocab.Cent
                                      | item)+
                                     (0 
                                      + ART.Cent | item),
                                   data = Data.CorrTrials,
                                   REML=TRUE)
# call model summary
summary(AOIKey.log2ndPassTime.REML)




####################################################
########### AREA OF INTEREST: COHERENCE CUE ########
####################################################

# In this region, models include only coherent trials

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove anomalous trials to retain only coherent trials
Data.CohOnly <-subset(Data.ExpTrials, subset=(Plausibility.code=="-0.5"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Coherence cue Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials,  IA_ID =="4")

# exclude the item "log" because Coherence and Spillover AOIs have not been defined properly for this item in Experiment Builder
Data.CorrTrials <- subset(Data.CorrTrials, !item_label =="log")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)



#### _first-fixation duration ####
# Construct a maximal lmer() model
# Because not all the necessary models converge, remove correlations between 
# random effects for all models for the first-fixation duration measure:
AOICohcue.FF.reduced <- lmer(logFirstFixDur ~ 1 + 
                           Ambiguity.code  +
                           Vocab.Cent +
                           ART.Cent +
                           Ambiguity.code : Vocab.Cent +
                           Ambiguity.code : ART.Cent +
                           (1 
                            | RECORDING_SESSION_LABEL) +
                           (0 
                            + Ambiguity.code   
                            | RECORDING_SESSION_LABEL) +
                           (1  
                            | item) +
                           (0  
                            + Vocab.Cent
                            | item) +
                           (0
                            + ART.Cent
                            | item),
                         data = Data.CorrTrials,
                         REML=FALSE)

# main effect of Ambiguity
AOICohcue.FF.noAmb <- lmer(logFirstFixDur ~ 1 + 
                           #Ambiguity.code  +
                           Vocab.Cent +
                           ART.Cent +
                           Ambiguity.code : Vocab.Cent +
                           Ambiguity.code : ART.Cent  +
                           (1 
                             | RECORDING_SESSION_LABEL) +
                             (0 
                              + Ambiguity.code   
                              | RECORDING_SESSION_LABEL) +
                             (1  
                              | item) +
                             (0  
                              + Vocab.Cent
                              | item) +
                             (0
                              + ART.Cent
                              | item),
                         data = Data.CorrTrials,
                         REML=FALSE)
# model comparison
anova(AOICohcue.FF.reduced, AOICohcue.FF.noAmb)

# main effect of Vocab
AOICohcue.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                                 Ambiguity.code  +
                                 #Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                   (1 
                                    | RECORDING_SESSION_LABEL) +
                                   (0 
                                    + Ambiguity.code   
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    | item) +
                                   (0  
                                    + Vocab.Cent
                                    | item) +
                                   (0
                                    + ART.Cent
                                    | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.reduced, AOICohcue.FF.noVocab)

# main effect of Print exposure
AOICohcue.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 #ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                 (1 
                                  | RECORDING_SESSION_LABEL) +
                                 (0 
                                  + Ambiguity.code   
                                  | RECORDING_SESSION_LABEL) +
                                 (1  
                                  | item) +
                                 (0  
                                  + Vocab.Cent
                                  | item) +
                                 (0
                                  + ART.Cent
                                  | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.reduced, AOICohcue.FF.noART)

# Vocab x Ambiguity interaction
AOICohcue.FF.noVocabAmb <- lmer(logFirstFixDur ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 #Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                   (1 
                                    | RECORDING_SESSION_LABEL) +
                                   (0 
                                    + Ambiguity.code   
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    | item) +
                                   (0  
                                    + Vocab.Cent
                                    | item) +
                                   (0
                                    + ART.Cent
                                    | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.reduced, AOICohcue.FF.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.FF.noARTAmb <- lmer(logFirstFixDur ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 #Ambiguity.code : ART.Cent +
                                   (1 
                                    | RECORDING_SESSION_LABEL) +
                                   (0 
                                    + Ambiguity.code   
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    | item) +
                                   (0  
                                    + Vocab.Cent
                                    | item) +
                                   (0
                                    + ART.Cent
                                    | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.reduced, AOICohcue.FF.noARTAmb)

# re-run maximal model with REML
AOICohcue.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                (1 
                                 | RECORDING_SESSION_LABEL) +
                                (0 
                                 + Ambiguity.code   
                                 | RECORDING_SESSION_LABEL) +
                                (1  
                                 | item) +
                                (0  
                                 + Vocab.Cent
                                 | item) +
                                (0
                                 + ART.Cent
                                 | item),
                               data = Data.CorrTrials,
                               REML=TRUE)
# call model summary
summary(AOICohcue.FF.REML)


#### _gaze duration ####
# Construct maximal lmer() model
AOICohcue.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                Ambiguity.code  +
                                Vocab.Cent +
                                ART.Cent +
                                Ambiguity.code : Vocab.Cent +
                                Ambiguity.code : ART.Cent +
                                (1  
                                 + Ambiguity.code  
                                 | RECORDING_SESSION_LABEL) +
                                (1  
                                 + Vocab.Cent 
                                 + ART.Cent
                                 | item),
                              data = Data.CorrTrials,
                              REML=FALSE)

# main effect of Ambiguity
AOICohcue.GazeDur.noAmb <- lmer(logGazeDur ~ 1 + 
                                #Ambiguity.code  +
                                Vocab.Cent +
                                ART.Cent +
                                Ambiguity.code : Vocab.Cent +
                                Ambiguity.code : ART.Cent +
                                (1  
                                 + Ambiguity.code  
                                 | RECORDING_SESSION_LABEL) +
                                  (1  
                                   + Vocab.Cent 
                                   + ART.Cent
                                   | item),
                              data = Data.CorrTrials,
                              REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noAmb)

# main effect of Vocab
AOICohcue.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                      Ambiguity.code  +
                                      #Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent 
                                         + ART.Cent
                                         | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noVocab)

# main effect of Print exposure
AOICohcue.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      #ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent 
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noART)

# vVcab x Ambiguity interaction
AOICohcue.GazeDur.noVocabAmb <- lmer(logGazeDur ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      #Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent 
                                         + ART.Cent
                                         | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.GazeDur.noARTAmb <- lmer(logGazeDur ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      #Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent 
                                         + ART.Cent
                                         | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noARTAmb)

# re-run maximal model with reml
AOICohcue.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                     (1  
                                      + Vocab.Cent 
                                      + ART.Cent
                                      | item),
                                    data = Data.CorrTrials,
                                    REML=TRUE)
# call model summary
summary(AOICohcue.GazeDur.REML)



#### _regressions out ####
# Construct a maximal glmer() model
AOICohcue.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                 (1 
                                  + Ambiguity.code  
                                  | RECORDING_SESSION_LABEL) +
                                 (1  
                                  + Vocab.Cent 
                                  + ART.Cent
                                  | item),
                               data = Data.CorrTrials,
                               family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOICohcue.RegrOut.max)

# main effect of Ambiguity
AOICohcue.RegrOut.noAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                # Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                 (1 
                                  + Ambiguity.code  
                                  | RECORDING_SESSION_LABEL) +
                                  (1  
                                   + Vocab.Cent 
                                   + ART.Cent
                                   | item),
                               data = Data.CorrTrials,
                               family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noAmb)

# main effect of Vocab
AOICohcue.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                       Ambiguity.code  +
                                      # Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1 
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent 
                                         + ART.Cent
                                         | item),
                                     data = Data.CorrTrials,
                                     family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noVocab)

# main effect of Print exposure
AOICohcue.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                        Ambiguity.code  +
                                       Vocab.Cent +
                                       #ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1 
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent 
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noART)

# Vocab x Ambiguity interaction
AOICohcue.RegrOut.noVocabAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                      # Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1 
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent 
                                         + ART.Cent
                                         | item),
                                     data = Data.CorrTrials,
                                     family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.RegrOut.noARTAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       #Ambiguity.code : ART.Cent +
                                       (1 
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent 
                                          + ART.Cent
                                          | item),
                                     data = Data.CorrTrials,
                                     family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noARTAmb)


#### _go-past time ####
# Construct a maximal lmer() model
AOICohcue.RegrPathDur.max <- lmer(logRegrPathDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1  
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent 
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)

# main effect of Ambiguity
AOICohcue.RegrPathDur.noAmb <- lmer(logRegrPathDur ~ 1 + 
                                   # Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1  
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                     (1  
                                      + Vocab.Cent 
                                      + ART.Cent
                                      | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noAmb)

# main effect of Vocab
AOICohcue.RegrPathDur.noVocab <- lmer(logRegrPathDur ~ 1 + 
                                          Ambiguity.code  +
                                          #Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent 
                                             + ART.Cent
                                             | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noVocab)

# main effect of Print exposure
AOICohcue.RegrPathDur.noART <- lmer(logRegrPathDur ~ 1 + 
                                         Ambiguity.code  +
                                          Vocab.Cent +
                                        #  ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent 
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noART)

# Vocab x Ambiguity interaction
AOICohcue.RegrPathDur.noVocabAmb <- lmer(logRegrPathDur ~ 1 + 
                                           Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          #Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent 
                                             + ART.Cent
                                             | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.RegrPathDur.noARTAmb <- lmer(logRegrPathDur ~ 1 + 
                                          Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          #Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent 
                                             + ART.Cent
                                             | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noARTAmb)

# re-run maximal model with reml
AOICohcue.RegrPathDur.REML <- lmer(logRegrPathDur ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent 
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=TRUE)
# call model summary
summary(AOICohcue.RegrPathDur.REML)


#### _second pass reading time ####
# Construct maximal lmer() model
AOICohcue.log2ndPassTime.max <- lmer(log2ndPassTime ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1  
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent 
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)

# main effect of Ambiguity
AOICohcue.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                         # Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent 
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.max, AOICohcue.log2ndPassTime.noAmb)

# main effect of Vocab
AOICohcue.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                           Ambiguity.code  +
                                           # Vocab.Cent +
                                           ART.Cent +
                                           Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent 
                                            + ART.Cent
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.max, AOICohcue.log2ndPassTime.noVocab)

# main effect of Print exposure
AOICohcue.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         # ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent 
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.max, AOICohcue.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOICohcue.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                              Ambiguity.code  +
                                              Vocab.Cent +
                                              ART.Cent +
                                              # Ambiguity.code : Vocab.Cent +
                                              Ambiguity.code : ART.Cent +
                                              (1  
                                               + Ambiguity.code  
                                               | RECORDING_SESSION_LABEL) +
                                              (1  
                                               + Vocab.Cent 
                                               + ART.Cent
                                               | item),
                                            data = Data.CorrTrials,
                                            REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.max, AOICohcue.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            #Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent 
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.max, AOICohcue.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOICohcue.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                        Ambiguity.code  +
                                        Vocab.Cent +
                                        ART.Cent +
                                        Ambiguity.code : Vocab.Cent +
                                        Ambiguity.code : ART.Cent +
                                        (1  
                                         + Ambiguity.code  
                                         | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent 
                                         + ART.Cent
                                         | item),
                                      data = Data.CorrTrials,
                                      REML=TRUE)
# call model summary
summary(AOICohcue.log2ndPassTime.REML)


####################################################
########### AREA OF INTEREST: SPILLOVER ############
####################################################

# In this region, models include only coherent trials

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove anomalous trials to retain only coherent trials
Data.CohOnly <-subset(Data.ExpTrials, subset=(Plausibility.code=="-0.5"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials,  IA_ID =="5")

# exclude the item "log" because Coherence and Spillover AOIs have not been defined properly for this item in Experiment Builder
Data.CorrTrials <- subset(Data.CorrTrials, !item_label =="log")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)



#### _first-fixation duration ####
# Construct a maximal lmer() model
AOISpillover.FF.max <- lmer(logFirstFixDur ~ 1 + 
                              Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code   
                               | RECORDING_SESSION_LABEL) +
                              (1  
                               + Vocab.Cent
                               + ART.Cent
                               | item),
                            data = Data.CorrTrials,
                            REML=FALSE)

# main effect of Ambiguity
AOISpillover.FF.noAmb <- lmer(logFirstFixDur ~ 1 + 
                              #Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code   
                               | RECORDING_SESSION_LABEL) +
                                (1  
                                 + Vocab.Cent
                                 + ART.Cent
                                 | item),
                            data = Data.CorrTrials,
                            REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noAmb)

# main effect of Vocab
AOISpillover.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    #Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noVocab)

# main effect of Print exposure
AOISpillover.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    #ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noART)

# Vocab x Ambiguity interaction
AOISpillover.FF.noVocabAmb <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    #Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.FF.noARTAmb <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    #Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noARTAmb)

# re-run maximal model with REML
AOISpillover.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                  data = Data.CorrTrials,
                                  REML=TRUE)
# call model summary
summary(AOISpillover.FF.REML)


#### _gaze duration ####
# Construct maximal lmer() model
AOISpillover.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)

# main effect of Ambiguity
AOISpillover.GazeDur.noAmb <- lmer(logGazeDur ~ 1 + 
                              #     Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                (1  
                                 + Vocab.Cent
                                 + ART.Cent
                                 | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noAmb)

# main effect of Vocab
AOISpillover.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                    #     Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noVocab)

# main effect of Print exposure
AOISpillover.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         #ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noART)

# Vocab x Ambiguity interaction
AOISpillover.GazeDur.noVocabAmb <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         #Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent
                                            + ART.Cent
                                            | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.GazeDur.noARTAmb <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                        # Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noARTAmb)

# re-run maximal model with reml
AOISpillover.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent
                                         + ART.Cent
                                         | item),
                                       data = Data.CorrTrials,
                                       REML=TRUE)
# call model summary
summary(AOISpillover.GazeDur.REML)


#### __ pairwise comparison: gaze durations in Amb vs UA trials ####

          ### ____ Amb trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="5")

# subset only rows for Amb
Data.CorrTrials <- subset(Data.CorrTrials, amb =="Amb")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)


# Construct a maximal lmer() model
AOISpillover.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                   Vocab.Cent +
                                   ART.Cent +
                                   (1   
                                    | RECORDING_SESSION_LABEL) +
                                  (1  
                                   + Vocab.Cent
                                   + ART.Cent
                                   | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)

# main effect of Vocab
AOISpillover.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                       # Vocab.Cent +
                                       ART.Cent +
                                       (1   
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noVocab)

# main effect of Print exposure
max.AOISpillover.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                            Vocab.Cent +
                                          #  ART.Cent +
                                            (1   
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noART)

# re-run maximal model with reml
AOISpillover.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                        Vocab.Cent +
                                        ART.Cent +
                                        (1   
                                         | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent
                                         + ART.Cent
                                         | item),
                                      data = Data.CorrTrials,
                                      REML=TRUE)
# model comparison
summary(AOISpillover.GazeDur.REML)


        ### ____ UA trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="5")

# subset only rows for UA
Data.CorrTrials <- subset(Data.CorrTrials, amb =="UA")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)


# Construct a maximal lmer() model
AOISpillover.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                   Vocab.Cent +
                                   ART.Cent +
                                   (1   
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)

# main effect of Vocab
AOISpillover.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                       # Vocab.Cent +
                                       ART.Cent +
                                       (1   
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noVocab)

# main effect of Print exposure
max.AOISpillover.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                         Vocab.Cent +
                                         #  ART.Cent +
                                         (1   
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noART)

# re-run maximal model with reml
AOISpillover.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                    Vocab.Cent +
                                    ART.Cent +
                                    (1   
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=TRUE)
# model comparison
summary(AOISpillover.GazeDur.REML)



#### _regressions out ####
# Construct a maximal glmer() model
AOISpillover.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOISpillover.RegrOut.max)

# main effect of Ambiguity
AOISpillover.RegrOut.noAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                  #  Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noAmb)

# main effect of Vocab
AOISpillover.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                            Ambiguity.code  +
                                          #Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noVocab)

# main effect of Print exposure
AOISpillover.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                            Ambiguity.code  +
                                          Vocab.Cent +
                                          #ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noART)

# Vocab x Ambiguity interaction
AOISpillover.RegrOut.noVocabAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                            Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          #Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.RegrOut.noARTAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                           Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          #Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noARTAmb)


#### __ pairwise comparison: regressions out in Amb vs UA trials ####

      ### ____ Amb trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="5")

# subset only rows for Amb
Data.CorrTrials <- subset(Data.CorrTrials, amb =="Amb")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)


# Construct a maximal glmer() model
AOISpillover.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                    Vocab.Cent +
                                    ART.Cent +
                                    (1  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOISpillover.RegrOut.max)

# main effet of Vocab
AOISpillover.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                      #  Vocab.Cent +
                                        ART.Cent +
                                        (1  
                                         | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent
                                         + ART.Cent
                                         | item),
                                      data = Data.CorrTrials,
                                      family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noVocab)

# main effect of Print exposure
AOISpillover.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                      Vocab.Cent +
                                     # ART.Cent +
                                      (1  
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noART)


          ### ____ UA trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="5")

# subset only rows for UA
Data.CorrTrials <- subset(Data.CorrTrials, amb =="UA")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)


# Construct a maximal glmer() model
AOISpillover.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                    Vocab.Cent +
                                    ART.Cent +
                                    (1  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOISpillover.RegrOut.max)

# main effet of Vocab
AOISpillover.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                        #  Vocab.Cent +
                                        ART.Cent +
                                        (1  
                                         | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent
                                         + ART.Cent
                                         | item),
                                      data = Data.CorrTrials,
                                      family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noVocab)

# main effect of Print exposure
AOISpillover.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                      Vocab.Cent +
                                      # ART.Cent +
                                      (1  
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noART)



#### _go-past time ####
# Construct a maximal lmer() model 
AOISpillover.RegrPathDur.max <- lmer(logRegrPathDur ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1  
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)

# main effect of Ambiguity
AOISpillover.RegrPathDur.noAmb <- lmer(logRegrPathDur ~ 1 + 
                                       #Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1  
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noAmb)

# main effect of Vocab
AOISpillover.RegrPathDur.noVocab <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             #Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noVocab)

# main effect of Print exposure
AOISpillover.RegrPathDur.noART <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             #ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noART)

# Vocab x Ambiguity interaction
AOISpillover.RegrPathDur.noVocabAmb <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             #Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.RegrPathDur.noARTAmb <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             #Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noARTAmb)

# re-run maximal model with reml
AOISpillover.RegrPathDur.REML <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                           data = Data.CorrTrials,
                                           REML=TRUE)
# call model summary
summary(AOISpillover.RegrPathDur.REML)


#### _second pass reading time ####
# Construct a maximal lmer() model
AOISpillover.log2ndPassTime <- lmer(log2ndPassTime ~ 1 + 
                                          Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)

# main effect of Ambiguity 
AOISpillover.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                                #Ambiguity.code  +
                                                Vocab.Cent +
                                                ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                Ambiguity.code : ART.Cent +
                                                (1  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                                (1  
                                                 + Vocab.Cent
                                                 + ART.Cent
                                                 | item),
                                              data = Data.CorrTrials,
                                              REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noAmb)

# main effect of Vocab
AOISpillover.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                                  Ambiguity.code  +
                                                  #Vocab.Cent +
                                                  ART.Cent +
                                                  Ambiguity.code : Vocab.Cent +
                                                  Ambiguity.code : ART.Cent +
                                                  (1  
                                                   + Ambiguity.code  
                                                   | RECORDING_SESSION_LABEL) +
                                                  (1  
                                                   + Vocab.Cent
                                                   + ART.Cent
                                                   | item),
                                                data = Data.CorrTrials,
                                                REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noVocab)

# main effect of Print exposure
AOISpillover.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                                Ambiguity.code  +
                                                Vocab.Cent +
                                                #ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                Ambiguity.code : ART.Cent +
                                                (1  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                                (1  
                                                 + Vocab.Cent
                                                 + ART.Cent
                                                 | item),
                                              data = Data.CorrTrials,
                                              REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOISpillover.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                                     Ambiguity.code  +
                                                     Vocab.Cent +
                                                     ART.Cent +
                                                     #Ambiguity.code : Vocab.Cent +
                                                     Ambiguity.code : ART.Cent +
                                                     (1  
                                                      + Ambiguity.code  
                                                      | RECORDING_SESSION_LABEL) +
                                                     (1  
                                                      + Vocab.Cent
                                                      + ART.Cent
                                                      | item),
                                                   data = Data.CorrTrials,
                                                   REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                                   Ambiguity.code  +
                                                   Vocab.Cent +
                                                   ART.Cent +
                                                   Ambiguity.code : Vocab.Cent +
                                                   #Ambiguity.code : ART.Cent +
                                                   (1  
                                                    + Ambiguity.code  
                                                    | RECORDING_SESSION_LABEL) +
                                                   (1  
                                                    + Vocab.Cent
                                                    + ART.Cent
                                                    | item),
                                                 data = Data.CorrTrials,
                                                 REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOISpillover.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                               Ambiguity.code  +
                                               Vocab.Cent +
                                               ART.Cent +
                                               Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                             data = Data.CorrTrials,
                                             REML=TRUE)
# call model summary
summary(AOISpillover.log2ndPassTime.REML)



####################################################
########### AREA OF INTEREST: WRAP-UP ##############
####################################################

# In this region, models include only coherent trials

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove anomalous trials to retain only coherent trials
Data.CohOnly <-subset(Data.ExpTrials, subset=(Plausibility.code=="-0.5"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Wrap-up Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials,  IA_ID =="6")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)



#### _first-fixation duration ####
# Construct a maximal lmer() model
AOIWrapUp.FF.max <- lmer(logFirstFixDur ~ 1 + 
                              Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code   
                               | RECORDING_SESSION_LABEL) +
                              (1  
                               + Vocab.Cent
                               + ART.Cent
                               | item),
                            data = Data.CorrTrials,
                            REML=FALSE)

# main effect of Ambiguity
AOIWrapUp.FF.noAmb <- lmer(logFirstFixDur ~ 1 + 
                                    #Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                 (1  
                                  + Vocab.Cent
                                  + ART.Cent
                                  | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOIWrapUp.FF.max, AOIWrapUp.FF.noAmb)

# main effect of Vocab
AOIWrapUp.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                                      Ambiguity.code  +
                                      #Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1 
                                       + Ambiguity.code   
                                       | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOIWrapUp.FF.max, AOIWrapUp.FF.noVocab)

# main effect of Print exposurer
AOIWrapUp.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    #ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                 (1  
                                  + Vocab.Cent
                                  + ART.Cent
                                  | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOIWrapUp.FF.max, AOIWrapUp.FF.noART)

# Vocab x Ambiguity interaction
AOIWrapUp.FF.noVocabAmb <- lmer(logFirstFixDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         #Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1 
                                          + Ambiguity.code   
                                          | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOIWrapUp.FF.max, AOIWrapUp.FF.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIWrapUp.FF.noARTAmb <- lmer(logFirstFixDur ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       #Ambiguity.code : ART.Cent +
                                       (1 
                                        + Ambiguity.code   
                                        | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)
# model comparison
anova(AOIWrapUp.FF.max, AOIWrapUp.FF.noARTAmb)

# re-run maximal model with REML
AOIWrapUp.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1 
                                    + Ambiguity.code   
                                    | RECORDING_SESSION_LABEL) +
                                (1  
                                 + Vocab.Cent
                                 + ART.Cent
                                 | item),
                                 data = Data.CorrTrials,
                                 REML=TRUE)
# call model summary
summary(AOIWrapUp.FF.REML)


#### _gaze duration ####
# Construct maximal lmer() model
AOIWrapUp.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                (1  
                                 + Vocab.Cent
                                 + ART.Cent
                                 | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)

# main effect of Ambiguity
AOIWrapUp.GazeDur.noAmb <- lmer(logGazeDur ~ 1 + 
                                         #     Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOIWrapUp.GazeDur.max, AOIWrapUp.GazeDur.noAmb)

# main effect of Vocab
AOIWrapUp.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                           Ambiguity.code  +
                                           #     Vocab.Cent +
                                           ART.Cent +
                                           Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent
                                         + ART.Cent
                                         | item),
                                         data = Data.CorrTrials,
                                         REML=FALSE)
# model comparison
anova(AOIWrapUp.GazeDur.max, AOIWrapUp.GazeDur.noVocab)

# main effect of Print exposure
AOIWrapUp.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         #ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOIWrapUp.GazeDur.max, AOIWrapUp.GazeDur.noARTAmb)

# Vocab x Ambiguity interaction
AOIWrapUp.GazeDur.noVocabAmb <- lmer(logGazeDur ~ 1 + 
                                              Ambiguity.code  +
                                              Vocab.Cent +
                                              ART.Cent +
                                              #Ambiguity.code : Vocab.Cent +
                                              Ambiguity.code : ART.Cent +
                                              (1  
                                               + Ambiguity.code  
                                               | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent
                                            + ART.Cent
                                            | item),
                                            data = Data.CorrTrials,
                                            REML=FALSE)
# model comparison
anova(AOIWrapUp.GazeDur.max, AOIWrapUp.GazeDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIWrapUp.GazeDur.noARTAmb <- lmer(logGazeDur ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            # Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOIWrapUp.GazeDur.max, AOIWrapUp.GazeDur.noARTAmb)

# re-run maximal model with reml
AOIWrapUp.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                        Ambiguity.code  +
                                        Vocab.Cent +
                                        ART.Cent +
                                        Ambiguity.code : Vocab.Cent +
                                        Ambiguity.code : ART.Cent +
                                        (1  
                                         + Ambiguity.code  
                                         | RECORDING_SESSION_LABEL) +
                                     (1  
                                      + Vocab.Cent
                                      + ART.Cent
                                      | item),
                                      data = Data.CorrTrials,
                                      REML=TRUE)
# call model summary
summary(AOIWrapUp.GazeDur.REML)


#### _regressions out ####
# Construct maximal glmer() model
AOIWrapUp.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                 (1  
                                  + Vocab.Cent
                                  + ART.Cent
                                  | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOIWrapUp.RegrOut.max)

# main effect of Ambiguity
AOIWrapUp.RegrOut.noAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                          #  Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIWrapUp.RegrOut.max, AOIWrapUp.RegrOut.noAmb)

# main effect of Vocab
AOIWrapUp.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                            Ambiguity.code  +
                                            #Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1 
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                          data = Data.CorrTrials,
                                          family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIWrapUp.RegrOut.max, AOIWrapUp.RegrOut.noVocab)

# main effect of Print exposure
AOIWrapUp.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                          Ambiguity.code  +
                                          Vocab.Cent +
                                          #ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIWrapUp.RegrOut.max, AOIWrapUp.RegrOut.noART)

# Vocab x Ambiguity interaction
AOIWrapUp.RegrOut.noVocabAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                               Ambiguity.code  +
                                               Vocab.Cent +
                                               ART.Cent +
                                               #Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1 
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                             data = Data.CorrTrials,
                                             family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIWrapUp.RegrOut.max, AOIWrapUp.RegrOut.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIWrapUp.RegrOut.noARTAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             #Ambiguity.code : ART.Cent +
                                             (1 
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                           data = Data.CorrTrials,
                                           family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOIWrapUp.RegrOut.max, AOIWrapUp.RegrOut.noARTAmb)


#### _go-past time ####
# Construct maximal lmer() model
AOIWrapUp.RegrPathDur.max <- lmer(logRegrPathDur ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1  
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)

# main effect of Ambiguity
AOIWrapUp.RegrPathDur.noAmb <- lmer(logRegrPathDur ~ 1 + 
                                             #Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOIWrapUp.RegrPathDur.max, AOIWrapUp.RegrPathDur.noAmb)

# main effect of Vocab
AOIWrapUp.RegrPathDur.noVocab <- lmer(logRegrPathDur ~ 1 + 
                                               Ambiguity.code  +
                                               #Vocab.Cent +
                                               ART.Cent +
                                               Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                             data = Data.CorrTrials,
                                             REML=FALSE)
# model comparison
anova(AOIWrapUp.RegrPathDur.max, AOIWrapUp.RegrPathDur.noVocab)

# main effect of Print exposure
AOIWrapUp.RegrPathDur.noART <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             #ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOIWrapUp.RegrPathDur.max, AOIWrapUp.RegrPathDur.noART)

# Vocab x Ambiguity interaction
AOIWrapUp.RegrPathDur.noVocabAmb <- lmer(logRegrPathDur ~ 1 + 
                                                  Ambiguity.code  +
                                                  Vocab.Cent +
                                                  ART.Cent +
                                                  #Ambiguity.code : Vocab.Cent +
                                                  Ambiguity.code : ART.Cent +
                                                  (1  
                                                   + Ambiguity.code  
                                                   | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                                data = Data.CorrTrials,
                                                REML=FALSE)
# model comparison
anova(AOIWrapUp.RegrPathDur.max, AOIWrapUp.RegrPathDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIWrapUp.RegrPathDur.noARTAmb <- lmer(logRegrPathDur ~ 1 + 
                                                Ambiguity.code  +
                                                Vocab.Cent +
                                                ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                #Ambiguity.code : ART.Cent +
                                                (1  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                              data = Data.CorrTrials,
                                              REML=FALSE)
# model comparison
anova(AOIWrapUp.RegrPathDur.max, AOIWrapUp.RegrPathDur.noARTAmb)

# re-run maximal model with reml
AOIWrapUp.RegrPathDur.REML <- lmer(logRegrPathDur ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                          data = Data.CorrTrials,
                                          REML=TRUE)
# call model summary
summary(AOIWrapUp.RegrPathDur.REML)



#### _second pass reading time ####
# Construct maximal lmer() model
AOIWrapUp.log2ndPassTime <- lmer(log2ndPassTime ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1  
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)

# main effect of Ambiguity
AOIWrapUp.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                             #Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOIWrapUp.log2ndPassTime.max, AOIWrapUp.log2ndPassTime.noAmb)

# main effect of Vocab
AOIWrapUp.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                               Ambiguity.code  +
                                               #Vocab.Cent +
                                               ART.Cent +
                                               Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                             data = Data.CorrTrials,
                                             REML=FALSE)
# model comparison
anova(AOIWrapUp.log2ndPassTime.max, AOIWrapUp.log2ndPassTime.noVocab)

# main effect of Print exposure
AOIWrapUp.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             #ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOIWrapUp.log2ndPassTime.max, AOIWrapUp.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOIWrapUp.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                                  Ambiguity.code  +
                                                  Vocab.Cent +
                                                  ART.Cent +
                                                  #Ambiguity.code : Vocab.Cent +
                                                  Ambiguity.code : ART.Cent +
                                                  (1  
                                                   + Ambiguity.code  
                                                   | RECORDING_SESSION_LABEL) +
                                                  (1  
                                                   + Vocab.Cent
                                                   + ART.Cent
                                                   | item),
                                                data = Data.CorrTrials,
                                                REML=FALSE)
# model comparison
anova(AOIWrapUp.log2ndPassTime.max, AOIWrapUp.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIWrapUp.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                                Ambiguity.code  +
                                                Vocab.Cent +
                                                ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                #Ambiguity.code : ART.Cent +
                                                (1  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                                (1  
                                                 + Vocab.Cent
                                                 + ART.Cent
                                                 | item),
                                              data = Data.CorrTrials,
                                              REML=FALSE)
# model comparison
anova(AOIWrapUp.log2ndPassTime.max, AOIWrapUp.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOIWrapUp.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=TRUE)
# call model summary
summary(AOIWrapUp.log2ndPassTime.REML)




####################################################################################################
#### READING TIME ANALYSES: processing temporarily ambiguous vs permanently anomalous sentences ####
####################################################################################################

#################################################################
################# AREA OF INTEREST: KEYWORD #####################
#################################################################

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove AnomAmb and CohUA trials to retain only CohAmb and AnomUA trials
Data.PlAmbAnomUAOnly <-subset(Data.ExpTrials, subset=(cond_label != "PlausUA"
                                                      & cond_label !="AnomAmb"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="2")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)


#### _second pass reading time ####
# Construct a maximal lmer() model
AOIAmb.log2ndPassTime.max <- lmer(log2ndPassTime ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1  
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent 
                                     + ART.Cent 
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)

# main effect of Ambiguity
AOIAmb.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                          #Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent 
                                           + ART.Cent 
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOIAmb.log2ndPassTime.max, AOIAmb.log2ndPassTime.noAmb)

# main effect of Vocab
AOIAmb.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                            Ambiguity.code  +
                                            #Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent 
                                             + ART.Cent 
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOIAmb.log2ndPassTime.max, AOIAmb.log2ndPassTime.noVocab)

# main effect of Print exposure
AOIAmb.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                          Ambiguity.code  +
                                          Vocab.Cent +
                                          #ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent 
                                           + ART.Cent 
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOIAmb.log2ndPassTime.max, AOIAmb.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOIAmb.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                               Ambiguity.code  +
                                               Vocab.Cent +
                                               ART.Cent +
                                               #Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent 
                                                + ART.Cent 
                                                | item),
                                             data = Data.CorrTrials,
                                             REML=FALSE)
# model comparison
anova(AOIAmb.log2ndPassTime.max, AOIAmb.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOIAmb.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             #Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent 
                                              + ART.Cent 
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOIAmb.log2ndPassTime.max, AOIAmb.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOIAmb.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent 
                                          + ART.Cent 
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=TRUE)
# call model summary
summary(AOIAmb.log2ndPassTime.REML)



####################################################
########### AREA OF INTEREST: COHERENCE CUE ########
####################################################

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove AnomAmb and CohUA trials to retain only CohAmb and AnomUA trials
Data.PlAmbAnomUAOnly <-subset(Data.ExpTrials, subset=(cond_label != "PlausUA"
                                                      & cond_label !="AnomAmb"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Keyword Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="4")

# exclude the item "log" because Coherence and Spillover AOIs have not been defined properly for this item in Experiment Builder
Data.CorrTrials <- subset(Data.CorrTrials, !item_label =="log")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)



#### _first-fixation duration ####
# Construct a maximal lmer() model
AOICohcue.FF.max <- lmer(logFirstFixDur ~ 1 + 
                           Ambiguity.code  +
                           Vocab.Cent +
                           ART.Cent +
                           Ambiguity.code : Vocab.Cent +
                           Ambiguity.code : ART.Cent +
                           (1 
                            + Ambiguity.code   
                            | RECORDING_SESSION_LABEL) +
                           (1  
                            + Vocab.Cent 
                            + ART.Cent  | item),
                         data = Data.CorrTrials,
                         REML=FALSE)

# main effect of Ambiguity
AOICohcue.FF.noAmb <- lmer(logFirstFixDur ~ 1 + 
                                 #Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                 (1 
                                  + Ambiguity.code   
                                  | RECORDING_SESSION_LABEL) +
                                 (1  
                                  + Vocab.Cent 
                                  + ART.Cent  | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noAmb)

# main effect of Vocab
AOICohcue.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                                   Ambiguity.code  +
                                   #Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1 
                                    + Ambiguity.code   
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent 
                                    + ART.Cent  | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noVocab)

# main effet of Print exposure
AOICohcue.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 #ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                 (1 
                                  + Ambiguity.code   
                                  | RECORDING_SESSION_LABEL) +
                                 (1  
                                  + Vocab.Cent 
                                  + ART.Cent  | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noART)

# Vocab x Ambiguity interaction
AOICohcue.FF.noVocabAmb <- lmer(logFirstFixDur ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      #Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1 
                                       + Ambiguity.code   
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent 
                                       + ART.Cent  | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.FF.noARTAmb <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    #Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent 
                                     + ART.Cent  | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noARTAmb)

# re-run maximal model with REML
AOICohcue.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                                Ambiguity.code  +
                                Vocab.Cent +
                                ART.Cent +
                                Ambiguity.code : Vocab.Cent +
                                Ambiguity.code : ART.Cent +
                                (1 
                                 + Ambiguity.code   
                                 | RECORDING_SESSION_LABEL) +
                                (1  
                                 + Vocab.Cent 
                                 + ART.Cent  | item),
                              data = Data.CorrTrials,
                              REML=TRUE)
# call model comparison
summary(AOICohcue.FF.REML)



#### __ pairwise comparison: first-fixation durations in CohAmb vs AnomUA trials ####

      ### ____ CohAmb trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Coherence cue Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="4")

# subset only rows for CohAmb
Data.CorrTrials <- subset(Data.CorrTrials, cond_label =="PlausAmb")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)

# Construct a maximal lmer() model
AOICohcue.FF.max <- lmer(logFirstFixDur ~ 1 + 
                           Vocab.Cent +
                           ART.Cent +
                           (1 
                            | RECORDING_SESSION_LABEL) +
                           (1 
                            + Vocab.Cent 
                            + ART.Cent | item),
                         data = Data.CorrTrials,
                         REML=FALSE)

# main effect of Vocab
AOICohcue.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                                 #  Vocab.Cent +
                                   ART.Cent +
                                   (1 
                                    | RECORDING_SESSION_LABEL) +
                                   (1 
                                    + Vocab.Cent 
                                    + ART.Cent | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noVocab)

# main effect of Print exposure
AOICohcue.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                                 Vocab.Cent +
                               #  ART.Cent +
                                 (1 
                                  | RECORDING_SESSION_LABEL) +
                                 (1 
                                  + Vocab.Cent 
                                  + ART.Cent | item),
                               data = Data.CorrTrials,
                               REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noART)

# re-run maximal model with REML
AOICohcue.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                                Vocab.Cent +
                                ART.Cent +
                                (1 
                                 | RECORDING_SESSION_LABEL) +
                                (1 
                                 + Vocab.Cent 
                                 + ART.Cent | item),
                              data = Data.CorrTrials,
                              REML=TRUE)
# call model summary
summary(AOICohcue.FF.REML)

        
          ### ____ AnomUA trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Coherence cue Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="4")

# subset only rows for AnomUA
Data.CorrTrials <- subset(Data.CorrTrials, cond_label =="AnomUA")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)

# Construct a maximal lmer() model
AOICohcue.FF.max <- lmer(logFirstFixDur ~ 1 + 
                           Vocab.Cent +
                           ART.Cent +
                           (1 
                            | RECORDING_SESSION_LABEL) +
                           (1 
                            + Vocab.Cent 
                            + ART.Cent | item),
                         data = Data.CorrTrials,
                         REML=FALSE)

# main effect of Vocab
AOICohcue.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                               #  Vocab.Cent +
                               ART.Cent +
                               (1 
                                | RECORDING_SESSION_LABEL) +
                               (1 
                                + Vocab.Cent 
                                + ART.Cent | item),
                             data = Data.CorrTrials,
                             REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noVocab)

# main effect of Print exposure
AOICohcue.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                             Vocab.Cent +
                             #  ART.Cent +
                             (1 
                              | RECORDING_SESSION_LABEL) +
                             (1 
                              + Vocab.Cent 
                              + ART.Cent | item),
                           data = Data.CorrTrials,
                           REML=FALSE)
# model comparison
anova(AOICohcue.FF.max, AOICohcue.FF.noART)

# re-run maximal model with REML
AOICohcue.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                            Vocab.Cent +
                            ART.Cent +
                            (1 
                             | RECORDING_SESSION_LABEL) +
                            (1 
                             + Vocab.Cent 
                             + ART.Cent | item),
                          data = Data.CorrTrials,
                          REML=TRUE)
# call model summary
summary(AOICohcue.FF.REML)



#### _gaze duration ####
# Construct a maximal lmer() model
AOICohcue.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                Ambiguity.code  +
                                Vocab.Cent +
                                ART.Cent +
                                Ambiguity.code : Vocab.Cent +
                                Ambiguity.code : ART.Cent +
                                (1  
                                 + Ambiguity.code  
                                 | RECORDING_SESSION_LABEL) +
                                (1
                                 + Vocab.Cent 
                                 + ART.Cent  
                                 | item),
                              data = Data.CorrTrials,
                              REML=FALSE)

# main effect of Ambiguity
AOICohcue.GazeDur.noAmb <- lmer(logGazeDur ~ 1 + 
                                      #Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                      (1
                                       + Vocab.Cent 
                                       + ART.Cent  
                                       | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noAmb)

# main effect of Vocab
AOICohcue.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                        Ambiguity.code  +
                                        #Vocab.Cent +
                                        ART.Cent +
                                        Ambiguity.code : Vocab.Cent +
                                        Ambiguity.code : ART.Cent +
                                        (1  
                                         + Ambiguity.code  
                                         | RECORDING_SESSION_LABEL) +
                                        (1
                                         + Vocab.Cent 
                                         + ART.Cent  
                                         | item),
                                      data = Data.CorrTrials,
                                      REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noVocab)

# main effect of Print exposure
AOICohcue.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      #ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                      (1
                                       + Vocab.Cent 
                                       + ART.Cent  
                                       | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noART)

# Vocab x Ambiguity interaction
AOICohcue.GazeDur.noVocabAmb <- lmer(logGazeDur ~ 1 + 
                                           Ambiguity.code  +
                                           Vocab.Cent +
                                           ART.Cent +
                                           #Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1
                                            + Vocab.Cent 
                                            + ART.Cent  
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.GazeDur.noARTAmb <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         #Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1
                                          + Vocab.Cent 
                                          + ART.Cent  
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOICohcue.GazeDur.max, AOICohcue.GazeDur.noARTAmb)

# re-run maximal model with reml
AOICohcue.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                     Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL) +
                                     (1
                                      + Vocab.Cent 
                                      + ART.Cent  
                                      | item),
                                   data = Data.CorrTrials,
                                   REML=TRUE)
# call model summary
summary(AOICohcue.GazeDur.REML)


#### _regressions out ####
# Because not all the necessary models converge, remove correlations between 
# random effects for all models for the regressions out measure:
AOICohcue.RegrOut.reduced <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                 Ambiguity.code  +
                                 Vocab.Cent +
                                 ART.Cent +
                                 Ambiguity.code : Vocab.Cent +
                                 Ambiguity.code : ART.Cent +
                                   (1 | RECORDING_SESSION_LABEL) +
                                   (0  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1
                                    | item) +
                                   (0
                                    + Vocab.Cent  
                                    | item)  +
                                   (0
                                    + ART.Cent  
                                    | item),
                               data = Data.CorrTrials,
                               family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOICohcue.RegrOut.reduced)

# main effect of Ambiguity
AOICohcue.RegrOut.noAmb  <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                   #  Ambiguity.code  +
                                     Vocab.Cent +
                                     ART.Cent +
                                     Ambiguity.code : Vocab.Cent +
                                     Ambiguity.code : ART.Cent +
                                     (1 | RECORDING_SESSION_LABEL) +
                                     (0  
                                      + Ambiguity.code  
                                      | RECORDING_SESSION_LABEL) +
                                     (1
                                      | item) +
                                     (0
                                      + Vocab.Cent  
                                      | item)  +
                                     (0
                                      + ART.Cent  
                                      | item),
                                   data = Data.CorrTrials,
                                   family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.reduced, AOICohcue.RegrOut.noAmb)

# main effect of Vocab
AOICohcue.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                          Ambiguity.code  +
                                          #Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 | RECORDING_SESSION_LABEL) +
                                          (0  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1
                                           | item) +
                                          (0
                                           + Vocab.Cent  
                                           | item)  +
                                          (0
                                           + ART.Cent  
                                           | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.reduced, AOICohcue.RegrOut.noVocab)

# main effect of Print exposure
AOICohcue.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             #ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1 | RECORDING_SESSION_LABEL) +
                                             (0  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1
                                              | item) +
                                             (0
                                              + Vocab.Cent  
                                              | item)  +
                                             (0
                                              + ART.Cent  
                                              | item),
                                           data = Data.CorrTrials,
                                           family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.reduced, AOICohcue.RegrOut.noART)

# Vocab x Ambiguity interaction
AOICohcue.RegrOut.noVocabAmb  <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                           Ambiguity.code  +
                                           Vocab.Cent +
                                           ART.Cent +
                                           #Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1 | RECORDING_SESSION_LABEL) +
                                           (0  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1
                                            | item) +
                                           (0
                                            + Vocab.Cent  
                                            | item)  +
                                           (0
                                            + ART.Cent  
                                            | item),
                                         data = Data.CorrTrials,
                                         family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.reduced, AOICohcue.RegrOut.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.RegrOut.noARTAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                                Ambiguity.code  +
                                                Vocab.Cent +
                                                ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                #Ambiguity.code : ART.Cent +
                                                (1 | RECORDING_SESSION_LABEL) +
                                                (0  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                                (1
                                                 | item) +
                                                (0
                                                 + Vocab.Cent  
                                                 | item)  +
                                                (0
                                                 + ART.Cent  
                                                 | item),
                                              data = Data.CorrTrials,
                                              family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.reduced, AOICohcue.RegrOut.noARTAmb)


#### __ pairwise comparison: regressions out in CohAmb vs AnomUA trials ####

        ### ____ CohAmb trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Coherence cue Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="4")

# subset only rows for CohAmb
Data.CorrTrials <- subset(Data.CorrTrials, cond_label =="PlausAmb")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# Construct a maximal glmer() model
AOICohcue.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                 Vocab.Cent +
                                 ART.Cent +
                                 (1 
                                  | RECORDING_SESSION_LABEL) +
                                 (1   +
                                    Vocab.Cent +
                                    ART.Cent| item),
                               data = Data.CorrTrials,
                               family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOICohcue.RegrOut.max)

# main effect of Vocab
AOICohcue.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                  #  Vocab.Cent +
                                    ART.Cent +
                                    (1 
                                     | RECORDING_SESSION_LABEL) +
                                    (1   +
                                       Vocab.Cent +
                                       ART.Cent| item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noVocab)

# main effect of Print exposure
AOICohcue.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                   Vocab.Cent +
                                #   ART.Cent +
                                   (1 
                                    | RECORDING_SESSION_LABEL) +
                                   (1   +
                                      Vocab.Cent +
                                      ART.Cent| item),
                                 data = Data.CorrTrials,
                                 family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noART)



      ### ____ AnomUA trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Coherence cue Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="4")

# subset only rows for AnomUA
Data.CorrTrials <- subset(Data.CorrTrials, cond_label =="AnomUA")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# Construct a maximal glmer() model
AOICohcue.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                 Vocab.Cent +
                                 ART.Cent +
                                 (1 
                                  | RECORDING_SESSION_LABEL) +
                                 (1   +
                                    Vocab.Cent +
                                    ART.Cent| item),
                               data = Data.CorrTrials,
                               family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOICohcue.RegrOut.max)

# main effect of Vocab
AOICohcue.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                     #  Vocab.Cent +
                                     ART.Cent +
                                     (1 
                                      | RECORDING_SESSION_LABEL) +
                                     (1   +
                                        Vocab.Cent +
                                        ART.Cent| item),
                                   data = Data.CorrTrials,
                                   family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noVocab)

# main effect of Print exposure
AOICohcue.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                   Vocab.Cent +
                                   #   ART.Cent +
                                   (1 
                                    | RECORDING_SESSION_LABEL) +
                                   (1   +
                                      Vocab.Cent +
                                      ART.Cent| item),
                                 data = Data.CorrTrials,
                                 family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOICohcue.RegrOut.max, AOICohcue.RegrOut.noART)


#### _go-past time ####
# Construct a maximal lmer() model
AOICohcue.RegrPathDur.max <- lmer(logRegrPathDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1  
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1   
                                     + Vocab.Cent 
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)

# main effect of Ambiguity
AOICohcue.RegrPathDur.noAmb <- lmer(logRegrPathDur ~ 1 + 
                                          # Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1   
                                           + Vocab.Cent 
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noAmb)

# main effect of Vocab
AOICohcue.RegrPathDur.noVocab <- lmer(logRegrPathDur ~ 1 + 
                                            Ambiguity.code  +
                                            #Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1   
                                             + Vocab.Cent 
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noVocab)

# main effect of Print exposure
AOICohcue.RegrPathDur.noART <- lmer(logRegrPathDur ~ 1 + 
                                          Ambiguity.code  +
                                          Vocab.Cent +
                                          #  ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1  
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1   
                                           + Vocab.Cent 
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noART)

# Vocab x Ambiguity interaction
AOICohcue.RegrPathDur.noVocabAmb <- lmer(logRegrPathDur ~ 1 + 
                                               Ambiguity.code  +
                                               Vocab.Cent +
                                               ART.Cent +
                                               #Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1   
                                                + Vocab.Cent 
                                                + ART.Cent
                                                | item),
                                             data = Data.CorrTrials,
                                             REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.RegrPathDur.noARTAmb <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             #Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1   
                                              + Vocab.Cent 
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOICohcue.RegrPathDur.max, AOICohcue.RegrPathDur.noARTAmb)

# re-run maximal model with reml
AOICohcue.RegrPathDur.REML <- lmer(logRegrPathDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1   
                                          + Vocab.Cent 
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=TRUE)
# call model summary
summary(AOICohcue.RegrPathDur.REML)


#### _second pass reading time ####
# Because not all the necessary models converge, remove correlations between 
# random effects for all models for the regressions out measure:
AOICohcue.log2ndPassTime.reduced <- lmer(log2ndPassTime ~ 1 + 
                                           Ambiguity.code  +
                                           Vocab.Cent +
                                           ART.Cent +
                                           Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1 | RECORDING_SESSION_LABEL) +
                                           (0  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1
                                            | item) +
                                           (0
                                            + Vocab.Cent  
                                            | item)  +
                                           (0
                                            + ART.Cent  
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=FALSE)

# main effect of Ambiguity
AOICohcue.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                                 # Ambiguity.code  +
                                                 Vocab.Cent +
                                                 ART.Cent +
                                                 Ambiguity.code : Vocab.Cent +
                                                 Ambiguity.code : ART.Cent +
                                                 (1 | RECORDING_SESSION_LABEL) +
                                                 (0  
                                                  + Ambiguity.code  
                                                  | RECORDING_SESSION_LABEL) +
                                                 (1
                                                  | item) +
                                                 (0
                                                  + Vocab.Cent  
                                                  | item)  +
                                                 (0
                                                  + ART.Cent  
                                                  | item),
                                               data = Data.CorrTrials,
                                               REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.reduced, AOICohcue.log2ndPassTime.noAmb)

# main effect of Vocab
AOICohcue.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                                   Ambiguity.code  +
                                                   #   Vocab.Cent +
                                                   ART.Cent +
                                                   Ambiguity.code : Vocab.Cent +
                                                   Ambiguity.code : ART.Cent +
                                                   (1 | RECORDING_SESSION_LABEL) +
                                                   (0  
                                                    + Ambiguity.code  
                                                    | RECORDING_SESSION_LABEL) +
                                                   (1
                                                    | item) +
                                                   (0
                                                    + Vocab.Cent  
                                                    | item)  +
                                                   (0
                                                    + ART.Cent  
                                                    | item),
                                                 data = Data.CorrTrials,
                                                 REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.reduced, AOICohcue.log2ndPassTime.noVocab)

# Print exposure
AOICohcue.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                                 Ambiguity.code  +
                                                 Vocab.Cent +
                                                 #     ART.Cent +
                                                 Ambiguity.code : Vocab.Cent +
                                                 Ambiguity.code : ART.Cent +
                                                 (1 | RECORDING_SESSION_LABEL) +
                                                 (0  
                                                  + Ambiguity.code  
                                                  | RECORDING_SESSION_LABEL) +
                                                 (1
                                                  | item) +
                                                 (0
                                                  + Vocab.Cent  
                                                  | item)  +
                                                 (0
                                                  + ART.Cent  
                                                  | item),
                                               data = Data.CorrTrials,
                                               REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.reduced, AOICohcue.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOICohcue.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                                      Ambiguity.code  +
                                                      Vocab.Cent +
                                                      ART.Cent +
                                                      # Ambiguity.code : Vocab.Cent +
                                                      Ambiguity.code : ART.Cent +
                                                      (1 | RECORDING_SESSION_LABEL) +
                                                      (0  
                                                       + Ambiguity.code  
                                                       | RECORDING_SESSION_LABEL) +
                                                      (1
                                                       | item) +
                                                      (0
                                                       + Vocab.Cent  
                                                       | item)  +
                                                      (0
                                                       + ART.Cent  
                                                       | item),
                                                    data = Data.CorrTrials,
                                                    REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.reduced, AOICohcue.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOICohcue.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                                    Ambiguity.code  +
                                                    Vocab.Cent +
                                                    ART.Cent +
                                                    Ambiguity.code : Vocab.Cent +
                                                    #  Ambiguity.code : ART.Cent +
                                                    (1 | RECORDING_SESSION_LABEL) +
                                                    (0  
                                                     + Ambiguity.code  
                                                     | RECORDING_SESSION_LABEL) +
                                                    (1
                                                     | item) +
                                                    (0
                                                     + Vocab.Cent  
                                                     | item)  +
                                                    (0
                                                     + ART.Cent  
                                                     | item),
                                                  data = Data.CorrTrials,
                                                  REML=FALSE)
# model comparison
anova(AOICohcue.log2ndPassTime.reduced, AOICohcue.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOICohcue.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                                Ambiguity.code  +
                                                Vocab.Cent +
                                                ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                Ambiguity.code : ART.Cent +
                                                (1 | RECORDING_SESSION_LABEL) +
                                                (0  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                                (1
                                                 | item) +
                                                (0
                                                 + Vocab.Cent  
                                                 | item)  +
                                                (0
                                                 + ART.Cent  
                                                 | item),
                                              data = Data.CorrTrials,
                                              REML=TRUE)
# call model summary
summary(AOICohcue.log2ndPassTime.REML)




####################################################
########### AREA OF INTEREST: SPILLOVER ############
####################################################

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# remove AnomAmb and CohUA trials to retain only CohAmb and AnomUA trials
Data.PlAmbAnomUAOnly <-subset(Data.ExpTrials, subset=(cond_label != "PlausUA"
                                                      & cond_label !="AnomAmb"))

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials,  IA_ID =="5")

# exclude the item "log" because Coherence and Spillover AOIs have not been defined properly for this item in Experiment Builder
Data.CorrTrials <- subset(Data.CorrTrials, !item_label =="log")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$logFirstFixDur <- log10(Data.CorrTrials$IA_FIRST_FIXATION_DURATION)
Data.CorrTrials$logGazeDur <- log10(Data.CorrTrials$IA_FIRST_RUN_DWELL_TIME)
Data.CorrTrials$logRegrPathDur <- log10(Data.CorrTrials$IA_REGRESSION_PATH_DURATION)
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)


#### _first-fixation duration ####
# Construct a maximal lmer() model
AOISpillover.FF.max <- lmer(logFirstFixDur ~ 1 + 
                              Ambiguity.code  +
                              Vocab.Cent +
                              ART.Cent +
                              Ambiguity.code : Vocab.Cent +
                              Ambiguity.code : ART.Cent +
                              (1 
                               + Ambiguity.code   
                               | RECORDING_SESSION_LABEL) +
                              (1  
                               + Vocab.Cent
                               + ART.Cent
                               | item),
                            data = Data.CorrTrials,
                            REML=FALSE)

# main effect of Ambiguity
AOISpillover.FF.noAmb <- lmer(logFirstFixDur ~ 1 + 
                                    #Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noAmb)

# main effect of Vocab
AOISpillover.FF.noVocab <- lmer(logFirstFixDur ~ 1 + 
                                      Ambiguity.code  +
                                      #Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1 
                                       + Ambiguity.code   
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noVocab)

# main effect of Print exposure
AOISpillover.FF.noART <- lmer(logFirstFixDur ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    #ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code   
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noART)

# Vocab x Ambiguity interaction
AOISpillover.FF.noVocabAmb <- lmer(logFirstFixDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         #Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1 
                                          + Ambiguity.code   
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.FF.noARTAmb <- lmer(logFirstFixDur ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       #Ambiguity.code : ART.Cent +
                                       (1 
                                        + Ambiguity.code   
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)
# model comparison
anova(AOISpillover.FF.max, AOISpillover.FF.noARTAmb)

# re-run maximal model with REML
AOISpillover.FF.REML <- lmer(logFirstFixDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1 
                                    + Ambiguity.code   
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=TRUE)
# call model summary
summary(AOISpillover.FF.REML)


#### _gaze duration ####
# Construct a maximal lmer() model
AOISpillover.GazeDur.max <- lmer(logGazeDur ~ 1 + 
                                   Ambiguity.code  +
                                   Vocab.Cent +
                                   ART.Cent +
                                   Ambiguity.code : Vocab.Cent +
                                   Ambiguity.code : ART.Cent +
                                   (1  
                                    + Ambiguity.code  
                                    | RECORDING_SESSION_LABEL) +
                                   (1  
                                    + Vocab.Cent
                                    + ART.Cent
                                    | item),
                                 data = Data.CorrTrials,
                                 REML=FALSE)

# main effect of Ambiguity
AOISpillover.GazeDur.noAmb <- lmer(logGazeDur ~ 1 + 
                                         #     Ambiguity.code  +
                                         Vocab.Cent +
                                         ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noAmb)

# main effect of Vocab
AOISpillover.GazeDur.noVocab <- lmer(logGazeDur ~ 1 + 
                                           Ambiguity.code  +
                                           #     Vocab.Cent +
                                           ART.Cent +
                                           Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent
                                            + ART.Cent
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noVocab)

# main effect of Print exposure
AOISpillover.GazeDur.noART <- lmer(logGazeDur ~ 1 + 
                                         Ambiguity.code  +
                                         Vocab.Cent +
                                         #ART.Cent +
                                         Ambiguity.code : Vocab.Cent +
                                         Ambiguity.code : ART.Cent +
                                         (1  
                                          + Ambiguity.code  
                                          | RECORDING_SESSION_LABEL) +
                                         (1  
                                          + Vocab.Cent
                                          + ART.Cent
                                          | item),
                                       data = Data.CorrTrials,
                                       REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noART)

# Vocab x Ambiguity interaction
AOISpillover.GazeDur.noVocabAmb <- lmer(logGazeDur ~ 1 + 
                                              Ambiguity.code  +
                                              Vocab.Cent +
                                              ART.Cent +
                                              #Ambiguity.code : Vocab.Cent +
                                              Ambiguity.code : ART.Cent +
                                              (1  
                                               + Ambiguity.code  
                                               | RECORDING_SESSION_LABEL) +
                                              (1  
                                               + Vocab.Cent
                                               + ART.Cent
                                               | item),
                                            data = Data.CorrTrials,
                                            REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.GazeDur.noARTAmb <- lmer(logGazeDur ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            # Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOISpillover.GazeDur.max, AOISpillover.GazeDur.noARTAmb)

# re-run maxial model with reml
AOISpillover.GazeDur.REML <- lmer(logGazeDur ~ 1 + 
                                        Ambiguity.code  +
                                        Vocab.Cent +
                                        ART.Cent +
                                        Ambiguity.code : Vocab.Cent +
                                        Ambiguity.code : ART.Cent +
                                        (1  
                                         + Ambiguity.code  
                                         | RECORDING_SESSION_LABEL) +
                                        (1  
                                         + Vocab.Cent
                                         + ART.Cent
                                         | item),
                                      data = Data.CorrTrials,
                                      REML=TRUE)
# model comparison
summary(AOISpillover.GazeDur.REML)


#### _regressions out ####
# Construct a maximal glmer() model 
AOISpillover.RegrOut.max <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                    Ambiguity.code  +
                                    Vocab.Cent +
                                    ART.Cent +
                                    Ambiguity.code : Vocab.Cent +
                                    Ambiguity.code : ART.Cent +
                                    (1 
                                     + Ambiguity.code  
                                     | RECORDING_SESSION_LABEL) +
                                    (1  
                                     + Vocab.Cent
                                     + ART.Cent
                                     | item),
                                  data = Data.CorrTrials,
                                  family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# call model summary
summary(AOISpillover.RegrOut.max)

# main effect of Ambiguity
AOISpillover.RegrOut.noAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                          #  Ambiguity.code  +
                                          Vocab.Cent +
                                          ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noAmb)

# main effect of Vocab
AOISpillover.RegrOut.noVocab <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                            Ambiguity.code  +
                                            # Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1 
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noVocab)

# main effect of Print exposure
AOISpillover.RegrOut.noART <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                          Ambiguity.code  +
                                          Vocab.Cent +
                                          #ART.Cent +
                                          Ambiguity.code : Vocab.Cent +
                                          Ambiguity.code : ART.Cent +
                                          (1 
                                           + Ambiguity.code  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noART)

# Vocab x Ambiguity interaction
AOISpillover.RegrOut.noVocabAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                               Ambiguity.code  +
                                               Vocab.Cent +
                                               ART.Cent +
                                               #Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1 
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                             data = Data.CorrTrials,
                                             family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.RegrOut.noARTAmb <- glmer(IA_REGRESSION_OUT ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             #Ambiguity.code : ART.Cent +
                                             (1 
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           family = "binomial", control = glmerControl(optimizer ="bobyqa"))
# model comparison
anova(AOISpillover.RegrOut.max, AOISpillover.RegrOut.noARTAmb)


#### _go-past time ####
# Construct a maximal lmer() model
AOISpillover.RegrPathDur.max <- lmer(logRegrPathDur ~ 1 + 
                                       Ambiguity.code  +
                                       Vocab.Cent +
                                       ART.Cent +
                                       Ambiguity.code : Vocab.Cent +
                                       Ambiguity.code : ART.Cent +
                                       (1  
                                        + Ambiguity.code  
                                        | RECORDING_SESSION_LABEL) +
                                       (1  
                                        + Vocab.Cent
                                        + ART.Cent
                                        | item),
                                     data = Data.CorrTrials,
                                     REML=FALSE)

# main effect of Ambiguity
AOISpillover.RegrPathDur.noAmb <- lmer(logRegrPathDur ~ 1 + 
                                             #Ambiguity.code  +
                                             Vocab.Cent +
                                             ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noAmb)

# main effect of Vocab
AOISpillover.RegrPathDur.noVocab <- lmer(logRegrPathDur ~ 1 + 
                                               Ambiguity.code  +
                                               #Vocab.Cent +
                                               ART.Cent +
                                               Ambiguity.code : Vocab.Cent +
                                               Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                             data = Data.CorrTrials,
                                             REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noVocab)

# main effect of Print exposure
AOISpillover.RegrPathDur.noART <- lmer(logRegrPathDur ~ 1 + 
                                             Ambiguity.code  +
                                             Vocab.Cent +
                                             #ART.Cent +
                                             Ambiguity.code : Vocab.Cent +
                                             Ambiguity.code : ART.Cent +
                                             (1  
                                              + Ambiguity.code  
                                              | RECORDING_SESSION_LABEL) +
                                             (1  
                                              + Vocab.Cent
                                              + ART.Cent
                                              | item),
                                           data = Data.CorrTrials,
                                           REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noART)

# Vocab x Ambiguity interaction
AOISpillover.RegrPathDur.noVocabAmb <- lmer(logRegrPathDur ~ 1 + 
                                                  Ambiguity.code  +
                                                  Vocab.Cent +
                                                  ART.Cent +
                                                  #Ambiguity.code : Vocab.Cent +
                                                  Ambiguity.code : ART.Cent +
                                                  (1  
                                                   + Ambiguity.code  
                                                   | RECORDING_SESSION_LABEL) +
                                                  (1  
                                                   + Vocab.Cent
                                                   + ART.Cent
                                                   | item),
                                                data = Data.CorrTrials,
                                                REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.RegrPathDur.noARTAmb <- lmer(logRegrPathDur ~ 1 + 
                                                Ambiguity.code  +
                                                Vocab.Cent +
                                                ART.Cent +
                                                Ambiguity.code : Vocab.Cent +
                                                #Ambiguity.code : ART.Cent +
                                                (1  
                                                 + Ambiguity.code  
                                                 | RECORDING_SESSION_LABEL) +
                                                (1  
                                                 + Vocab.Cent
                                                 + ART.Cent
                                                 | item),
                                              data = Data.CorrTrials,
                                              REML=FALSE)
# model comparison
anova(AOISpillover.RegrPathDur.max, AOISpillover.RegrPathDur.noARTAmb)

# re-run maximal model with reml
AOISpillover.RegrPathDur.REML <- lmer(logRegrPathDur ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=TRUE)
# call model summary
summary(AOISpillover.RegrPathDur.REML)


#### _second pass reading time ####
# Construct a maximal lmer() model
AOISpillover.log2ndPassTime.max <- lmer(log2ndPassTime ~ 1 + 
                                      Ambiguity.code  +
                                      Vocab.Cent +
                                      ART.Cent +
                                      Ambiguity.code : Vocab.Cent +
                                      Ambiguity.code : ART.Cent +
                                      (1  
                                       + Ambiguity.code  
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)

# main effect of Ambiguity
AOISpillover.log2ndPassTime.noAmb <- lmer(log2ndPassTime ~ 1 + 
                                            #Ambiguity.code  +
                                            Vocab.Cent +
                                            ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noAmb)

# main effect of Vocab
AOISpillover.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                              Ambiguity.code  +
                                              #Vocab.Cent +
                                              ART.Cent +
                                              Ambiguity.code : Vocab.Cent +
                                              Ambiguity.code : ART.Cent +
                                              (1  
                                               + Ambiguity.code  
                                               | RECORDING_SESSION_LABEL) +
                                              (1  
                                               + Vocab.Cent
                                               + ART.Cent
                                               | item),
                                            data = Data.CorrTrials,
                                            REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noVocab)

# main effect of Print exposure
AOISpillover.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                            Ambiguity.code  +
                                            Vocab.Cent +
                                            #ART.Cent +
                                            Ambiguity.code : Vocab.Cent +
                                            Ambiguity.code : ART.Cent +
                                            (1  
                                             + Ambiguity.code  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noART)

# Vocab x Ambiguity interaction
AOISpillover.log2ndPassTime.noVocabAmb <- lmer(log2ndPassTime ~ 1 + 
                                                 Ambiguity.code  +
                                                 Vocab.Cent +
                                                 ART.Cent +
                                                 #Ambiguity.code : Vocab.Cent +
                                                 Ambiguity.code : ART.Cent +
                                                 (1  
                                                  + Ambiguity.code  
                                                  | RECORDING_SESSION_LABEL) +
                                                 (1  
                                                  + Vocab.Cent
                                                  + ART.Cent
                                                  | item),
                                               data = Data.CorrTrials,
                                               REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noVocabAmb)

# Print exposure x Ambiguity interaction
AOISpillover.log2ndPassTime.noARTAmb <- lmer(log2ndPassTime ~ 1 + 
                                               Ambiguity.code  +
                                               Vocab.Cent +
                                               ART.Cent +
                                               Ambiguity.code : Vocab.Cent +
                                               #Ambiguity.code : ART.Cent +
                                               (1  
                                                + Ambiguity.code  
                                                | RECORDING_SESSION_LABEL) +
                                               (1  
                                                + Vocab.Cent
                                                + ART.Cent
                                                | item),
                                             data = Data.CorrTrials,
                                             REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noARTAmb)

# re-run maximal model with reml
AOISpillover.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                           Ambiguity.code  +
                                           Vocab.Cent +
                                           ART.Cent +
                                           Ambiguity.code : Vocab.Cent +
                                           Ambiguity.code : ART.Cent +
                                           (1  
                                            + Ambiguity.code  
                                            | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent
                                            + ART.Cent
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=TRUE)
# call model summary
summary(AOISpillover.log2ndPassTime.REML)


#### __ pairwise comparison: second-pass reading time in CohAmb vs AnomUA trials ####

       ### ____ CohAmb trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="5")

# subset only rows for CohAmb
Data.CorrTrials <- subset(Data.CorrTrials, cond_label =="PlausAmb")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)


# Construct a maximal lmer() model
AOISpillover.log2ndPassTime.max <- lmer(log2ndPassTime ~ 1 + 
                                      Vocab.Cent +
                                      ART.Cent +
                                      (1  
                                       | RECORDING_SESSION_LABEL) +
                                      (1  
                                       + Vocab.Cent
                                       + ART.Cent
                                       | item),
                                    data = Data.CorrTrials,
                                    REML=FALSE)

# main effect of Vocab
AOISpillover.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                            #  Vocab.Cent +
                                              ART.Cent +
                                              (1  
                                               | RECORDING_SESSION_LABEL) +
                                              (1  
                                               + Vocab.Cent
                                               + ART.Cent
                                               | item),
                                            data = Data.CorrTrials,
                                            REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noVocab)

# main effect of Print exposure
AOISpillover.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                            Vocab.Cent +
                                         #   ART.Cent +
                                            (1  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noART)

# re-run maximal model with reml
AOISpillover.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                           Vocab.Cent +
                                           ART.Cent +
                                           (1  
                                            | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent
                                            + ART.Cent
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=TRUE)
# test
summary(AOISpillover.log2ndPassTime.REML)


        #### ____ AnomUA trials ####

# read in the file that contains only experimental trials
Data.ExpTrials <- read.csv("~Data_ExpTrials.csv")

# subset correct trials only
Data.CorrTrials <- subset(Data.ExpTrials.CohOnly, accuracy =="1")

# subset only rows for Spillover Area of Interest (AOI)
Data.CorrTrials <- subset(Data.CorrTrials, IA_ID =="5")

# subset only rows for AnomUA
Data.CorrTrials <- subset(Data.CorrTrials, cond_label =="AnomUA")

# centre & scale individual differences variables
Data.CorrTrials$Vocab.Cent <- scale(Data.CorrTrials$Vocab,center=TRUE, scale=TRUE)
Data.CorrTrials$ART.Cent <- scale(Data.CorrTrials$ART,center=TRUE, scale=TRUE)

# log-transform relevant measures
Data.CorrTrials$log2ndPassTime <- log10(Data.CorrTrials$IA_SECOND_RUN_DWELL_TIME)


# Construct a maximal lmer() model
AOISpillover.log2ndPassTime.max <- lmer(log2ndPassTime ~ 1 + 
                                          Vocab.Cent +
                                          ART.Cent +
                                          (1  
                                           | RECORDING_SESSION_LABEL) +
                                          (1  
                                           + Vocab.Cent
                                           + ART.Cent
                                           | item),
                                        data = Data.CorrTrials,
                                        REML=FALSE)

# main effect of Vocab
AOISpillover.log2ndPassTime.noVocab <- lmer(log2ndPassTime ~ 1 + 
                                              #  Vocab.Cent +
                                              ART.Cent +
                                              (1  
                                               | RECORDING_SESSION_LABEL) +
                                              (1  
                                               + Vocab.Cent
                                               + ART.Cent
                                               | item),
                                            data = Data.CorrTrials,
                                            REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noVocab)

# main effect of Print exposure
AOISpillover.log2ndPassTime.noART <- lmer(log2ndPassTime ~ 1 + 
                                            Vocab.Cent +
                                            #   ART.Cent +
                                            (1  
                                             | RECORDING_SESSION_LABEL) +
                                            (1  
                                             + Vocab.Cent
                                             + ART.Cent
                                             | item),
                                          data = Data.CorrTrials,
                                          REML=FALSE)
# model comparison
anova(AOISpillover.log2ndPassTime.max, AOISpillover.log2ndPassTime.noART)

# re-run maximal model with reml
AOISpillover.log2ndPassTime.REML <- lmer(log2ndPassTime ~ 1 + 
                                           Vocab.Cent +
                                           ART.Cent +
                                           (1  
                                            | RECORDING_SESSION_LABEL) +
                                           (1  
                                            + Vocab.Cent
                                            + ART.Cent
                                            | item),
                                         data = Data.CorrTrials,
                                         REML=TRUE)
# test
summary(AOISpillover.log2ndPassTime.REML)

##################################################################################