# Keith Lohse
# Date: 2017-08-01

## Opening libraries -----------------------------------------------------------
library("ggplot2"); library("lme4"); library("lmerTest");library("dplyr");
library("car")

# if you do not already have these libraries installed, you will need to run:
# install.packages("ggplot2"); install.packages("lme4"); install.packages("lmerTest");
# install.packages("dplyr"); install.packages("car")

## Set working Directory ------------------------------------------------
getwd()

setwd("C:/Otter/Goblin Study/Behavioral/processed_data/")
getwd()

# let's see what is in the data folder
list.files()


##  Create a new smaller dataframe, REST, PRE, PRAC ----------------------------
DATA <- read.csv("./data_MASTER.csv", 
                 header = TRUE, sep=",",na.strings=c("","NA"))
head(DATA)

DATA$trial.c<-(DATA$trial-mean(DATA$trial))/10 #1-unit change = 10 trials
DATA$stim_cat<-factor(DATA$stim_cat)
DATA$resp_cat<-factor(DATA$resp_cat)
DATA$next_resp_same_cat<-factor(DATA$next_resp_same_cat)
DATA$prev_resp_same_cat<-factor(DATA$prev_resp_same_cat)

# Removing NAs for missed responses (1 exclusion)
DAT2<-subset(DATA, correct != "NA")
# Removing NAs for artifact rejection in the RewP (92 exclusions)
DAT2<-subset(DAT2, RewP != "NA")

## Removing statistical outliers in the data -----------------------------------
# Removing RewP Outliers ----
summary(DAT2$RewP)
sd(DAT2$RewP)
quantile(DAT2$RewP, c(0.0005,0.50,0.9995))
plot(density(DAT2$RewP))

DAT2<-subset(DAT2, RewP >= -50)
DAT2<-subset(DAT2, RewP <= 50)
plot(density(DAT2$RewP))


# Removing Encoding SPN Outliers ----
summary(DATA$stim_SPN)
sd(DATA$stim_SPN, na.rm=TRUE)
plot(density(DAT2$stim_SPN, na.rm=TRUE))
quantile(DATA$stim_SPN, c(0.0005,0.50,0.9995), na.rm=TRUE)

DAT2<-subset(DAT2, stim_SPN >= -130)
DAT2<-subset(DAT2, stim_SPN <= 130)
plot(density(DAT2$stim_SPN, na.rm=TRUE))


# Removing Feedback SPN Outliers ----
summary(DATA$fdbk_SPN)
sd(DATA$fdbk_SPN, na.rm=TRUE)
plot(density(DATA$fdbk_SPN, na.rm=TRUE))
quantile(DATA$fdbk_SPN, c(0.0005,0.50,0.9995), na.rm=TRUE)

DAT2<-subset(DAT2, fdbk_SPN >= -196)
DAT2<-subset(DAT2, fdbk_SPN <= 196)

plot(density(DAT2$RewP, na.rm=TRUE))
plot(density(DAT2$stim_SPN, na.rm=TRUE))
plot(density(DAT2$fdbk_SPN, na.rm=TRUE))


## Merging the acquistion data with the learning data --------------------------
LEARN <- read.csv("./data_POST_AVE.csv", 
                 header = TRUE, sep=",",na.strings=c("","NA"))
head(LEARN)
# selecting the subset of columns we want (ignoring individual categories for now)
head(LEARN[c(1,2,8,14,15,21,27,28,29,30,36:46)])

LEARN<-LEARN[c(1,2,8,14,15,21,27,28,29,30,36:46)]

COMB <- merge(DAT2, LEARN, by ="subID")
head(COMB)

## -------------------------- Overall Analyses ---------------------------------
## Primary Analysis: Effects on Learning ---------------------------------------
POST <- read.csv("./data_POST_LONG.csv", 
                  header = TRUE, sep=",",na.strings=c("","NA"))
head(POST)
POST$sterile<-(as.numeric(POST$acq_cond)-1.5)*2
summary(as.factor(POST$sterile))  

m01<-lmer(rt_correct~
            # Fixed-effects
            1+
            sterile*retention*rotated+ # adding in interaction
            # Random-effects
            (1|subID)+(1|retention:subID)+(1|rotated:subID), data=POST, REML=FALSE)
Anova(m01, type="III")
summary(m01)



## Figure XA: Number Correct as a Function of Group, Condition, and Rotation -----
labels <- c(game = "Game Group", sterile = "Sterile Group")

ggplot(data = POST, 
       mapping = aes(x = post_cond, y = num_correct)) +
  geom_jitter(aes(fill=as.factor(rotated)), position=position_jitterdodge(dodge.width=0.75), 
              pch=21, size=3, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(aes(fill=as.factor(rotated)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  facet_wrap(~acq_cond, labeller=labeller(acq_cond=labels))+
  scale_x_discrete(name = "Post-Test Condition") +
  scale_y_continuous(name = "Number Correct")+
  scale_fill_manual(name="Stimulus Orientation",
                      breaks=c("-1", "1"),
                      labels=c("Original", "Rotated"),
                      values = c("grey20", "white"))+
    theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")







## Primary Analysis: Response Time on Correct Trials ---------------------------
m01<-lmer(md_crt_time~
            # Fixed-effects
            1+
            sterile*retention*rotated+ # adding in interaction
            # Random-effects
            (1|subID)+(1|retention:subID)+(1|rotated:subID), data=POST, REML=FALSE)

Anova(m01, type = "III")
summary(m01)



## Figure XA: Median Correct Response Time as a Function of Group, Condition, and Rotation -----
labels <- c(game = "Game Group", sterile = "Sterile Group")

ggplot(data = POST, 
       mapping = aes(x = post_cond, y = md_crt_time)) +
  geom_jitter(aes(fill=as.factor(rotated)), position=position_jitterdodge(dodge.width=0.75), 
              pch=21, size=3, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(aes(fill=as.factor(rotated)), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  facet_wrap(~acq_cond, labeller=labeller(acq_cond=labels))+
  scale_x_discrete(name = "Post-Test Condition") +
  scale_y_continuous(name = "Median Response Time on\n Correct Trials (s)")+
  scale_fill_manual(name="Stimulus Orientation",
                    breaks=c("-1", "1"),
                    labels=c("Original", "Rotated"),
                    values = c("grey20", "white"))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")


## Primary Analysis: RewP -------------------------------------------------------
head(POST)
AVE<-subset(POST, post_cond=="game" & rotated=="-1")
AVE<-subset(AVE, RewP_diff_Windows!="NA")
summary(AVE$RewP_diff_Windows)
AVE<-subset(AVE, enc_SPN!="NA")
AVE<-subset(AVE, fb_SPN!="NA")

head(AVE)
m02<-lm(RewP_diff_Windows~acq_cond, data=AVE)
summary(m02)
anova(m02)
t.test(RewP_diff_Windows~acq_cond, data=AVE, paired=FALSE, var.equal=TRUE)

head(LEARN)
LEARN$rt_correct<-sqrt(LEARN$post_total)
m02b<-(lm(rt_correct~acq_cond+RewP_ave+acq_total, data=LEARN))
summary(m02b)
plot(m02b)

## Figure XB: Average RewP Difference as a Function of Group --------------------
ggplot(data = AVE, 
       mapping = aes(x = acq_cond, y = RewP_diff_Windows)) +
  geom_jitter(aes(fill=acq_cond), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=3, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(aes(fill=acq_cond), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  scale_fill_manual(values = c("grey20", "white"))+
  scale_x_discrete(name = "Acquisition Group") +
  scale_y_continuous(name = expression("Average RewP (Correct - Incorrect; "*mu*"V)"))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")


## Primary Analysis: eSPN -------------------------------------------------------
head(AVE)
m03<-lm(enc_SPN~acq_cond, data=AVE)
summary(m03)
t.test(enc_SPN~acq_cond, data=AVE ,var.equal=TRUE)

head(LEARN)
m03b<-(lm(rt_correct~acq_cond+enc_SPN+acq_total, data=LEARN))
summary(m03b)
plot(m03b)

## Figure XC: Average eSPN as a Function of Group ------------------------------
ggplot(data = AVE, 
       mapping = aes(x = acq_cond, y = enc_SPN)) +
  geom_jitter(aes(fill=acq_cond), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=3, stroke=1, col="black", alpha = .7) + 
  geom_boxplot(aes(fill=acq_cond), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  scale_fill_manual(values = c("grey20", "white"))+
  scale_x_discrete(name = "Acquisition Group") +
  scale_y_continuous(name = expression("Average Encoding SPN ( "*mu*"V)"), limits=c(-30,5))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")


## Primary Analysis: fSPN ------------------------------------------------------
head(AVE)
m04<-lm(fb_SPN~acq_cond, data=AVE)
summary(m04)
t.test(fb_SPN~acq_cond, data=AVE ,var.equal=TRUE)

head(LEARN)
m04b<-(lm(rt_correct~acq_cond+fb_SPN+acq_total, data=LEARN))
summary(m04b)
plot(m04b)


## Figure XB: Average fSPN as a Function of Group ------------------------------
ggplot(data = AVE, 
       mapping = aes(x = acq_cond, y = fb_SPN)) +
  geom_jitter(aes(fill=acq_cond), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=3, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(aes(fill=acq_cond), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  scale_fill_manual(values = c("grey20", "white"))+
  scale_x_discrete(name = "Acquisition Group") +
  scale_y_continuous(name = expression("Average Feedback SPN ( "*mu*"V)"), limits = c(-30,5))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")


## Primary Analysis: Engagement --------------------------------------------------
head(AVE)
m05<-lm(eng_total~acq_cond, data=AVE)
summary(m05)
t.test(eng_total~acq_cond, data=AVE ,var.equal=TRUE)

head(LEARN)
m05b<-(lm(rt_correct~acq_cond+eng_total+acq_total, data=LEARN))
summary(m05b)
plot(m05b)





## ------------------------ Single Trial Analyses -------------------------------
## Improvement Over Time During Practice ----------------------------------------
## Does accuracy improve over time?
head(COMB)
COMB$hit_binom<-(COMB$hit+1)/2
COMB$sterile.c<-(as.numeric(COMB$acq_cond)-1.5)*2
summary(as.factor(COMB$sterile))
summary(COMB$trial.c)

contrasts(COMB$stim_cat)<-contr.poly(5)

p00<-glmer(hit_binom~
             # Fixed-effects
             1+ 
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=COMB, family = binomial)

p01<-glmer(hit_binom~
             # Fixed-effects
             1+sterile.c+stim_cat+trial.c+ # adding in interaction
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=COMB, family = binomial)

p02<-glmer(hit_binom~
             # Fixed-effects
             1+sterile.c+stim_cat*trial.c+ # adding in interaction
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=COMB, family = binomial)


p03<-glmer(hit_binom~
             # Fixed-effects
             1+sterile.c*trial.c+stim_cat*trial.c+ # adding in interaction
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=COMB, family = binomial)

p04<-glmer(hit_binom~
             # Fixed-effects
             1+sterile.c*stim_cat*trial.c+
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=COMB, family = binomial)

anova(p00,p01,p02,p03,p04)
AIC(logLik(p03))-AIC(logLik(p04))
summary(p04)

Anova(p04, type=c("III"))  

## Follow-Up Analyses for the three-way interaction:
head(COMB)
GAME<-subset(COMB, acq_cond == "game")
STERILE <- subset(COMB, acq_cond =="sterile")

# Analysis of the Game group only:
p04b<-glmer(hit_binom~
             # Fixed-effects
             1+stim_cat*trial.c+
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=GAME, family = binomial)
summary(p04b)
Anova(p04b, type=c("III"))  


# Analysis of the Sterile group only:
p04c<-glmer(hit_binom~
              # Fixed-effects
              1+stim_cat*trial.c+
              # Random-effects
              (1+trial.c|subID)+(1|subID:stim_cat), data=STERILE, family = binomial)
summary(p04c)
Anova(p04c, type=c("III"))  


## Figure YA: Plot of Accuracy over Time by Category ---------------------------
labels <- c(game = "Game Group", sterile = "Sterile Group")

ggplot(data = COMB, 
       mapping = aes(x = trial, y = hit_binom)) +
  geom_jitter(aes(fill=stim_cat), pch=21, size= 2, height=0.01) + 
  stat_smooth(aes(color=stim_cat, lty=stim_cat), lwd=1.5, method="lm", se=FALSE)+
  facet_wrap(~acq_cond, labeller=labeller(acq_cond=labels))+
  scale_x_continuous(name = "Trial Number") +
  scale_y_continuous(name = "Proportion Correct")+
  scale_linetype_manual(name="Stimulus Category",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5"),
                    values = c(1,2,4,5,6))+
  scale_fill_manual(name="Stimulus Category",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5"),
                    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))+
  scale_color_manual(name="Stimulus Category",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5"),
                    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")
## -----------------------------------------------------------------------------



## Sequential Effects on the RewP -----------------------------------------------
head(COMB)
SEQ <- subset(COMB, prev_resp_hit != "NA")
summary(as.factor(SEQ$sterile.c))  
summary(as.factor(SEQ$hit))  
summary(as.factor(SEQ$prev_resp_hit))  


s00<-lmer(RewP~
            # Fixed-effects
            1+
            # Random-effects
            (1+trial.c|subID)+(1|subID:stim_cat), data=SEQ, REML=FALSE)

s01<-lmer(RewP~
            # Fixed-effects
            1+
            trial.c*hit+ # adding in interaction
            # Random-effects
            (1+trial.c|subID)+(1|subID:stim_cat), data=SEQ, REML=FALSE)

s02<-lmer(RewP~
            # Fixed-effects
            1+sterile.c+
            trial.c*hit*prev_resp_hit+ # adding in interaction
            # Random-effects
            (1+trial.c|subID)+(1|subID:stim_cat), data=SEQ, REML=FALSE)

s03<-lmer(RewP~
            # Fixed-effects
            1+sterile.c*trial.c*hit*prev_resp_hit+ # adding in interaction
            # Random-effects
            (1+trial.c|subID)+(1|subID:stim_cat), data=SEQ, REML=FALSE)

anova(s00,s01,s02,s03)
AIC(logLik(s01))-AIC(logLik(s02))
summary(s02)

Anova(s02, type=c("III"))


## Figure 6A: Sequential Effect on RewP ----------------------------------------
head(COMB)
labels <- c(correct = "Previous Correct", incorrect = "Previous Incorrect")

ggplot(data = SEQ, 
       mapping = aes(x = correct, y = RewP)) +
  geom_jitter(aes(fill=correct), position=position_jitterdodge(dodge.width=1), 
              pch=21, size=3, alpha = .6) + 
  geom_boxplot(aes(fill=correct), alpha = .8, notch=FALSE, 
               col="black", lwd=1, outlier.shape=NA)+
  scale_fill_manual(values = c("grey40", "white"))+
  facet_wrap(~prev_resp_correct, labeller=labeller(prev_resp_correct=labels))+
  scale_x_discrete(name = "Current Response") +
  scale_y_continuous(name = expression("sFA ("*mu*"V)"), limits=c(-50,50))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")
## -----------------------------------------------------------------------------
## Figure 6B: RewP Over Time ---------------------------------------------------
head(COMB)
labels <- c(correct = "Previous Correct", incorrect = "Previous Incorrect")

ggplot(data = SEQ, 
       mapping = aes(x = trial, y = RewP)) +
  geom_point(aes(fill=correct), pch=21, size=3, alpha = .6) + 
  stat_smooth(aes(lty=correct), col="black", method="lm", lwd = 1.5, se=FALSE)+
  facet_wrap(~prev_resp_correct, labeller=labeller(prev_resp_correct=labels))+
  scale_x_continuous(name = "Trial Number") +
  scale_y_continuous(name = expression("sFA ("*mu*"V)"), limits=c(-50,50))+
  scale_linetype_manual(name="Current Response",
                        breaks=c("correct", "incorrect"),
                        labels=c("Correct", "Incorrect"),
                        values = c(1,4))+
  scale_fill_manual(name="Current Response",
                    breaks=c("correct", "incorrect"),
                    labels=c("Correct", "Incorrect"),
                    values = c("grey40","white"))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")
## -----------------------------------------------------------------------------





## Retaining or Changing Responses ---------------------------------------------
## Does RewP predict keeping the same response or changing your response
# the next time you see a stimulus of the same category:
NEXT<-subset(COMB, next_cat_change != "NA")
head(NEXT)
summary(NEXT$next_cat_change)
summary(NEXT$RewP)
NEXT$RewP.c<-NEXT$RewP-mean(NEXT$RewP)
summary(NEXT$RewP.c)
summary(as.factor(NEXT$hit))
summary(as.factor(NEXT$sterile.c))


n00<-glmer(next_cat_change~
             # Fixed-effects
             1+ 
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=NEXT, family = binomial)

n01<-glmer(next_cat_change~
             # Fixed-effects
             1+sterile.c+trial.c+RewP.c+ # adding in interaction
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=NEXT, family = binomial)

n02<-glmer(next_cat_change~
             # Fixed-effects
             1+sterile.c+trial.c+RewP.c+hit+ # adding in interaction
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=NEXT, family = binomial)

n03<-glmer(next_cat_change~
             # Fixed-effects
             1+sterile.c*trial.c*RewP.c*hit+ # adding in interaction
             # Random-effects
             (1+trial.c|subID)+(1|subID:stim_cat), data=NEXT, family = binomial)

anova(n00,n01,n02,n03)
AIC(logLik(n02))-AIC(logLik(n03))
summary(n03)
Anova(n03, type=c("III"))

## Figure 7A: Plot of Accuracy over Time by Category ---------------------------
labels <- c(game = "Game Group", sterile = "Sterile Group")

ggplot(data = NEXT, 
       mapping = aes(x = trial, y = next_cat_change)) +
  geom_jitter(aes(fill=stim_cat), pch=21, size= 2, height=0.01) + 
  stat_smooth(aes(col=stim_cat, lty=stim_cat), lwd=1.5, method="lm", se=FALSE)+
  facet_wrap(~acq_cond, labeller=labeller(acq_cond=labels))+
  scale_linetype_manual(name="Stimulus Category",
                        breaks=c("1", "2", "3", "4", "5"),
                        labels=c("Cat 1", "Cat 2", "Cat 3", "Cat4", "Cat 5"),
                        values = c(1,2,4,5,6))+
  scale_fill_manual(name="Stimulus Category",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Cat 1", "Cat 2", "Cat 3", "Cat4", "Cat 5"),
                    values = c("black", "grey20", "grey30", "grey40", "grey50"))+
  scale_color_manual(name="Stimulus Category",
                     breaks=c("1", "2", "3", "4", "5"),
                     labels=c("Cat 1", "Cat 2", "Cat 3", "Cat4", "Cat 5"),
                     values = c("black", "grey20", "grey30", "grey40", "grey50"))+
  scale_x_continuous(name = "Trial Number") +
  scale_y_continuous(name = "Proportion of Change")+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")

## Figure 7B: Switching by Trial and Accuracy ----------------------------------
labels <- c(game = "Game Group", sterile = "Sterile Group")

ggplot(data = NEXT, 
       mapping = aes(x = trial, y = next_cat_change)) +
  geom_point(aes(fill=correct), pch=21, size=3, alpha = .6) + 
  stat_smooth(aes(lty=correct, col=correct), method="lm", lwd = 1.5, se=FALSE)+
  facet_wrap(~acq_cond, labeller=labeller(acq_cond=labels))+
  scale_x_continuous(name = "Trial Number") +
  scale_y_continuous(name = "Proportion of Change")+
  scale_linetype_manual(name="Current Response",
                        breaks=c("correct", "incorrect"),
                        labels=c("Correct", "Incorrect"),
                        values = c(1,4))+
  scale_color_manual(name="Current Response",
                    breaks=c("correct", "incorrect"),
                    labels=c("Correct", "Incorrect"),
                    values = c("black","black"))+
  scale_fill_manual(name="Current Response",
                    breaks=c("correct", "incorrect"),
                    labels=c("Correct", "Incorrect"),
                    values = c("grey20","white"))+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")
## -----------------------------------------------------------------------------

## Figure 7B: Switching by RewP and Accuracy ----------------------------------
labels <- c(game = "Game Group", sterile = "Sterile Group")

head(NEXT)

ggplot(data = NEXT, 
       mapping = aes(x = RewP, y = next_cat_change)) +
  geom_point(aes(fill=correct), pch=21, size=3, alpha = .6) + 
  stat_smooth(aes(lty=correct, col=correct), method="lm", lwd = 1.5, se=FALSE)+
  facet_wrap(~acq_cond, labeller=labeller(acq_cond=labels))+
  scale_x_continuous(name = expression("Current sFA ("*mu*"V)")) +
  scale_y_continuous(name = "Proportion of Change")+
  scale_linetype_manual(name="Current Response",
                        breaks=c("correct", "incorrect"),
                        labels=c("Correct", "Incorrect"),
                        values = c(1,4))+
  scale_color_manual(name="Current Response",
                     breaks=c("correct", "incorrect"),
                     labels=c("Correct", "Incorrect"),
                     values = c("black","black"))+
  scale_fill_manual(name="Current Response",
                    breaks=c("correct", "incorrect"),
                    labels=c("Correct", "Incorrect"),
                    values = c("black","white"))+  
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")
## -----------------------------------------------------------------------------


## ------------------ RewP Simulations -----------------------------------------
makeseq = function(){return(sample(rep(1:5,16)))} #create random ordering of stimuli

learn = function(alpha,temp,sigmaR,sigmaRewP){
  w = matrix(-.6,5,5) #association weights, cat by response
  r = rep(0,80) #response on each trial
  delta = rep(0,80) #prediction error on each trial
  c = makeseq() #stimulus sequence -- actually just sequence of correct categories
  for(t in 1:80){ #loop trials
    r[t] = sample(5,1,prob=exp(w[c[t],]/temp)) #choose response by softmax
    delta[t] = ((r[t]==c[t])*2-1)*rnorm(1,1,sigmaR) - w[c[t],r[t]] #prediction error: reward as +/- N(0,sigmaR), minus expectation
    w[c[t],r[t]] = w[c[t],r[t]] + alpha*delta[t] #update association for current cat and response
  }
  delta = delta + rnorm(80,0,sigmaRewP) #add noise to observed delta
  return(cbind(c,r,delta)) #returns matrix of [cat,resp,delta] x trial
}

getprevcorr = function(cat,resp){ #determine whether previous response to current cat was correct
  prevcorr = rep(-1,length(cat))
  for(t in 2:length(cat)){ #loop trials
    if(any(cat[1:(t-1)]==cat[t])) { #skip if this is first instance of this category, leaving value as -1
      prevtrial = max(which(cat[1:(t-1)]==cat[t])) #most recent instance of this category
      prevcorr[t] = resp[prevtrial]==cat[prevtrial]
    }
  }
  return(prevcorr)
}

getnextswitch = function(cat,resp){ #determine whether next response to current cat switches from current response
  nextswitch = rep(-1,length(cat))
  for(t in 1:(length(cat)-1)){ #loop trials
    if(any(cat[(t+1):length(cat)]==cat[t])) { #skip if this is last instance of this category, leaving value as -1
      nexttrial = min(which(cat[(t+1):length(cat)]==cat[t])) + t #next instance of this category
      nextswitch[t] = resp[nexttrial]!=resp[t]
    }
  }
  return(nextswitch)
}

makeplots = function(meandelta,switchratebytrial,deltacorrswitch,alpha,temp,sigmaR,sigmaRewP){
  par(mfrow=c(2,2))
  ##sequential effects on delta
  meandeltatitle = 'Sequential Effects on Prediction Error\nCurrent response: correct=black, incorrect=red\nPrevious response: correct=solid, incorrect=dashed'
  plot(2:80,meandelta[2:80,'CC'],ylim=c(-1,2),ty='l',xlab='Trial',ylab='Mean Prediction Error',main=meandeltatitle,cex.main=.8)
  lines(2:80,meandelta[2:80,'IC'],lty=2)
  lines(2:80,meandelta[2:80,'CI'],col='red')
  lines(2:80,meandelta[2:80,'II'],lty=2,col='red')
  ##display parameter values
  plot.new()
  mtext(sprintf('alpha=%.2f\ntemp=%.2f\nsigmaR=%.2f\nsigmaRewP=%.2f',alpha,temp,sigmaR,sigmaRewP),side=1)
  ##switch probabilility by trial
  switchratebytrialtitle = 'Switch Probability by Trial\nCurrent response: correct=black, incorrect = red'
  plot(1:79,switchratebytrial[1:79,'C'],ylim=0:1,ty='l',xlab='Trial',ylab='Switch Proportion',main=switchratebytrialtitle,cex.main=.8)
  lines(1:79,switchratebytrial[1:79,'I'],col='red')
  ##switch probability by delta
  bins = 50 #number of bins for delta values in plot
  switchratebydeltatitle = 'Switch Probability by Prediction Error\nCurrent response: correct=black, incorrect = red'
  #switch probability following correct responses
  if(length(unique(deltacorrswitch[deltacorrswitch[,2]==1,1])) < bins){ #fewer unique delta values than bins; use unique values in plot
    vals = sort(unique(deltacorrswitch[deltacorrswitch[,2]==1,1])) #all unique values of delta following correct responses
    breaks = c(1.5*vals[1]-.5*vals[2],(vals[1:(length(vals)-1)]+vals[2:length(vals)])/2,1.5*vals[length(vals)]-.5*vals[length(vals)-1]) #breaks equally spaced between unique values
  }
  else {
    vals = sort(deltacorrswitch[deltacorrswitch[,2]==1,1]) #all individual values of delta following correct responses
    breaks = vals[c(1,length(vals)*(1:bins)/bins)] #breaks are all 1/bins quartiles
  }
  switchprobs = hist(deltacorrswitch[deltacorrswitch[,2]==1&deltacorrswitch[,3]==1,1],breaks=breaks,plot=F)$counts/hist(deltacorrswitch[deltacorrswitch[,2]==1,1],breaks=breaks,plot=F)$counts
  plot((breaks[1:(length(breaks)-1)]+breaks[2:length(breaks)])/2,switchprobs,xlab='Delta',ylab='Switch Proportion',main=switchratebydeltatitle,cex.main=.8,ty='l',xlim=range(deltacorrswitch[,1]),ylim=0:1)
  #switch probability following error responses
  if(length(unique(deltacorrswitch[deltacorrswitch[,2]==0,1])) < bins){ #fewer unique delta values than bins; use unique values in plot
    vals = sort(unique(deltacorrswitch[deltacorrswitch[,2]==0,1])) #all unique values of delta following error responses
    breaks = c(1.5*vals[1]-.5*vals[2],(vals[1:(length(vals)-1)]+vals[2:length(vals)])/2,1.5*vals[length(vals)]-.5*vals[length(vals)-1]) #breaks equally spaced between unique values
  }
  else {
    vals = sort(deltacorrswitch[deltacorrswitch[,2]==0,1]) #all individual values of delta following error responses
    breaks = vals[c(1,length(vals)*(1:bins)/bins)] #breaks are all 1/bins quartiles
  }
  switchprobs = hist(deltacorrswitch[deltacorrswitch[,2]==0&deltacorrswitch[,3]==1,1],breaks=breaks,plot=F)$counts/hist(deltacorrswitch[deltacorrswitch[,2]==0,1],breaks=breaks,plot=F)$counts
  lines((breaks[1:(length(breaks)-1)]+breaks[2:length(breaks)])/2,switchprobs,col='red')
}

learningcurve = function(n,alpha,temp,sigmaR,sigmaRewP){
  perf = rep(0,80) #mean correct by trial
  meandelta = matrix(0,80,4) #mean delta by trial number and trial type
  colnames(meandelta)=c('CC','IC','CI','II') #trial types, [previous instance, current trial], Correct or Incorrect
  meandeltacounts = matrix(0,80,4) #counts of trial types for meandelta, at each trial number
  switchratebytrial = matrix(0,80,2) #switch rate; trial number x current accuracy
  colnames(switchratebytrial) = c('C','I') #Correct or Incorrect
  switchcountsbytrial = matrix(0,80,2) #counts of trial types for switchratebytrial, at each trial number
  deltaresolution = .1
  switchratebydelta = matrix(0,4/deltaresolution,2)
  switchdeltas = numeric(0) #list of deltas on trials followed by switch
  staydeltas = numeric(0) #list of deltas on trials followed by nonswitch
  deltacorrswitch = matrix(0,n*75,3) #tracking delta, current-trial accuracy, and response switch over all trials
  for(i in 1:n) { #loop subjects
    data = learn(alpha,temp,sigmaR,sigmaRewP) #simulate subject; returns [cat resp delta] x trial
    currentcorr = (data[,1]==data[,2]) #whether correct on current trial
    perf = perf + currentcorr #add performance for this subject
    #sequential effects on delta
    prevcorr = getprevcorr(data[,1],data[,2]) #whether correct on previous instance of this cat; 0/1 or -1 for NA
    meandelta[prevcorr==1&currentcorr==1,'CC'] = meandelta[prevcorr==1&currentcorr==1,'CC'] + data[prevcorr==1&currentcorr==1,3] #add deltas on CC trials
    meandelta[prevcorr==0&currentcorr==1,'IC'] = meandelta[prevcorr==0&currentcorr==1,'IC'] + data[prevcorr==0&currentcorr==1,3] #add deltas on IC trials 
    meandelta[prevcorr==1&currentcorr==0,'CI'] = meandelta[prevcorr==1&currentcorr==0,'CI'] + data[prevcorr==1&currentcorr==0,3] #add deltas on CI trials
    meandelta[prevcorr==0&currentcorr==0,'II'] = meandelta[prevcorr==0&currentcorr==0,'II'] + data[prevcorr==0&currentcorr==0,3] #add deltas on II trials
    meandeltacounts = meandeltacounts + cbind(prevcorr==1&currentcorr==1, prevcorr==0&currentcorr==1, prevcorr==1&currentcorr==0, prevcorr==0&currentcorr==0)
    #switch probability
    nextswitch = getnextswitch(data[,1],data[,2])
    switchratebytrial[nextswitch==1&currentcorr==1,'C'] = switchratebytrial[nextswitch==1&currentcorr==1,'C'] + 1 #increment number of switches on all correct trials followed by a response switch
    switchratebytrial[nextswitch==1&currentcorr==0,'I'] = switchratebytrial[nextswitch==1&currentcorr==0,'I'] + 1 #increment number of switches on all error trials followed by a response switch
    switchcountsbytrial = switchcountsbytrial + cbind(nextswitch!=-1&currentcorr==1,nextswitch!=-1&currentcorr==0)
    switchdeltas = c(switchdeltas,data[nextswitch==1,3]) #append deltas on all trials followed by switch
    staydeltas = c(staydeltas,data[nextswitch==0,3]) #append deltas on all trials followed by nonswitch
    deltacorrswitch[75*(i-1)+1:75,] = cbind(data[,3],currentcorr,nextswitch)[nextswitch!=-1,]
  }
  perf = perf/n #convert to mean
  meandelta[2:80,] = meandelta[2:80,]/meandeltacounts[2:80,] #convert to mean; counts for trial 1 will always be zero
  switchratebytrial[1:79,] = switchratebytrial[1:79,]/switchcountsbytrial[1:79,] #convert to mean; counts for last trial will always be zero
  makeplots(meandelta,switchratebytrial,deltacorrswitch,alpha,temp,sigmaR,sigmaRewP)
  return(list(perf,meandelta,switchratebytrial,deltacorrswitch))
}

