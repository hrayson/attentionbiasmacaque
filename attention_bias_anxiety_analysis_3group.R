library("lme4")
library("ggplot2")
library("lsmeans")
library("car")
library("doBy")
library('emmeans')
source('plot_fit.R')

# Read gaze data
gaze_data<-read.csv(file='./data/all_facial_gestures_gaze_summary.csv',header=TRUE,sep=',')

#Only consider trials where proportion onscreen is more than 0
n_trials_pre<-nrow(gaze_data)
gaze_data<- gaze_data[gaze_data$PropOnscreenAmbigOther>0,]
n_trials_post<-nrow(gaze_data)
print(paste0((n_trials_pre-n_trials_post),' gaze trials removed with proportion onscreen/ambigOther is 0'))

# Remove gaze outliers
gaze_sds<-2.5
gaze_data_filt<-data.frame()
subjects<-unique(gaze_data$Subject)
conditions<-unique(gaze_data$EmotionType)
for (i in 1:length(subjects)) {
  for (j in 1:length(conditions)) {
    subj_data<-gaze_data[gaze_data$Subject==subjects[i] & gaze_data$EmotionType==conditions[j],]
    mean_bias<-mean(subj_data$EmotionBiasOnscreenAmbigOther)
    sd_bias<-sd(subj_data$EmotionBiasOnscreenAmbigOther)
    outliers<-which(abs(subj_data$EmotionBiasOnscreenAmbigOther)>mean_bias+gaze_sds*sd_bias)
    if (length(outliers)==0) {
      subj_data_filt<-subj_data
    } else{
      subj_data_filt<-subj_data[-outliers, ]
    }
    gaze_data_filt<-rbind(gaze_data_filt, subj_data_filt)
  }
}
noutliers<-nrow(gaze_data)-nrow(gaze_data_filt)
print('')
print(paste0('Within subject and condition: ',noutliers,' removed from gaze_data'))

# Average over trials to get gaze bias
summary_gaze_data<-summaryBy(EmotionBias+EmotionBiasLeftRight+EmotionBiasOnscreen+EmotionBiasOnscreenAmbigOther~Subject+EmotionType, data = gaze_data_filt, keep.names = TRUE)
# Convert to wide to get gaze bias column for each expression type
wide_summary_gaze_data<-reshape(summary_gaze_data, idvar = "Subject", timevar = "EmotionType", direction = "wide")
wide_summary_gaze_data$CEmotionBiasOnscreenAmbigOther.threat<-scale(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat)[,1]
wide_summary_gaze_data$CEmotionBiasOnscreenAmbigOther.lps<-scale(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps)[,1]

#Load rank data
rank_file<-'./data/rank.csv'
ranks<-read.csv(file=rank_file,header=TRUE,sep=',')
# Merge behavior and gaze data
names(ranks)[names(ranks) == "subject"] <- "Subject"
ranks$Cscore_m<-scale(ranks$score_m)

sex_file<-'./data/juvenile_sex_ids.csv'
sex_data<-read.csv(file=sex_file,header=TRUE,sep=',')
sex_data$Sex<-as.factor(sex_data$Sex)

# Read observation data
#obs_data<-read.csv(file='./data/all_group-ind_obs_summary.csv',header=TRUE,sep=',')
obs_data<-read.csv(file='./data/behaviour/year1/all_group_obs_summary.csv',header=TRUE,sep=',')
# Remove weird extra column
obs_data = subset(obs_data, select = -c(X) )
# Comput self-directed duration and frequency
obs_data$SelfDirectFreq<-obs_data$Self.scratchFreq+obs_data$Self.groomFreq+obs_data$ShakingFreq+obs_data$YawnFreq
# Use only group observation
#obs_data<-obs_data[obs_data$ObsType=='group',]

# Analyze self-directed behaviour frequency
print('Self-directed frequency')

# Remove outliers - within subject
sds<-2.5
print(paste0('Removing outliers >',sds,' deviations from the mean'))

# Remove outliers from behavioural data
obs_data_filt<-data.frame()
subjects<-unique(obs_data$Subject)
for (i in 1:length(subjects)) {
  subj_data<-obs_data[obs_data$Subject==subjects[i],]
  mean_freq<-mean(subj_data$SelfDirectFreq)
  sd_freq<-sd(subj_data$SelfDirectFreq)
  outliers<-which(abs(subj_data$SelfDirectFreq)>mean_freq+sds*sd_freq)
  if (length(outliers)==0) {
    subj_data_filt<-subj_data
  } else{
    subj_data_filt<-subj_data[-outliers, ]
  }
  obs_data_filt<-rbind(obs_data_filt, subj_data_filt)
}
noutliers<-nrow(obs_data)-nrow(obs_data_filt)
print(paste0('Within subject: ',noutliers,' removed from behavior'))

# Merge behavior and gaze data
all_data_filt<-merge(wide_summary_gaze_data,obs_data_filt,by=c('Subject'),all.x = TRUE, all.y = TRUE)
all_data_filt<-merge(all_data_filt, ranks, by=c('Subject'),all.x = TRUE, all.y = TRUE)
all_data_filt$Subject<-as.factor(all_data_filt$Subject)
all_data_filt<-merge(all_data_filt,sex_data,by=c('Subject'))

groups<-read.csv(file='./data/groups_3.csv',header=TRUE,sep=',')
all_data_filt$Group <- groups$group[match(all_data_filt$Subject, groups$subject)]


#### THREAT BIAS AND ANXIETY FREQUENCY
# Just group observation data: Mixed model with group and threat bias as fixed effects
print('Group')
glmernb_group_model <- glmer.nb(SelfDirectFreq~Group*CEmotionBiasOnscreenAmbigOther.threat+Cscore_m+(1|Subject), data = all_data_filt, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))

# Check and compare the two models - they are the same
print(glmernb_group_model)

# Model results
glmernb_group_results<-Anova(glmernb_group_model, type = 3)
print(glmernb_group_results)

print(emmeans(glmernb_group_model,pairwise~Group, type='response'))
print(emtrends(glmernb_group_model,~Group,var="Cscore_m", type='response'))

# Group x AB interaction
pw<-emtrends(glmernb_group_model,~Group|Group,var="CEmotionBiasOnscreenAmbigOther.threat", type='response')
print(summary(pw, infer = c(TRUE, TRUE)))


# Plot self-directed duration x threat bias by group
summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther.threat+SelfDirectFreq~Subject+Group, data = all_data_filt, keep.names = TRUE)
group_pred<-plot_fit(glmernb_group_model,focal_var = "CEmotionBiasOnscreenAmbigOther.threat",inter_var = c("Group"),RE = "Subject")
group_pred$CEmotionBiasOnscreenAmbigOther.threat<-(group_pred$CEmotionBiasOnscreenAmbigOther.threat*sd(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat))+mean(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat)

dev.new()
g <- ggplot(all_data_filt)+
 geom_point(aes(x=EmotionBiasOnscreenAmbigOther.threat, y=SelfDirectFreq, colour=Group), alpha = 0.2) +
 geom_point(data=summary_data_filt, aes(x=EmotionBiasOnscreenAmbigOther.threat, y=SelfDirectFreq, colour=Group), size=3)+
 geom_line(aes(x=CEmotionBiasOnscreenAmbigOther.threat, y=Pred, colour=Group), data=group_pred, size=1) +
 geom_ribbon(aes(x=CEmotionBiasOnscreenAmbigOther.threat,y=Pred, fill=Group,ymin=LSE,ymax=USE, colour=Group), data=group_pred, alpha=.5)+
 scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


# LPS BIAS AND ANXIETY FREQUENCY
# Just group observation data: Mixed model with group and threat bias as fixed effects
print('Group')
glmernb_group_model <- glmer.nb(SelfDirectFreq~Group*CEmotionBiasOnscreenAmbigOther.lps+Cscore_m+(1|Subject), data = all_data_filt, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))

# Check and compare the two models - they are the same
print(glmernb_group_model)

# Model results
glmernb_group_results<-Anova(glmernb_group_model, type = 3)
print(glmernb_group_results)


print(emmeans(glmernb_group_model,pairwise~Group, type='response'))
print(emtrends(glmernb_group_model,~Group,var="Cscore_m", type='response'))


pw<-emtrends(glmernb_group_model,~Group|Group,var="CEmotionBiasOnscreenAmbigOther.lps")
print(summary(pw, infer = c(TRUE, TRUE)))

# Plot self-directed duration x threat bias by group
summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther.lps+SelfDirectFreq~Subject+Group, data = all_data_filt, keep.names = TRUE)
group_pred<-plot_fit(glmernb_group_model,focal_var = "CEmotionBiasOnscreenAmbigOther.lps",inter_var = c("Group"),RE = "Subject")
group_pred$CEmotionBiasOnscreenAmbigOther.lps<-(group_pred$CEmotionBiasOnscreenAmbigOther.lps*sd(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps))+mean(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps)

dev.new()
g <- ggplot(all_data_filt)+
 geom_point(aes(x=EmotionBiasOnscreenAmbigOther.lps, y=SelfDirectFreq, colour=Group), alpha = 0.2, position='jitter') +
 geom_point(data=summary_data_filt, aes(x=EmotionBiasOnscreenAmbigOther.lps, y=SelfDirectFreq, colour=Group), size=3)+
 geom_line(aes(x=CEmotionBiasOnscreenAmbigOther.lps, y=Pred, colour=Group), data=group_pred, size=1) +
 geom_ribbon(aes(x=CEmotionBiasOnscreenAmbigOther.lps,y=Pred, fill=Group,ymin=LSE,ymax=USE, colour=Group), data=group_pred, alpha=0.5)+
 scale_colour_brewer(palette='Set1')+theme_bw()
print(g)
