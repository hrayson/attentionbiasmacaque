library("lme4")
library("ggplot2")
library("car")
library("doBy")
source('plot_fit.R')
library('emmeans')


# Read gaze data
gaze_data<-read.csv(file='./data/all_facial_gestures_gaze_summary.csv',header=TRUE,sep=',')

sex_file<-'./data/juvenile_sex_ids.csv'
sex_data<-read.csv(file=sex_file,header=TRUE,sep=',')
sex_data$Sex<-as.factor(sex_data$Sex)

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

# Read observation data
#obs_data<-read.csv(file='./data/all_group-ind_obs_summary.csv',header=TRUE,sep=',')
obs_data<-read.csv(file='./data/behaviour/year1/all_group_obs_summary.csv',header=TRUE,sep=',')
# Remove weird extra column
obs_data = subset(obs_data, select = -c(X) )


#####
# GROOM DURATION
#####
# Remove outliers - within subject
obs_sds<-2.5
print(paste0('Removing outliers >',obs_sds,' deviations from the mean'))

#### Total Groom
# Remove outliers from behavioural data
obs_data_filt<-data.frame()
subjects<-unique(obs_data$Subject)
for (i in 1:length(subjects)) {
  subj_data<-obs_data[obs_data$Subject==subjects[i],]
  mean_val<-mean(subj_data$TotalGroomInterruptedDur)
  sd_val<-sd(subj_data$TotalGroomInterruptedDur)
  outliers<-which(abs(subj_data$TotalGroomInterruptedDur)>mean_val+obs_sds*sd_val)
  if (length(outliers)==0) {
    subj_data_filt<-subj_data
  } else{
    subj_data_filt<-subj_data[-outliers, ]
  }
  obs_data_filt<-rbind(obs_data_filt, subj_data_filt)
}
noutliers<-nrow(obs_data)-nrow(obs_data_filt)
print('')
print(paste0('Within subject: ',noutliers,' removed from behavior'))

# Merge behavior and gaze data
all_data_filt<-merge(wide_summary_gaze_data,obs_data_filt,by=c('Subject'),all.x = TRUE, all.y = TRUE)
all_data_filt<-merge(all_data_filt,ranks,by=c('Subject'),all.x = TRUE, all.y = TRUE)
all_data_filt$Subject<-as.factor(all_data_filt$Subject)
all_data_filt<-merge(all_data_filt,sex_data,by=c('Subject'))

groups<-read.csv(file='./data/groups_3.csv',header=TRUE,sep=',')
all_data_filt$Group <- groups$group[match(all_data_filt$Subject, groups$subject)]


## THREAT - Total Groom
# Just group observation data: Mixed model with group and threat bias as fixed effects
print('Group')
glmernb_group_model <- glmer.nb(TotalGroomInterruptedDur ~ Group*CEmotionBiasOnscreenAmbigOther.threat+Cscore_m+(1|Subject), data = all_data_filt, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
print(glmernb_group_model)

glmer_results<-Anova(glmernb_group_model, type = 3)
print(glmer_results)

print(emmeans(glmernb_group_model,pairwise~Group, type='response'))
print(emtrends(glmernb_group_model,~Group,var="Cscore_m", type='response'))

pw<-emtrends(glmernb_group_model,~Group|Group,var="CEmotionBiasOnscreenAmbigOther.threat")
print(summary(pw, infer = c(TRUE, TRUE)))


summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther.threat+TotalGroomInterruptedDur~Subject+Group, data = all_data_filt, keep.names = TRUE)
group_pred<-plot_fit(glmernb_group_model,focal_var = "CEmotionBiasOnscreenAmbigOther.threat",inter_var = c("Group"),RE = "Subject")
group_pred$CEmotionBiasOnscreenAmbigOther.threat<-(group_pred$CEmotionBiasOnscreenAmbigOther.threat*sd(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat))+mean(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat)

dev.new()
g <- ggplot(all_data_filt)+
 geom_point(aes(x=EmotionBiasOnscreenAmbigOther.threat, y=TotalGroomInterruptedDur, colour=Group), alpha = 0.2, position='jitter') +
 geom_point(data=summary_data_filt, aes(x=EmotionBiasOnscreenAmbigOther.threat, y=TotalGroomInterruptedDur, colour=Group), size=3)+
 geom_line(aes(x=CEmotionBiasOnscreenAmbigOther.threat, y=Pred, colour=Group), data=group_pred, size=1) +
 geom_ribbon(aes(x=CEmotionBiasOnscreenAmbigOther.threat,y=Pred, fill=Group,ymin=LSE,ymax=USE, colour=Group), data=group_pred, alpha=0.5)+
 scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


# LPS- Total Groom
# Just group observation data: Mixed model with group and lps bias as fixed effects
print('Group')
glmernb_group_model <- glmer.nb(TotalGroomInterruptedDur ~ Group*CEmotionBiasOnscreenAmbigOther.lps+Cscore_m+(1|Subject), data = all_data_filt, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))

print(glmernb_group_model)

glmernb_results<-Anova(glmernb_group_model, type = 3)
print(glmernb_results)

print(emmeans(glmernb_group_model,pairwise~Group, type='response'))
print(emtrends(glmernb_group_model,~Group,var="Cscore_m", type='response'))

pw<-emtrends(glmernb_group_model,~Group|Group,var="CEmotionBiasOnscreenAmbigOther.lps")
print(summary(pw, infer = c(TRUE, TRUE)))

summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther.lps+TotalGroomInterruptedDur~Subject+Group, data = all_data_filt, keep.names = TRUE)
group_pred<-plot_fit(glmernb_group_model,focal_var = "CEmotionBiasOnscreenAmbigOther.lps",inter_var = c("Group"),RE = "Subject")
group_pred$CEmotionBiasOnscreenAmbigOther.lps<-(group_pred$CEmotionBiasOnscreenAmbigOther.lps*sd(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps))+mean(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps)

dev.new()
g <- ggplot(all_data_filt)+
 geom_point(aes(x=EmotionBiasOnscreenAmbigOther.lps, y=TotalGroomInterruptedDur, colour=Group), alpha = 0.2, position='jitter') +
 geom_point(data=summary_data_filt, aes(x=EmotionBiasOnscreenAmbigOther.lps, y=TotalGroomInterruptedDur, colour=Group), size=3)+
 geom_line(aes(x=CEmotionBiasOnscreenAmbigOther.lps, y=Pred, colour=Group), data=group_pred, size=1) +
 geom_ribbon(aes(x=CEmotionBiasOnscreenAmbigOther.lps,y=Pred, fill=Group,ymin=LSE,ymax=USE, colour=Group), data=group_pred, alpha=0.5)+
 scale_colour_brewer(palette='Set1')+theme_bw()
print(g)





## GROOM FREQUENCY
# Remove outliers - within subject
obs_sds<-2.5
print(paste0('Removing outliers >',obs_sds,' deviations from the mean'))


#### Total Groom
# Remove outliers from behavioural data
obs_data_filt<-data.frame()
subjects<-unique(obs_data$Subject)
for (i in 1:length(subjects)) {
  subj_data<-obs_data[obs_data$Subject==subjects[i],]
  mean_val<-mean(subj_data$TotalGroomInterruptedFreq)
  sd_val<-sd(subj_data$TotalGroomInterruptedFreq)
  outliers<-which(abs(subj_data$TotalGroomInterruptedFreq)>mean_val+obs_sds*sd_val)
  if (length(outliers)==0) {
    subj_data_filt<-subj_data
  } else{
    subj_data_filt<-subj_data[-outliers, ]
  }
  obs_data_filt<-rbind(obs_data_filt, subj_data_filt)
}
noutliers<-nrow(obs_data)-nrow(obs_data_filt)
print('')
print(paste0('Within subject: ',noutliers,' removed from behavior'))


# Merge behavior and gaze data
all_data_filt<-merge(wide_summary_gaze_data,obs_data_filt,by=c('Subject'),all.x = TRUE, all.y = TRUE)
all_data_filt<-merge(all_data_filt,ranks,by=c('Subject'),all.x = TRUE, all.y = TRUE)
all_data_filt$Subject<-as.factor(all_data_filt$Subject)
all_data_filt<-merge(all_data_filt,sex_data,by=c('Subject'))

groups<-read.csv(file='./data/groups.csv',header=TRUE,sep=',')
all_data_filt$Group <- groups$group[match(all_data_filt$Subject, groups$subject)]


# THREAT- Total Groom
# Just group observation data: Mixed model with group and threat bias as fixed effects
print('Group')
glmernb_group_model <- glmer.nb(TotalGroomInterruptedFreq ~ Group*CEmotionBiasOnscreenAmbigOther.threat+Cscore_m+(1|Subject), data = all_data_filt, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
print(glmernb_group_model)

glmernb_results<-Anova(glmernb_group_model, type = 3)
print(glmernb_results)

print(emmeans(glmernb_group_model,pairwise~Group, type='response'))
print(emtrends(glmernb_group_model,~Group,var="Cscore_m", type='response'))
pw<-emtrends(glmernb_group_model,~Group|Group,var="CEmotionBiasOnscreenAmbigOther.threat")
print(summary(pw, infer = c(TRUE, TRUE)))

summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther.threat+TotalGroomInterruptedFreq~Subject+Group, data = all_data_filt, keep.names = TRUE)
group_pred<-plot_fit(glmernb_group_model,focal_var = "CEmotionBiasOnscreenAmbigOther.threat",inter_var = c("Group"),RE = "Subject")
group_pred$CEmotionBiasOnscreenAmbigOther.threat<-(group_pred$CEmotionBiasOnscreenAmbigOther.threat*sd(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat))+mean(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.threat)

dev.new()
g <- ggplot(all_data_filt)+
 geom_point(aes(x=EmotionBiasOnscreenAmbigOther.threat, y=TotalGroomInterruptedFreq, colour=Group), alpha = 0.2, position='jitter') +
 geom_point(data=summary_data_filt, aes(x=EmotionBiasOnscreenAmbigOther.threat, y=TotalGroomInterruptedFreq, colour=Group), size=3)+
 geom_line(aes(x=CEmotionBiasOnscreenAmbigOther.threat, y=Pred, colour=Group), data=group_pred, size=1) +
 geom_ribbon(aes(x=CEmotionBiasOnscreenAmbigOther.threat,y=Pred, fill=Group,ymin=LSE,ymax=USE, colour=Group), data=group_pred, alpha=0.5)+
 scale_colour_brewer(palette='Set1')+theme_bw()
print(g)



#LPS- Total Groom
# Just group observation data: Mixed model with group and lps bias as fixed effects
print('Group')
glmernb_group_model <- glmer.nb(TotalGroomInterruptedFreq ~ Group*CEmotionBiasOnscreenAmbigOther.lps+Cscore_m+(1|Subject), data = all_data_filt, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
print(glmernb_group_model)

glmernb_results<-Anova(glmernb_group_model, type = 3)
print(glmernb_results)

print(emmeans(glmernb_group_model,pairwise~Group, type='response'))
print(emtrends(glmernb_group_model,~Group,var="Cscore_m", type='response'))
pw<-emtrends(glmernb_group_model,~Group|Group,var="CEmotionBiasOnscreenAmbigOther.lps")
print(summary(pw, infer = c(TRUE, TRUE)))


summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther.lps+TotalGroomInterruptedFreq~Subject+Group, data = all_data_filt, keep.names = TRUE)
group_pred<-plot_fit(glmernb_group_model,focal_var = "CEmotionBiasOnscreenAmbigOther.lps",inter_var = c("Group"),RE = "Subject")
group_pred$CEmotionBiasOnscreenAmbigOther.lps<-(group_pred$CEmotionBiasOnscreenAmbigOther.lps*sd(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps))+mean(wide_summary_gaze_data$EmotionBiasOnscreenAmbigOther.lps)

dev.new()
g <- ggplot(all_data_filt)+
 geom_point(aes(x=EmotionBiasOnscreenAmbigOther.lps, y=TotalGroomInterruptedFreq, colour=Group), alpha = 0.2, position='jitter') +
 geom_point(data=summary_data_filt, aes(x=EmotionBiasOnscreenAmbigOther.lps, y=TotalGroomInterruptedFreq, colour=Group), size=3)+
 geom_line(aes(x=CEmotionBiasOnscreenAmbigOther.lps, y=Pred, colour=Group), data=group_pred, size=1) +
 geom_ribbon(aes(x=CEmotionBiasOnscreenAmbigOther.lps,y=Pred, fill=Group,ymin=LSE,ymax=USE, colour=Group), data=group_pred, alpha=0.5)+
 scale_colour_brewer(palette='Set1')+theme_bw()
print(g)
