library("lme4")
library("ggplot2")
library("lsmeans")
library("car")
library("doBy")

# Read gaze data
data_file<-'./data/all_facial_gestures_gaze_summary.csv'
data<-read.csv(file=data_file,header=TRUE,sep=',')

#Only consider trials where proportion onscreen is more than 0
n_trials_pre<-nrow(data)
data<- data[data$PropOnscreenAmbigOther>0,]
n_trials_post<-nrow(data)
print(paste0((n_trials_pre-n_trials_post),' trials removed where PropOnscreenAmbigOther==0'))


## Threat and LPS bias
# Remove outliers
sds<-2.5
print(paste0('Removing outliers >',sds,' deviations from the mean'))

# within subject and condition
data_filt<-data.frame()
subjects<-unique(data$Subject)
conditions<-unique(data$EmotionType)
for (i in 1:length(subjects)) {
  for (j in 1:length(conditions)) {
    subj_data<-data[data$Subject==subjects[i] & data$EmotionType==conditions[j],]
    mean_bias<-mean(subj_data$EmotionBiasOnscreenAmbigOther)
    sd_bias<-sd(subj_data$EmotionBiasOnscreenAmbigOther)
    outliers<-which(abs(subj_data$EmotionBiasOnscreenAmbigOther)>mean_bias+sds*sd_bias)
    if (length(outliers)==0) {
      subj_data_filt<-subj_data
    } else{
      subj_data_filt<-subj_data[-outliers, ]
    }
    data_filt<-rbind(data_filt, subj_data_filt)
  }
}
noutliers<-nrow(data)-nrow(data_filt)
print(paste0('Within subject and condition: ',noutliers,' trials removed'))

#Load rank data
rank_file<-'./data/rank.csv'
ranks<-read.csv(file=rank_file,header=TRUE,sep=',')
ranks$Cscore_m<-scale(ranks$score_m)

groups<-read.csv(file='./data/groups.csv',header=TRUE,sep=',')
data_filt$Group <- groups$group[match(data_filt$Subject, groups$subject)]

# Merge behavior and gaze data
names(ranks)[names(ranks) == "subject"] <- "Subject"
all_data<-merge(ranks,data_filt,by=c('Subject'))
all_data$Group<-as.factor(all_data$Group)

# Mixed model
lmer_model <- lmer(EmotionBiasOnscreenAmbigOther ~ EmotionType*Group+Cscore_m+(1|Subject), data = all_data, control = lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
shapiro.test(resid(lmer_model))
# Test for homoscedascity
leveneTest(residuals(lmer_model) ~ all_data$EmotionType*all_data$Group)

# Check coefficients
print(lmer_model)

results <- Anova(lmer_model, type = 3, test.statistic='F')
print(results)

print(emmeans(lmer_model,pairwise~Group, type='response'))
print(emmeans(lmer_model,pairwise~EmotionType, type='response'))

twow_pw1<-emmeans(lmer_model, pairwise~EmotionType|Group, adjust='tukey')
print(summary(twow_pw1)$contrasts)

twow_pw2<-lsmeans(lmer_model, pairwise~Group|EmotionType, adjust='tukey')
print(summary(twow_pw2)$contrasts)

dev.new()
summary_data_filt<-summaryBy(EmotionBiasOnscreenAmbigOther+PropEmotionalOnscreenAmbigOther+PropNeutralOnscreenAmbigOther~Subject+Group+EmotionType, data = data_filt, keep.names = TRUE)
g<-ggplot(data_filt, aes(x=EmotionType, y=EmotionBiasOnscreenAmbigOther, group=Subject, color=Group)) + 
  geom_point(aes(colour=Group), position=position_jitter(width=0.2))+
  geom_point(data=summary_data_filt, aes(x=EmotionType, y=EmotionBiasOnscreenAmbigOther, colour=Group), size=3)+
  geom_line(data=summary_data_filt, aes(x=EmotionType, y=EmotionBiasOnscreenAmbigOther, colour=Group, group=Subject), size=1)+
  geom_boxplot(data=summary_data_filt, aes(x=EmotionType, y=EmotionBiasOnscreenAmbigOther, group=EmotionType), colour='black', outlier.size=0)+
  scale_colour_brewer(palette='Set1')+
  facet_grid(~Group)+ylab('Emotion Bias')+xlab('Emotion Type')+theme_bw()
print(g)

