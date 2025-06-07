source('read.tcsv.R')
source('apply_constraints.R')
source('fill_gaps.R')
source('load_data.R')
library('doBy')

for(year in c(1)) {
  data_dir=paste0('./data/behaviour/year',year)
  
  groups<-read.csv(paste0('./data/groups.csv'))
  unique_subjects<-unique(groups$subject)
  
  all_subj_df<-data.frame()
  
  obj_states<-c("Contact", "Proximity", "Groom", "Rough.and.tumble.play",
                "Chase.play", "Huddle", "Mouthing.other", "Anogenital.exploration",
                "Aggression","Lip.smack","Threat.face","Fear.grimace")
  obj_events<-c("Coo", "Grunt", "Approach", "Present.groom", "Present.genital", "Mount",
                "Displacement", "Object.steal","Other.threat","Flee", "Flinch",
                "Withdraw", "Kiss", "Bark", "Stare", "Scream")
  
  state_change_behaviours=c('Groom', 'Rough.and.tumble.play', 'Chase.play', 'Huddle', 
                            'Mouthing.other', 'Anogenital.exploration', 'Aggression', 'Sleeping', 'Locomotion', 'Eat.drink',
                            'Object.play', 'Manual.oral.exploration', 'Hunching', 'Shaking', 'Self.scratch', 
                            'Self.groom', 'Self.mouth', 'Pace', 'Somersault', 'Bounce', 'Rocking', 'Spin', 'Swinging', 'Head.twist', 
                            'Floating.limb', 'Other.motor')
  
  duration<-600
  
  all_data<-load_data(paste0(data_dir,'/group_obs/'),groups,obj_states, obj_events)
  ss_all_data<-subset(all_data, select=-c(Subject,Group,Date,Time))
  st_idx<-which(colnames(ss_all_data)=='STATES')+1
  en_idx<-which(colnames(ss_all_data)=='EVENTS')-1
  states<-colnames(ss_all_data)[st_idx:en_idx]
  
  st_idx<-which(colnames(ss_all_data)=='EVENTS')+1
  en_idx=length(colnames(ss_all_data))
  events<-colnames(ss_all_data)[st_idx:en_idx]
  
  formula_parts<-c(paste0(states,'Dur'),
                   paste0(states,'Freq'),
                   paste0(states,'Prop'),
                   paste0(states,'Rate'),
                   paste0(events,'Freq'),
                   paste0(events,'Rate'))
  formula_str<-paste(formula_parts,collapse='+')
  
  all_data$Session<-paste(all_data$Date,all_data$Time,sep='_')
  
  sessions<-unique(all_data$Session)
  
  for (session in sessions) {
    session_parts<-unlist(strsplit(session,'_'))
    date<-session_parts[1]
    time<-session_parts[2]
    session_subjects<-unique(all_data$Subject[all_data$Session==session])
    
    for(subject in session_subjects) {
      
      group<-groups$group[groups$subject==subject]
    
      subj_df<-all_data[all_data$Subject==subject & all_data$Session==session,]
  
      # Initialize subject date frame  
      subj_data<-data.frame(Subject = subject, Group = group, Date = date, Time = time)
    
      not_visible<-which(subj_df$Not.visible>=1)
      time_not_visible<-length(not_visible)
      time_visible<-duration-time_not_visible
      
      for(state in states){
        # True whenever state is coded (false otherwise)
        state_bin<-subj_df[[state]]>=1
        
        # Sum wherever state is true to get total duration (in seconds)
        state_duration<-sum(state_bin)
        # Divide by number of rows to get proportion
        state_prop<-state_duration/time_visible
        # Find where going from non-state to state (will be 1), where state stays the same (will be 0),
        # and when going from state to non-state (will be -1)
        state_diff_bin<-state_bin[2:length(state_bin)]-state_bin[1:length(state_bin)-1]
        state_diff<-subj_df[[state]][2:length(state_bin)]-subj_df[[state]][1:length(state_bin)-1]
        # Because changing objects counts as two instances, first find all binary
        # state transitions, then search the remaining rows for changes of object
        state_freq<-sum(state_diff_bin==1)+sum(state_diff[which(state_diff_bin==0)]!=0)
        # Divide freq by number of rows to get rate
        state_rate<-state_freq/time_visible
        subj_data[paste0(state,'Dur')]<-state_duration
        subj_data[paste0(state,'Prop')]<-state_prop
        subj_data[paste0(state,'Freq')]<-state_freq
        subj_data[paste0(state,'Rate')]<-state_rate
      }
      
      for(event in events){
        # True whenever event is coded (false otherwise)
        event_bin<-subj_df[[event]]>=1
        
        # Sum wherever event is true to get total frequency
        event_freq<-sum(event_bin)
        # Divide freq by number of rows to get rate
        event_rate<-event_freq/time_visible
        
        subj_data[paste0(event,'Freq')]<-event_freq
        subj_data[paste0(event,'Rate')]<-event_rate
      }
      
      state_change_num<-0
      for (j in 1:length(state_change_behaviours)) {
        state_change_behaviour<-state_change_behaviours[j]
        # True whenever state is coded (false otherwise)
        state_bin<-subj_df[[state_change_behaviour]]>=1
        
        state_diff_bin<-state_bin[2:length(state_bin)]-state_bin[1:length(state_bin)-1]
        state_diff<-subj_df[[state]][2:length(state_bin)]-subj_df[[state]][1:length(state_bin)-1]
        # Because changing objects counts as two instances, first find all binary
        # state transitions, then search the remaining rows for changes of object
        state_freq<-sum(state_diff_bin==1)+sum(state_diff[which(state_diff_bin==0)]!=0)
        
        # Number of state changes
        state_change_num<-state_change_num + state_freq
      }
      subj_data$StateChanges<-state_change_num
    
      all_subj_df<- rbind(all_subj_df, subj_data)
    }
  }
  
  write.csv(all_subj_df, paste0(data_dir,'/','all_group_obs_summary.csv'))
  
  agg_all_subj_df<-summaryBy(formula(paste0(formula_str,'~Subject+Group')), data=all_subj_df, 
                             FUN=mean,keep.names = TRUE, na.rm=TRUE)
  write.csv(agg_all_subj_df, paste0(data_dir,'/','all_group_obs_summary_agg_mean.csv'))
  agg_all_subj_df<-summaryBy(formula(paste0(formula_str,'~Subject+Group')), data=all_subj_df,
                             FUN=sum, keep.names = TRUE, na.rm=TRUE)
  write.csv(agg_all_subj_df, paste0(data_dir,'/','all_group_obs_summary_agg_sum.csv'))
}