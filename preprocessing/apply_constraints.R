apply_constraints = function(subj_df) {
  
  # Locomotion mutually exclusive from everything but 'Lip.smack', 'Fear.grimace', 'Threat.face', 'Coo', 'Grunt, 'Bark', 'Approach', 'Displacement', 'Flee', 'Withdraw'
  locomotion_state<-subj_df$Locomotion>=1
  col_names<-colnames(subj_df)
  for(i in 1:length(col_names)) {
    col_name<-col_names[i]
    if(!(col_name %in% c('Locomotion','Lip.smack', 'Fear.grimace', 'Threat.face', 'Coo', 'Grunt', 'Bark', 'Approach', 'Displacement', 'Flee', 'Withdraw'))) {
      subj_df[[col_name]][which(locomotion_state)]<-0
    }
  }
  
  groom_object_state<-subj_df$GroomObject>=1
  
  # Adjust contact - not if animal is being groomed
  subj_df$Contact[which(groom_object_state)]<-0
  
  return(subj_df)
  
}
