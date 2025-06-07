library(tibble)
add_extra_codes = function(subj_df) {
  
  groom_state<-subj_df$Groom>=1
  groom_object_state<-subj_df$GroomObject>=1
  contact_state<-subj_df$Contact>=1
  huddle_state<-subj_df$Huddle>=1
  proximity_state<-subj_df$Proximity>=1
  anogenital_state<-subj_df$Anogenital.exploration>=1
  aggression_state<-subj_df$Aggression>=1
  object_play_state<-subj_df$Object.play>=1
  pace_state<-subj_df$Pace>=1
  somersault_state<-subj_df$Somersault>=1
  bounce_state<-subj_df$Bounce>=1
  rocking_state<-subj_df$Rocking>=1
  spin_state<-subj_df$Spin>=1
  swing_state<-subj_df$Swinging>=1
  head_twist_state<-subj_df$Head.twist>=1
  floating_limb_state<-subj_df$Floating.limb>=1
  other_motor_state<-subj_df$Other.motor>=1
  locomotion_state<-subj_df$Locomotion>=1
  
  thresh<-5
  
  # Mutually exclusive contact - not when giving groom or huddle
  subj_df<-add_column(subj_df, ContactMutExcl=subj_df$Contact, .after='STATES')
  subj_df$ContactMutExcl[groom_state | huddle_state]<-0
  
  # Mutually exclusive huddle - not when giving or receiving groom
  subj_df<-add_column(subj_df, HuddleMutExcl=subj_df$Huddle, .after='STATES')
  subj_df$HuddleMutExcl[groom_state | groom_object_state]<-0
  
  # Mutually exclusive proximity - not when giving or receiving groom, huddle, or contact
  subj_df<-add_column(subj_df, ProximityMutExcl=subj_df$Proximity, .after='STATES')
  subj_df$ProximityMutExcl[groom_state | groom_object_state | huddle_state | contact_state]<-0
  
  # Total groom
  subj_df<-add_column(subj_df, TotalGroom=subj_df$Groom+subj_df$GroomObject, .after='STATES')
  
  # Total chase play
  subj_df<-add_column(subj_df, TotalChase.play=subj_df$Chase.play+subj_df$Chase.playObject, .after='STATES')
  
  # Total play
  subj_df<-add_column(subj_df, TotalPlay=subj_df$Rough.and.tumble.play+subj_df$TotalChase.play, .after='STATES')
  
  # Alone
  subj_df<-add_column(subj_df, AloneWithProximity=0, .after='STATES')
  total_groom_state<-subj_df$TotalGroom>=1
  total_play_state<-subj_df$TotalPlay>=1
  subj_df$AloneWithProximity[!(contact_state | total_groom_state | total_play_state | huddle_state | anogenital_state | aggression_state | locomotion_state)]<-1
  
  subj_df<-add_column(subj_df, AloneWithoutProximity=0, .after='STATES')
  subj_df$AloneWithoutProximity[!(contact_state | proximity_state | total_groom_state | total_play_state | huddle_state | anogenital_state | aggression_state | locomotion_state)]<-1
  
  # Exploration total
  subj_df<-add_column(subj_df, TotalExploration=subj_df$Manual.oral.exploration, .after='STATES')
  subj_df$TotalExploration[!subj_df$TotalExploration & object_play_state]<-1
  
  # Exclude hunching if not alone
  subj_df<-add_column(subj_df, HunchingNotAloneWithProximity=subj_df$Hunching, .after='STATES')
  alone_with_proximity_state<-subj_df$AloneWithProximity>=1
  subj_df$HunchingNotAloneWithProximity[!alone_with_proximity_state]<-0
  
  subj_df<-add_column(subj_df, HunchingNotAloneWithoutProximity=subj_df$Hunching, .after='STATES')
  alone_without_proximity_state<-subj_df$AloneWithoutProximity>=1
  subj_df$HunchingNotAloneWithoutProximity[!alone_without_proximity_state]<-0
  
  # Motor.stereotypy
  subj_df<-add_column(subj_df, Motor.stereotypy=0, .after='STATES')
  subj_df$Motor.stereotypy[pace_state | somersault_state | bounce_state | rocking_state | spin_state | swing_state | head_twist_state | floating_limb_state | other_motor_state]<-1
  
  # Total mount
  subj_df<-add_column(subj_df, TotalMount=subj_df$Mount+subj_df$MountObject, .after='EVENTS')
  
  # Frequency/Duration of give groom - counting as same bout if separated by threshold
  interrupted_groom_state<-fill_gaps(subj_df$Groom, thresh)
  subj_df<-add_column(subj_df, GroomInterrupted=interrupted_groom_state, .after='STATES')
  
  # Frequency/Duration of receive groom - counting as same bout if separated by threshold
  interrupted_groom_object_state<-fill_gaps(subj_df$GroomObject, thresh)
  subj_df<-add_column(subj_df, GroomObjectInterrupted=interrupted_groom_object_state, .after='STATES')
  
  # Frequency of give or receive groom (not counting overlap) - counting as same bout if separated
  # by threshold
  interrupted_total_groom_state<-fill_gaps(subj_df$Groom+subj_df$GroomObject, thresh)
  subj_df<-add_column(subj_df, TotalGroomInterrupted=interrupted_total_groom_state, .after='STATES')
  
  return(subj_df)
  
}
