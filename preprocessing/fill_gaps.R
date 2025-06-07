fill_gaps<-function(state, thresh) {
  
  state_bin<-state>=1
  state_diff<-state[2:length(state)]-state[1:length(state)-1]
  state_diff_bin<-state_bin[2:length(state)]-state_bin[1:length(state)-1]
  
  start_times<-which(state_diff==1)
  end_times<-which(state_diff==-1)
  if (length(end_times)<length(start_times)) {
    end_times<-c(end_times, length(state))
  } else if(length(start_times)<length(end_times)) {
    start_times<-c(1, start_times)
  }
  if (length(start_times)>1 & length(end_times)>1) {
    gaps<-start_times[2:length(start_times)]-end_times[1:length(end_times)-1]
    gaps_to_fill<-which(gaps<=thresh)
    if (length(gaps_to_fill)>0) {
      for (k in 1:length(gaps_to_fill)) {
        val_to_fill=state[(end_times[gaps_to_fill[k]])]
        state[(end_times[gaps_to_fill[k]]+1):start_times[gaps_to_fill[k]+1]]<-val_to_fill
      }
    }
  }
  return(state)
}