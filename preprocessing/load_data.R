source('add_extra_codes.R')
source('apply_constraints.R')
source('read.tcsv.R')
library(tibble)
library(readxl)

load_data=function(data_dir, groups, obj_states, obj_events) {
  unique_subjects<-unique(groups$subject)
  
  all_subj_df<-data.frame()
  duration<-300
  
  filepaths <- Sys.glob(paste0(data_dir,"/*.xlsm"))
  for(filepath in filepaths) {
    path_parts=unlist(strsplit(filepath,'/'))
    filename=path_parts[length(path_parts)]
    filename_parts=unlist(strsplit(filename,"_"))
    subject<-filename_parts[1]
    date<-filename_parts[5]
    time<-filename_parts[6]
    date_parts=unlist(strsplit(date,'[.]'))
    if(as.numeric(date_parts[2])>6) {
      date<-paste0(date,'.18')
    } else {
      date<-paste0(date,'.19')
    }
    
    group<-groups$group[groups$subject==subject]
    
    subj_df <- read_excel(filepath, sheet = 'Actions')
    csv_fname<-paste0(data_dir,'/',paste0(filename_parts[1:length(filename_parts)-1], sep='_',collapse=''), '5min.csv')
    write.csv(subj_df, csv_fname, na = '', row.names = FALSE)
    
    print(csv_fname)
    # Open file using transposed read csv (skip first column)
    subj_df<-tryCatch({
      read.tcsv(csv_fname,skip=1)
    }, warning=function(w) {
      print(w)
    }, error=function(e) {
      print(e)
      print(csv_fname)
    })
    
    # Cutoff last rows (because read.tcsv adds some weird last rows with all NA)
    subj_df<-subj_df[1:duration,]
    
    subj_df<-subset(subj_df, select=-c(...2,X,X.1,Non.Social,X.2,Self.directed,X.3,Motor.stereotypies,X.4,Other,X.5,Affiliative.behaviour,X.6,Other.aggression,X.7,Fear.stress.behaviour,X.8,Self.directed.behaviour,X.9,Other.behaviours))
    
    behav_cols<-colnames(subset(subj_df,select=-c(STATES,EVENTS)))
    for (col in behav_cols) {
      # Convert to character and trim whitespace
      subj_df[[col]]<-as.character(subj_df[[col]])
      subj_df[[col]]<-trimws(subj_df[[col]])
      # Convert x's to 100s and empty cells to 0s. This way, when object is nothing, value is 100
      subj_df[[col]][subj_df[[col]]=='x']<-'100'
      subj_df[[col]][subj_df[[col]]=='']<-'0'
      # Convert to numeric
      subj_df[[col]]<-as.numeric(subj_df[[col]])
      # Set NA's to 0
      subj_df[[col]][is.na(subj_df[[col]])]<-0
    }
    
    subj_obj_df <- read_excel(filepath, sheet = 'Objects')
    for(col in colnames(subj_obj_df)) {
      subj_obj_df[[col]]<-as.character(subj_obj_df[[col]])
      subj_obj_df[[col]]<-trimws(subj_obj_df[[col]])
      # Convert x's to 1s and empty cells to 0s
      subj_obj_df[[col]][subj_obj_df[[col]]=='x']<-'1'
      subj_obj_df[[col]][subj_obj_df[[col]]=='']<-'0'
      # Convert to numeric
      subj_obj_df[[col]]<-as.numeric(subj_obj_df[[col]])
      # Set NA's to 0
      subj_obj_df[[col]][is.na(subj_obj_df[[col]])]<-0
    }
    for(obj_state in obj_states) {
      subj_df<-add_column(subj_df, !!(paste0(obj_state,'Monkey')):=subj_df[[obj_state]], .after='STATES')
      subj_df[[paste0(obj_state,'Monkey')]][subj_df[[paste0(obj_state,'Monkey')]]==100]<-0
      
      subj_df<-add_column(subj_df, !!(paste0(obj_state,'NoObj')):=0, .after='STATES')
      subj_df[[paste0(obj_state,'NoObj')]]<-0
      subj_df[[paste0(obj_state,'NoObj')]][subj_df[[obj_state]]==100]<-1
      
      # All cells where object is set
      nz_cells<-subj_df[[paste0(obj_state,'Monkey')]]>=1
      # For each unique action id
      unique_vals<-unique(subj_df[[paste0(obj_state,'Monkey')]][nz_cells])
      for(val in unique_vals) {
        cells<-which(subj_df[[obj_state]]==val)
        # Find row for action ID in column table
        row=which(subj_obj_df$`Action ID`==val)
        if(length(row)>0) {
          col=which(subj_obj_df[row,2:length(colnames(subj_obj_df))]==1)+1
          # If no object marked - no object
          if(length(col)==0) {
            subj_df[[paste0(obj_state,'Monkey')]][cells]<-0
            subj_df[[paste0(obj_state,'NoObj')]][cells]<-1
          }
          # Monkey marked
          else {
            other_subj<-colnames(subj_obj_df)[col]
            subj_df[[obj_state]][which(subj_df[[obj_state]]==val)]<-sum(which(unique_subjects %in% tolower(other_subj)))
            subj_df[[paste0(obj_state,'Monkey')]][which(subj_df[[obj_state]]==val)]<-sum(which(unique_subjects %in% tolower(other_subj)))
            subj_df[[paste0(obj_state,'NoObj')]][cells]<-0
          }
        }
        # No row in object sheet - no object
        else {
          subj_df[[paste0(obj_state,'Monkey')]][cells]<-0
          subj_df[[paste0(obj_state,'NoObj')]][cells]<-1
        }
      }
    }
    for(obj_event in obj_events) {
      subj_df<-add_column(subj_df, !!(paste0(obj_event,'Monkey')):=subj_df[[obj_event]], .after='EVENTS')
      subj_df[[paste0(obj_event,'Monkey')]][subj_df[[paste0(obj_event,'Monkey')]]==100]<-0
      
      subj_df<-add_column(subj_df, !!(paste0(obj_event,'NoObj')):=0, .after='EVENTS')
      subj_df[[paste0(obj_event,'NoObj')]]<-0
      subj_df[[paste0(obj_event,'NoObj')]][subj_df[[obj_event]]==100]<-1
      
      # All cells where object is set
      nz_cells<-subj_df[[paste0(obj_event,'Monkey')]]>=1
      # For each unique action id
      unique_vals<-unique(subj_df[[paste0(obj_event,'Monkey')]][nz_cells])
      for(val in unique_vals) {
        cells<-which(subj_df[[obj_event]]==val)
        # Find row for action ID in column table
        row=which(subj_obj_df$`Action ID`==val)
        if(length(row)>0) {
          col=which(subj_obj_df[row,2:length(colnames(subj_obj_df))]==1)+1
          # If no object marked - no object
          if(length(col)==0) {
            subj_df[[paste0(obj_event,'Monkey')]][cells]<-0
            subj_df[[paste0(obj_event,'NoObj')]][cells]<-1
          } 
          # Monkey marked
          else {
            other_subj<-colnames(subj_obj_df)[col]
            subj_df[[obj_event]][which(subj_df[[obj_event]]==val)]<-sum(which(unique_subjects %in% tolower(other_subj)))
            subj_df[[paste0(obj_event,'Monkey')]][which(subj_df[[obj_event]]==val)]<-sum(which(unique_subjects %in% tolower(other_subj)))
            subj_df[[paste0(obj_event,'NoObj')]][cells]<-0
          }
        } 
        # No row found in object sheet - no object
        else {
          subj_df[[paste0(obj_event,'Monkey')]][cells]<-0
          subj_df[[paste0(obj_event,'NoObj')]][cells]<-1
        }
      }
    }
    
    # Add object behaviors
    for (obj_state in obj_states) {
      col_name<-paste0(obj_state,'Object')
      state<-logical(length=duration)
      
      for (subject2 in unique_subjects) {
        if(subject2!=subject) {
          # Construct filename
          file_name<-paste0(data_dir,'/',subject2,'_',paste0(filename_parts[3:length(filename_parts)-1], sep='_',collapse=''),'5min.xlsm')
          if(file.exists(file_name)){
            csv_fname<-paste0(data_dir,'/',subject2,'_',paste0(filename_parts[3:length(filename_parts)-1], sep='_',collapse=''),'5min.csv')
            if(!file.exists(csv_fname)){
              subj2_df <- read_excel(filepath, sheet = 'Actions')
              write.csv(subj2_df, csv_fname, na = '', row.names = FALSE)
            }
            
            # Open file using transposed read csv (skip first column)
            subj2_df<-tryCatch({
              read.tcsv(csv_fname,skip=1)
            }, warning=function(w) {
              print(w)
            }, error=function(e) {
              print(e)
              print(csv_fname)
            })
            
            # Cutoff last rows (because read.tcsv adds some weird last rows with all NA)
            subj2_df<-subj2_df[1:duration,]  
            subj2_df<-subset(subj2_df, select=-c(X.1,Non.Social,X.2,Self.directed,X.3,Motor.stereotypies,X.4,Other,X.5,Affiliative.behaviour,X.6,Other.aggression,X.7,Fear.stress.behaviour,X.8,Self.directed.behaviour,X.9,Other.behaviours))
            
            behav_cols<-colnames(subset(subj2_df,select=-c(STATES,EVENTS)))
            for (col in behav_cols) {
              # Convert to character and trim whitespace
              subj2_df[[col]]<-as.character(subj2_df[[col]])
              subj2_df[[col]]<-trimws(subj2_df[[col]])
              # Convert x's to 10s and empty cells to 0s
              subj2_df[[col]][subj2_df[[col]]=='x']<-'100'
              subj2_df[[col]][subj2_df[[col]]=='']<-'0'
              # Convert to numeric
              subj2_df[[col]]<-as.numeric(subj2_df[[col]])
              # Set NA's to 0
              subj2_df[[col]][is.na(subj2_df[[col]])]<-0
            }
            
            subj2_obj_df <- read_excel(file_name, sheet = 'Objects')
            for(col in colnames(subj2_obj_df)) {
              subj2_obj_df[[col]]<-as.character(subj2_obj_df[[col]])
              subj2_obj_df[[col]]<-trimws(subj2_obj_df[[col]])
              # Convert x's to 1s and empty cells to 0s
              subj2_obj_df[[col]][subj2_obj_df[[col]]=='x']<-'1'
              subj2_obj_df[[col]][subj2_obj_df[[col]]=='']<-'0'
              # Convert to numeric
              subj2_obj_df[[col]]<-as.numeric(subj2_obj_df[[col]])
              # Set NA's to 0
              subj2_obj_df[[col]][is.na(subj2_obj_df[[col]])]<-0
            }
            subj2_df<-apply_constraints(subj2_df)
            
            obj_actions<-subj2_obj_df$`Action ID`[subj2_obj_df[[toupper(subject)]]>=1]
            
            # True whenever state is coded (false otherwise)
            state[(subj2_df[[obj_state]] %in% obj_actions)]<-which(unique_subjects==subject2)
          }
        }
      }
      subj_df<-add_column(subj_df, !!(col_name):=state, .after='STATES')
    }
    
    for (obj_event in obj_events) {
      col_name<-paste0(obj_event,'Object')
      state<-logical(length=duration)
      
      for (subject2 in unique_subjects) {
        if(subject2!=subject) {
          # Construct filename
          file_name<-paste0(data_dir,'/',subject2,'_',paste0(filename_parts[3:length(filename_parts)-1], sep='_',collapse=''),'5min.xlsm')
          if(file.exists(file_name)){
            csv_fname<-paste0(data_dir,'/',subject2,'_',paste0(filename_parts[3:length(filename_parts)-1], sep='_',collapse=''),'5min.csv')
            if(!file.exists(file_name)){
              subj2_df <- read_excel(file_name, sheet = 'Actions')
              write.csv(subj2_df, csv_fname, na = '', row.names = FALSE)
            }
            
            # Open file using transposed read csv (skip first column)
            subj2_df<-tryCatch({
              read.tcsv(csv_fname,skip=1)
            }, warning = function(w) {
              print(w)
            }, error = function(e) {
              print(e)#
              print(csv_fname)
            })
            # Cutoff last rows (because read.tcsv adds some weird last rows with all NA)
            subj2_df<-subj2_df[1:duration,]  
            subj2_df<-subset(subj2_df, select=-c(...2,X,X.1,Non.Social,X.2,Self.directed,X.3,Motor.stereotypies,X.4,Other,X.5,Affiliative.behaviour,X.6,Other.aggression,X.7,Fear.stress.behaviour,X.8,Self.directed.behaviour,X.9,Other.behaviours))
            
            behav_cols<-colnames(subset(subj2_df,select=-c(STATES,EVENTS)))
            for (col in behav_cols) {
              # Convert to character and trim whitespace
              subj2_df[[col]]<-as.character(subj2_df[[col]])
              subj2_df[[col]]<-trimws(subj2_df[[col]])
              # Convert x's to 1s and empty cells to 0s
              subj2_df[[col]][subj2_df[[col]]=='x']<-'1'
              subj2_df[[col]][subj2_df[[col]]=='']<-'0'
              # Convert to numeric
              subj2_df[[col]]<-as.numeric(subj2_df[[col]])
              # Set NA's to 0
              subj2_df[[col]][is.na(subj2_df[[col]])]<-0
            }
            
            subj2_obj_df <- read_excel(file_name, sheet = 'Objects')
            for(col in colnames(subj2_obj_df)) {
              subj2_obj_df[[col]]<-as.character(subj2_obj_df[[col]])
              subj2_obj_df[[col]]<-trimws(subj2_obj_df[[col]])
              # Convert x's to 1s and empty cells to 0s
              subj2_obj_df[[col]][subj2_obj_df[[col]]=='x']<-'1'
              subj2_obj_df[[col]][subj2_obj_df[[col]]=='']<-'0'
              # Convert to numeric
              subj2_obj_df[[col]]<-as.numeric(subj2_obj_df[[col]])
              # Set NA's to 0
              subj2_obj_df[[col]][is.na(subj2_obj_df[[col]])]<-0
            }
            subj2_df<-apply_constraints(subj2_df)
            
            obj_actions<-subj2_obj_df$`Action ID`[subj2_obj_df[[toupper(subject)]]>=1]
            
            # True whenever state is coded (false otherwise)
            state[(subj2_df[[obj_event]] %in% obj_actions)]<-which(unique_subjects==subject2)
          }
        }
      }
      subj_df<-add_column(subj_df, !!(col_name):=state, .after='EVENTS')
    }
    
    subj_df<-apply_constraints(subj_df)
    subj_df<-add_extra_codes(subj_df)
    subj_df$Subject<-subject
    subj_df$Group<-group
    subj_df$Date<-date
    subj_df$Time<-time
    
    all_subj_df<- rbind(all_subj_df, subj_df)
  }
  return(all_subj_df)
}
