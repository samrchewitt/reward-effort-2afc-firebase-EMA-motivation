# functions for reward-effort, multi-timepoint game study 
# Sam Hewitt 2023 - 2024


# check that the values with levels have appropriate proportions
calculate_proportions <- function(values, levels) {
  n_levels <- length(levels)
  proportions <- numeric(n_levels)
  for (i in 1:n_levels) {
    proportions[i] <- sum(values == levels[i]) / length(values)
  }
  return(proportions)
}

# read a file and remind user that this file was manually provided and is not created 
# anywhere else (brain explorer data): designed for a forgetful researcher
read_csv_with_reminder_BEX = function(file_path) {
  tryCatch(
    {
      read.csv(file_path)
    },
    error = function(e) {
      message("Reminder: This file was sent manually over Slack.")
      stop(e)
    }
  )
}

# read a file and remind user that this file was manually provided and is not created 
# anywhere else (redcap data): designed for a forgetful PhD student
read_csv_with_reminder_redcap = function(file_path) {
  tryCatch(
    {
      read.csv(file_path)
    },
    error = function(e) {
      message("Reminder: This file needs to be downloaded from RedCap DSH.")
      stop(e)
    }
  )
}

# prepare data for model fitting for a single timepoint 
rewEff_prepare <- function (data, trials=max(data$trialNo)+1,
                            exclude=NULL, 
                            method="seq", 
                            joint=F,
                            joint_inputs){
  
  # format data for stan 
  # inputs:
  # data (df): data in long format 
  # trials (int): select number of trials (default=all) 
  # exclude (array): an array of trials to exclude in sample if not taking all trials (e.g., catch trial idx)
  # method=c("rand", "seq") - passed to sample trials
  # joint (boolean): prepare data for a joint model or not
  # if joint ==T, joint_inputs is required 
  # joint inputs are quest data joint_inputs$quest and joint_inputs$vars_to_model (which variables to include)
  
  # first check that userID has been specified:
  # Check if 'userID' column exists
  if (!"userID" %in% colnames(data)) {
    # Rename 'subID' column to 'userID'
    colnames(data)[colnames(data) == "subID"] <- "userID"
  }
  
  nPpts=length(unique(data$userID))  # get number of participants
  nTrials_all=data %>%              # get number of trials each participant actually completed
    group_by(userID) %>%
    dplyr::summarize(nTrials = n()) %>%
    mutate(ID=seq(1, nPpts, 1))             # add sequential numeric IDs for compatibility with rstan output
  # maximum number of trials 
  nTrials_max <- nTrials_all %>%
    {max(.$nTrials)}
  data=merge(data, nTrials_all, by="userID") %>%
    mutate(choice01=recode(choice, "route 1"=0, "route 2"=1))
  
  
  ## create arrays of choice options and responses for each participant
  r1 <- e1 <- r2 <- e2 <-  array(1, dim = c(nPpts, nTrials_max))   # pad with 1s else stan will complain
  choice01 <- array(0, dim = c(nPpts, nTrials_max))    # pad with 0s
  for (i in 1:nPpts) {
    r1[i, 1:nTrials_all$nTrials[i]] <- with(data, trialReward1[ID==i])
    e1[i, 1:nTrials_all$nTrials[i]] <- with(data, trialEffortPropMax1[ID==i])
    r2[i, 1:nTrials_all$nTrials[i]] <- with(data, trialReward2[ID==i])
    e2[i, 1:nTrials_all$nTrials[i]] <- with(data, trialEffortPropMax2[ID==i])
    choice01[i, 1:nTrials_all$nTrials[i]] <- with(data, choice01[ID==i])  # but bernoulli logit function likes 1 and 0s
  }
  # if desired N trials is maximum, then save all trials in list
  # else save a sample
  if (trials == nTrials_max) {
    ## data list to pass to rstan
    data_list<- list(
      nPpts = nPpts,                 # number of participants
      nTrials_max = trials,     # max number of trials per participant
      nT_ppt = nTrials_all$nTrials,  # actual number of trials per participant
      rew1 = r1,                     # reward level for left option 
      eff1 = e1,                     # effort level for left balloon 
      rew2 = r2,                     # reward level for right option 
      eff2 = e2,                     # effort level for right option
      choice01 = choice01,         # chosen option ([0,1]=[left,right]), for bernoulli logit
      subID=nTrials_all$userID
    )
  } else {
    # sample a subset of trials: 
    trial_sample=sample_trials(trials, exclude)
    
    ## shorter data list to pass to rstan
    data_list <- list(
      nPpts = nPpts,                 # number of participants
      nTrials_max = trials,     # max number of trials per participant
      nT_ppt = rep(trials, nPpts),  # actual number of trials per participant
      sample_trials=trial_sample, # sample of trials taken
      rew1 = r1[, trial_sample],                     # reward level for left option 
      eff1 = e1[, trial_sample],                     # effort level for left balloon 
      rew2 = r2[, trial_sample],                     # reward level for right option 
      eff2 = e2[, trial_sample],                     # effort level for right option
      choice01 = choice01[, trial_sample],            # chosen option ([0,1]=[left,right]), for bernoulli logit
      subID=nTrials_all$userID
    )
  }
  # if it's a joint model: add the extra symptom related data
  if(joint==T){
    message('preparing joint model...')
    sx_list = rewEff_prepareGRM(joint_inputs[[1]], joint_inputs[[2]],
                                joint_inputs[[3]])
    
    # add the sx_list
    data_list = c(data_list, sx_list[2:length(sx_list)-1])
  }
  
  return(data_list)
}


# exclude duplicate games 

rewEff_exclude_duplicate_plays <- function(data_long, 
                                           gameTrials = 24){
  # function which takes the long data format and excludes the second play of a game by the participant
  # check for dupliate games
  nTrials_all <- data_long %>%  # calculate nTrials on each session
    group_by(subID, sess) %>%
    dplyr::summarize(nTrials = n()) 
  # how many sessions contained extra games ? 
  # if there are extra games, remove these duplicates
  # note we should use these extra plays in some way 
  if(sum(nTrials_all$nTrials>gameTrials)>0){
    # warn the user
    warning(paste0(sum(nTrials_all$nTrials>gameTrials), ' sessions with duplicate games. Taking first play (', gameTrials, ' trials)...'))
    
    filtered_data <- data_long %>%
      group_by(subID, sess) %>%
      mutate(t = seq(1:n()))%>%  # Calculate the number of trials to keep
      filter(t %in% seq(1,gameTrials,1)) %>%
      ungroup() %>%
      select(-t)  # Remove the temporary 'gameTrials' column
    
    nTrials_all <- filtered_data %>%
      group_by(subID, sess) %>%
      dplyr::summarize(nTrials = n()) 
    
    # replace the data for further use 
    data_long = filtered_data;
    
    # warn the user
    warning(paste0(sum(nTrials_all$nTrials<gameTrials), ' sessions with incomplete games. Dropping...'))
    
    filtered_data <- data_long %>%
      group_by(subID, sess) %>%
      mutate(t = seq(1:n()))%>%  # Calculate the number of trials to keep
      filter(t %in% seq(1,gameTrials,1)) %>%
      ungroup() %>%
      select(-t)  # Remove the temporary 'gameTrials' column
    
    nTrials_all <- filtered_data %>%
      group_by(subID, sess) %>%
      dplyr::summarize(nTrials = n()) 
    
    # replace the data for further use 
    data_long = filtered_data;
  }
  
  # and plays without complete data 
  if(sum(nTrials_all$nTrials>gameTrials)>0){
    # warn the user
    warning(paste0(sum(nTrials_all$nTrials>gameTrials), ' sessions with duplicate games. Taking first play (', gameTrials, ' trials)...'))
    
    filtered_data <- data_long %>%
      group_by(subID, sess) %>%
      mutate(t = seq(1:n()))%>%  # Calculate the number of trials to keep
      filter(t %in% seq(1,gameTrials,1)) %>%
      ungroup() %>%
      select(-t)  # Remove the temporary 'gameTrials' column
    
    nTrials_all <- filtered_data %>%
      group_by(subID, sess) %>%
      dplyr::summarize(nTrials = n()) 
    
    # replace the data for further use 
    data_long = filtered_data;
  }
  
  return(data_long)
  
}

# prepare data for test-re-test analysis
rewEff_prepare_retest <- function(data_long_all, 
                                  minSess=length(unique(data_long$sess)), 
                                  gameTrials=length(unique(data_long$trialNo)),
                                  joint=F,
                                  joint_inputs){
  # data_long_all = long trial data
  # minSess: specify the minimum sessions played for participant to be included in the analysis (if not already excluded)
    #default: all sessions in data 
  #gameTrials: specify N trials in the game, default is all trials 
  
  # joint=F 
  # run a joint model with states & traits 

  # filter participants that completed min games (minSess)
  completion <- data_long_all %>%
    group_by(user_id) %>%
    dplyr::summarize(session_count = n_distinct(sess)) %>%
    filter(session_count >= minSess)
  
  # print
  message(paste0('preparing data for ', nrow(completion), " subjects with minimum ", 
                 minSess, " games"))
  
  # get nTrials for the filtered participants
  nTrials_all <- data_long_all %>%
    filter(user_id %in% completion$user_id) %>% # filter only those who meet completion requirements
    arrange(user_id) %>%
    group_by(user_id, sess) %>%
    dplyr::summarize(nTrials = n()) 
  # get nPpts
  nPpts <- length(unique(nTrials_all$user_id))
  
  # create subID list
  subIDs <- data_long_all %>%
    filter(user_id %in% completion$user_id) %>% # filter only those who meet completion requirements
    arrange(user_id) %>%
    group_by(user_id) %>%
    dplyr::summarize() %>%
    mutate(ID=seq(1, nPpts, 1))     
  # add sequential numeric IDs for compatibility with rstan output
  data_long_all <- merge(data_long_all, subIDs, by="user_id") %>%
    mutate(choice01=recode(choice, "route 1"=0, "route 2"=1)) %>%
    arrange(user_id,sess,trialIdx) %>%
    ungroup()
  # also re-arrange the trials by trialIdx so that values are matched by participant
  # maximum number of trials
  nTrials_max <- nTrials_all%>%
    {max(.$nTrials)}
  
  # get the remaining n participants
  nT_ppts <- pivot_wider(nTrials_all, id_cols="user_id",
                         names_from="sess",
                         values_from="nTrials") %>% ungroup() %>%
    select(-user_id) %>% as.matrix()
  # replace NaN with gameTrials (stan doesn;t accept NaN; we will deal with missing games later)
  nT_ppts[is.na(nT_ppts)]=gameTrials
  # get the maximum nSessions
  nTimes=pmin(length(unique(nTrials_all$sess)), max(nTrials_all$sess))
  
  # Initialize data arrays with sentinel values (mis_indicator)
  mis_indicator=1 # define sentinel for missing data
  r1 <- e1 <- r2 <- e2 <- array(mis_indicator, dim = c(nPpts, nTimes, nTrials_max))
  
  choice01 <- array(mis_indicator, dim = c(nPpts, nTimes, nTrials_max))
  # Initialize is_missing with 0s
  is_missing <- array(0, dim = c(nPpts, nTimes, nTrials_max))
  
  # loop the participants (if data is not available leave as mis_indictor)
  for (i in 1:nPpts) {
    for (t in min(data_long_all$sess):nTimes) {
      tryCatch({
        # Set values as -9999 if the ID and session combination doesn't exist
        if (length(with(data_long_all, trialReward1[ID == i & sess == t])) == 0) {
          r1[i, t, ] <- mis_indicator
          e1[i, t, ] <- mis_indicator
          r2[i, t, ] <- mis_indicator
          e2[i, t, ] <- mis_indicator
          choice01[i, t, ] <- mis_indicator
          
          # Set is_missing to 1 for all trials in the missing session
          is_missing[i, t, ] <- 1
        } else {
          r1[i, t, ] <- with(data_long_all, trialReward1[ID == i & sess == t])
          e1[i, t, ] <- with(data_long_all, trialEffortPropMax1[ID == i & sess == t])
          r2[i, t, ] <- with(data_long_all, trialReward2[ID == i & sess == t])
          e2[i, t, ] <- with(data_long_all, trialEffortPropMax2[ID == i & sess == t])
          choice01[i, t, ] <- with(data_long_all, choice01[ID == i & sess == t])
        }
      }, error = function(e) { # also catch any errors with -9999
        r1[i, t, ] <- mis_indicator
        e1[i, t, ] <- mis_indicator
        r2[i, t, ] <- mis_indicator
        e2[i, t, ] <- mis_indicator
        choice01[i, t, ] <- mis_indicator
        
        # Set is_missing to 1 for all trials in the missing session
        is_missing[i, t, ] = 1
      })
    }
  }
  
  ## data list to pass to rstan
  data_list <- list(
    nTimes = nTimes,               # number of time points (sessions)
    nPpts = nPpts,                 # number of participants
    nTrials_max = nTrials_max,     # max number of trials per session per participant
    nT_ppts = nT_ppts,             # actual number of trials per session per participant
    rew1 = r1,                     # reward level for left option per session per participant
    eff1 = e1,                     # effort level for left balloon per session per participant
    rew2 = r2,                     # reward level for right option per session per participant
    eff2 = e2,                     # effort level for right option per session per participant
    choice01 = choice01,            # chosen option ([0,1]=[left,right]) per session per participant
    user_id = subIDs$user_id,           # Participant IDs - user_id version
    subID = unique(data_long_all$subID),           # Participant IDs - subID (firebase ID)
    is_missing = is_missing       # Indicator array for missing data
  )
  
  # warn user about amount of missing data 
  warning('\nmarked ', round((sum(is_missing)/sum(nT_ppts))*100), '% of trials as missing\n')
  
  # if user wants a joint model, add the joint inputs 
  if(joint==T){
    # make sure you only take subjects with choice data 
    quest=joint_inputs$quest %>% filter(user_id %in% data_list$user_id) 
    
    # generate a subjective state list, similar to above 
    sx_list = rewEff_prepareGRM(quest, data_long_all,joint_inputs$vars_to_model)
    
    # subset the list
    sx_list=sx_list$data_list;
    
      # combine the data outputs together, subset only the necessary self-report data 
      vars=c("nStateTimes", "nItems", "N", "jj", "ii", "y", "r", "is_missing_state", "ss", "apathyCFA")
      data_list = c(data_list, sx_list[vars])
      # rename variables for model
      names(data_list)[names(data_list) == "nTimes"] <- "nTaskTimes"
      
      # add a binaries endorsement of each variable 
      data_list$Y = matrix(NA, nrow=length(data_list$y[,1]), ncol=data_list$nStateTimes)
      
      # cut SS to match nTaskTimes
      data_list$ss = data_list$ss[,min(data_long_all$sess):max(data_long_all$sess)]
    
    # loop the responses
    for(t in 1:ncol(data_list$Y)){
      data_list$Y[data_list$y[,t]<median(data_list$y),t]=0; # anything not=True; is -
      data_list$Y[data_list$y[,t]>=median(data_list$y), t]=1 # 4-6 are "true" responses; they are 1 
    }
    
  }
  
  
  return(data_list)
  
}

# reward-effort study prepare GRM 
rewEff_prepareGRM = function(quest_data, data_long, vars_to_model){
  # wrapper function to prepare data for IRT model in stan 
  # inputs: 
  # quest_data: self report data 
  # data_long: trial-wise task data 
  # and self-report-var-names to select (vars_to_model)

  # save the number of timepoints 
  total_tasks = (max(data_long$sess) - min(data_long$sess))+1;

   # overwrite quest with the long data 
    quest = quest_data
      
    # determine whether there are any duplicate entries
    nGames_all <- quest %>%
        arrange(user_id) %>%
        group_by(user_id, obs) %>%
        dplyr::summarize(nGames = n()) 
    
    # get n ppts
    nPpts <- length(unique(nGames_all$user_id))

    # summarise symptom data 
    sx_grm <- quest %>%
        dplyr::select(all_of(c(vars_to_model,   # motivation items
                               "user_id", "sess", "obs"))) %>%
        arrange(user_id, obs) %>%
        mutate(IDsx = dense_rank(user_id)) %>%
        melt(id.vars=c("IDsx", "user_id", "obs", "sess"))%>%
        arrange(IDsx, variable) %>%
        mutate(variable2 = unclass(variable))
      
  # get max n state timepoints (obs); note sess refers to games/tasks
    nTimes=pmin(length(unique(sx_grm$obs, na.rm=T)), max(sx_grm$obs, na.rm=T))
    # get nItems  
    nItems <- max(sx_grm$variable2)
    # Initialize data arrays with sentinel values
    mis_indicator <- 1 # define sentinel for missing data in y
    
    # Create matrices to store jj, ii, and y
    jj <- matrix(mis_indicator, nrow = nPpts * nItems, ncol = nTimes)
    ii <- matrix(mis_indicator, nrow = nPpts * nItems, ncol = nTimes)
    y <- matrix(mis_indicator, nrow = nPpts * nItems, ncol = nTimes)
    
    # Create a matrix to store is_missing (by item)
    is_missingItem <- matrix(0, nrow = nPpts * nItems, ncol = nTimes)
    # initlaising missing matrix by participants
    is_missing <- array(0, dim = c(length(unique(sx_grm$user_id)), nTimes))

    # Loop through participants and sessions
    # and identify the matching rows to fill arrays 
    # update variable name to match below code 
    
 
    sx_grm=dplyr::rename(sx_grm, "session"="sess")
    ss_summary = sx_grm %>% filter(!is.na(session)) %>%
         group_by(IDsx, session) %>%
         summarise(ss = unique(obs)) %>% ungroup()
      #
      # # the column ss, gives the index in the state time-series for the game for each individual subject
      # # convert this to a matrix with each row a participant
      #
      ss <- matrix(ss_summary$ss, nrow = length(unique(ss_summary$IDsx)), byrow = TRUE)
      # 
      # sx_grm=dplyr::rename(sx_grm, "session"="sess", "sess"="obs")
      # # create state task index
      # ss = vector();
      # # ss is a vector indicating at which positions along the time-series of states (sess/obs) the tasks (sessions) fall for each subject
      # # ss begins at the first session / game (min) and continues to the end (nTimes)
      # # the 8-total_tasks+1 index allows us to start from a later game if that was specified in the input (total_tasks = (max(data_long$sess) - min(data_long$sess))+1)
      # # e.g., if for whatever reason, the modelling starts at game 2, then there will only be 7 total games 
      # # so ss will start at game 8-7+1 = 2; and be length 7 
      # ss= sx_grm$sess[min(data_long$sess):nTimes][!is.na(sx_grm$session[min(data_long$sess):nTimes])]

      for (i in 1:nPpts) {
      for (t in 1:nTimes) {
        # Find rows that match the current participant and session
        matching_rows <- which(sx_grm$IDsx == i & sx_grm$obs == t)
        start_idx <- (i - 1) * nItems + 1
        end_idx <- i * nItems
        if (length(matching_rows)>0) {
          # Fill jj, ii, and y with data from the matching rows
          jj[start_idx:end_idx, t] <- sx_grm$IDsx[matching_rows]
          ii[start_idx:end_idx, t] <- sx_grm$variable2[matching_rows]
          y[start_idx:end_idx, t] <- sx_grm$value[matching_rows]} # take raw state 
        else {
          is_missingItem[start_idx:end_idx, t] <- 1
          y[start_idx:end_idx, t] <- 0
        }
        
        # fill the missing person/sess indicator 
        if(is.na(y[start_idx,t])){
          is_missing[i, t] = 1
          is_missingItem[start_idx:end_idx, t] = 1
        }

      }
    }
    
    # in the long IRT model, the input data contains all obs, with NaN values
    # to account for this, mark NaN values as missing
    # and convert the nan values in y to 0 because stan will not accept NaN 
    y[is.na(y)]=0
    
    warning('\nmarked ', round((sum(is_missing==1)/(nrow(is_missing)*ncol(is_missing)))*100,2), '% of self-report timepoints as missing')
    
    ## create list to pass to stan
    data_list <- list(
      nPpts = nPpts,              # number of participants
      nTimes = nTimes,           # number of sessions
      nItems = nItems,            # number of items per participant
      N = nPpts*nItems,           # total number of observations per timepoint
      jj = jj[, 1],          # person id for observation n
      ii = ii[, 1],       # item id for observation n
      y = y,           # response for observations n; y in {0 ... m_i}
      r = y, # modified response
      is_missing = is_missing, # a logical index to missing responses 
      subID = unique(sx_grm$subID) # save the subject IDs for later easy
    )
    # add a binaries endorsement of each variable 
    data_list$Y = matrix(NA, nrow=length(data_list$y[,1]), ncol=data_list$nTimes)
    for(t in 1:data_list$nTimes){
        data_list$Y[data_list$y[,t]<=median(data_list$y),t]=0
        data_list$Y[data_list$y[,t]>median(data_list$y), t]=1
        
  }
    
    # state idx for nGames 
    data_list$ss = ss;
    names(data_list)[names(data_list) == "nTimes"] <- "nStateTimes"
    names(data_list)[names(data_list) == "is_missing"] <- "is_missing_state"

  
  # add trait-apathy scores, horrible tidy verse version 
    data_list$apathyCFA = quest %>% 
      # arrange as you did for sx_grm
      arrange(user_id, sess) %>%
      group_by(user_id) %>% summarise(apathy = max(apathy, na.rm=T)) %>% # df repeats obs for each subject so max, min, mean or first is all same
      ungroup() %>% 
      select(apathy) %>% pull()
  out=list(data_list=data_list, quest=quest)
  return(out)
}

  
# for pilot data, rstan wrapper for fitting model
rewEff_fit <- function (data_long, model, nTrials=max(data$trialNo)+1,exclude=NULL,method,
                        sampling_params=list(nFits=1,chains=4, warmup=2000,iter=4000,adapt_delta=0.95, thin=0), 
                        # file name to save result
                        f_name=paste0(data_dir, "stan_fits/",
                                      mod_name_full,
                                      "_", task_ver,
                                      "_adapt_delta", sampling_params$adapt_delta, 
                                      ".rds"),
                        # is it a joint model ? 
                        joint=F, joint_inputs){
  # wrapper function for rStan model data prepare, fit and save
  # inputs:
    # options used to format data for stan are passed rewEff_prepare #
    # data (df): data in long format 
    # trials (int): select number of trials (default=all) 
    # exclude (array): an array of trials to exclude in sample if not taking all trials (e.g., catch trial idx)
    # method=c("rand", "seq") - passed to sample trials
    # joint (boolean): prepare data for a joint model or not
    # if joint ==T, joint_inputs is required 
    # joint inputs are quest data joint_inputs$quest and joint_inputs$vars_to_model (which variables to include)
  # fit options #:
    # model: a compiled stan model (model = rstan::stan_model(paste0(stan_dir,mod_name_full,".stan")) 
    # sampling_params (list):
        # nFits: number of model fits to run (capacity to run mutiple variants)
        # chains: chains to sample
        # warmup: samples warmup; iter = samples to sample 
        # adapt_delta = Target average acceptance probability (default = rstan default)
        # thin: thinning factor (default = 0/none)
  
  # prepare data 
  data=list();
  for (m in sampling_params$nFits){
    data_list=rewEff_prepare(data_long, nTrials, 
                             exclude, 
                             method, joint, joint_inputs)
    # save data to list:
    data[[m]]=data_list;
    
    print(paste0("Running model ", m, "..."))
    
    # fit model: 
    fit <- rstan::sampling(
      model,
      data = data_list, 
      chains = sampling_params$chains, # n separate chains to assess convergence
      warmup = sampling_params$warmup, # these are used to tune the sampler and ’burn in’
      iter = sampling_params$iter, # number of iterations (#kept = chains*(iter - warmup)
      pars=sampling_params$params_to_save, # only save specific parameters to speed up sampling and reduce ram
      cores = parallel::detectCores(),    # get number of cores available for parallelisation
      control = list(adapt_delta = sampling_params$adapt_delta),
      thin = sampling_params$thin # thinning factor to save memory
    )
    # save the model object for later
    saveRDS(fit, file = f_name)
    
    print(paste0("Model object ", sampling_params$nFits[m], " saved."))
    
    
    data[[m]]=data_list;
    
  }
  # return a list of the data that was used
  # let's also save the model data object for later
  #saveRDS(data, file = paste0("./stan-models/ntrials_testing/",
  #mod_name_full,
  #"_",task_ver,"_t", 
  #data_list$nT_ppt[1], 
  #"_fits_data",
  #".rds"))
  #print(paste0("Saved data used in [", m, "] to /n_trials_testing/data_samples/"))
  
  return(fit)
}

rewEff_fit_retest <- function (data_long_all, model_data, method,
                               model, 
                               sampling_params=list(nFits=1,chains=4, warmup=2000,iter=4000,adapt_delta=0.95, thin=1,
                                                    max_treedepth = 10, init_r = 2), 
                               f_name, 
                               minSess=2, 
                               gameTrials=24,
                               # is it a joint model ? 
                               joint=F, joint_inputs) {
  # wrapper function for fitting a stan model with rstan:
  # fit a multiple timepoint retest model and save output
  # inputs:
  # data_long_all = long task data
  # model_data = if specified, passed to rewEff_prepare_retest
  # method = method to sample trials - passed to rewEff_prepare_retest
  # model = name of stan model to fit
  # sampling_params (list):
    # nFits: number of model fits to run 
    # chains: chains to sample
    # warmup: samples warmup; iter = samples to sample 
    # adapt_delta = Target average acceptance probability (default = rstan default)
    # thin: thinning factor (default = 0/none)
  # 
  # joint (boolean): fit a joint model or not 
  # if joint ==T, joint_inputs is required 
  # joint inputs are quest data joint_inputs$quest and joint_inputs$vars_to_model (which variables to include)
  
  # first get data
  data=list();
  out=rewEff_prepare_retest(data_long_all, model_data, method, minSess, gameTrials, joint, joint_inputs)
  print(paste0("Running model..."))
  fit <- rstan::sampling(
    model,
    data = out$data_list, 
    chains = sampling_params$chains, # n separate chains to assess convergence
    warmup = sampling_params$warmup, # these are used to tune the sampler and ’burn in’
    iter = sampling_params$iter, # number of iterations (#kept = chains*(iter - warmup)
    pars=sampling_params$params_to_save, # only save specific parameters to speed up sampling and reduce ram
    cores = parallel::detectCores(),    # get number of cores available for parallelisation
    control = list(adapt_delta = sampling_params$adapt_delta,
                   max_treedepth  = sampling_params$max_treedepth),
    thin = sampling_params$thin, # thinning factor to save memory
    init_r = sampling_params$init_r
    )
  
  # save the model for later
  # save the model object for later
  if(exists("f_name")){
    saveRDS(fit, file = f_name)
    message("Model object saved.")
    
  }
  else{saveRDS(fit, file = paste0("D:/EMA_Motivation/data/study1/ema/task/stan_fits/tmp-", model@model_name, ".rds"))
    warning('Model object saved with tmp filename\nfile: tmp-', model@model_name, '.rds')}
  
  # return fit 
  return(fit)
}

rewEff_fit_retestCMDstan <- function (data_long_all, model_data, model,
                                      sampling_params=list(nFits=1,chains=4, warmup=2000,iter=4000,adapt_delta=0.95, thin=0), 
                                      f_name, savepath,
                               minSess=2, gameTrials=24,
                               # is it a joint model ? 
                               joint=F, joint_inputs) {
  # wrapper function for fitting a stan model with cmdstanr:
  # fit a multiple timepoint retest model and save output
  # inputs:
  # data_long_all = long task data
  # model_data = if specified, passed to rewEff_prepare_retest
  # method = method to sample trials - passed to rewEff_prepare_retest
  # model = name of stan model to fit
  # sampling_params (list):
    # nFits: number of model fits to run 
    # chains: chains to sample
    # warmup: samples warmup; iter = samples to sample 
    # adapt_delta = Target average acceptance probability (default = rstan default)
    # thin: thinning factor (default = 0/none)
  # joint (boolean): fit a joint model or not 
  # if joint ==T, joint_inputs is required 
  # joint inputs are quest data joint_inputs$quest and joint_inputs$vars_to_model (which variables to include)
  
  # first get data
  data=list();
  data_list=rewEff_prepare_retest(data_long_all, minSess, gameTrials, joint, joint_inputs)
  print(paste0("Running model..."))
  # drop subID as command stan doesn't like it
  data_list["subID"]=NULL
  
  message('fitting ', model$model_name(), "...")
  
  message("local device time started: ", Sys.time())
  fit <- model$sample(
    data = data_list, 
    chains = sampling_params$chains, # n separate chains to assess convergence
    iter_warmup = sampling_params$warmup, # these are used to tune the sampler and ’burn in’
    iter_sampling = sampling_params$iter, # number of iterations (#kept = chains*(iter - warmup)
    parallel_chains = pmin(parallel::detectCores(),sampling_params$chains),   # get number of cores available for parallelisation (either all or max available)
    adapt_delta = sampling_params$adapt_delta,
    max_treedepth = sampling_params$max_treedepth,
    #threads_per_chain = sampling_params$threads_per_chain, # 
    thin = sampling_params$thin, # thinning factor to save memory
    init = sampling_params$init_r, # intiialise distribution between -init_r and + init_r
    output_dir = savepath,
    #opencl_ids = c(0, 0)
  )
  
  # save the model for later
  # save the model object for later
  saveRDS(fit, file = f_name)
  
  print(paste0("Model object saved."))
  # return fit 
  return(fit)
}


# sample a specific array within a larger array, while excluding certain values 
sample_trials <- function(trials, excl,
                          method){
  # trials = array of int to sample from
  # excl = array of int to exclude from output
  # method = "rand" (sample randomly) or "seq" sample sequentially
  # trials to exclude defaults to NULL to prevent error 
  if(missing(excl)) excl=NULL
  if(missing(method)) method="rand"
  array=1:length(trials)
  array=array[!array %in% excl]
  # for random sample
  if (method == "rand") {
    out=sort(sample(array, trials))
  }
  else if (method == "seq") {
    out=array[1:trials] 
  }
  return(out)
}

# calculate posterior predictive accuracy for several pilot study models in a loop 
rewEff_predictiveAcc <- function (fits, model_data,
                                  mod_names){
  # fits = list of model fit objects
  # model_data is the saved original data_list 
  # loop each iteration:
  accs=list();
  for (f in seq(fits)){
    y_rep <- as.data.frame(rstan::summary(fits[[f]], pars = c("y_rep"))$summary) %>%
      select(`50%`) %>%
      tibble::rownames_to_column(var = "var") %>%
      separate(var, sep="\\[", into=c("variable","tmp"), remove=TRUE, extra="drop") %>%
      separate(tmp, sep=",", into=c("subID","trial"), remove=TRUE, extra="drop") %>%
      separate(trial, sep=-1, into="trial", extra="drop") %>%
      mutate(subID=as.numeric(subID)) %>%
      arrange(subID) %>%
      dplyr::rename(mean_predicted = `50%`)
    y_rep$trial = as.numeric(y_rep$trial) # convert trial to numeric
    
    # predictive accuracy
    # get observed responses from model data
    choice=as.data.frame(model_data[[f]][["choice01"]]) %>%
      tibble::rownames_to_column(var="subID")
    Choice = reshape2::melt(choice)
    # extract numeric trial
    Choice$trial=as.numeric(Choice$variable); Choice$subID=as.numeric(Choice$subID); 
    names(Choice)[3]="observed";
    # arrange by Subject:
    Choice=Choice[, c("subID", "trial", "observed")]
    Choice=Choice[order(Choice$subID),]
    # combine with predicted responses:
    y_rep <- cbind(y_rep, observed=Choice$observed)
    acc=y_rep %>% group_by(subID) %>%
      mutate(acc = (mean_predicted==observed)) %>%
      summarise(mean_acc = mean(acc))
    # make a table
    tbl=knitr::kable(y_rep %>%
                       group_by(subID) %>%
                       mutate(acc = (mean_predicted==observed)) %>%
                       summarise(mean_acc = mean(acc)) %>%
                       summarise(mean_pred_acc=mean(mean_acc), sd_pred_acc=sd(mean_acc)),
                     caption = paste0("predictive accuracy of posterior trials = ", max(y_rep$trial)))
    
    
    
    # rename for iteration
    names(acc)= c(paste0("SubID_", f), paste0("mean_acc_", f))
    # save into list
    accs[[f]]=acc
  }
  # concatenate list of dataframes 
  accuracy = bind_cols(accs)
  accuracy=accuracy %>% select(-contains("SubID"))
  
  plots=list();
  for (f in seq(fits)){
    
    # violin plot with scatter
    plots[[f]]=ggplot(accuracy, aes_string(x=2, y = paste0("mean_acc_", f))) + 
      labs(x=paste0(mod_names[f]), y="Post predictive accuracy") +
      theme(legend.position="none",
            panel.background= element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_blank())+
      ylim(0, 1) +
      #add distribution
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")
  }
  for (p in seq(plots)){
    print(plots[[p]]) +
      plot_annotation(paste0(mod_names[p], 
                             " mean of meanP = ", 
                             round(mean(as.matrix(accuracy)), 2)))
  }
  return(accuracy)
}

# function to save next file after an iteration of RDS
next_rds <- function(cache_dir) {
  f = list.files(file.path(getwd(), cache_dir), pattern = paste0("^", filestem, "_\\d+\\.rds"))
  num = max(as.numeric(gsub(paste0("^", filestem, "_(\\d)\\.rds"), "\\1", f)) + 1)
  paste0(cache_dir, "/", filestem, "_", num)
}

# plot a list of lists in a grid:
# e.g., from several models 
plot_grid <- function(plist){
  # requires grid extra
  n = length(plist)
  nCol <- floor(sqrt(n))+1
  do.call("grid.arrange", c(plist, ncol=nCol, 
                            top = names(plist)[1]))
  
}

# plot rew-eff sensitivity agaisnt each other to show bivariate relationship
plotBivariate = function(fit, formula = y~x, exclude=F, quantiles=c(0.025, 0.975),
                         data_list){
  # inputs: fit = rstan fit object
  # formula: 
  
  posts <- as.data.frame(rstan::summary(fit, pars = c("rewSens","effSens"))$summary) %>%
    select(mean, sd) %>%
    rownames_to_column(var = "var") %>%
    separate(var, sep="\\[|\\,|\\]", into=c("parameter","subID"), remove=TRUE, extra="drop")
  
  # pivot to make it wider
  posts_wide=posts %>%
    pivot_wider(id_cols=c("subID"), names_from = c("parameter"), values_from = c("mean", "sd"))
  # add the identifiers:
  posts_wide$user_id=data_list$subID
  #exclude outliers
  if (exclude==T){
    warning(paste0('excluding outliers effSens <= ', quantiles[[1]]*100, '%'))
    nExclude = posts_wide %>% filter(mean_effSens <= quantile(mean_effSens, quantiles[[1]])) %>% nrow()
    posts_wide=posts_wide %>% filter(mean_effSens > -quantile(mean_effSens, quantiles[[1]]))
    warning(paste0('excluded total: '), nExclude)
    
  }
  
  p1=posts_wide %>%
    ggplot(aes(x=mean_rewSens, y=mean_effSens))  +
    geom_smooth(method = "lm", se=T, alpha=.2, formula = formula) +
    geom_point() +
    geom_errorbarh(aes(xmin = mean_rewSens-sd_rewSens, xmax = mean_rewSens+sd_rewSens), alpha=.2) + 
    geom_errorbar(aes(ymin = mean_effSens-sd_effSens, ymax = mean_effSens+sd_effSens), alpha=.2) +
    xlab("posterior mean reward sensitivity") + ylab("posterior mean effort sensitivity") + 
    theme_minimal() + theme(strip.text = element_text(face = "bold", size=12)) + 
    labs(title="Baseline (Day 0)")
  p=ggExtra::ggMarginal(p1, type="densigram")
  
  # return the plot
  return(p)
  
}

# plot parameter recovery
# take two models and plot the correspondence 
rewEff_plotParamRecovery = function(fit, fit_rep, param_select=c("rewSens", "effSens"), 
                                    retest=T, plot_ref, quantiles=c(0.1, 0.9)){
  # inputs: 
  # fit=original fit
  # fit_rep = replicated fit based on parameters in fit 
  # params_select: which parameters to plot
  # retest (boolean): is it a retest model or not
  # plot_ref: a name to give the model when plotting the output
  # quantiles: of outliers to exclude quantiles[1] is lower bound, quantiles[2] is upper bound
  
  # plot original posterior vs replicated data posterior means/SDs
  if(retest==T){
      posts = as.data.frame(rstan::summary(fitLinearFU, pars = param_select)$summary) %>%
      select(mean,sd) %>%
      tibble::rownames_to_column(var = "var")%>%
      separate(var, sep="\\[|\\,|\\]", into=c("parameter","subID", "session"), remove=TRUE, extra="drop")%>%
      # convert new variables to numeric for merging
      mutate(subID = as.numeric(subID), session=as.numeric(session))
      
      posts_rep = as.data.frame(rstan::summary(fit_rep, pars = param_select)$summary) %>%
        select(mean,sd) %>%
        tibble::rownames_to_column(var = "var")%>%
        separate(var, sep="\\[|\\,|\\]", into=c("parameter","subID", "session"), remove=TRUE, extra="drop") %>%
        mutate(subID = as.numeric(subID), session=as.numeric(session)) %>%
        # and adjust the variable names to differentiate in the combined df
        dplyr::rename(mean_rep=mean, sd_rep=sd)
      
      
      #merge them
      posts_both = merge(posts, posts_rep, by=c("subID", "session", "parameter"), no.dups = TRUE)
      posts_both$parameter <- factor(posts_both$parameter, levels = param_select)  #reorder
      
      # create a labeller for subplots
      s=sort(unique(posts_both$session))
      session_labeller <- function(x) paste0("game ", rep(s))
      
      #plot them
      warning('outliers excluded')
      pRew <- posts_both %>% filter(parameter=="rewSens",
                                    mean >=quantile(mean, quantiles[1]), mean <= quantile(mean, quantiles[2])) %>%
        ggplot(aes(x=mean, y=mean_rep)) +
        geom_abline(slope = 1, linetype="dashed", colour="grey") +
        geom_point() +
        geom_errorbarh(aes(xmin = mean-sd, xmax = mean+sd), alpha=.2) + 
        geom_errorbar(aes(ymin = mean_rep-sd_rep, ymax = mean_rep+sd_rep), alpha=.2) +
        facet_wrap(~session, ncol=4,labeller=as_labeller(session_labeller)) + xlim(0, 10)+ylim(0, 10)+
        ggpubr::stat_cor(method = "spearman", label.x = 5, label.y = 8,
                         label.sep='\n',cor.coef.name="rho",
                         p.accuracy = 0.001, r.accuracy = 0.01) +
        labs(x="posterior mean (sd)", y="posterior mean (sd) replicated",
             title=paste0("Simple param recovery: Reward sensitivity (", plot_ref, ")")) +
        theme_minimal() + theme(strip.text = element_text(face = "bold", size=12)) 
      
      pEff <- posts_both %>% filter(parameter=="effSens",
                                    mean >=quantile(mean, quantiles[1]), mean <= quantile(mean, quantiles[2])) %>%
        ggplot(aes(x=mean, y=mean_rep)) +
        geom_abline(slope = 1, linetype="dashed", colour="grey") +
        geom_point() +
        geom_errorbarh(aes(xmin = mean-sd, xmax = mean+sd), alpha=.2) + 
        geom_errorbar(aes(ymin = mean_rep-sd_rep, ymax = mean_rep+sd_rep), alpha=.2) +
        facet_wrap(~session, ncol=4,labeller=as_labeller(session_labeller)) + xlim(-2.5, 10)+ylim(-2.5, 10)+
        ggpubr::stat_cor(method = "spearman", label.x = 5, label.y = 8,
                         label.sep='\n',cor.coef.name="rho",
                         p.accuracy = 0.001, r.accuracy = 0.01) +
        labs(x="posterior mean (sd)", y="posterior mean (sd) replicated",
             title=paste0("Simple param recovery: Effort sensitivity (", plot_ref, ")")) +
        theme_minimal() + theme(strip.text = element_text(face = "bold", size=12)) 
      
      # get the corr matrix for rewSens
      # split the data by session and loop these
      postsEx = posts_both %>% filter(parameter=="rewSens", mean >=quantile(mean, quantiles[1]), mean <= quantile(mean, quantiles[2])) 
      postsExSplit = split(postsEx, postsEx$session)
      rho_magn1 <- vector("numeric"); rho_magn2 <- vector("numeric")
      cor1=list(); cor2=list(); 
      for(s in seq(postsExSplit)){
        cor1[[s]] = Hmisc::rcorr(as.matrix(postsExSplit[[s]][, c("mean", "mean_rep")]), type="spearman")
        # extract the estimates
        rho_magn1[s] <- cor1[[s]]$r[1, 2]  # Assuming you want the correlation between "mean" and "mean_rep"
        
      }
      
      # run a mixed effect model
      formula = as.formula(paste0("mean ~ mean_rep*session+(1|subID)"))
      m1 = lmerTest::lmer(formula,data=postsEx, REML=F, control = lme4::lmerControl(check.nobs.vs.nRE = 'ignore'))
      
      # get the corr matrix for effSens
      postsEx = posts_both %>% filter(parameter=="effSens",
                                      mean >=quantile(mean, quantiles[1]), mean <= quantile(mean, quantiles[2])) 
      
      # split the data by session
      postsExSplit = split(postsEx, postsEx$session)
      for(s in seq(postsExSplit)){
        cor2[[s]] = Hmisc::rcorr(as.matrix(postsExSplit[[s]][, c("mean", "mean_rep")]), type="spearman")
        # extract the estimates
        rho_magn2[s] <- cor2[[s]]$r[1, 2]  # Assuming you want the correlation between "mean" and "mean_rep"
        
      }
      
      # run a mixed effect model
      formula = as.formula(paste0("mean ~ mean_rep*session+(1+session|subID)"))
      m2 = lmerTest::lmer(formula,data=postsEx, REML=F, control = lme4::lmerControl(check.nobs.vs.nRE = 'ignore'))
      
      # take the grand means for correlations as well
      rho_mu = c(mean(rho_magn1), mean(rho_magn2))
      
      # output the results to a list
      p=list(pRew, pEff, cor1, cor2, rho_mu, m1, m2, posts_both)
      names(p)=c("plotRewSens", "plotEffSens", "corRew", "corEff", "rho_mu", "meRewSens", "meEffSens", "posteriors")
      
  }
  # if it's a baseline session, the parsing and plotting is slightly different as there's no session variable
  else{
    # original posteriors
    posts = as.data.frame(rstan::summary(fitLinearFU, pars = param_select)$summary) %>%
      select(mean,sd) %>%
      tibble::rownames_to_column(var = "var") %>%
      separate(var, sep="\\[", into=c("parameter","subID"), remove=TRUE, extra="drop") %>%
      separate(subID, sep=-1, into="subID", extra="drop")  %>%
      dplyr::rename(mean_rep=mean, sd_rep=sd)
    
    # replicated posteriors 
    posts_rep = as.data.frame(rstan::summary(fit_rep, pars = param_select)$summary) %>%
      select(mean,sd) %>%
      tibble::rownames_to_column(var = "var") %>%
      separate(var, sep="\\[", into=c("parameter","subID"), remove=TRUE, extra="drop") %>%
      separate(subID, sep=-1, into="subID", extra="drop")  %>%
      dplyr::rename(mean_rep=mean, sd_rep=sd)
    
    #merge them
    posts_both = merge(posts, posts_rep, by=c("subID","parameter"), no.dups = TRUE)
    posts_both$parameter <- factor(posts_both$parameter, levels = param_select)  #reorder
    
    #plot them
    p <- ggplot(posts_both, aes(x=mean, y=mean_rep)) +
      geom_abline(slope = 1, linetype="dashed", colour="grey") +
      geom_point() +
      geom_errorbarh(aes(xmin = mean-sd, xmax = mean+sd), alpha=.2) + 
      geom_errorbar(aes(ymin = mean_rep-sd_rep, ymax = mean_rep+sd_rep), alpha=.2) +
      ggpubr::stat_cor(method = "pearson", label.x = -2, label.y = 2,
                       label.sep='\n',
                       p.accuracy = 0.001, r.accuracy = 0.01) +
      xlab("posterior mean (sd)") + ylab("posterior mean (sd) replicated") +
      theme_minimal() + theme(strip.text = element_text(face = "bold", size=12)) +
      facet_wrap(~parameter, scales = "free")  
    
    p = list(p)
  }
  

  
  return(p)
}


# function to read all files in input list from json to nice df
read_quest_json <- function(file) {
  # read file from json into an R list:
  data_f <- rjson::fromJSON(file = file, simplify = TRUE)
  # select only trial data (numbered list items)
  data_f_quest <- data_f[grepl("^[[:digit:]]", names(data_f))]
  
  # convert to tibble to easily un-nesting of list data
  data_tib <- tibble(data_f_quest)  %>% 
    unnest_wider(data_f_quest)
  
  # and finally convert to a df with all the other info we want:
  data_df <- as.data.frame(data_tib) %>%
    arrange(questionNo) %>%
    mutate(subID = data_f$userHash, 
           attemptID = data_f$attemptHash,
           study_date = data_f$sessionStartDate,
           start_time = sub(".*\\s+", "", strptime(data_f$sessionStartTime, "%H:%M:%S")),
           # add the order if it's present
           taskFirst = ifelse("task1" %in% names(data_f) && data_f$task1 == "InstructionsScene", 1, 0))
  
}

# function to read all task data files in input list from json to nice df
read_reweff_json <- function(file) {
  # read file from json into an R list:
  data_f <- rjson::fromJSON(file = file, simplify = TRUE)
  
  # select only trial data (numbered list items)
  data_f_task <- data_f[grepl("^[[:digit:]]", names(data_f))]
  
  # process press times for all trials for this participant:
  data_f_task_proc <- tryCatch(
    expr = lapply(data_f_task, process_bps),
    error = function(err) {
      message("Error:", err)
      message("data_f$userHash:", data_f$userHash)
      return(NULL) # or handle the error in an appropriate way
    }
  )
  
  # convert to tibble to easily un-nesting of list data
  data_tib <- tibble(data_f_task_proc)  %>% 
    unnest_wider(data_f_task_proc)
  
  # and finally convert to a df with all the other info we want:
  data_df <- as.data.frame(data_tib) %>%
    arrange(trialNo) %>%
    mutate(subID = data_f$userHash,
           attemptID = data_f$attemptHash,
           firebaseID = data_f$firebaseUID,
           gameType=data_f$gameType,
           taskVer = task_ver,
           study_date= data_f$sessionStartDate,
           studyStartTime = sub(".*\\s+", "", strptime(data_f$sessionStartTime, "%H:%M:%S")),
           OS=data_f$participantOS)
  
}

# function to read all task data files in input list from json to nice df
read_reweff_prac_json <- function(file) {
  # read file from json into an R list:
  data_f <- rjson::fromJSON(file = file, simplify = TRUE)
  
  # select only trial data (list items which begin with a digit)
  data_f_task <- data_f[grepl("^[[:digit:]]", names(data_f))]
  
  # process press times for all trials for this participant:
  data_f_task_proc <- lapply(data_f_task, process_bps) 
  # convert to tibble to easily un-nesting of list data
  data_tib <- tibble(data_f_task_proc)  %>% 
    unnest_wider(data_f_task_proc)
  
  # and finally convert to a df with all the other info we want:
  data_df <- as.data.frame(data_tib) %>%
    arrange(pracTrialNo) %>%
    mutate(subID = data_f$userHash,
           attemptID = data_f$attemptHash,
           firebaseID = data_f$firebaseUID,
           study_date= data_f$sessionStartDate,
           studyStartTime = sub(".*\\s+", "", strptime(data_f$sessionStartTime, "%H:%M:%S")),
           OS=data_f$participantOS)
  # { try(
  #   mutate(totalStudyTime = round(as.double(strptime(data_f$endTimeDB, "%H:%M:%S") - 
  #                                    strptime(data_f$consentTime, "%H:%M:%S")),1))
  #   )
  # }
}

# function to read trial data from JSON that was specified for task
read_trials_json <- function(file) {
  # read file from json into an R list:
  data_f <- rjson::fromJSON(file = file, simplify = TRUE)
  
  # convert to tibble to easily un-nesting of list data
  data_df = as.data.frame(data_f)
  # add delta columns
  data_df$deltaReward = (data_df$reward1-data_df$reward2)
  data_df$deltaEffort = (data_df$effort1-data_df$effort2)
  
  return(data_df)
}

# function to calculate average time between  button presses 
process_bps <- function(x) {
  data_f_bps = x[["pressTimes"]]  # get list of press times for this trial
  x[["meanPressLag"]] <- mean(as.numeric(diff(data_f_bps)))  # get mean difference between presses, in ms
  x[["pressTimesTot"]] <- (unlist(x$pressTimes[length(x$pressTimes)]) - 
                             unlist(x$pressTimes[1]))  # get total pressing time
  x[["pressTimes"]] <- "see raw data" # remove raw data for now for df-ability
  return(x)
}

# function reads highest resolution vigour data (exact press times)
read_reweff_vigor <- function(file) {
  # read file from json into an R list:
  data_f <- rjson::fromJSON(file = file, simplify = TRUE)
  
  # select only trial data (numbered list items)
  data_f_task <- data_f[grepl("[[:digit:]]", names(data_f))]
  
  # convert to tibble to easily un-nesting of list data
  # unnest_longer, replicates the other variables to match length of pressTimes
  data_tib <- tibble(data_f_task)  %>% 
    unnest_wider(data_f_task) %>% unnest_longer(pressTimes) 
    
  
  # and finally convert to a df with all the other info we want:
  data_df <- as.data.frame(data_tib) %>%
    arrange(trialNo) %>%
    mutate(subID = data_f$userHash,
           attemptID = data_f$attemptHash,
           firebaseID = data_f$firebaseUID,
           gameType=data_f$gameType,
           taskVer = task_ver,
           study_date= data_f$sessionStartDate,
           studyStartTime = sub(".*\\s+", "", strptime(data_f$sessionStartTime, "%H:%M:%S")),
           OS=data_f$participantOS)
}


# get basic summary variables in task data: 
gen_basic_vars = function(data_long){
  # let's compute a few basic derived metrics which will be useful to look at
  data_long <- data_long %>%
    mutate(deltaReward = as.factor(abs(trialReward1 - trialReward2)),
           deltaEffort = as.factor(abs(round(trialEffortPropMax1 - trialEffortPropMax2,2))),
           effortChosen = ifelse(choice=="route 1", trialEffortPropMax1, trialEffortPropMax2),
           effortUnchosen = ifelse(choice=="route 1", trialEffortPropMax2, trialEffortPropMax1),
           deltaEffortSigned = effortChosen - effortUnchosen,
           higherEffChosen = as.integer((effortChosen > effortUnchosen)),
           highEffortOption = pmax(effortChosen, effortUnchosen),
           rewardChosen = ifelse(choice=="route 1", trialReward1, trialReward2),
           rewardUnchosen = ifelse(choice=="route 1", trialReward2, trialReward1),
           higherRewChosen = as.integer((rewardChosen > rewardUnchosen)),
           highRewardOption = as.factor(pmax(rewardChosen, rewardUnchosen))) %>%
    group_by(subID) %>%
    mutate(maxPressCount = round(trialEffort1[1]/trialEffortPropMax1[1]))
  
  # return
  return(data_long)
}


# check that all participants did in fact use a mobile 
# note should now be redundant as firebase game prevents use on desktop
check_device = function(data){
  # input data and get out the device the participant used from firebase
  # data is a list imported via read json function 
  # returns subjects that didn't use device
  nPpts=length(data)
  # loop ppts
  OS=list(); subs=list();
  for (n in seq(nPpts)){
    OS[[n]]=str_detect(data[[n]][1, "OS"], regex("Android|Iphone", ignore_case = T))
    
    if (OS[[n]]==FALSE){print(paste0('Warning! Participant ', n, " did not use Android or iPhone."))
      print("Returning their OS")
      print(paste0("Subject ID ", data[[n]][1, "subID"], ": ",
                   data[[n]][1, "OS"]))
      
      subs[[n]]=data[[n]][1, "subID"]
    }
  }
  return(unlist(subs))
}

# function to replicate choices from simulated parameters 
rep_choices = function(data, sim_params, softmax=1){
  # give the function some choice values and simulated parameter means
  # sim_params and data are dataframes
  # function will simulate choices of these participants from these values 
  
  # print
  print(paste0("Simulating choices for ", nrow(sim_params), " participants..."))
  
  # manipulate data
  nPpts=nrow(sim_params) # get number of participants
  nTrials=nrow(data) # get number of trials each participant actually completed
  
  nTrials_all = sim_params %>% 
    group_by(subID) %>%
    dplyr::summarize(nTrials = nTrials)
  # add sequential numeric IDs for compatibility with rstan output
  # maximum number of trials 
  nTrials_max <- nTrials_all %>%
    {max(.$nTrials)}
  
  ## create arrays of choice options and responses for each participant
  r1 <- e1 <- r2 <- e2 <-  array(1, dim = c(nPpts, nTrials_max))   # pad with 1s else stan will complain
  choice01 <- array(0, dim = c(nPpts, nTrials_max))    # pad with 0s
  for (i in 1:nPpts) {
    r1[i, 1:nTrials_all$nTrials[i]] = data$reward1
    e1[i, 1:nTrials_all$nTrials[i]] = data$effort1/10
    r2[i, 1:nTrials_all$nTrials[i]] = data$reward2
    e2[i, 1:nTrials_all$nTrials[i]] = data$effort2/10
    # simulate choices 
    v = matrix(NA, nTrials_all$nTrials[i], 2);
    # option values for each trial 
    for (t in 1:nTrials_all$nTrials[i]) {
      v[t,1] <- sim_params$sim_rewSens[i]*r1[i,t] - sim_params$sim_effSens[i]*e1[i,t];
      v[t,2] <- sim_params$sim_rewSens[i]*r2[i,t] - sim_params$sim_effSens[i]*e2[i,t];
      eta = (v[t,2]-v[t,1])
      if (softmax==1){
      beta <- runif(1, 0.5, 5) # generate a random beta between 0.5 and 5
      p_choice1 <- exp(beta * v[t, 1]) / (exp(beta * v[t, 1]) + exp(beta * v[t, 2]))
      choice01[i, t] <- Rlab::rbern(1, p_choice1)
      }
      else{
        eta = (v[t,2]-v[t,1])
        choice01[i,t] = Rlab::rbern(1,boot::inv.logit(eta));
      }
    }
  }
  # save in a list of dataframes with subID as index
  rew1 = melt(as.data.frame(cbind(sim_params$subID, r1)), id.vars=1,
              variable.name="trialNo",
              value.name="trialReward1") %>%arrange(1)
  rew1$trialNo=as.numeric(rew1$trialNo) # make the trial n numeric
  
  rew2 = melt(as.data.frame(cbind(sim_params$subID, r2)), id.vars=1,
              variable.name="trialNo",
              value.name="trialReward2") %>%arrange(1) 
  rew2$trialNo=as.numeric(rew2$trialNo) # make the trial n numeric
  
  eff1 = melt(as.data.frame(cbind(sim_params$subID, e1)), id.vars=1,
              variable.name="trialNo",
              value.name="trialEffortPropMax1") %>%arrange(1)
  eff1$trialNo=as.numeric(eff1$trialNo) # make the trial n numeric
  
  eff2 = melt(as.data.frame(cbind(sim_params$subID, e2)), id.vars=1,
              variable.name="trialNo",
              value.name="trialEffortPropMax2") %>%arrange(1)
  eff2$trialNo=as.numeric(eff2$trialNo) # make the trial n numeric
  
  sim_choice = melt(as.data.frame(cbind(sim_params$subID, choice01)), id.vars=1,
                    variable.name="trialNo",
                    value.name="choice") %>%arrange(1) %>% 
    mutate(choice01=case_when(choice==1 ~ "route 2", TRUE ~ "route 1"))
  
  sim_choice$trialNo=as.numeric(sim_choice$trialNo) # make the trial n numeric
  
  # add subID name
  names(rew1)[1]="subID"; names(rew2)[1]="subID"; 
  names(eff1)[1]="subID"; names(eff2)[1]="subID"; 
  names(sim_choice)[1]="subID";  
  
  
  # join in a long dataframe: 
  data=rew1%>% inner_join(rew2, by=c("subID", "trialNo")) %>% 
    inner_join(eff1, by=c("subID", "trialNo")) %>% 
    inner_join(eff2, by=c("subID", "trialNo")) %>% 
    inner_join(sim_choice, by=c("subID", "trialNo")) %>% arrange(subID) 
  
  # add some basic vars to help
  data <- data %>%
    mutate(deltaReward = as.factor(abs(trialReward1 - trialReward2)),
           deltaEffort = as.factor(abs(round(trialEffortPropMax1 - trialEffortPropMax2,2))),
           effortChosen = ifelse(choice=="route 1", trialEffortPropMax1, trialEffortPropMax2),
           effortUnchosen = ifelse(choice=="route 1", trialEffortPropMax2, trialEffortPropMax1),
           deltaEffortSigned = effortChosen - effortUnchosen, 
           higherEffChosen = as.integer((effortChosen > effortUnchosen)),
           highEffortOption = pmax(effortChosen, effortUnchosen),
           rewardChosen = ifelse(choice=="route 1", trialReward1, trialReward2),
           rewardUnchosen = ifelse(choice=="route 1", trialReward2, trialReward1),
           higherRewChosen = as.integer((rewardChosen > rewardUnchosen)),
           highRewardOption = as.factor(pmax(rewardChosen, rewardUnchosen))) 
  # then convert the list to long df 
  print("Saved as output.")
  return(data)
}

# quality check behavioural data     
QC_check = function(data_long){
  # data_long: long data with sess variable to indicate multiple timepoints
  # use all data and include recalibration trials 
  if (data_long$taskVer[1]=="study1"){
    # for baseline do not calculate the catchCorrect as it was 23 trials
    if(max(data_long$sess==0)){
    QC_bonus <- data_long %>%              
      group_by(subID, sess) %>%
      arrange(studyStartTime) %>%
      dplyr::summarise(attemptID=attemptID[1],
                       date=study_date[1],
                       nTrials = n(),
                       timeOnTask = round((trialStartTime[nTrials]-trialStartTime[1])/60000, 1),
                       failedTrials = sum(trialSuccess==0), 
                       nonAttempts = sum(pressCount==0),
                       prop1 = round(sum(choice=="route 1")/(nTrials[1]), 2),
                       propHigherEff=mean(higherEffChosen),
                       catchCorrect = as.integer(choice[trialIdx==13]=="route 2"),
                       totalCoins = coinsRunningTotal[nTrials],
                       bonusGBP = round(totalCoins*0.01,2),
                       maxPressCount = max(thresholdMax), 
                       nRecalibrations = sum(recalibration), 
                       recalibrationUpdate=max(thresholdMax)-maxPressCount)
    }
    else {
      QC_bonus <- data_long %>%              
        group_by(subID, sess) %>%
        arrange(studyStartTime) %>%
        dplyr::summarise(attemptID=attemptID[1],
                         date=study_date[1],
                         nTrials = n(),
                         timeOnTask = round((trialStartTime[nTrials]-trialStartTime[1])/60000, 1),
                         failedTrials = sum(trialSuccess==0), 
                         nonAttempts = sum(pressCount==0),
                         prop1 = round(sum(choice=="route 1")/(nTrials[1]), 2),
                         propHigherEff=mean(higherEffChosen),
                         catchCorrect = as.integer(choice[trialIdx==13]=="route 2"),
                         totalCoins = coinsRunningTotal[nTrials],
                         bonusGBP = round(totalCoins*0.01,2),
                         maxPressCount = max(thresholdMax), 
                         nRecalibrations = sum(recalibration), 
                         recalibrationUpdate=max(thresholdMax)-maxPressCount)
    }
  }
  # pilot1 version of this function preserved if desired:
  else if (data_long$taskVer[1]=="pilot1"){
  if (!is.na(data_long$trialIdx[1])){
    # if its a follow-up session it will have trialIdex
    # get basic quality control data and determine profilic bonuses
    QC_bonus <- data_long %>%              
      group_by(subID, sess) %>%
      arrange(studyStartTime) %>%
      dplyr::summarise(attemptID=attemptID[1],
                       date=study_date[1],
                       nTrials = n(),
                       timeOnTask = round((trialStartTime[nTrials]-trialStartTime[1])/60000, 1),
                       failedTrials = sum(trialSuccess==0), 
                       nonAttempts = sum(pressCount==0),
                       prop1 = round(sum(choice=="route 1")/(nTrials[1]), 2),
                       propHigherEff=mean(higherEffChosen),
                       catchCorrect1 = as.integer(choice[trialIdx==0]=="route 2"),
                       catchCorrect2 = as.integer(choice[trialIdx==10]=="route 1"),
                       totalCoins = coinsRunningTotal[nTrials],
                       bonusGBP = round(totalCoins*0.01,2),
                       maxPressCount = round(trialEffort1[1]/trialEffortPropMax1[1]), 
                       nRecalibrations = sum(recalibration), 
                       recalibrationUpdate=max(thresholdMax)-maxPressCount) # delta: recalibration and practice calibration
  } else {
    # it's baseline session: 
    QC_bonus <- data_long %>%              
      group_by(subID, sess) %>%
      arrange(studyStartTime) %>%
      dplyr::summarise(attemptID=attemptID[1],
                       date=study_date[1],
                       nTrials = n(),
                       timeOnTask = round((trialStartTime[nTrials]-trialStartTime[1])/60000, 1),
                       failedTrials = sum(trialSuccess==0), 
                       nonAttempts = sum(pressCount==0),
                       prop1 = round(sum(choice=="route 1")/(nTrials[1]), 2),
                       propHigherEff=mean(higherEffChosen),
                       catchCorrect1 = as.integer(choice[4]=="route 1"),
                       catchCorrect2 = as.integer(choice[17]=="route 2"),
                       totalCoins = coinsRunningTotal[nTrials],
                       bonusGBP = round(totalCoins*0.01,2),
                       maxPressCount = round(trialEffort1[1]/trialEffortPropMax1[1]), 
                       nRecalibrations = sum(recalibration), 
                       recalibrationUpdate=max(thresholdMax)-maxPressCount) # delta: recalibration and practice calibration
  }
  }
  # find any duplicated subjects
  duplicateSub = QC_bonus$subID[duplicated(QC_bonus$subID)]

  
  # output QC bonus and missing subs
  out=list(QC_bonus=QC_bonus, 
           missing_subs = data_long %>% 
             filter(!subID %in% QC_bonus$subID) %>% select(subID) %>% distinct(),
           duplicate_subs = duplicateSub)
  return(out)
}


# similar to rewEff_predictive accuracy but for test retest models (two timepoints)
post_predictive_acc_retest = function(fit, data_long, model_data){
  # filter the data for the specific trials and N
  data_long = filter(data_long, subID %in% model_data$subID,
                     trialNo %in% model_data$sample_trials)
  nPpts <- length(unique(data_long$subID))
  nTimes <- max(data_long$sess)
  subIDs <- data_long %>%
    group_by(subID) %>%
    dplyr::summarize() %>%
    mutate(ID=seq(1, nPpts, 1))     # add sequential numeric IDs for compatibility with rstan output
  data_long <- merge(data_long, subIDs, by="subID") %>%
    mutate(choice01=recode(choice, "route 1"=0, "route 2"=1))
  ## time 1 
  # observed data
  y1 <- data_long$choice01[data_long$sess==1]
  
  post_pred_t1 <- as.data.frame(rstan::summary(fit, pars = c("post_pred_t1"))$summary) %>%
    filter(mean>=0) %>%   #remove padded values for trials ppts didn't actually complete
    select(`50%`) %>%
    add_rownames(var = "var") %>%
    separate(var, sep="\\[", into=c("variable","tmp"), remove=TRUE, extra="drop") %>%
    separate(tmp, sep=",", into=c("subID","trial"), remove=TRUE, extra="drop") %>%
    separate(trial, sep=-1, into="trial", extra="drop") %>%
    mutate(subID=as.numeric(subID)) %>%
    arrange(subID) %>%
    dplyr::rename(mean_predicted = `50%`)
  
  # predictive accuracy (per sub)
  y_rep <- cbind(post_pred_t1, observed=y1)
  accs <- y_rep %>%
    group_by(subID) %>%
    mutate(acc = (mean_predicted==observed)) %>%
    summarise(mean_acc = mean(acc))
  
  # predictive accuracy (overall)
  summ <- accs %>%
    summarise(mean_pred_acc=mean(mean_acc), sd_pred_acc=sd(mean_acc))
  print(paste0("N = ", nPpts, " posterior predictive accuracy (session 1): ", round(summ[1], 2)), quote=FALSE)
  print(summ)
  
  ## time 2
  # posterior predicted data
  post_pred_t2 <- as.data.frame(rstan::summary(fit, pars = c("post_pred_t2"))$summary) %>%
    filter(mean>=0) %>%   #remove padded values for trials ppts didn't actually complete
    select(`50%`) %>%
    add_rownames(var = "var") %>%
    separate(var, sep="\\[", into=c("variable","tmp"), remove=TRUE, extra="drop") %>%
    separate(tmp, sep=",", into=c("subID","trial"), remove=TRUE, extra="drop") %>%
    separate(trial, sep=-1, into="trial", extra="drop") %>%
    mutate(subID=as.numeric(subID)) %>%
    arrange(subID) %>%
    dplyr::rename(mean_predicted = `50%`)
  # observed data
  y2 <- data_long$choice01[data_long$sess==2]
  
  # predictive accuracy (per sub)
  y_rep2 <- cbind(post_pred_t2, observed=y2)
  accs2 <- y_rep2 %>%
    group_by(subID) %>%
    mutate(acc = (mean_predicted==observed)) %>%
    summarise(mean_acc = mean(acc))
  
  # predictive accuracy (overall)
  summ2 <- accs2 %>%
    summarise(mean_pred_acc=mean(mean_acc), sd_pred_acc=sd(mean_acc))
  print(paste0("N = ", nPpts, " posterior predictive accuracy (session 2): ", round(summ2[1], 2)), quote=FALSE)
  print(summ2)
  
}
# 


# create an rstan like posterior object from a set of cmdstanr-outputted files 
make_posterior <- function(files, num_warmup = -1){
  
  # read first line to get parameter (column) names
  params <- names(data.table::fread(files[1], skip = 38, nrow = 0))
  
  # skip warmup draws if they were saved
  skip <- ifelse(num_warmup > 0, num_warmup + 40, 38)
  
  # bind together all the chains, skipping warm-up if necessary
  chains <- lapply(files, function(file){
    chain <- suppressWarnings(data.table::fread(file, skip = skip))
    names(chain) <- params
    chain
  }) %>%
    bind_rows()
  
  iter <- nrow(chains)
  
  # figure out highest dimension in parameters
  max_dim <- max(str_count(params, "\\."))
  
  # extract parameter names and indices
  param_dims <- data.frame(param = params) %>%
    separate(param, into = c('param', paste0('dim', 1:max_dim)), 
             sep = "\\.", convert = TRUE, fill = "right") %>%
    group_by(param) %>%
    summarize_all(max)
  
  posterior <- list()
  
  # loop over parameters
  for(i in 1:nrow(param_dims)){
    
    param <- param_dims %>%
      slice(i) %>%
      pull(param)
    
    dims <- param_dims %>%
      slice(i) %>%
      select(-param) %>%
      as.numeric()
    
    # reshape draws into array defined by dims
    draws <- chains %>%
      select(which(str_detect(names(.), param))) %>%
      as.matrix() %>%
      array(c(iter, dims[!is.na(dims)]))
    
    posterior[[param]] <- draws
  }
  
  posterior
}


# calculate how many thresholds were exceeded
calcThresholdExceeded = function(data, secOffset=1){
  # calculates how many times a participant exceeded the threshold 
  # according to formula 
  # the press time (s) is offset by secOffet(1 second default)
  # to represent a reasonable buffer participants use during effort exertion 
  # to guarantee success before the maximum time elaspes. 
  # I.e., estimated threshold is considered exceeded if
  # (press time (s) + 1 (s)) < (chosen effort % /10).
  
  data = data %>% 
    mutate(exceedThresh = ifelse((pressTimesTot/1000)+secOffset < (effortChosen*10), 1, 0))
  # summarise by person
  sumThresh = data %>% group_by(subID, trialNo) %>%
    summarise(sess = sess[1],
              nExceedThresh = exceedThresh[1]) %>%
    ungroup() %>% 
    summarise(nTrials = n(),
              propExceed = sum(nExceedThresh)/n())
    
    
  
  message(paste0("mean prop of exceeded (estimated) threshold trials: ",
               round(mean(sumThresh$propExceed, na.rm=T),2)))

  return(sumThresh)
  

}


# extract mixed effect model statistics into a table 
extractModelStats <- function(model, name="") {
  # Extract fixed effects coefficients and p-values
  fixed_effects <- summary(model)$coefficients
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  
  # Extract AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # Calculate R-squared statistics using the performance package (if available)
  r_squared <- tryCatch(
    performance::model_performance(model),
    error = function(e) NA
  )
  
  # Create a data frame for summary statistics
  model_summary <- data.frame(
    "Fixed_Effect" = rownames(fixed_effects),
    "Estimate" = fixed_effects[, "Estimate"],
    "df" = fixed_effects[, "df"],
    "P_Value" = p_values
  )
  
  # Add AIC and BIC to the summary
  model_summary <- rbind(
    model_summary,
    data.frame("Fixed_Effect" = "AIC", "Estimate" = aic, df=NA, "P_Value" = NA),
    data.frame("Fixed_Effect" = "BIC", "Estimate" = bic, df=NA, "P_Value" = NA)
  )
  
  # Add R-squared statistics to the summary if available
  if (!is.na(r_squared)) {
    model_summary <- rbind(
      model_summary,
      data.frame("Fixed_Effect" = "Conditional_R2", "Estimate" = r_squared$R2_conditional, df=NA,  "P_Value" = NA),
      data.frame("Fixed_Effect" = "Marginal_R2", "Estimate" = r_squared$R2_marginal, df=NA,  "P_Value" = NA)
    ) %>%
      dplyr::rename("fixed effect / model statistic"="Fixed_Effect") 
  }
  
  # round numeric variables
  model_summary[, c("Estimate", "P_Value")] = round(model_summary[, c("Estimate", "P_Value")], 4)
  
  # Format p-values as bold if they are below 0.05
  model_summary$P_Value <- ifelse(model_summary$P_Value < 0.05, 
                                  sprintf("**%.4f**", model_summary$P_Value), 
                                  sprintf("%.4f", model_summary$P_Value))
  
  # Convert the data frame to a pretty table using kable
  model_summary_table <- kable(model_summary, format = "markdown", caption=name)
  
  return(model_summary_table)
}





  
