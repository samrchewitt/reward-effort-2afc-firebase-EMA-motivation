# utility functions for projects

# quick load common packages 
ema_packages <- function() {
  packages=c("")
  library(nlme); library(esmpack);
  library(psych); library("entropy"); library("zoo"); library(magrittr);
  library(jtools); library(mlVAR)
  done <- "EMA packages loaded."
  return(done)
}

plot_packages <- function() {
  library(gridExtra); library(kableExtra); library(ggplot2); library(corrplot); 
  library(jtools); library(grid); library(ggpubr)
  done <- "Plotting packages loaded."
  return(done)
}

fa_packages <- function() {
  library(psych); library(EFAtools); library(nFactors)
  library(lavaan); library(GPArotation); library(polycor); library(dplyr); library(EFAtools)
  done <- "Factor analysis packages loaded."
  return(done)
}

network_packages <- function() {
  library(mlVAR); library(igraph); library(pompom);
  library(LncPath); library(qgraph); library(ctnet);
  library(ctsem); library(bootnet); library(psychonetrics);
  done <- "Network packages loaded."
  return(done)
}

# add eta squared effect size to anova table 
eta2_effects = function(anova_table){
  # effect sizes are not always given so calculate manually from ss results
  
  # input/ example use
  # anova_result <- aov(y ~ x*a data = data)
  # anova_table = summary(anova_result)
  # anova_table = eta2_effects(anova_table)
  
  # Calculate total sum of squares
  total_ss <- sum(anova_table[[1]]$`Sum Sq`)
  # Calculate eta squared for each effect
  eta_squared <- anova_table[[1]]$`Sum Sq` / total_ss
  # Add the eta squared values to the ANOVA table
  anova_table[[1]]$eta_squared <- round(eta_squared,2)
  
  return(anova_table)
}


# plots distributions 
check_distributions <- function(data, vars) {
  raw_plots <- list(); ax.x <- list(); ax.y <-list()
  for (v in seq(vars)){
    raw_plots[[v]]<-ggplot(data, aes_string(x=data[, vars[v]])) + 
      geom_bar(alpha=.5,show.legend = FALSE) + theme_classic(base_size = 14) + 
      labs(title=vars[v], x="score")
    names(raw_plots) <- c(vars[v])
    # add axes:
  }
  return(raw_plots)
}

# get_icc values 
get_icc <- function(data, vars, group_var="user_id") {
  res <- list(); icc<-list()
  if(names(data)[1]=="subID"){names(data)[1]=group_var}
  for (v in seq(vars)){
    # build formula
    fm <- as.formula(paste(vars[v], "~ sess"))
    # run lme 
    res[[v]] <- lme(fm, random = ~1|user_id , data=data, na.action=na.omit)
    #save ICC using varcorr
    icc[[v]]<-round(as.numeric(VarCorr(res[[v]]))[1] / (as.numeric(VarCorr(res[[v]]))[1] + 
                                                          as.numeric(VarCorr(res[[v]]))[2]), digits=2)
  }
  icc<-as.data.frame(icc)
  colnames(icc)<-c(vars)
  return(icc)
}

# run several glms at once 
glm_base <- function(data, y, x){
  mdl <- list();
  for (v in seq(y)){
    # build formula
    fm<-as.formula(paste(y[v], paste(x, collapse=" + "), sep=" ~ "))
    # run glm
    mdl[[v]]<- lm(fm, data=data)
  }
  names(mdl)<-c(y)
  return(mdl)
}

# plot glm coefficients 
plot_glm_base = function(res, dv){
  coeff <- as.data.frame(summary(res[[dv]])$coefficients)
  
  figure = ggplot(res[[var]], aes(x=Item, y=Estimate)) + geom_bar(stat="identity", color="black",fill=ISmatmixed$Colour ,position=position_dodge(), size=1.2) +
    geom_errorbar(aes(ymin=Estimate-Std..Error, ymax=Estimate+Std..Error), width=.2,position=position_dodge(.9),size=1.2) +
    ylim(-0.7, 0.6)+labs(title="Information Seeking", x="", y = "Regression Estimates ") + 
    theme(text = element_text(family = "Helvetica"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.y= element_text(colour = "black",size=20),axis.title.y = element_text(size=20),axis.title.x = element_blank(), 
          plot.title = element_text(size=24,face = "bold"), axis.text.x  = element_text(colour = "black",size=20,angle = 60,vjust = 1, hjust = 1), panel.spacing = unit(2, "lines"),legend.position = "none")+ 
    geom_hline(yintercept = 0,size=1.2)
  
}

# plot multivariate observations along a timeseries 
plot_multivariate<- function(data, x="obs", vars, group){
  p<-ggplot(data = data,
            aes_string(x = x, group= "user_id")) +
    #first variable
    for (v in seq(vars)){
      p<- p + geom_line(aes(y=vars[[v]], color=v))  
    } +
    #plot layouts
    scale_x_continuous(name="Observation") +
    scale_y_continuous(name="EMA") +  
    theme_classic() +
    theme(axis.title=element_text(size=14),
          axis.text=element_text(size=6),
          plot.title=element_text(size=14, hjust=.5)) +
    facet_wrap(~user_id, ncol=2) +
    ggtitle(paste0(length(vars), " EMA variables"))
  print(p)
}

# mode games 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# get group means of a set of variables in a loop using dplyr 
# programatically 
group_means = function(data, .group_var, .variables){
  summary_data <- vector("list", length(.variables))
  .group_var=rlang::sym(.group_var)
  
  for (v in .variables) {
    summary_data[[v]] <- data %>%
      group_by(!! .group_var) %>% 
      summarise(n = n(),
                min = min(get(v), na.rm = TRUE),
                max = max(get(v), na.rm = TRUE),
                median = median(get(v), na.rm = TRUE),
                mean = mean(get(v), na.rm=TRUE),
                sd = sd(get(v), na.rm=TRUE),
                sem = sd/sqrt(n)
      ) 
  }
  # output a dataframe not a list
  
  summary_df=do.call(rbind, summary_data[-c(1:length(.variables))]) %>%
    mutate(variable=rep(.variables, each=2))
  rownames(summary_df) <- NULL
  
  return(summary_df)
}

# also calculate t test statistic and cohen's d 
group_means_test <- function(data, .group_var, .variables, independent = TRUE) {
  summary_data <- vector("list", length(.variables))
  group=.group_var;
  .group_var <- rlang::sym(.group_var)
  
  for (v in .variables) {
    if (independent) {
      group1 <- data %>%
        filter(!! .group_var == 0) %>%
        pull(v)
      
      group2 <- data %>%
        filter(!! .group_var == 1) %>%
        pull(v)
      
      t_test <- t.test(group1, group2)
      
      cohens_d <- effsize::cohen.d(group1, group2, na.rm=T)
    } else {
      paired_data <- data %>%
        filter(!! .group_var == 0) %>%
        select(.group_var, v) %>%
        bind_rows(data %>%
                    filter(!! .group_var == 1) %>%
                    select(.group_var, v))
      
      t_test <- t.test(paired_data$v ~ paired_data$.group_var, paired = TRUE)
      
      cohens_d <- effsize::cohen.d(paired_data$v ~ paired_data$.group_var, na.rm=T)$estimate
    }
    
    summary_data[[v]] <- data.frame(
      group = group,
      variable = v,
      n = c(length(group1), length(group2)),
      min = c(min(group1, na.rm = TRUE), min(group2, na.rm = TRUE)),
      max = c(max(group1, na.rm = TRUE), max(group2, na.rm = TRUE)),
      median = c(median(group1, na.rm = TRUE), median(group2, na.rm = TRUE)),
      mean = c(mean(group1, na.rm = TRUE), mean(group2, na.rm = TRUE)),
      sd = c(sd(group1, na.rm = TRUE), sd(group2, na.rm = TRUE)),
      sem = c(sd(group1, na.rm = TRUE) / sqrt(length(group1)), 
              sd(group2, na.rm = TRUE) / sqrt(length(group2))),
      p.value = t_test$p.value,
      Cohen_d = cohens_d$estimate
    )
  }
  
  # Combine the t-test and group means results into a dataframe
  summary_df <- do.call(rbind, summary_data)
  rownames(summary_df) <- NULL
  
  return(summary_df)
}


# calculate summary statistics variability for EMA data 
calc_variability = function(data, raw_vars, base_vars=NULL, # inputs
                            #options:
                            log=TRUE, entropy=TRUE){
  
  # data = df; raw_vars = vars to calculate variability on
  # base_vars = baseline (time-invariant) vars e.g., traits to include in the output table 
  # options: 
  # log==True (calculate the log values for MSSD & RMSSD as well)
  # entropy = True (calculate sample entropy using pracma package)
  
  # outputs
  #data_base: data_frame containing calculated variables
  
  # MSSD and RMSSD
  # initialise
  mssd_ema=vector(mode='list'); rmssd_ema=vector(mode='list');
  normality=vector(mode='list')
  # loop through vars; and calculate for each person
  message('calculating MSSD and RMSSD....')
  for (x in 1:length(seq(raw_vars))){
    # MSSD
    mssd_ema[x] <-list(mssd(data[, raw_vars[x]], 
                            group=data[, "user_id"], 
                            lag=1,
                            na.rm=TRUE))
    rmssd_ema[x] <-list(rmssd(data[, raw_vars[x]], 
                              group=data[, "user_id"], 
                              lag=1,
                              na.rm=TRUE)) 
    
  }
  
  #make df from lists
  
  mssd_data<-as.data.frame(mssd_ema); rmssd_data <- as.data.frame(rmssd_ema)
  #names:
  colnames(mssd_data) <-paste(raw_vars, "MSSD", sep = "_")
  colnames(rmssd_data) <-paste(raw_vars, "RMSSD", sep = "_")
  #merge them
  data_ssd <- cbind(mssd_data, rmssd_data)
  
  # if you want to calculate log values
  if (log==TRUE){
    message('adding logMSSD....')
    
    #get log MSSD 
    for (x in seq(raw_vars)){
      data_ssd[, paste0(raw_vars[x], "_logMSSD")]<-log(data_ssd[, paste0(raw_vars[x], "_MSSD")])
    }
    #replace inf and nan to 0
    data_ssd<-do.call(data.frame,lapply(data_ssd, function(x) replace(x, is.infinite(x),0)))
    data_ssd <-do.call(data.frame,lapply(data_ssd, function(x) replace(x, is.na(x),0)))
    
  }
  
  #split data for individuals
  data.sp <- split(data, data$user_id)
  # if entropy is wanted
  if (entropy==TRUE){
    message('calculating sample entropy...')
    ## calculate sample entropy:
    data.sp.sample_E=vector(mode='list'); #initialise
    #sample entropy is less affected by vector length 
    for (v in raw_vars){
      data.sp.sample_E[[v]] <- lapply(seq(data.sp), function(i) {
        pracma::sample_entropy(array(unlist(data.sp[[i]][v])))}) 
    }
    #edit names:
    names(data.sp.sample_E) <- paste(names(data.sp.sample_E), "sample_E", sep = "_") # add SE suffix
    
  }
  # save the data together
  # get baseline qs and means
  if (!is.null(base_vars)){idx<-c("user_id", paste0("m", raw_vars), paste0("sd", raw_vars), base_vars)} #columns wanted 
  else {idx<-c("user_id", paste0("m", raw_vars))}
  tmp=vector(mode='list'); #initialise
  for (i in seq(data.sp)) {
    tmp[[i]]<-(data.sp[[i]][idx][1,])
    #take only first row
  }
  #concatenate tmps:
  data_base<-dplyr::bind_rows(tmp, .id = "id")
  #append variability measurements to baseline questionnaires
  for (v in seq(raw_vars)){
    data_base[, names(mssd_data[v])] <- mssd_data[v]
    if (entropy==TRUE){data_base[, names(data.sp.sample_E[v])] <-array(unlist(data.sp.sample_E[v]))}
    data_base[, names(rmssd_data[v])] <- rmssd_data[v]
  }
  # also append log values:
  data_log <- data_ssd %>% dplyr:: select(contains("log"))
  data_base[, names(data_log)] <- data_log
  #replace inf and nan to 0
  data_base<-do.call(data.frame,lapply(data_base, function(x) replace(x, is.infinite(x),0)))
  
  message('done')
  # return
  return(data_base)
}


# calc_mean wrapper
# calculate the mean across study of EMA variables for each subject
EMA_calc_mean <- function(data, vars, expand_op=T){
  # a wrapper function to calculate for many variables at once
  # inputs: data and variables you wish to calculate means on
  # note uses esmpack calc.mean function 
  # expand_op is passed to calc.mean
  
  for (v in seq(vars)){
    
    data[, paste0('m', vars[v])] = esmpack::calc.mean(get(vars[v]), 
                                                      user_id,
                                                      data=data,
                                                      expand=expand_op)
    
    # also the sd 
    data[, paste0('sd', vars[v])] = esmpack::calc.fun(get(vars[v]), 
                                                      user_id,
                                                      data=data,
                                                      FUN=sd,
                                                      expand=expand_op, na.rm=T)
  }
  # clean the names for mean
  mmean_cols <- grep("^mmean", names(data), value = TRUE)
  # remove mmean from each col
  new_names <- gsub("^mmean", "", mmean_cols) 
  names(data)[names(data) %in% mmean_cols] <- paste0("m", new_names) # assign the new names to the matching columns
  
  # clean names for sd
  #mmean_cols <- grep("^mean", names(data), value = TRUE)
  # remove mmean from each col
  #new_names <- gsub("^mmean", "", mmean_cols) 
  #names(data)[names(data) %in% mmean_cols] <- paste0("m", new_names) # assign the new names to the matching columns
  # output new data
  return(data)
  
}


normalise_predictors <- function(data, predictors, between=1, drop=F){
  # Wrapper function for z-scoring a number of predictors
  # Inputs:
  # data: dataframe
  # predictors: variables to z-score 
  # between: between or within person z-score (default = between=1)
  # drop: drop the raw values from the output dataframe (default = F)
  
  if (between == 1) {
    print(paste0('Calculating between-subjects z-score for ', length(predictors), ' predictors...'))
    
    # Loop through predictors and calculate the z-score
    for (v in predictors) {
      z_column_name <- paste0("z", v)
      data[[z_column_name]] <- (data[[v]] - mean(data[[v]], na.rm = TRUE)) / sd(data[[v]], na.rm = TRUE)
    }
  } else {
    print(paste0('Calculating within-subjects z-score for ', length(predictors), ' predictors...'))
    
    # Loop through predictors and calculate within-subjects z-scores
    for (v in predictors) {
      z_column_name <- paste0("z", v)
      lagged_column_name <- paste0("zL", v)
      
      data[[z_column_name]] <- ave(data[[v]], data$user_id, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
      data[[lagged_column_name]] <- ave(data[[v]], data$user_id, FUN = function(x) c(NA, head(x, -1))) # Adding lagged value
    }
  }
  
  if (drop == TRUE) {
    # Drop raw variables if requested
    print('Dropping raw variables...')
    data <- data[ , !(names(data) %in% predictors)]
  }
  
  return(data)
}

# visualise raw ema distributions
plot_distributions <-function(data, items, title){
  # inputs: 
  # data to plot
  # items to select
  # title for the plot
  
  # gather the data and plot the items together to visualise the distributions 
  ggplot(gather(data[,items], Item, value), aes(x = value)) +
    geom_density(aes(color=Item), alpha=0.6) + labs(x='score', 
                                                    title = title) + 
    theme_classic()
  
}


# simulations: 
simR_wrapper = function (formula, beta, data, options){
  # a wrapper function to use simR to estimate power to 
  # detect fixed effect in mixed effect models
  # inputs:
  # formula (str): a mixed effect model a model which will be fit with lmer 
  # beta (str): the specific predictor to test e.g., 'zTotal' or 'zTotal*Apathy'
  
  # additional options: 
  # N (int): how many subjects to test power up to (default=100)
  # t (int): how many timepoints within subjects to test power up to (default=14)
  # power: desired level of power (default=0.8)
  # observed (boolean): 
  # take the observed effect size (True) 
  # or take the lower 2.5% CI as a conservative estimate (False); default
  
  # fixef: specify the fixed effect size directly (default=NULL)
  # specify default options
  defaults = list(N = 100, t = 14, power=0.9, observed = FALSE, fixedf = NULL, lm=F)
  options = modifyList(defaults, options)
  # Extract the options
  N <- options$N
  t <- options$t
  power <- options$power
  observed <- options$observed
  fixedf <- options$fixedf
  lm <- options$lm
  
  # turn off progress because it's ugly 
  #simrOptions(progress=F)
  
  # fit the model according to the formula 
  message(paste0('Estimating power to detect ', beta, ' in ', formula))
  if (lm==F){
    model=lmer(as.formula(formula), data=data, REML=F, control = lme4::lmerControl(check.nobs.vs.nRE = 'ignore'))
  }
  else{
    model=lm(as.formula(formula), data=data)
  }
  # if not observed power, adjust fixed effect to be the lower ci 
  if (observed==F && is.null(fixedf)){ 
    message('manually adjusted effect size to observed 2.5% CI')
    # catch error: replace '*' with ':'
    if (grepl("\\*", beta)) {beta = gsub("\\*", ":", beta)}
    fixef(model)[beta] = confint(model)[beta, "2.5 %"]
  } 
  else if (observed==F && !is.null(fixedf)) {
    message(paste0('manually adjust effect size to: ', fixedf))
    if (grepl("\\*", beta)) {beta = gsub("\\*", ":", beta)}
    fixef(model)[beta] = fixedf
  }
  # 1. increase the sample along N 
  model_extN <- extend(model, along="user_id", n=N)
  
  # plot the power curve 
  message(paste0('fitting power curve up to ', N, ' simulated subjects....'))
  pc1 = suppressMessages(powerCurve(model_extN, along="user_id", test=fixed(beta)))
  message('done.')
  # correct an error in the df
  pc1$xval <- pc1$nlevels
  
  
  # save for later
  out=list();
  out$options = options
  out$n$model=model_extN
  out$n$formula=formula
  out$n$beta = beta
  out$n$powercurve = pc1
  
  
  # 2. increase sample along T within user_id
  model_extT <- extend(model, within=paste0(beta, "+user_id"), n=14)
  # plot the power curve 
  message(paste0('fitting power curve up to ', t, ' simulated timepoints...'))
  pc2 <- suppressMessages(powerCurve(model_extT, 
                                     within=paste0(beta,"+user_id"),  test=fixed(beta),
                                     breaks=1:14))
  
  powerSim(model, fixed(beta, "lr"), nsim=100)
  
  
  message('done.')
  
  # save for later
  out$t$model=model_extT
  out$t$formula=formula
  out$t$beta = beta
  out$t$powercurve = pc2
  
  # plot together
  plot(pc1)+title(paste0('Simulated power based on subjects'))+plot(pc2)+title(paste0('Simulated power based on timepoints within subjects'))
  
  # return the output
  message('all done.')
  return(out)
}

# plot power output from mixedpower package doesn't work 
# so I adapted for my needs below
# source: https://rdrr.io/github/DejanDraschkow/mixedpower/src/R/plot_functions.R
powerPlot = function(power, x_label="sample size (N)", ylims=c(0.5,1), power_lim = 0.95, 
                     var_labels=c("discovery mean", "discovery 2.5% CI"),
                     titles=NULL){
  # input is a dataframe output from mixedpower::mixedpower() 
  # optional plot graphics: x_label, power_lim (where to mark desired power target)
  # var_labels: for the colours indicating the effects passed to mixedpower()
  # outputs a pretty plot showing this 
  
  # reshape into long format for ggplot
  power_all <- reshape2::melt(power,
                              id.vars = c("mode", "effect"),
                              variable.name = "sample_size") %>%
    filter(grepl("zTotal|zTotal:zAES_Total", effect)) 
  
  # ------------------------------------------#
  # 2. create plot for every effect
  # how many plots do we need? and which effects are they?
  fixeffects <- unique(power_all$effect)
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9")
  
  # collect plots
  all_plots <- list()
  
  # if no title specified, use the fixed eff
  if(is.null(titles)){titles=fixeff}
  i <- 1 # index loop
  for (fixeff in fixeffects){
    
    # create subset of plot
    plot_data <- subset(power_all,  effect == fixeff,
                        select = c(mode, sample_size,value))
    
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sample_size, y = value, color=mode, group = mode)) + # basic graph
      ggplot2::geom_point() + # add points
      ggplot2::geom_line()+
      ggplot2::geom_hline(yintercept = power_lim, linetype = "dotted") + # add horizontal line to indicate
      ggplot2::xlab(x_label) + ggplot2::ylab("power") +  # change name of x and y axes
      ggplot2::theme(axis.line = ggplot2::element_line(color = "black"), # axis line
                     panel.background = ggplot2::element_rect(fill = "white"), # background
                     legend.title = ggplot2::element_blank()) + # remove legend title 
      #labs(linetype = "condition", color = "condition") +
      #scale_color_viridis(discrete = T, option = "inferno")  +  # color
      ggplot2::scale_color_manual(values = cbPalette, labels=var_labels) +
      ggplot2::expand_limits(y = ylims) + # fix axis to 0 - 1
      ggplot2::ggtitle(titles[i])
    
    # store plot
    all_plots[[i]] <- p
    i <- i+1
  }
  
  # plot with multiplot from mixedpower
  mixedpower::multiplot(plotlist = all_plots)
}


# source: https://github.com/mikojeske/Nicolas_Leenaerts_Scripts/blob/main/R/ESM/Psychometric_properties/within_between_alpha.R
# a function to calculate within and between person cronbach alpha
within_between_alpha <- function(data,participant,check.keys=F){
  # Load libraries
  require(psych)
  require(dplyr)
  
  # Get total data alpha (no averaging data for each participant)
  total_alpha = alpha(as.data.frame(lapply(select(data,-participant),as.numeric)),check.keys=check.keys)$total
  
  # Get between-person alpha (after data is averaged for each participant)
  between_person_alpha = alpha(as.data.frame(select(aggregate(lapply(select(data,-participant),as.numeric),by=data[participant],mean,na.rm=T),-participant)),check.keys=check.keys)$total
  
  # Get within-person alpha (on the data of each individual participant)
  within_person_alphas = apply(unique(data[participant]),1,function(x){
    select(subset(data,data[participant]==x),-participant)
    alpha(as.data.frame(lapply(select(subset(data,data[participant]==x),-participant),as.numeric)),check.keys=check.keys)$total
  })
  
  # Create results dataframe
  results = data.frame(matrix(nrow=4,ncol=length(between_person_alpha)+1))
  colnames(results) = c('type',colnames(between_person_alpha))
  
  # Store results
  results$type = c('total','between_person','within_person_mean','within_person_median')
  results[1,2:(length(between_person_alpha)+1)] = total_alpha
  results[2,2:(length(between_person_alpha)+1)] = between_person_alpha
  results[3,2:(length(between_person_alpha)+1)] = apply(bind_rows(within_person_alphas),2,function(x) mean(x,na.rm=T))
  results[4,2:(length(between_person_alpha)+1)] = apply(bind_rows(within_person_alphas),2,function(x) median(x,na.rm=T))
  
  # Return results
  return(results)
}



# plot the half-cauchy prior on sigma group level
half_cauchy_density <- function(x, scale) {
  2 / (pi * scale * (1 + (x / scale)^2))
}

half_cauchy_prior_distribution <- function(scales) {
  x_values <- seq(0, 5, length.out = 1000)
  plot_data <- expand.grid(x = x_values, scale = scales)
  plot_data$density <- mapply(half_cauchy_density, plot_data$x, plot_data$scale)
  
  return(ggplot(plot_data, aes(x = x, y = density, color = factor(scale))) +
           geom_line() +
           labs(title = "Half-Cauchy Distributions with Varying Scales",
                x = "x", y = "Density") +
           theme_minimal())
}



# extract the first element or handle character patterns
extract_first_element <- function(x) {
  if (is.list(x)) {
    if (length(x) > 1) {
      # If there are multiple values, return the first one
      return(x[1])
    }
  } else if (is.character(x)) {
    # Check for the pattern 'c(x, y)' in character strings
    pattern <- "\\bc\\((\\d+),\\s*(\\d+)\\)\\b"
    matches <- regmatches(x, gregexpr(pattern, x))
    
    if (length(matches) > 0 && !all(sapply(matches, function(match) length(match) == 0))) {
      # Extract the first number if the pattern is found
      first_number <- as.numeric(gsub(pattern, "\\1", matches[[1]]))
      return(first_number)
    }
  }
  # If not a list, character string with the pattern, or has only one value, return as is
  return(x)
}

# plot manual significance with ggplot if you can't use geom_Signif or something else
geomPlotManualSig <- function(x, xend, y, yend=NULL, label, size = 1, color = "black", text_size = 6, text_color = "black") {
  # Automatically calculate yBracketOffset as a function of size (e.g., 20% of the size)
  if (is.null(yend)){
      yend <- size * 0.025  # You can adjust this factor for more or less offset
  # Adjust label's vertical position slightly above the bracket
  }
  # Calculate the midpoint for the label placement
  label_x <- (x + xend) / 2
  label_y <- y + y*0.01  
  
  # Return the necessary ggplot layers for the bracket and label with optional size and color
  list(
    geom_segment(aes(x = x, xend = xend, y = y, yend = y), color = color, size = size),  # Horizontal line
    geom_segment(aes(x = x, xend = x, y = y - yend, yend = y), color = color, size = size),  # Left vertical line
    geom_segment(aes(x = xend, xend = xend, y = y - yend, yend = y), color = color, size = size),  # Right vertical line
    geom_text(aes(x = label_x, y = label_y, label = label), color = text_color, size = text_size)  # Text label for significance
  )
}

# convert p values to significance asterics e.g., from a dataframe or vector 
convertPvalue <- function(input_data, column_name = "p.value", alphas=c(0.001, 0.01, 0.05)) {
  # Create a function to apply asterisks to p-values
  convert <- function(p) {
    if (p < alphas[1]) {
      return("***")
    } else if (p <= alphas[2]) {
      return("**")
    } else if (p <= alphas[3]) {
      return("*")
    } else {
      return("n.s.")
    }
  }
  
  # Check if input is a dataframe
  if (is.data.frame(input_data)) {
    # If input is a dataframe, assume the p-values are in the specified column
    if (!(column_name %in% colnames(input_data))) {
      stop("Column name not found in the dataframe")
    }
    input_data$sig <- sapply(input_data[[column_name]], convert)  # Apply conversion
    return(input_data)  # Return the modified dataframe with the new sig column
  }
  
  # If input is a vector, apply the conversion function and return the new vector
  if (is.vector(input_data)) {
    return(sapply(input_data, convert))  # Return the new sig vector
  }
  
  # If input is not a dataframe or vector, return an error
  stop("Input must be either a vector or a dataframe.")
}

# Function to parse contrasts and assign x and xend positions
parse_contrasts <- function(contrast_str) {
  # Print the contrast string to debug
  print(paste("Processing contrast:", contrast_str))
  
  # Extract deltaRewardB and mean_state using regular expressions
  deltaRewardB1 <- as.numeric(sub(".*deltaRewardB(\\d).*", "\\1", contrast_str))  # Extract deltaRewardB1, deltaRewardB2, etc.
  mean_state1 <- as.numeric(sub(".*mean_state([+-]?\\d*\\.?\\d+).*", "\\1", contrast_str))  # Extract mean_state (-1.5 or 1.5)
  
  # Check if extraction worked properly
  print(paste("deltaRewardB1:", deltaRewardB1, "mean_state1:", mean_state1))
  
  # Handle any failed extraction (NA values)
  if (is.na(deltaRewardB1) || is.na(mean_state1)) {
    return(c(x = NA, xend = NA))  # Return NA if extraction failed
  }
  
  # Define offsets based on mean_state values
  offset <- ifelse(mean_state1 == -1.5, -0.1, ifelse(mean_state1 == 1.5, 0.1, 0))
  
  # Calculate x and xend based on deltaRewardB1 and mean_state
  x <- deltaRewardB1 + offset  # Left side (x)
  
  # Extract the second deltaRewardB value (deltaRewardB2, deltaRewardB3, etc.) and compute xend similarly
  deltaRewardB2 <- as.numeric(sub(".*deltaRewardB(\\d).*", "\\1", sub(".*- deltaRewardB(\\d)", "\\1", contrast_str)))
  
  # Handle any failed extraction (NA values)
  if (is.na(deltaRewardB2)) {
    return(c(x = NA, xend = NA))  # Return NA if extraction failed
  }
  
  xend <- deltaRewardB2 + offset  # Right side (xend)
  
  return(c(x = x, xend = xend))  # Return x and xend positions
}


## SPM results table to word table w/ anatomical labels 
spmRes_table_gen <- function(file, custom_caption) {
  # Load necessary libraries
  library(dplyr)
  library(flextable)
  
  # Read the CSV file
  raw_tt <- read.csv(file)
  
  col_vars <- c("cluster", "cluster.2", "peak", "peak.2", "X", "X.1", "X.2")
  
  # Process the data
  tt <- raw_tt %>%
    select(all_of(col_vars)) %>%
    # Filter secondary clusters
    filter(cluster.2 != "equivk") %>%
    mutate(
      Region = "",
      Side = ifelse(X < 0, "L", "R"),
      peak.2 = as.numeric(peak.2), 
      cluster.2=as.numeric(cluster.2)
    ) %>%
    mutate_if(is.numeric, round, 2)  # Round numeric columns
  
  # Reorder and rename columns
  tt <- tt[, c("Region", "Side", "cluster.2", "peak.2", "X", "X.1", "X.2")]
  colnames(tt) <- c("Region", "Side", "Extent (voxels)", "T", "x", "y", "z")
  
  # Map MNI coordinates to region names
  Result <- as.data.frame(t(mapply(
    FUN = mni_to_region_name, template = "aal",
    x = as.numeric(tt$x), y = as.numeric(tt$y), z = as.numeric(tt$z)
  )))
  
  tt$Region <- gsub("_", " ", gsub("_([LR])$", "", Result$aal.label))
  
  tt = group_by(tt, Region, Side) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  # redefine bilaterality for bilateral clusters
  tt = tt %>% 
    group_by(Region) %>% 
    mutate(Hemisphere = ifelse(n_distinct(Side) == 2, 
                               ifelse(row_number() == 1, "bilateral", ""), 
                               ifelse(Side == "L", "L", "R")))
  
  
  # Reorder the data frame by Region and Hemisphere
  tt <- tt %>%
    arrange(Region, "T", Side) %>%
    select(Region, Hemisphere, Side, "Extent (voxels)", "T", x,y,z)

  tt <- tt %>%
    group_by(Region) %>% 
    mutate(Hemisphere = if (n_distinct(Side) == 2) {
      # For bilateral regions, add the side in parentheses
      paste0("bilateral (", Side, ")")
    } else {
      # For non-bilateral regions, leave Hemisphere as is (e.g., "L" or "R")
      Hemisphere
    }) %>%
    ungroup() %>% # finally drop side
    select(-Side)
  
  
  # Create the flextable
  tbl_out <- flextable(tt) %>%
    set_table_properties(layout = "autofit") %>%
    set_caption(
      caption = as_paragraph(custom_caption),
      style = "Table Caption"
    )
  
  # Return the flextable object
  return(tbl_out)
}

# mediation path wrapper
plot_mediation=function(labs, coeffs, plotOptions){
  # create df
  med_data <-
    data.frame(
      lab_x   = labs$x,
      lab_m   = labs$m,
      lab_y   = labs$y,
      coef_xm = coeffs$xm,
      coef_my = coeffs$my,
      coef_xy = coeffs$xy
    )
  
  med_diagram <- function(data, height = .75, width = 2, graph_label = NA, node_text_size = 20, edge_text_size = 18, color = "black", ranksep = .2, minlen = 3){
    
    require(glue)
    require(DiagrammeR)
    
    data$height  <- height   # node height
    data$width   <- width    # node width
    data$color   <- color    # node + edge border color
    data$ranksep <- ranksep  # separation btwn mediator row and x->y row
    data$minlen  <- minlen   # minimum edge length
    
    data$node_text_size  <- node_text_size
    data$edge_text_size  <- edge_text_size
    
    data$graph_label <- ifelse(is.na(graph_label), "", paste0("label = '", graph_label, "'"))
    
    diagram_out <- glue::glue_data(data,
                                   "digraph flowchart {
      fontname = Helvetica
      <<graph_label>>
      graph [ranksep = <<ranksep>>]

      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fixedsize = TRUE, width = <<width>>, height = <<height>>, fontsize = <<node_text_size>>, color = <<color>>]        
        mm [label = '<<lab_m>>']
        xx [label = '<<lab_x>>']
        yy [label = '<<lab_y>>']

      # edge definitions with the node IDs
      edge [minlen = <<minlen>>, fontname = Helvetica, fontsize = <<edge_text_size>>, color = <<color>>]
        mm -> yy [label = '<<coef_my>>'];
        xx -> mm [label = '<<coef_xm>>'];
        xx -> yy [label = '<<coef_xy>>'];
      
      { rank = same; mm }
      { rank = same; xx; yy }
      
      }

      ", .open = "<<", .close = ">>")  
    
    
    DiagrammeR::grViz(diagram_out)  
  }
  
  med_diagram(med_data)
}
