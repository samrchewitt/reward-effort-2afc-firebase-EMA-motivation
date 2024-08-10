# script to summarise & analyse previously fit models 
# S Hewitt 2023 

# Set up ----
rm(list=ls()) #clear workspace 
# load packages
packages <- c("rjson","dplyr", "tidyverse", "tidyr", "reshape2",
              "ggpmisc", "patchwork", "plotly", "devtools", "cmdstanr", "posterior", "flextable", "cowplot")
if (length(setdiff(packages, rownames(installed.packages()))) > 0 ) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only=TRUE)

# load project functions
repo_path = paste0('C:/Users/', Sys.getenv("USERNAME"),'/Documents/GitHub/reward-effort-2afc-firebase-BEX-FU/')
source(paste0(repo_path, 'analysis/functions/rew_eff_functions.R'))
source(paste0(repo_path, 'analysis/functions/util_functions.R'))
# options
options(dplyr.summarise.inform = FALSE)
options(scipen=999)

## set-up directories
data_dir <- "C:/Users/SAM/Documents/data/" # data path stem
model_dir = paste0(repo_path, "analysis/stan_fits/") # save / load stan fits
fig_dir = paste0(repo_path, "analysis/figs/") # figures 
table_dir = paste0(repo_path, "analysis/tables/") # tables 
task_data_dir <- paste0(data_dir, 'task/') # csv files for task data 

# load anon data 
long_data = read.csv(file = paste0(task_data_dir, "/long_data_anon.csv"))# first load all long data
long_data_ex = read.csv(file = paste0(task_data_dir, "/long_data_exclCatch_anon.csv"))#
quest = read.csv(file = paste0(task_data_dir, "/quest_data.csv"))
beh_data = read.csv(file=paste0(task_data_dir, "/behavioural_data_CFA_anon.csv"))

## Key variables 
# DeltaEffortChosen = behavioural y-variable mean(effort chosen - unchosen)
# Total = sum(motivation items) i.e., state motivation 

# calculate summary of games completed
summaryComplete = beh_data %>% group_by(user_id) %>%
  summarise(tGames = sum(!is.na(DeltaEffortChosen)),
            tStates = sum(!is.na(Total)),
            responses = sum(!is.na(delay_mins))) 

# prepare data that was fitted  
# subjective state items which were modelled
vars_to_model=c("Effort", "Motivated", "Conversations", "Plans")
# set up various parameters for joint modelling (see rewEf_functions.r)

# keep the same subjects only in long trial data 
long_data = long_data %>% filter(user_id %in% beh_data$user_id)# 
long_data_ex = filter(long_data_ex, user_id %in% beh_data$user_id)

# specify a list of data which we need to use in our joint model 
joint_inputs = list(quest_data=filter(beh_data, user_id %in% long_data_ex$user_id), # self-report EMA data
                    vars_to_model=vars_to_model) # apathy data 

# check the data that will be modelled
data_list = rewEff_prepare_retest(long_data_ex,minSess=4, gameTrials=23, joint=T, joint_inputs=joint_inputs)

# load the model fit 
fit = readRDS(file= paste0(model_dir, "rewEff-linear-bernoulli-multisess-joint-IRT-1param.rds"))
                       
# model diagnostics table ----
fit$diagnostic_summary()

# get summary stats for all key parameters
summary_stats = fit$summary(variables = c("thetaZ", "rewSens", "effSens", "mu_theta",
                                          "R_rewSens", "R_effSens", "R_theta",
                                          "sigma_rewSens", "sigma_effSens", "sigma_theta", 
                                          "beta_rew", "beta_eff", "beta_trait_rew", "beta_trait_eff", "delta"))

# show if any rhat > 1.05
rhat= as.data.frame(summary_stats) %>%
  filter(rhat >1.01) 

# tell us whether any ess are < 10% of samples (4000)
ess=as.data.frame(summary_stats) %>%
  mutate(neff_ratio_bulk = ess_bulk /  fit$metadata()$iter_sampling)

# tables ----
# 1. subject parameters
# rewSens = reward sensitivity 
# effSens = effort sensitivity 
# thetaZ = person-centred latent state 
params = c("rewSens", "effSens", "thetaZ")
params_long = summary_stats %>% 
  filter(grepl(paste0("^(", paste(params, collapse = "|"), ")\\[\\d+"), variable)) %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "t"), remove=TRUE, extra="drop") 

# get a summary table by timepoint 
params_subject_sessions = params_long %>% filter(parameter=="thetaZ") %>% group_by(t) %>% 
  summarise(across(3:11,  # Exclude the column right after 't'
                   .fns = list(mean = mean, 
                               sd = sd, 
                               sem = ~sd(.)/sqrt(length(.))))) %>%
  mutate(t = as.numeric(t)) %>% arrange(t)
# subset variables of interest for the table 
col_vars = c("mean_mean", "mean_sem", "mean_sd", "q5_mean", "q95_mean", "ess_bulk_mean", "rhat_mean")
ss_tt_table_theta = select(params_subject_sessions, all_of(col_vars)) %>%
  tibble::rownames_to_column(var="t") %>%
  mutate_if(is.numeric, round, 2) # round the numbers

  
# set flextable defaults for all tables
set_flextable_defaults(theme_fun = "theme_booktabs", font.size=10, digits=2)
# supplementary table: theta ----
StableTheta=flextable(ss_tt_table_theta) %>%
  set_table_properties(layout = "autofit") %>%
  set_caption(
    caption = as_paragraph("Posterior parameter estimates (individuals) : ", as_i("\U03B8")),
    style = "Table Caption") %>%
  compose(j = ncol(ss_tt_table_theta)-1, part = "header",
          value = as_paragraph("N",as_sub("eff"))) %>%
  # italicise some 
  italic(j = c( ncol(ss_tt_table_theta)-1,  ncol(ss_tt_table_theta)), italic = TRUE, part = "header") %>%
  #adjust column labels
  set_header_labels("mean_mean"="mean", "mean_sem"="se (mean)", "mean_sd" = "sd",
                    "q5_mean" = "5%", "q95_mean"="95%", "rhat_mean"="R\U0302")


# supplementary table: rew & effsens ----
# get a summary table by timepoint 
params_subject_sessions = params_long %>% filter(parameter %in% c("rewSens", "effSens")) %>% 
  group_by(parameter, t) %>% 
  summarise(across(  # Exclude the column right after 't'
                   .fns = list(mean = mean, 
                               sd = sd, 
                               sem = ~sd(.)/sqrt(length(.))))) %>%
  mutate(t = as.numeric(t)) %>% arrange(t)
# subset variables of interest for the table 
col_vars = c("t", "mean_mean", "mean_sem", "mean_sd", "q5_mean", "q95_mean", "ess_bulk_mean", "rhat_mean")
ss_tt_table_sens= select(params_subject_sessions, all_of(col_vars)) %>% ungroup() %>%
  mutate(Day = as.integer(t-1)*2) %>%
  select(Day, parameter, everything()) %>% arrange(parameter, Day) %>%
  mutate_if(is.numeric, round, 2)

# create table 
stableSens=flextable(select(ss_tt_table_sens, -t)) %>%
  set_table_properties(layout = "autofit") %>%
  set_caption(
    caption = as_paragraph("Posterior parameter estimates (individual level): ", as_i("rewSens, effSens")),
    style = "Table Caption") %>%
  italic(j = c("ess_bulk_mean", "rhat_mean"), italic = TRUE, part = "header") %>%
  compose(j = "ess_bulk_mean", part = "header",
          value = as_paragraph("N",as_sub("eff"))) %>%
  # italicise some 
  #adjust column labels
  set_header_labels("mean_mean"="mean", "mean_sem"="se (mean)", "mean_sd" = "sd",
                    "q5_mean" = "5%", "q95_mean"="95%", "rhat_mean"="R\U0302") %>%
  # set day as integer
  colformat_num(j = c("Day"), digits = 1) %>%
  # change labels
  compose(i = which(ss_tt_table_sens$parameter=="rewSens"), j = "parameter", as_paragraph("reward sensitivity")) %>%
  compose(i = which(ss_tt_table_sens$parameter=="effSens"), j = "parameter", as_paragraph("effort sensitivity")) 


  
# supplementary table betas ----
# parameter names to extract
params=c("beta_rew", "beta_eff", "beta_trait_rew", "beta_trait_eff")
params_beta = summary_stats %>% 
  filter(grepl(paste0(params, collapse = "|"), variable)) %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter"), remove=TRUE, extra="drop") %>%
  mutate(level=ifelse(grepl("trait", parameter),"trait","state"),
         domain = ifelse(grepl("rew", parameter),"reward","effort")) %>%
  select(parameter,level, domain, mean, sd, q5, q95, ess_bulk, rhat) %>%
  mutate_if(is.numeric, round, 2)

# create table of beta (effects)
betaT=flextable(params_beta) %>%
  set_table_properties(layout = "autofit") %>%
  set_caption(
    caption = as_paragraph("Posterior parameter estimates for effects (group): ", as_i("\U03B2")),
    style = "Table Caption") %>%
  italic(j = c("ess_bulk", "rhat"), italic = TRUE, part = "header") %>%
  compose(j = "ess_bulk", part = "header",
          value = as_paragraph("N",as_sub("eff"))) %>%
  # italicise some 
  #adjust column labels
  set_header_labels("q5" = "5%", "q95"="95%", "rhat_mean"="R\U0302")  %>%
  # change the rownames
  compose(i = which(params_beta$parameter=="beta_rew"), j = "parameter", as_paragraph(as_i("\U03B2"), as_sub("stateR"))) %>%
  compose(i = which(params_beta$parameter=="beta_eff"), j = "parameter", as_paragraph(as_i("\U03B2"), as_sub("stateE"))) %>%
  compose(i = which(params_beta$parameter=="beta_trait_rew"), j = "parameter", as_paragraph(as_i("\U03B2"), as_sub("traitR"))) %>%
  compose(i = which(params_beta$parameter=="beta_trait_eff"), j = "parameter", as_paragraph(as_i("\U03B2"), as_sub("traitE"))) 

# write all tables to 1 doc 
save_as_docx(`Table S4` = stableSens, `Table S5` = StableTheta, `Table S6` = betaT, 
             path=paste0(table_dir, "supplementaryTables_modelling.docx"))


# PPC state -----
# posterior predictive checks - check that thetaZ is correlated with observed summary stats
summary_draws = as.data.frame(fit$summary(variables = c("thetaZ", "theta"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "obs"), remove=TRUE, extra="drop") %>%
  pivot_wider(id_cols=c("subject", "obs"), names_from = c("parameter"), values_from = c("mean", "sd")) %>%
  mutate(obs=as.numeric(obs),
         user_id = rep(unique(data_list$user_id), times=max(obs))) %>%
  # join the posterior means and sds with the observed data summaries
  inner_join(select(beh_data, all_of(c("user_id", "obs", "sess", "Total", "zTotal",
                                       "zHappy", "zBA", "zSM", "zTired", "apathy"))), by=c("user_id", "obs")) 


# make a long format version for ggplot
SxPosts_long = summary_draws %>%
  pivot_longer(cols=c("zTotal", "mean_thetaZ"), values_to="value", names_to="estimate") %>%
  mutate(estimate = recode(estimate, "zTotal"="observed", "mean_thetaZ"="model"))

#  correlations by subject
subCorr = summary_draws %>% group_by(subject) %>%
  dplyr::summarize(user_id = user_id[1],
                   r = cor(zTotal, mean_thetaZ, use="pairwise.complete.obs", method="pearson"),
                   r_altHappy = cor(zHappy, mean_thetaZ, use="pairwise.complete.obs", method="pearson"),
                   r_altTired = cor(zTired, mean_thetaZ, use="pairwise.complete.obs", method="pearson"))

message("median pearson r theta (z) and observed state (z) = ", round(median(subCorr$r),2))
message("median pearson r theta (z) and observed state Happiness = ", round(median(subCorr$r_altHappy, na.rm=T),2))
message("median pearson r theta (z) and observed state fatigue = ", round(median(subCorr$r_altTired, na.rm=T),2))


# PPC rew & eff Sens -----
# individual level parameter distribution means (for computational efficiency: full posterior is slow!)
summary_sens = as.data.frame(fit$summary(variables = c("rewSens", "effSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "sess"), remove=TRUE, extra="drop") %>%
  pivot_wider(id_cols=c("subject", "sess"), names_from = c("parameter"), values_from = c("mean", "sd")) %>%
  mutate(sess=as.numeric(sess),
         user_id = rep(unique(data_list$user_id), times=max(sess)),
         missing = melt(data_list$is_missing[,,1])$value) %>%
  # join the posterior means and sds with the observed data summaries
  inner_join(summary_draws, by=c("user_id", "subject", "sess"))

# get the correlations by subject
p1s = SxPosts_long  %>%
  # get median subject
  filter(subject==59) %>%
  ggplot(aes(x = obs, group=estimate, color=estimate, shape=estimate)) +
  geom_point(aes(y=value), alpha=0.75, size=5) +
  geom_line(aes(y=value), alpha=0.75, linewidth=1.5)+
  theme_classic(base_size=26) + theme(strip.background = element_blank(),
                                      legend.position=c(0.18, 0.98),
                                      legend.background = element_blank()) +
  scale_shape_manual(values=c(20, 19))+
  labs(y="State motivation", x="observation", color="", shape="")+
  scale_color_manual(values=c("#004c6d", "#7aa6c2"))


# plot correlation between reward and effort sens 
p1= summary_sens %>%
  filter(missing == 0) %>%
  ggplot(aes(x=mean_rewSens, y=mean_effSens, group=sess, color=sess, fill=sess))  +
  geom_point()+
  geom_errorbarh(aes(xmin = mean_rewSens-sd_rewSens, xmax = mean_rewSens+sd_rewSens), alpha=.2) +
  geom_errorbar(aes(ymin = mean_effSens-sd_effSens, ymax = mean_effSens+sd_effSens), alpha=.2) +
  xlab("posterior mean reward sensitivity") + ylab("posterior mean effort sensitivity") + 
  theme_classic(base_size=28) +
  facet_wrap(~sess) + labs(color="game", fill="game")

ggsave(plot=p1,filename=paste0(fig_dir, "bivariateSensitivities.png"), 
       dpi=300, units="cm", height=30, width=30, bg="white")

# predictive accuracy -----
# get the model replicated self-report endorsements (median forces a 1 or 0 )
y_rep_state = as.data.frame(fit$summary(variables = c("y_rep"), median))  %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","N", "obs"), remove=TRUE, extra="drop") %>%
  mutate(obs = as.numeric(obs), N=as.numeric(N),
         y =  melt(data_list$Y[,])$value,
         # add the missing indicator, and rep by 4 because there are 4 items for each timepoint
         missing = rep(melt(data_list$is_missing_state[,])$value, each=4))

PPaccState <- y_rep_state %>% mutate(acc = (median==y)) %>%
  filter(missing==0) %>%
  summarise(mean_pred_acc=mean(acc), sd_pred_acc=sd(acc))

message('model posterior predictive accuracy states M/SEM = ', round(PPaccState$mean_pred_acc,3), " +/- ", round(PPaccState$sd_pred_acc/sqrt(nrow(y_rep_state)),3))

# reformat them to match the input data 
Y_rep = array(NA, dim = dim(data_list$Y))
# model replicated
for (i in 1:data_list$N) {
  for(s in 1:data_list$nStateTimes){
    Y_rep[i, s] <- with(y_rep_state, as.integer(median[N==i & obs==s]))
  }
}


# retrieve the model replicated choices
post_pred_t = as.data.frame(fit$summary(variables = c("post_pred_t"), median)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "sess", "trial"), remove=TRUE, extra="drop") %>%
  mutate(trial = as.numeric(trial), 
         sess=as.numeric(sess), 
         subject=as.numeric(subject),
         y =  melt(data_list$choice01[,,])$value,
         missing =  melt(data_list$is_missing[,,])$value) 


PPacc <- post_pred_t %>% mutate(acc = (median==y)) %>%
  filter(missing==0) %>%
  summarise(mean_pred_acc=mean(acc), sd_pred_acc=sd(acc))

message('model posterior predictive accuracy choices M/SEM = ', round(PPacc$mean_pred_acc,3), " +/- ", round(PPacc$sd_pred_acc/sqrt(nrow(post_pred_t)),3))

# plot PPC ----
source_url("https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R")

# ppc choices 
ppc_Choice_subs = post_pred_t %>%
  filter(missing==0) %>% mutate(acc = (median==y),
                                day=(as.numeric(sess)-1)*2) %>% group_by(subject, day) %>%
  summarise(mean_pred_acc=mean(acc), sd_pred_acc=sd(acc)) %>%
  ggplot(aes(x=1, y=mean_pred_acc))+
  geom_flat_violin(position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .1, aes(group=as.factor(day),
                                                                                                         color=as.factor(day), fill=as.factor(day))) +
  geom_point(aes(color=as.factor(day), fill=as.factor(day)),
             position = position_jitter(width = .05), size = 2, shape = 20, alpha=0.5) +  
  scale_colour_brewer(type = "seq") +
  scale_fill_brewer(type="seq") +
  theme_classic(base_size=28) + 
  #guides(fill="Day", color=F) + 
  labs(x= "", y="Posterior predictive accuracy", fill="Day", color="Day")+ 
  theme(legend.position=c(0.6, 0.35),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1), limits=c(0.5,1), 
                     labels=c("0.5", "0.6", "0.7", "0.8", "0.9", "1")) 

# ppc self-report endorsements where accuracy (observed==model-replicated)
ppc_yRep_subs = y_rep_state %>% 
  filter(missing==0) %>% mutate(acc = (median==y)) %>% group_by(obs) %>%
  summarise(mean_pred_acc=mean(acc), sd_pred_acc=sd(acc)) %>%
  ggplot(aes(x=1, y=mean_pred_acc))+
  ggdist::stat_dist_interval() + geom_jitter(width=0.05, alpha=0.4, size=4)+
  labs(x= "", y="Posterior predictive accuracy", color="CI")+ theme_classic(base_size=22) +
  scale_y_continuous(breaks=c(0.75, 0.8, 0.85, 0.9), limits=c(0.75,0.9)) + 
  xlim(0.9,1.1) +
  theme(legend.position=c(0.1, 0.2),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 


# pseudR2 ----
# get sum_log_lik by participant:
sum_log_lik = as.data.frame(fit$summary(variables = c("sum_log_lik"), mean, sd)) %>%
    rownames_to_column(var = "var") %>%
    separate(variable, sep="\\[|\\,|\\]", into=c("parameter", "subject", "session"), remove=TRUE, extra="drop") %>%
    pivot_wider(id_cols=c("subject", "session"), names_from = c("parameter"), values_from = c("mean", "sd")) %>%
    mutate(user_id = rep(data_list$user_id, times=8)) %>%
    # filter out the missing games 
    filter(!row_number() %in% which(data_list$is_missing[,,1]==1)) 
  
# calculate pseudo-R2 
# Pseudo-R2 is the amount of variance explained by the model relative to a model of pure chance (1-L/C). Where L is the summed log likelihood over participants and C is the chance likelihood of
# observed responses for two options (log(0.5,t), (Daw, 2011)
pseudoR2 = round(1-(sum(sum_log_lik$mean_sum_log_lik)/(log(0.5)*data_list$nTrials_max*nrow(sum_log_lik))),2)

message('model pseudoR2 = ', round(pseudoR2,3))


# reliability ----
# get summary of model estimated reliability all game combinations similar to behavioural analyses with ICC
# reward sens
summary_R_rewSens = as.data.frame(fit$summary(variables = c("R_rewSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","game1", "game2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("game1", "game2")], 1, sort))), 
         !duplicated(t(apply(.[c("game1", "game2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!game1 == game2) %>% 
  # get mean values across all game combinations
  summarise(meanR = mean(mean), meanSD = mean(sd), 
            semR = sd(mean)/sqrt(28), lbCI=meanR-(2*semR), ubCI = meanR+(2*semR)) %>%
  mutate_if(is.numeric, round,2)

summary_R_rewSens

# get summary of model estimated reliability all game combinations similar to behavioural analyses with ICC
# eff sens 
summary_R_effSens = as.data.frame(fit$summary(variables = c("R_effSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","game1", "game2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("game1", "game2")], 1, sort))), 
         !duplicated(t(apply(.[c("game1", "game2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!game1 == game2) %>% 
  # get mean values across all game combinations
  summarise(meanR = mean(mean), meanSD = mean(sd), 
            semR = sd(mean)/sqrt(28), lbCI=meanR-(2*semR), ubCI = meanR+(2*semR)) %>%
  mutate_if(is.numeric, round,2)

# theta (state motivation, raw value, not person centred)
summary_R_theta= as.data.frame(fit$summary(variables = c("R_theta"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","t1", "t2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("t1", "t2")], 1, sort))), 
         !duplicated(t(apply(.[c("t1", "t2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!t1 == t2) %>% 
  # get mean values across all game combinations
  summarise(meanR = mean(mean), meanSD = mean(sd), 
            semR = sd(mean)/sqrt(406), lbCI=meanR-(2*semR), ubCI = meanR+(2*semR)) %>%
  mutate_if(is.numeric, round,2)


# Posterior betas ----
# extract beta posteriors
draws_beta = as.data.frame(unlist(posterior::merge_chains(fit$draws(variables=c("beta_rew", "beta_eff",
                                                                                "beta_trait_rew", "beta_trait_eff"))))) %>%
  tibble::rownames_to_column(var="draw") %>%
  dplyr::rename("state_rew"="1.beta_rew",
                "state_eff"="1.beta_eff",
                "trait_rew" = "1.beta_trait_rew",
                "trait_eff" = "1.beta_trait_eff")

# describe posteior summary stats
library(bayestestR)
# set rope range as -0.1,0.1 (Krusche, 2018) recommendation
describe_posterior(draws_beta$state_rew, ci=0.9, ci_method = "hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1, 0.1))
describe_posterior(draws_beta$state_eff, ci=0.9, ci_method = "hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1, 0.1))
describe_posterior(draws_beta$trait_rew, ci=0.9, ci_method = "hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1, 0.1))
describe_posterior(draws_beta$trait_eff, ci=0.9, ci_method = "hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1, 0.1))


# state main effect pub worthy
pMain = draws_beta %>% select(c("draw", "state_rew", "state_eff", "trait_rew", "trait_eff")) %>%
  pivot_longer(cols=2:5,names_to = "beta") %>%
  ggplot(aes(x=value, y=beta)) +
  ggdist::stat_dist_halfeye(.width=c(0.1,0.9), alpha=0.5, size=5)+
  theme_classic(base_size=32) + 
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_discrete(limits=c("state_rew", "trait_rew", "state_eff", "trait_eff"), 
                   labels=c("stateR", "trait-apathyR", "stateE", "trait-apathyE")) +
  labs(fill="state effect on", x="posterior estimate", y="\U03B2") +
  theme(legend.position="none") 


# save the summary data 
# remove userID before saving for anonyminity 
summary_sens = select(summary_sens, -user_id) 
# subject is just an autogenerated ID from 1 to N; a subject list can be obtained upon request 
saveRDS(summary_sens, file = paste0(repo_path, "analysis/stan_fits/", fit$metadata()$model_name, "_subject_posterior_means.rds"))
# also save the draws plot 
saveRDS(draws_beta, file = paste0(repo_path, "analysis/stan_fits/", fit$metadata()$model_name, "_posterior_draws_group_level_betas.rds"))

# manuscript figure 4 is created in behavioural_analysis script; by loading these ^^ 

# PPC figure ---
figureS9=ggdraw() +
  draw_plot(ppc_Choice_subs, x = 0, y = 0, width = .5, height = 1) +
  draw_plot(ppc_yRep_subs, x = 0.5, y = 0.66, width = .5, height = .33) +
  draw_plot(p1s , x = 0.5, y = 0, width = .5, height = .66) +
  draw_plot_label(label = c("A", "B", "C"), size = 28,
                  x = c(0, .5, .5), y = c(1, 1, 0.5)) +
  # draw two subheadings for clarity 
  draw_label("Choices", x=0.275, y=0.985, size=36, fontface="bold") +
  draw_label("Self-report", x=0.75, y=0.985, size=36, fontface="bold") 
  
# save the figure 
ggsave(filename = paste0(fig_dir, "figureS9_PPC.png"),
       plot = figureS9,
       height = 40, width = 40,
       units = "cm", bg = "white")


# TS model ----
# load the timeseries models and compare to current state 
# timeseries model is the same but now the past and future state motivation compete for variance to influence current rewSens 
#  beta-state-1 and beta-stateR+1 prior is the empirical mean + sd for the current effect (beta-state), normal(0.19,0.08)
# this prior assumes that the past and future state are equally strongly related 

# rewSens = beta-traitR * apathy-score[i] + rewSens_tilde[i,t] + beta-stateR-1*thetaZ[i,t-1] + beta-stateR+1*thetaZ[i,t+1]
# effSens = beta-traitE * apathy-score[i] + effSens_tilde[i,t]

# we remove the state effects on effort sensitivity since these were not meaningful / significant 

# load the model fit 
fitTS = readRDS(file=paste0(model_dir, "rewEff-linear-bernoulli-multisess-joint-IRT-1param-rewSens-lag1.rds"))

# get the next state betas
draws_beta = as.data.frame(unlist(posterior::merge_chains(fitTS$draws(variables=c("beta_rew_prev", "beta_rew_next"))))) %>%
  tibble::rownames_to_column(var="draw") %>%
  dplyr::rename("previousState"="1.beta_rew_prev",
                "nextState"="1.beta_rew_next"
  ) 

describe_posterior(draws_beta[, "previousState"], ci=0.9, ci_method="hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1,0.1))
describe_posterior(draws_beta[, "nextState"], ci=0.9, ci_method="hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1,0.1))

# compare with the concurrent effect 
current_beta = as.data.frame(unlist(posterior::merge_chains(fit$draws(variables=c("beta_rew"))))) %>%
  tibble::rownames_to_column(var="draw") %>%
  dplyr::rename("beta_rew"="1.beta_rew",
  ) 

# delta 
delta_TSB = current_beta$beta_rew - draws_beta$previousState 
delta_TSF = current_beta$beta_rew - draws_beta$nextState 

describe_posterior(delta_TSB, ci=0.9, ci_method="hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1,0.1))
describe_posterior(delta_TSF, ci=0.9, ci_method="hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1,0.1))

# plot
pBr= draws_beta %>% 
  pivot_longer(cols=2:3, names_to="beta", values_to="estimate") %>%
  ggplot(aes(x=estimate, y=beta, group=beta, fill=beta)) +
  ggdist::stat_dist_slabinterval(.width=c(.1,.9), alpha=0.5) +geom_vline(xintercept=0, linetype="dashed")+
  theme_classic(base_size=24) +
  scale_y_discrete(labels=c("\U03B2 future", "\U03B2 past"))+
  scale_fill_manual(values=c("#ffa097", "#ff6361"))+
  labs(y="", x="state effect on reward sensitivity") + theme(legend.position="none") 
  
# save the posterior 
saveRDS(draws_beta, file = paste0(repo_path, "analysis/stan_fits/", fitTS$metadata()$model_name, "_posterior_draws_group_level_betas.rds"))

## TS2 ----
# add the TS-lag 2 model 
fitTS2 = readRDS(file=paste0(model_dir, "rewEff-linear-bernoulli-multisess-joint-IRT-1param-rewSens-lag2.rds"))

# get the next state betas
draws_beta = as.data.frame(unlist(posterior::merge_chains(fitTS2$draws(variables=c("beta_rew_prev1", "beta_rew_prev2"))))) %>%
  tibble::rownames_to_column(var="draw") %>%
  dplyr::rename("lag1"="1.beta_rew_prev1",
                "lag2"="1.beta_rew_prev2"
  ) %>%
  mutate(delta = lag1 - lag2)

describe_posterior(draws_beta[, "lag1"], ci=0.9, ci_method="hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1,0.1))
describe_posterior(draws_beta[, "lag2"], ci=0.9, ci_method="hdi", centrality="mean", test="p_direction", dispersion=T, rope_range=c(-0.1,0.1))

# plot
pBr2= draws_beta %>% 
  pivot_longer(cols=2:3, names_to="beta", values_to="estimate") %>%
  ggplot(aes(x=estimate, y=beta, group=beta, fill=beta)) +
  ggdist::stat_dist_slabinterval(.width=c(.1,.9), alpha=0.5) +geom_vline(xintercept=0, linetype="dashed")+
  theme_classic(base_size=28) +
  labs(fill="direction", y="\U03B2", title="lag-2 and lag-1 effects") + theme(legend.position="none")

# save the posterior draws (reload in behavioural analysis, figure 4)
saveRDS(draws_beta, file = paste0(repo_path, "analysis/stan_fits/", fitTS2$metadata()$model_name, "_posterior_draws_group_level_betas.rds"))

# Param recovery (PR) ----
# # set the model to load 
original_model = "rewEff-linear-bernoulli-multisess-joint-IRT-1param" # to load the original fit 
recovery_model = paste0(original_model, "-bounded-recovery")
# # load the original fitted model
fit = readRDS(file=paste0(model_dir, original_model, '.rds'))
# 
# # load the param recovery model
fit_rep = readRDS(file=paste0(model_dir, recovery_model, '.rds'))

## plot PR reward and eff sens ----
#original posteriors 
posts = as.data.frame(fit$summary(variables = c("rewSens", "effSens"), mean, sd)) %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "sess"), remove=TRUE, extra="drop") %>%
  mutate(user_id = rep(data_list$user_id, times=data_list$nTaskTimes*2),
         missing = rep(melt(data_list$is_missing[,,1])$value, times=2))
# replicated posteriors
posts_rep = as.data.frame(fit_rep$summary(variables = c("rewSens", "effSens"), mean, sd)) %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "sess"), remove=TRUE, extra="drop")

# merge them
posts_PR = merge(posts, posts_rep, by=c("subject", "parameter", "sess"), suffixes = c("_og", "_rep"),
                 no.dups = TRUE) %>% 
  mutate(day=(as.numeric(sess)-1)*2)


# plot them agaisnt each other
pR= posts_PR %>%
  filter(parameter=="rewSens") %>%
  ggplot(aes(y=mean_rep, x=mean_og))  +
  geom_point(aes(fill=day, color=day), size=2, alpha=0.7)+geom_smooth(se=F, method="lm", color="black", alpha=0.4)+
  # geom_errorbar(aes(ymin = mean_rep-sd_rep, ymax = mean_rep+sd_rep, color=day), alpha=.2) +
  # geom_errorbarh(aes(xmin = mean_og-sd_og, xmax = mean_og+sd_og, color=day), alpha=.2) +
  ggpubr::stat_cor(method = "pearson", label.y = 0, label.x=0, size=6, 
                   cor.coef.name= "r",
                   label.sep='\n',
                   p.accuracy = 0.001, r.accuracy = 0.01) +
  ylab("recovered posterior mean ± SD") + xlab("posterior mean ± SD") +
  theme_classic(base_size=28) +
  labs(title="reward sensitivity", color="day", fill="day") + facet_wrap(~day, ncol=4) + theme(legend.position="none")

ggsave(filename = paste0(fig_dir,file, "_PR_rewSens.png"), 
       plot=pR,
       dpi=300, height=30, width=30, units="cm")

# effort
pE= posts_PR %>%
  filter(parameter=="effSens") %>%
  ggplot(aes(y=mean_rep, x=mean_og))  +
  geom_point(aes(fill=day, color=day), size=2, alpha=0.7)+geom_smooth(se=F, method="lm", color="black", alpha=0.4)+
  # geom_errorbar(aes(ymin = mean_rep-sd_rep, ymax = mean_rep+sd_rep, color=day), alpha=.2) +
  # geom_errorbarh(aes(xmin = mean_og-sd_og, xmax = mean_og+sd_og, color=day), alpha=.2) +
  ggpubr::stat_cor(method = "pearson", label.y = 0,
                   label.x=0, size=6,
                   cor.coef.name= "r",
                   label.sep='\n',
                   p.accuracy = 0.001, r.accuracy = 0.01) +
  ylab("recovered posterior mean ± SD") + xlab("posterior mean ± SD") +
  theme_classic(base_size=28) +
  labs(title="effort sensitivity", color="day", fill="day") + facet_wrap(~day, ncol=4) + theme(legend.position="none")


ggsave(filename = paste0(fig_dir,file,"_PR_effSens.png"), 
       plot=pE,
       dpi=300, height=30, width=30, units="cm")

# get the correlation across all timepoints
cor.test(posts_PR$mean_rep[posts_PR$parameter=="rewSens"], posts_PR$mean_og[posts_PR$parameter=="rewSens"])
cor.test(posts_PR$mean_rep[posts_PR$parameter=="effSens"], posts_PR$mean_og[posts_PR$parameter=="effSens"])

## plot PR state ----
# individual level parameter distribution means (for computational efficiency: full posterior is slow!)
posts_state = as.data.frame(fit$summary(variables = c("thetaZ"), mean, sd)) %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "obs"), remove=TRUE, extra="drop")

# replicated posteriors
posts_state_rep = as.data.frame(fit_rep$summary(variables = c("thetaZ"), mean, sd)) %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","subject", "obs"), remove=TRUE, extra="drop") 

# merge them
posts_state_PR = merge(posts_state, posts_state_rep, by=c("subject", "parameter", "obs"), suffixes = c("_og", "_rep"),no.dups = TRUE) %>%
  mutate(t = as.numeric(obs),
         subject=as.numeric(subject))


# plot them agaisnt each other
pT= posts_state_PR %>%
  ggplot(aes(y=mean_rep, x=mean_og))  +
  geom_point(aes(fill=t, color=t), size=2, alpha=0.7)+
  geom_errorbar(aes(ymin = mean_rep-sd_rep, ymax = mean_rep+sd_rep, color=t), alpha=.2) +
  geom_errorbarh(aes(xmin = mean_og-sd_og, xmax = mean_og+sd_og, color=t), alpha=.2) +
  geom_smooth(se=F, method="lm", color="black", alpha=0.4)+
  ggpubr::stat_cor(method = "pearson", label.y = max(posts_PR$mean_rep)*0.1,
                   label.x=max(posts_state_PR$mean_og)*0.5, size=6,
                   cor.coef.name= "r",
                   label.sep='\n',
                   p.accuracy = 0.001, r.accuracy = 0.01) +
  ylab("recovered posterior mean ± SD") + xlab("posterior mean ± SD") +
  theme_classic(base_size=28) +
  labs(title="latent state", color="t", fill="t") + 
  scale_color_continuous(breaks=seq(0,28,7)) +
  scale_fill_continuous(breaks=seq(0,28,7)) 

pT

ggsave(filename = paste0(fig_dir,fit$metadata()$model_name, "_PR_theta.png"), 
       plot=pT,
       dpi=300, height=30, width=30, units="cm")

# correlation across timepoints 
cor.test(posts_state_PR$mean_rep, posts_state_PR$mean_og)

## PR: test re-test ----
# rewSens, get reliabilities for each possible combination (t=28), original model
summary_R_rewSens = as.data.frame(fit$summary(variables = c("R_rewSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","game1", "game2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("game1", "game2")], 1, sort))), 
         !duplicated(t(apply(.[c("game1", "game2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!game1 == game2) 
# replicated model 
summary_R_rewSens_rep = as.data.frame(fit_rep$summary(variables = c("R_rewSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","game1", "game2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("game1", "game2")], 1, sort))), 
         !duplicated(t(apply(.[c("game1", "game2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!game1 == game2) 

# bind them together
R_rewSens = merge(summary_R_rewSens, summary_R_rewSens_rep,
                  by=c("var", "parameter", "game1", "game2"), suffixes = c("_og", "_rep"),
                  no.dups = TRUE)
# correlate original mean with replicated
cor.test(R_rewSens$mean_og, R_rewSens$mean_rep) 

# get summary of model estimated reliability all game combinations similar to behavioural analyses with ICC
# eff sens 
summary_R_effSens = as.data.frame(fit$summary(variables = c("R_effSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","game1", "game2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("game1", "game2")], 1, sort))), 
         !duplicated(t(apply(.[c("game1", "game2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!game1 == game2) 
# replicated model 
summary_R_effSens_rep = as.data.frame(fit_rep$summary(variables = c("R_effSens"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","game1", "game2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("game1", "game2")], 1, sort))), 
         !duplicated(t(apply(.[c("game1", "game2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same game
  filter(!game1 == game2) 

# bind them together
R_effSens = merge(summary_R_effSens, summary_R_effSens_rep,
                  by=c("var", "parameter", "game1", "game2"), suffixes = c("_og", "_rep"),
                  no.dups = TRUE)
# correlate original mean with replicated
cor.test(R_effSens$mean_og, R_effSens$mean_rep) 

# theta (state motivation, raw value, not person centred)
summary_R_theta= as.data.frame(fit$summary(variables = c("R_theta"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","t1", "t2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("t1", "t2")], 1, sort))), 
         !duplicated(t(apply(.[c("t1", "t2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same timepoint
  filter(!t1 == t2) %>% 
  # get mean values across all game combinations
  summarise(meanR = mean(mean), meanSD = mean(sd)) %>%
  mutate_if(is.numeric, round,2)
# replicated model
summary_R_theta_rep= as.data.frame(fit$summary(variables = c("R_theta"), mean, sd)) %>%
  rownames_to_column(var = "var") %>%
  separate(variable, sep="\\[|\\,|\\]", into=c("parameter","t1", "t2"), remove=TRUE, extra="drop") %>%
  # take only the unique pairs 
  filter(!duplicated(t(apply(.[c("t1", "t2")], 1, sort))), 
         !duplicated(t(apply(.[c("t1", "t2")], 1, sort, decreasing = TRUE))))%>%
  # filter out correlations between the same timepoint
  filter(!t1 == t2) %>% 
  # get mean values across all game combinations
  summarise(meanR = mean(mean), meanSD = mean(sd)) %>%
  mutate_if(is.numeric, round,2)

# merge and run corr 
R_theta = merge(summary_R_theta, summary_R_theta_rep,
                by=c("var", "parameter", "t1", "t2"), suffixes = c("_og", "_rep"),
                no.dups = TRUE)
# correlate original mean with replicated
cor.test(R_theta$mean_og, R_theta$mean_rep) 