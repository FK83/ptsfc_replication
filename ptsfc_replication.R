rm(list = ls())
setwd("[Enter working directory]")

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

# set seed (for simulation below)
set.seed(23)

# Read and combine evaluation files for all rounds
# Download files from https://git.scc.kit.edu/ng3223/ptsfc_results/-/tree/main/evaluation
# Then save them in a local folder, say localfolder
eval_files <- list.files("[Enter path to localfolder]")
eval_files <- eval_files[grep("evaluation_2", eval_files)]
all_evals <- NULL

for (fl in eval_files){
  to_add <- read.csv(paste0("[Enter path to localfolder]", fl))
  if (is.null(all_evals)){
    all_evals <- to_add
  } else {
    all_evals <- rbind(all_evals, to_add)
  }
}

# prepare score data
scores0 <- all_evals %>%
  # remove weather forecasts from Oct 27 (location changed from Karlsruhe to Berlin)
  filter(! (forecast_date == "2021-10-27" & target != "DAX") ) %>%
  # remove cases with missing ae (due to missing truth data)
  filter(!is.na(ae)) %>%
  # remove imputed benchmark forecasts for other variables
  filter(! ( (model == "DAX_benchmark") & (target != "DAX") ),
         ! ( (model == "t2m_benchmark") & (target != "temperature") ),
         ! ( (model == "wind_benchmark") & (target != "wind") ), 
         ! ( (model == "t2m_EMOS") & (target != "temperature") ), 
         ! ( (model == "wind_EMOS") & (target != "wind")) ) %>%
  # assign same name to all benchmark models
  mutate(model = if_else(grepl("benchmark", model), "Benchmark", model)) %>% 
  # assign same name to all EMOS models
  mutate(model = if_else(grepl("EMOS", model), "EMOS", model)) %>%
  mutate(qscore = mean_qscore) %>% select(-mean_qscore) 

# Summarize coverage (reflecting calibration)
scores0 %>% filter(model %in% c("Benchmark", "mean_ensemble")) %>%
  group_by(target, horizon, model) %>%
  summarize(cov_50 = 100*mean(interval_coverage_0.5),
            cov_95 = 100*mean(interval_coverage_0.95)) %>%
  pivot_wider(names_from = "model", 
              values_from = c("cov_50", "cov_95")) %>%
  mutate(target = if_else(target == "temperature", "Temp.", 
                          if_else(target == "wind", "Wind", target))) %>%
  kable(digits = 1, format = "latex", booktabs = TRUE)

# Summarize length of prediction intervals (reflecting sharpness)
scores0 %>% filter(model %in% c("Benchmark", "mean_ensemble")) %>%
  group_by(target, horizon, model) %>%
  summarize(len_50 = mean(q0.75-q0.25),
            len_95 = mean(q0.975-q0.025)) %>%
  pivot_wider(names_from = "model", 
              values_from = c("len_50", "len_95")) %>%
  mutate(target = if_else(target == "temperature", "Temp.", 
                          if_else(target == "wind", "Wind", target))) %>%
  kable(digits = 1, format = "latex", booktabs = TRUE)
  
# Compute mean scores and skill scores
mean_scores <- scores0 %>% group_by(model, target, horizon) %>%
  summarise(qscore = mean(qscore)) %>% ungroup %>%
  group_by(target, horizon) %>% 
  mutate(skill_score = (qscore[model == "Benchmark"] - qscore)/
           qscore[model == "Benchmark"]) %>% ungroup %>%
  mutate(setup = factor(paste0(target, "_", horizon))) %>%
  filter(model != "Benchmark", model != "median_ensemble",  
         model != "JabbaTheHutt", model != "RossGeller") %>% # remove forecasters who dropped out
  mutate(setup2 = as.numeric(setup)) %>%
  mutate(setup2 = if_else(setup2 %in% 11:15, setup2 + 2, setup2)) %>% 
  mutate(setup2 = if_else(setup2 %in% 6:10, setup2 + 1, setup2))
  
# Make plot
pdf("ptsfc_performance_summary.pdf", width = 16, height = 9)
cl <- c("indianred2", "deepskyblue4")
cx_font <- 2
cx_point <- 3
left_lim <- min(mean_scores$skill_score)-1.3
plot(x = mean_scores$skill_score, y = mean_scores$setup2, 
     col = grey(.2, .2), 
     pch = 20, cex = cx_point, axes = FALSE, xlab = "", 
     ylab = "", xlim = c(left_lim, max(mean_scores$skill_score)), 
     ylim = c(0, 21))
axis(1, at = seq(from = -2.5, to = .5, by = .5), cex.axis = cx_font)

# Add highlighting for ensemble
sl <- mean_scores %>% filter(model == "mean_ensemble")
points(x = sl$skill_score, y = sl$setup2, 
       col = cl[1], pch = 20, cex = cx_point)
lines(x = sl$skill_score, y = sl$setup2, 
      col = cl[1], lwd = 2)

# Add highlighting for EMOS
sl2 <- mean_scores %>% filter(model == "EMOS")
points(x = sl2$skill_score, y = sl2$setup2, 
       col = cl[2], pch = 20, cex = cx_point)
lines(x = sl2$skill_score, y = sl2$setup2, 
      col = cl[2], lwd = 2)

# Add names of setups (targets and horizons)
setup_names <- mean_scores[, c("setup", "setup2")] %>% unique %>%
  mutate(setup = gsub("_", " ", setup)) %>% 
  mutate(setup = gsub("temperature", "", setup)) %>%
  mutate(setup = gsub("DAX", "", setup)) %>% 
  mutate(setup = gsub("wind", "", setup))
for (jj in 1:15){
  text(x = left_lim + .5, y = setup_names$setup2[jj], 
       labels = setup_names$setup[jj], 
       cex = cx_font)
}
text(x = left_lim, y = 3, labels = "DAX", cex = cx_font)
text(x = left_lim, y = 9, labels = "Temp.", cex = cx_font)
text(x = left_lim, y = 15, labels = "Wind", cex = cx_font)

# Add legend
legend(x = left_lim + .3, y = 20, 
       c("Individual Forecasters  ", "Mean Ensemble", "EMOS"), 
       col = c("grey", cl), 
       pch = rep(20, 3), lty = c(NA, 1, 1), bty = "n", 
       lwd = c(NA, 2, 2), horiz = TRUE, cex = cx_font)

# Add lines
segments(0, .5, 0, 17.5, lwd = 2)
abline(h = c(6, 12), lty = 2, lwd = .4)
dev.off()

# Summary info on nr of forecasters that beat the benchmark
mean_scores %>% 
  group_by(target, horizon) %>% 
  summarize(n = length(skill_score[model != "mean_ensemble"]), 
            beat_bench = sum(skill_score[model != "mean_ensemble"] >= 0), 
            beat_ens = sum(skill_score[model != "mean_ensemble"] >= 
                             skill_score[model == "mean_ensemble"]))

# Compare performance of mean versus median ensemble
scores0 %>% filter(grepl("ensemble", model) | model == "Benchmark") %>% 
  group_by(target, horizon, model) %>% 
  summarise(qscore = mean(qscore), n = n()) %>%
  ungroup %>% 
  mutate(setup = paste0(target, "_", gsub(" ", "", horizon)), 
         setup = as.numeric(as.factor(setup))) %>%
  ggplot(aes(x = setup, y = qscore, color = model)) + 
  geom_point(size = I(2)) + theme_minimal(base_size = 14) + 
  theme(legend.position = "top") + scale_color_viridis_d(name = "") + 
  xlab("Setup Nr") + ylab("Quantile Score")

# Compute share of forecasters who beat the benchmark
beat_benchmark <- scores0 %>% filter(!grepl("EMOS", model), 
                                  !grepl("ensemble", model)) %>%
  group_by(target, horizon, forecast_date) %>% 
  mutate(qscore_benchmark = qscore[model == "Benchmark"]) %>% ungroup %>%
  select(forecast_date, target, horizon, model, qscore, qscore_benchmark) %>%
  filter(model != "Benchmark",  # remove benchmark
         model != "JabbaTheHutt", model != "RossGeller") %>% # remove forecasters who dropped out
  group_by(target, forecast_date, model) %>% 
  summarise(beat_benchmark = mean(qscore) <= mean(qscore_benchmark)) %>%
  ungroup %>% group_by(target, forecast_date) %>%
  summarise(beat_benchmark = mean(beat_benchmark)) %>%
  ungroup %>% group_by(target) %>% 
  mutate(date2 = rank(forecast_date)) %>% ungroup %>%
  mutate(date2 = if_else(target == "DAX", date2, date2 + 1))
ggplot(beat_benchmark, 
       aes(x = date2, y = beat_benchmark, color = target)) + 
  geom_point(size = I(2)) + geom_line(size = I(.2)) + theme_minimal(base_size = 14) + 
  xlab("Submission round") + ylab("% beats benchmark") + 
  scale_color_viridis_d(name = "", 
                        breaks = c("DAX", "temperature", "wind"), 
                        labels = c("DAX", "Temp.", "Wind")) + 
  theme(legend.position = "top") + 
  scale_x_continuous(breaks = c(1, 5, 10, 14), 
                     minor_breaks = NULL)
ggsave(filename = "ptsfc_share_beats_benchmark.pdf", 
       height = 9, width = 16, units = "cm")

# Simulation study to stability of ranking method
n_mc <- 1e4 # nr of MC iterations
rks_df_all <- data.frame()

# drop ensembles (not part of ranking)
scores <- scores0 %>%
  group_by(target, horizon, forecast_date) %>%
  mutate(qscore_benchmark = qscore[model == "Benchmark"], 
         ae_benchmark = ae[model == "Benchmark"]) %>%
  ungroup %>%
  filter(!model %in% c("Benchmark", "EMOS"), 
         !grepl("ensemble", model))

# DAX sample
scores_dax <- scores %>% filter(target == "DAX") %>%
  transmute(model, horizon, forecast_date, qscore)
dts_dax <- scores_dax$forecast_date %>% unique %>% sort %>%
  as.factor
n_dax <- length(dts_dax)

# Weather sample
scores_weather <- scores %>% filter(target != "DAX") %>%
  transmute(target, model, horizon, forecast_date, qscore)
dts_weather <- scores_weather$forecast_date %>% unique %>% sort %>% 
  as.factor
n_weather <- length(dts_weather)

# loop over MC iterations
for (jj in 1:n_mc){
  # draw dates with replacement
  # compute implied weight for each date
  w_dts_dax <- sample(dts_dax, size = n_dax, replace = TRUE) %>%
    table %>% (function(x) data.frame(forecast_date = dimnames(x)$., 
                                      w = as.numeric(x)))
  w_dts_weather <- sample(dts_weather, 
                          size = n_weather, replace = TRUE) %>% 
    table %>% (function(x) data.frame(forecast_date = dimnames(x)$., 
                                      w = as.numeric(x)))
  
  # compute scores for weighted sample
  scores_dax_tmp <- scores_dax %>% 
    merge(w_dts_dax) %>% mutate(w_qscore = w*qscore) %>%
    group_by(model, horizon) %>% 
    summarise(w_qscore = sum(w_qscore), .groups = "keep") %>%
    ungroup %>% group_by(horizon) %>% 
    mutate(model, rk = rank(w_qscore)) %>% ungroup %>%
    transmute(model, horizon, rk, target = "DAX") 
  
  scores_weather_tmp <- scores_weather %>% 
    merge(w_dts_weather) %>% mutate(w_qscore = w*qscore) %>%
    group_by(model, horizon, target) %>% 
    summarise(w_qscore = sum(w_qscore), .groups = "keep") %>%
    ungroup %>% group_by(target, horizon) %>% 
    mutate(target, model, rk = rank(w_qscore)) %>% ungroup %>%
    transmute(target, model, horizon, rk)
  
  # compute overall ranking for this iteration
  rks_tmp <- rbind(scores_dax_tmp, scores_weather_tmp) %>% 
    group_by(model) %>% 
    summarise(rk = sum(rk), n = n(), .groups = "keep") %>%
    ungroup %>% mutate(rk = rank(rk))
  
  # check
  stopifnot(all(rks_tmp$n == 15))
  
  # expand simulation data frame
  rks_df_all <- rbind(rks_df_all, data.frame(rks_tmp, mc_iter = jj))
  
}

# summarize simulations
final_df1 <- rks_df_all %>% group_by(model) %>% 
  summarise(share_wins = mean(rk == 1), mean_rank = mean(rk), 
            q90_rank = quantile(rk, 0.90), q99_rank = quantile(rk, 0.99))
# add information on actual rank in empirical sample
final_df2 <- scores %>% 
  group_by(model, target, horizon) %>% 
  summarise(qscore = sum(qscore), .groups = "keep") %>% ungroup %>%
  group_by(target, horizon) %>% 
  mutate(target, model, rk = rank(qscore)) %>% ungroup %>%
  transmute(target, model, horizon, rk) %>%
  group_by(model) %>% summarise(n = n(), rk = sum(rk)) %>%
  ungroup %>% mutate(actual_rank = rank(rk))

# final table on summary across MC iterations
final_df <- merge(final_df1, final_df2[, c("model", "actual_rank")]) %>% 
  arrange(-share_wins) %>% mutate(share_wins = 100*share_wins)
kable(head(final_df, 5), digits = 1, format = "latex", booktabs = TRUE)

# comparison to alternative ranking scheme (simply adding up scores)
rank_cp <- scores %>% group_by(model, target, horizon) %>% 
  summarise(qscore = sum(qscore), 
            skill_score = (sum(qscore_benchmark)-sum(qscore))/
              sum(qscore_benchmark), .groups = "keep") %>% 
  ungroup %>% group_by(model) %>%
  summarise(v3 = sum(qscore), v2 = -sum(skill_score)) %>%
  ungroup %>% mutate(v2 = rank(v2), v3 = rank(v3))  %>%
  merge(data.frame(model = final_df2$model, v1 = final_df2$actual_rank),
        by = "model") %>% arrange(v1) %>% transmute(model, v1, v2, v3)

pdf("ptsfc_ranking_comparison.pdf", width = 16, height = 9)
par(mar=c(5,6,4,1)+.4)
matplot(rank_cp[,-(1:2)], axes = FALSE, pch = 20, ylab = "Rank (alternative)", 
        xlab = "Rank (Version 1)", cex = cx_point, cex.lab = cx_font)
axis(1, at = c(1, 5, 10, 15, 20), cex.axis = cx_font)
axis(2, at = c(1, 5, 10, 15, 20), cex.axis = cx_font)
legend("topleft", c("Version 2", "Version 3"), col = 1:2, pch = 20, bty = "n", 
       horiz = TRUE, cex = cx_font)
abline(a = 0, b = 1, lwd = .4)
dev.off()

# Save image
save.image(file = paste0("results_", Sys.Date(), ".RData"))
