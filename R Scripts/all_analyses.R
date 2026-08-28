
# Packages
require(tidyverse)
require(cowplot)
require(psych)
require(afex)

# Experiment 1 ====

# Behavior ####
e1_behavior <- read.csv("Data/Experiment 1/behavior/e1_rts.csv")
e1_behavior <- e1_behavior %>%
  mutate(
    experiment = str_remove(experiment, "^PVT150"),
    experiment = str_remove(experiment, "300TET$"),
  ) %>%
  group_by(subject) %>%
  arrange(rt, .by_group = TRUE) %>%   # fastest to slowest
  mutate(rt_bin = ntile(rt, 5)) %>%      # 1 = fastest, 5 = slowest
  ungroup() %>%
  mutate(trial = (block - 1) * 28 + trial)

table(e1_behavior$experiment)

e1_behavior = e1_behavior %>%
  mutate(condition = case_when(experiment == 'goal' ~ 'hard goal',
                               experiment == 'goal800TET' ~ 'easy goal',
                               experiment == 'nogoalTET' ~ 'no goal'),
         goal = case_when(condition == 'hard goal' ~ 'goal',
                          condition == 'easy goal' ~ 'goal',
                          condition == 'no goal' ~ 'no goal'))

e1_rt_subject_block = group_by(e1_behavior, subject, condition, goal, block) %>%
  filter(between(rt, 200, 3000)) %>%
  summarize(rt = mean(rt, na.rm = T))

e1_rt_block_condition = e1_rt_subject_block %>%
  group_by(condition, block) %>%
  mutate(z = scale(rt)[,1]) %>%
  filter(abs(z) < 3) %>%
  summarize(mean = mean(rt, na.rm = T),
            se = sd(rt, na.rm = T)/sqrt(n()))

e1_average_rt_p <- ggplot(
  e1_rt_block_condition,
  aes(
    x = block,
    y = mean,
    ymin = mean - se, 
    ymax = mean + se,
    group = condition,
    color = condition
  )
) +
  geom_line(
    aes(linetype = condition),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = condition, color = condition),
    position = pd
  ) +
  geom_errorbar(
    width = 0.20,
    position = pd,
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "condition",
    values = c("easy goal" = 15,
               "hard goal" = 16,
               "no goal" = 17)
  ) +
  scale_linetype_manual(
    values = c(
      "easy goal" = "dotted",
      "hard goal" = "dashed",
      "no goal" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "easy goal" = "gray60",
      "hard goal" = "gray35",
      "no goal" = "black"
    )
  ) +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  theme_bw(base_size = 17) +
  theme(legend.position = 'inside',
        legend.position.inside = c(.2, .8),
        axis.text = element_text(color = 'black'))
e1_average_rt_p
ggsave(e1_average_rt_p, file = 'Figures/e1_rt.png', height = 6, width = 6, units = 'in', dpi = 600)

# Pretrial ####

e1_pretrial_files <- list.files(
  path = "Data/Experiment 1/pretrial x trial",
  pattern = "^e1_pretrial_trial_[0-9]+\\.tsv$",
  full.names = TRUE
)
e1_pretrial_data <- read_tsv(e1_pretrial_files)
e1_pretrial_data <- e1_pretrial_data %>%
  mutate(block = ceiling(TrialId / 28))

# excluding participants missing 50% of data
e1_subject_exclusions <- e1_pretrial_data %>%
  group_by(Subject) %>%
  summarise(
    total_trials = n(),
    excluded_trials = sum(is.na(pretrial_pupil)),
    proportion_excluded = excluded_trials / total_trials,
    .groups = "drop"
  )

e1_clean_subjects <- e1_subject_exclusions %>%
  filter(proportion_excluded < 0.50) %>%
  pull(Subject)

e1_pretrial_data <- e1_pretrial_data %>%
  filter(
    Subject %in% e1_clean_subjects,
    !is.na(pretrial_pupil)
  )

e1_summary <- e1_pretrial_data %>%
  group_by(Subject, block, condition) %>%
  summarise(
    mean_pupil = mean(pretrial_pupil, na.rm = TRUE),
    sd_pupil = sd(pretrial_pupil, na.rm = TRUE),
    cv_pupil = sd(pretrial_pupil, na.rm = TRUE)/mean(pretrial_pupil, na.rm = TRUE),
    .groups = "drop"
  )

# mean by condition

easy_goal_mean <- e1_summary %>%
  filter(condition == "easy goal") %>%
  group_by(block) %>%
  summarise(
    group_mean = mean(mean_pupil, na.rm = TRUE),
    se = sd(mean_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
hard_goal_mean <- e1_summary %>%
  filter(condition == "hard goal") %>%
  group_by(block) %>%
  summarise(
    group_mean = mean(mean_pupil, na.rm = TRUE),
    se = sd(mean_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
no_goal_mean <- e1_summary %>%
  filter(condition == "no goal") %>%
  group_by(block) %>%
  summarise(
    group_mean = mean(mean_pupil, na.rm = TRUE),
    se = sd(mean_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# average arousal by condition plot 

e1_average_arousal_data <- e1_summary %>%
  group_by(condition, block) %>%
  summarise(
    group_mean = mean(mean_pupil, na.rm = TRUE),
    se = sd(mean_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

pd <- position_dodge(width = 0.15)

e1_average_arousal_p <- ggplot(
  e1_average_arousal_data,
  aes(
    x = factor(block),
    y = group_mean,
    group = condition,
    color = condition
  )
) +
  geom_line(
    aes(linetype = condition),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = condition, color = condition),
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = group_mean - se,
      ymax = group_mean + se
    ),
    width = 0.20,
    position = pd,
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "condition",
    values = c("easy goal" = 15,
               "hard goal" = 16,
               "no goal" = 17)
  ) +
  scale_linetype_manual(
    values = c(
      "easy goal" = "dotted",
      "hard goal" = "dashed",
      "no goal" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "easy goal" = "gray60",
      "hard goal" = "gray35",
      "no goal" = "black"
    )
  ) +
  xlab("Block") +
  ylab("Pretrial pupil diameter (mm)") +
  coord_cartesian(ylim = c(2.7, 3.3))

e1_average_arousal_p = e1_average_arousal_p + theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.75, .75))

e1_average_arousal_p

# coefficient of variation by condition

easy_cv <- e1_summary %>%
  filter(condition == "easy goal") %>%
  group_by(block) %>%
  summarise(
    group_cv = mean(cv_pupil, na.rm = TRUE),
    se = sd(cv_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
hard_cv <- e1_summary %>%
  filter(condition == "hard goal") %>%
  group_by(block) %>%
  summarise(
    group_cv = mean(cv_pupil, na.rm = TRUE),
    se = sd(cv_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
no_goal_cv <- e1_summary %>%
  filter(condition == "no goal") %>%
  group_by(block) %>%
  summarise(
    group_cv = mean(cv_pupil, na.rm = TRUE),
    se = sd(cv_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# coefficient of varition by condition

e1_cv_plot_data <- e1_summary %>%
  group_by(condition, block) %>%
  summarise(
    group_cv = mean(cv_pupil, na.rm = TRUE),
    se = sd(cv_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(condition %in% c("easy goal", "hard goal", "no goal"))

pd <- position_dodge(width = 0.18)

e1_cv_p <- ggplot(
  e1_cv_plot_data,
  aes(
    x = factor(block),
    y = group_cv,
    group = condition,
    linetype = condition,
    color = condition
  )
) +
  geom_line(
    position = pd,
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = condition, 
      shape = condition),
    position = pd) +
  geom_errorbar(
    aes(
      ymin = group_cv - se,
      ymax = group_cv + se,
      color = condition
    ),
    position = pd,
    width = 0.2,
    linewidth = 1,
    linetype = 1
  ) +
  scale_shape_manual(
    name = "condition",
    values = c("easy goal" = 15,
               "hard goal" = 16,
               "no goal" = 17)
  ) +
  scale_color_manual(
    values = c(
      "easy goal" = "gray60",
      "hard goal" = "gray35",
      "no goal" = "black"
    )) +
  scale_linetype_manual(
    name = "condition",
    values = c(
      "easy goal" = "dotted",
      "hard goal" = "dashed",
      "no goal" = "solid"
    )
  ) +
  labs(
    x = "Block",
    y = "Pretrial pupil variability"
  ) +
  coord_cartesian(ylim = c(0.04, 0.09))

e1_cv_p = e1_cv_p + theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.title = element_blank(),
        legend.position = "none")

e1_cv_p

# ANOVAs
e1_pretrial_subject_block = e1_pretrial_data %>%
  group_by(Subject, condition, block) %>%
  summarise(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/mean(pretrial_pupil, na.rm = T))


pretrial_anova_afex <- aov_ez(
  id = "Subject",
  dv = "pretrial_mean",
  data = e1_pretrial_subject_block,
  between = "condition",
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

pretrial_anova_afex <- aov_ez(
  id = "Subject",
  dv = "pretrial_cv",
  data = e1_pretrial_subject_block,
  between = "condition",
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

# RT Bins
e1_pretrial_data = e1_pretrial_data %>%
  rename(subject = Subject,
         trial = TrialId) %>%
  full_join(e1_behavior)

e1_pretrial_subject_bin = e1_pretrial_data %>%
  group_by(subject) %>%
  mutate(z = scale(pretrial_pupil)[,1]) %>%
  group_by(subject, condition, rt_bin) %>%
  summarise(pretrial_pupil = mean(z, na.rm = T)) %>%
  filter(!is.na(rt_bin))

pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_pupil",
  data = e1_pretrial_subject_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

e1_pretrial_rt_bin = e1_pretrial_subject_bin %>%
  group_by(rt_bin) %>%
  summarise(mean = mean(pretrial_pupil, na.rm = T),
            se = sd(pretrial_pupil, na.rm = T)/sqrt(n()))

e1_pretrial_bin_p = ggplot(e1_pretrial_rt_bin, aes(x = rt_bin, y = mean, ymin = mean - se, ymax = mean + se)) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = .2) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = 'RT Quintile', y = 'Pretrial Pupil (normalized)')

e1_pretrial_p = plot_grid(plot_grid(e1_average_arousal_p, e1_cv_p, ncol = 2, labels = 'AUTO'),
                          plot_grid(NULL, e1_pretrial_bin_p, NULL, ncol = 3, rel_widths = c(1, 2, 1), labels = c("", "C", "")),
                          ncol = 1)
e1_pretrial_p
ggsave(e1_pretrial_p, file = 'Figures/e1_pretrial.png', height = 10, width = 10, units = 'in', dpi = 600)

# Phasic ####
e1_phasic_files <- list.files(
  path = "Data/Experiment 1/phasic x trial",
  pattern = "^e1_phasic_trial_[0-9]+\\.tsv$",
  full.names = TRUE
)
e1_phasic_data <- read_tsv(e1_phasic_files)

e1_phasic_data <- e1_phasic_data %>%
  rename(subject = Subject,
         trial = TrialId) %>%
  mutate(block = ceiling(trial / 28))

waveform_data <- e1_phasic_data %>%
  group_by(subject, block, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = TRUE)) %>%
  group_by(block, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  )

e1_TEPR_block <- ggplot(waveform_data,
                        aes(x = bin,
                            y = mean_response,
                            group = factor(block),
                            color = factor(block))) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "Block"
  ) + ylim(-0.02, 0.14) +
  theme_bw() +
  theme(axis.text  = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e1_TEPR_block

waveform_data_condition <- e1_phasic_data %>%
  group_by(subject, condition, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = TRUE)) %>%
  group_by(condition, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  )

e1_TEPR_condition <- ggplot(waveform_data_condition,
                            aes(x = bin,
                                y = mean_response,
                                group = factor(condition),
                                color = factor(condition))) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "Condition"
  ) + ylim(-0.02, 0.14) +
  theme_bw() +
  theme(axis.text  = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e1_TEPR_condition

e1_max_pupil <- e1_phasic_data %>%
  filter(bin >= 500, bin <= 800) %>%
  filter(!is.na(pupil_change)) %>%
  group_by(subject, trial, condition, block) %>%
  summarise(
    max_pupil = max(pupil_change),
    .groups = "drop"
  )

plot_max_e1 <- e1_max_pupil %>%
  group_by(condition, block) %>%
  summarise(
    mean_max = mean(max_pupil, na.rm = TRUE),
    se = sd(max_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(condition %in% c("easy goal", "hard goal", "no goal"))

pd <- position_dodge(width = 0.25)

e1_max_mean_p <- ggplot(plot_max_e1,
                        aes(x = block,
                            y = mean_max,
                            group = condition,
                            linetype = condition,
                            color = condition,
                            shape = condition,
                            ymin = mean_max - se,
                            ymax = mean_max + se)) +
  geom_line(position = pd, linewidth = 0.5) +
  geom_point(position = pd, size = 1) +
  geom_errorbar(
    position = pd,
    width = 0.15,
    linetype = 1
  ) +
  scale_linetype_manual(
    name = "Condition",
    values = c(
      "easy goal" = "dotted",
      "hard goal" = "dashed",
      "no goal" = "solid"
    )
  ) +
  labs(
    x = "Block",
    y = "Maximum Dilation"
  ) +
  ylim(0, 0.2) +
  scale_color_grey(name = "Condition", 
                   start = 0.1, end = 0.8) +
  theme_bw() +
  labs(color = "Condition", shape = "Condition", linetype = "Condition")+
  theme(legend.position = 'inside',
        legend.position.inside = c(.8, .8),
        axis.text = element_text(color = 'black'))

e1_max_mean_p

e1_tepr_plots = plot_grid(plot_grid(e1_TEPR_block, e1_TEPR_condition, labels = 'AUTO', ncol = 2), plot_grid(NULL, e1_max_mean_p, NULL, rel_widths = c(1, 2, 1), labels = c("", "C", ""), ncol = 3), ncol = 1)
ggsave(e1_tepr_plots, file = 'Figures/e1_tepr.png', height = 8, width = 8, units = 'in', dpi = 600)

e1_phasic_data <- full_join(e1_phasic_data, e1_behavior, by = c("subject", "trial"))

e1_rts <- e1_phasic_data %>%
  dplyr::select(subject, condition, trial, rt, pupil_change, bin, rt_bin) %>%
  filter(!is.na(rt), rt >= 200, rt <= 3000)

e1_rt_bins <- e1_rts

rt_waveform <- e1_rt_bins %>%
  group_by(subject, rt_bin, bin) %>%
  summarize(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(rt_bin, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  )

e1_bin_p = ggplot(rt_waveform,
                  aes(x = bin,
                      y = mean_response,
                      color = factor(rt_bin),
                  )) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "RT quintile",
  ) + 
  ylim(-0.02, 0.14) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e1_tepr_plots = plot_grid(e1_TEPR_block, e1_TEPR_condition, e1_max_mean_p, e1_bin_p, labels = 'AUTO', ncol = 2)
e1_tepr_plots
ggsave(e1_tepr_plots, file = 'Figures/e1_tepr.png', height = 8, width = 8, units = 'in', dpi = 600)


# TEPR ANOVA
e1_tepr_subject_rt_bin = e1_rt_bins %>%
  group_by(subject, condition, rt_bin) %>%
  filter(between(bin, 500, 800)) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T))

tepr_anova_afex <- aov_ez(
  id = "subject",
  dv = "pupil_change",
  data = e1_tepr_subject_rt_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
tepr_anova_afex <- as.data.frame(tepr_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
tepr_anova_afex

# Quiet Eye ####

# e1 qe analysis

e1_quiet_eye_files <- list.files(
  path = "Data/Experiment 1/quiet eye",
  pattern = "^e1_wait_eye_subject_trial_bin_[0-9]+\\.csv$",
  full.names =  TRUE
)

e1_quiet_eye_data <- read_csv(e1_quiet_eye_files) %>%
  rename(subject = Subject) %>%
  rename(trial = TrialId)

# excluding missing samples within trials

e1_qe_exclusions <- e1_quiet_eye_data %>%
  group_by(subject, trial) %>%
  summarise(
    total_samples = n(),
    missing_samples = sum(
      is.na(right_gaze_x) | is.na(right_gaze_y)
    ),
    proportion_missing = missing_samples / total_samples,
    .groups = "drop"
  )

e1_qe_valid_trials <- e1_qe_exclusions %>%
  filter(proportion_missing < 0.50)

# excluding subjects missing 50% or more trials

e1_qe_subject_exclusions <- e1_qe_exclusions %>%
  group_by(subject) %>%
  summarise(
    total_trials = n(),
    excluded_trials = sum(proportion_missing >= 0.50),
    proportion_trials_excluded = excluded_trials / total_trials,
    .groups = "drop"
  )

e1_qe_valid_subjects <- e1_qe_subject_exclusions %>%
  filter(proportion_trials_excluded < 0.50) 

e1_qe <- merge(
  e1_quiet_eye_data,
  e1_qe_valid_subjects,
  by = "subject"
)

# calculating qe from x and y

e1_quiet_eye_sd <- e1_qe %>%
  group_by(trial, subject) %>%
  summarise(
    sd_gaze_x = sd(right_gaze_x, na.rm = TRUE), 
    sd_gaze_y = sd(right_gaze_y, na.rm = T),
    .groups = "drop"
  )

e1_sd_gaze <- e1_quiet_eye_sd %>%
  group_by(trial, subject) %>%
  summarise(
    sd_gaze = (sd_gaze_x + sd_gaze_y)/2,
    .groups = "drop"
  )

e1_qe_data <- e1_sd_gaze %>%
  group_by(trial, subject) %>%
  summarise(
    qe = log(1/sd_gaze),
    .groups ="drop"
  )

e1_qe_data <- e1_qe_data %>%
  mutate(block = ceiling(trial/28))

e1_condition_data <- e1_summary %>%
  dplyr::select(Subject, condition) %>%
  group_by(Subject, condition) %>%
  summarise() %>%
  rename(subject = Subject)

e1_qe <- merge(e1_qe_data, e1_condition_data, by = "subject")

e1_average_qe_data <- e1_qe %>%
  group_by(subject, condition, trial, block) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(subject, condition, block) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(block, condition) %>%
  summarise(
    mean_qe = mean(qe, na.rm = T),
    se = sd(qe, na.rm = T) / sqrt(n()),
    .groups = "drop"
  )

e1_subject_block_qe_data <- e1_qe %>%
  group_by(subject, condition, trial, block) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(subject, condition, block) %>%
  summarise(qe = mean(qe, na.rm = T))

# ANOVA
e1_qe_anova_afex <- aov_ez(
  id = "subject",
  dv = "qe",
  data = e1_subject_block_qe_data,
  between = "condition",
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)

e1_qe_anova_table <- as.data.frame(e1_qe_anova_afex$anova_table) %>% 
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~round(.x,3)))
e1_qe_anova_table

# e1 average qe by goal plot

pd <- position_dodge(width = 0.15)

e1_qe_block_p <- ggplot(e1_average_qe_data,
                        aes(x = block,
                            y = mean_qe,
                            group = condition,
                            color = condition,
                            linetype = condition, 
                            shape = condition)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 1.5) +
  geom_errorbar(
    aes(
      ymin = mean_qe - se,
      ymax = mean_qe + se
    ),
    width = 0.2,
    position = pd,
    linewidth = 1,
    linetype = 1
  ) +
  scale_linetype_manual(
    values = c(
      "easy goal" = "dotted",
      "hard goal" = "dashed",
      "no goal" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "easy goal" = "gray60",
      "hard goal" = "gray35",
      "no goal" = "black"
    )
  ) +
  labs(x = "Block", 
       y = "Gaze Stability (QE)", 
       linetype = "Condition",
       color = "Condition",
       shape = "Condition")

e1_qe_block_p = e1_qe_block_p + theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.75, .85))
e1_qe_block_p

# RT Bins
e1_qe_data = full_join(e1_qe_data, e1_behavior)

e1_qe_subject_bin = e1_qe_data %>%
  full_join(e1_condition_data) %>%
  group_by(subject, condition, rt_bin) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  filter(!is.na(rt_bin))

e1_qe_bin = e1_qe_subject_bin %>%
  group_by(rt_bin) %>%
  summarise(mean = mean(qe, na.rm = T),
            se = sd(qe, na.rm = T)/sqrt(n())) %>%
  na.omit()

e1_qe_bin_p = ggplot(e1_qe_bin, aes(x = rt_bin, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(stat = 'identity', width = .5, color = 'black', fill = 'grey') +
  geom_line() + 
  geom_point() +
  geom_errorbar(width = .2) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = 'RT Quintile', y = 'Gaze Stability (Quiet Eye)')

e1_qe_p = plot_grid(e1_qe_block_p, e1_qe_bin_p, ncol = 2, labels = 'AUTO')
e1_qe_p
ggsave(e1_qe_p, file = 'Figures/e1_qe.png', height = 6, width = 12, units = 'in', dpi = 600)

qe_anova_afex <- aov_ez(
  id = "subject",
  dv = "qe",
  data = e1_qe_subject_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
qe_anova_table <- as.data.frame(qe_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
qe_anova_table

# Probe responses ####

# Filter to include only trials with probes
e1_clean_proberesp <- e1_behavior %>% filter(proberesp != "")

# Label responses
e1_mw_data <- e1_clean_proberesp %>%
  mutate(
    thought_state = case_when(
      proberesp == 1 ~ "on-task",
      proberesp == 4 ~ "intentional\nMW",
      proberesp == 5 ~ "unintentional\nMW",
      proberesp == 6 ~ 'mind-blanking',
      T ~ NA_character_
    )
  ) 

# make responses a factor
e1_mw_data <- e1_mw_data %>%
  mutate(
    thought_state = factor(thought_state, 
                           levels = c("on-task",
                                      "intentional\nMW",
                                      "unintentional\nMW",
                                      "mind-blanking")
    )
  ) %>%
  select(subject, trial, thought_state, block, rt)

# e1 RT lme

e1_rt_trial <- e1_rts %>%
  group_by(subject, trial, condition) %>%
  summarise(
    avg_rt = mean(rt, na.rm = T),
    .groups = "drop"
  )

e1_mw_rt_lme <- full_join(e1_mw_data, e1_rt_trial, by = c("subject", "trial"))

e1_mw_rt_model <- lmer(avg_rt ~ thought_state + (thought_state|subject), data = e1_mw_rt_lme)
e1_mw_rt_model = lmer(avg_rt ~ thought_state + (1|subject), data = e1_mw_rt_lme)
summary(e1_mw_rt_model)

# Aggregate for plot
e1_mw_rt = e1_mw_data %>%
  group_by(subject, thought_state) %>%
  summarise(rt = mean(rt, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarize(mean = mean(rt, na.rm = T),
            se = sd(rt, na.rm = T)/sqrt(n())) %>%
  na.omit()

# Plot

e1_rt_mw_p = ggplot(e1_mw_rt, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Reaction Time (ms)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e1_rt_mw_p

e1_pretrial_clean <- e1_pretrial_data %>%
  group_by(subject) %>%
  mutate(z = scale(pretrial_pupil)[,1])

e1_mw_pretrial_data <- full_join(e1_mw_data, e1_pretrial_clean, by = c("subject", "trial")) 


# Linear mixed effects model
e1_mw_pretrial_model <- lmer(z ~ thought_state + (1|subject), data = e1_mw_pretrial_data)
summary(e1_mw_pretrial_model)

# Aggregate for plot
e1_mw_pretrial = e1_mw_pretrial_data %>%
  group_by(subject, thought_state) %>%
  summarize(z = mean(z, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarize(mean = mean(z, na.rm = T),
            se = sd(z, na.rm = T)/sqrt(n())) %>%
  na.omit()

e1_pretrial_mw_p = ggplot(e1_mw_pretrial, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se))+
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Pretrial Pupil (normalized)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e1_pretrial_mw_p

# e1 TEPR lme
e1_tepr_mw_data = full_join(e1_mw_data, e1_phasic_data) %>%
  filter(!is.na(thought_state))

table(e1_tepr_mw_data$thought_state)

e1_tepr_mw = e1_tepr_mw_data %>%
  group_by(subject, thought_state, bin) %>%
  mutate(z = scale(pupil_change)[1]) %>%
  filter(abs(z) < 3) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T)) %>%
  mutate(z = scale(pupil_change)[1]) %>%
  filter(abs(z) < 3) %>%
  group_by(thought_state, bin) %>%
  summarise(mean = mean(pupil_change, na.rm = T),
            se = sd(pupil_change, na.rm = T)/sqrt(n()))

# Plot
e1_tepr_mw_p = ggplot(e1_tepr_mw, aes(x = bin, y = mean, group = thought_state, color = thought_state, linetype = thought_state)) +
  geom_line(linewidth = 1) +
  labs(x = 'Time (ms)', y = 'Task-evoked Response (mm)', color = 'Probe Response', linetype = 'Probe Response') +
  theme_bw(base_size = 17) +
  scale_color_grey(start = 0.1, end = 0.8) +
  ylim(-.02, .14) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))
e1_tepr_mw_p

e1_tepr_mw_lme = e1_tepr_mw_data %>%
  filter(between(bin, 500, 800)) %>%
  group_by(subject, thought_state, trial) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(subject, thought_state) %>%
  summarise(tepr = mean(pupil_change, na.rm = T))

e1_mw_tepr_model = lmer(tepr ~ thought_state + (1|subject), data = e1_tepr_mw_lme)
summary(e1_mw_tepr_model)

# e1 QE lme

e1_qe_clean <- e1_qe %>%
  group_by(subject, trial) %>%
  summarise(qe = mean(qe, na.rm = T))

e1_mw_qe_lme <- full_join(e1_mw_data, e1_qe_clean) %>%
  filter(!is.na(thought_state))

e1_mw_qe_model = lmer(qe ~ thought_state + (thought_state|subject), data = e1_mw_qe_lme)
e1_mw_qe_model = lmer(qe ~ thought_state + (1|subject), data = e1_mw_qe_lme)
summary(e1_mw_qe_model)

# Aggregate for plot
e1_qe_mw = e1_mw_qe_lme %>%
  group_by(subject, thought_state) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarise(mean = mean(qe, na.rm = T),
            se = sd(qe, na.rm = T)/sqrt(n()))

e1_qe_mw_p = ggplot(e1_qe_mw, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(stat = 'identity', width = .5, color = 'black', fill = 'grey') +
  geom_point() +
  geom_errorbar(width = .2, position = position_dodge(.2)) +
  labs(x = 'Probe Response', y = 'Gaze Stability (Quiet Eye)') +
  theme_bw(base_size = 17) +
  scale_color_grey(start = 0.1, end = 0.8) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))
e1_qe_mw_p

e1_probe_plots = plot_grid(e1_rt_mw_p, e1_pretrial_mw_p, e1_tepr_mw_p, e1_qe_mw_p, ncol = 2, labels = 'AUTO')
e1_probe_plots
ggsave(e1_probe_plots, file = 'Figures/e1_probe.png', height = 12, width = 12, units = 'in', dpi = 600)

# Individual Differences ####
e1_behavior_subject = e1_behavior %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject) %>%
  summarize(rt_mean = mean(rt, na.rm = T))

e1_mot_alert_subject = e1_behavior %>%
  group_by(subject, block) %>%
  summarise(motivation = mean(motivation, na.rm = T),
            alertness = mean(alertness, na.rm = T)) %>%
  group_by(subject) %>%
  summarise(motivation = mean(motivation, na.rm = T),
            alertness = mean(alertness, na.rm = T))

e1_pretrial_subject = e1_pretrial_data %>%
  group_by(subject) %>%
  summarize(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/pretrial_mean)

e1_tepr_subject = e1_max_pupil %>%
  group_by(subject) %>%
  summarize(tepr = mean(max_pupil, na.rm = T))

e1_qe_subject = e1_qe_data %>%
  group_by(subject) %>%
  summarize(qe = mean(qe, na.rm = T))

e1_subject = full_join(e1_behavior_subject, e1_pretrial_subject) %>%
  full_join(e1_tepr_subject) %>%
  full_join(e1_qe_subject) %>%
  full_join(e1_mot_alert_subject)

corr.test(select(e1_subject, rt_mean, motivation, alertness, pretrial_cv, tepr, qe))
describe(select(e1_subject, rt_mean, motivation, alertness, pretrial_cv, tepr, qe)) %>%
  select(mean, sd)

# Experiment 2 ====

# Behavior ####
e2_behavior_data <- read_csv("Data/Experiment 2/behavior/e2_rts.csv") %>%
  mutate(
    trial = (block - 1)*28 + trial,
    experiment = str_remove(experiment, "^PVT150"),
    experiment = str_remove(experiment, "TET$"),
    condition = case_when(
      experiment == "goalfeedback"     ~ "goal + feedback",
      experiment == "nogoalfeedback"   ~ "no goal + feedback",
      experiment == "goalnofeedback"   ~ "goal + no feedback",
      experiment == "nogoalnofeedback" ~ "no goal + no feedback",
      TRUE ~ experiment),
    goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                     condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
    feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                         condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback',
                         TRUE ~ experiment)
  ) %>%
  group_by(subject) %>%
  arrange(rt, .by_group = TRUE) %>%   # fastest to slowest
  mutate(rt_bin = ntile(rt, 5))

e2_conditions = e2_behavior_data %>%
  group_by(subject, condition) %>%
  summarise()

e2_rt_subject_block = e2_behavior_data %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject, block, condition, goal, feedback) %>%
  summarise(rt = mean(rt, na.rm = T))

e2_rt_block_goal_feedback = e2_rt_subject_block %>%
  group_by(block, goal, feedback) %>%
  mutate(z = scale(rt)[,1]) %>%
  filter(abs(z) < 3) %>%
  summarise(mean = mean(rt, na.rm = T),
            se = sd(rt, na.rm = T)/sqrt(n()))

e2_average_rt_p <- ggplot(
  e2_rt_block_goal_feedback,
  aes(
    x = block,
    y = mean,
    ymin = mean - se, 
    ymax = mean + se,
    group = feedback,
    color = feedback
  )
) +
  geom_line(
    aes(linetype = feedback),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = feedback, color = feedback),
    position = pd
  ) +
  geom_errorbar(
    width = 0.20,
    position = pd,
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "feedback",
    values = c("no feedback" = 15,
               "feedback" = 17)
  ) +
  scale_linetype_manual(
    values = c(
      name = "feedback",
      "feedback" = "dotted",
      "no feedback" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      name = "feedback",
      "feedback" = "gray60",
      "no feedback" = "black"
    )
  ) +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  theme_bw(base_size = 17) +
  theme(legend.position = 'inside',
        legend.position.inside = c(.3, .9),
        axis.text = element_text(color = 'black'),
        legend.title = element_blank()) +
  facet_grid(cols = vars(goal))
e2_average_rt_p
ggsave(e2_average_rt_p, file = 'Figures/e2_rt.png', height = 6, width = 12, units = 'in', dpi = 600)

# Pretrial ####

e2_pretrial_files <- list.files(
  path = "Data/Experiment 2/pretrial x trial",
  pattern = "^e2_pretrial_trial_[0-9]+\\.tsv$",
  full.names = TRUE
)
e2_pretrial_data <- read_tsv(e2_pretrial_files)

e2_pretrial_data <- e2_pretrial_data %>%
  select(Subject, TrialId, pretrial_pupil) %>%
  rename(trial = TrialId,
         subject = Subject)

e2_pretrial_data <- e2_pretrial_data %>%
  mutate(block = ceiling(trial / 28))

e2_pretrial_data = e2_pretrial_data %>%
  filter(subject %in% unique(e2_behavior_data$subject))

e2_pretrial_data <- full_join(e2_pretrial_data, e2_behavior_data)

# exclude participants misisng 50% of the data

e2_pretrial_data = e2_pretrial_data %>%
  group_by(subject) %>%
  mutate(missing = ifelse(is.na(pretrial_pupil), 1, 0),
         missing = mean(missing, na.rm = T)) %>%
  filter(missing < .5)

# avg pretrial diameter by condition

e2_summary <- e2_pretrial_data %>%
  group_by(subject, condition, block) %>%
  summarise(
    mean_pupil = mean(pretrial_pupil, na.rm = TRUE),
    sd_pupil = sd(pretrial_pupil, na.rm = TRUE),
    cv_pupil = sd_pupil/mean_pupil,
    .groups = "drop" )

# summary by block, experiment

e2_pretrial_block_mean <- e2_pretrial_data %>%
  group_by(subject, condition, block) %>%
  summarise(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_sd = sd(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/mean(pretrial_pupil, na.rm = T)) %>%
  group_by(condition, block) %>%
  summarise(mean = mean(pretrial_mean, na.rm  = T),
            se = sd(pretrial_mean, na.rm = T)/sqrt(n())) %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  na.omit()

# avg pretrial pupil diameter plot by experiment

e2_condition_mean_p <- ggplot(e2_pretrial_block_mean, aes(x = block, y = mean, linetype = feedback,
                                                          ymin = mean - se,
                                                          ymax = mean + se,
                                                          shape = feedback, 
                                                          group = feedback))  +
  geom_line(position = position_dodge(.4)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(width =.2, position = position_dodge(.4), linetype = 1) +
  facet_grid(cols = vars(goal)) +
  scale_linetype_manual(
    name = "feedback",
    values = c(
      "feedback" = "dashed",
      "no feedback" = "solid"
    )
  ) +
  labs(x = "Block", y = "Pretrial pupil diameter (mm)", color = NULL, linetype = NULL) +
  
  ylim(2.5 , 3.1) + 
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .25))
e2_condition_mean_p

# cv summary by block, experiment

e2_pretrial_block_cv = e2_pretrial_data %>%
  group_by(subject, condition, block) %>%
  summarise(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_sd = sd(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/mean(pretrial_pupil, na.rm = T)) %>%
  group_by(condition, block) %>%
  summarise(mean = mean(pretrial_cv, na.rm  = T),
            se = sd(pretrial_cv, na.rm = T)/sqrt(n())) %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  na.omit()

# cv plot by condition

e2_condition_cv_p <- ggplot(e2_pretrial_block_cv, aes(x = block, y = mean, linetype = feedback,
                                                      ymin = mean - se,
                                                      ymax = mean + se,
                                                      group = feedback))  +
  geom_line(position = position_dodge(.4)) +
  scale_linetype_manual(
    name = "feedback",
    values = c(
      "feedback" = "dashed",
      "no feedback" = "solid"
    )
  ) +
  xlab("Block") +
  ylab("Pretrial pupil variability") +
  geom_errorbar(width =.2, position = position_dodge(0.4), linetype = 1) +
  ylim(0.04 , 0.09) + 
  facet_grid(cols = vars(goal)) + 
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'none')

e2_condition_cv_p

# ANOVAs
e2_pretrial_subject_block = e2_pretrial_data %>%
  group_by(subject, condition, block) %>%
  summarise(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/mean(pretrial_pupil, na.rm = T)) %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback'))


pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_mean",
  data = e2_pretrial_subject_block,
  between = c("goal", "feedback"),
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_cv",
  data = e2_pretrial_subject_block,
  between = c("goal", "feedback"),
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

# RT Bin Analysis
e2_pretrial_subject_bin <- e2_pretrial_data %>%
  group_by(subject) %>%
  mutate(z = scale(pretrial_pupil)[,1],
         rt_rank = rank(rt),
         rt_bin = ntile(rt, 5)) %>%
  group_by(subject, condition, rt_bin) %>%
  summarise(
    pretrial_pupil = mean(z, na.rm = T)) %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  na.omit()

pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_pupil",
  data = e2_pretrial_subject_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

e2_pretrial_bin = e2_pretrial_subject_bin %>%
  group_by(rt_bin) %>%
  summarize(mean = mean(pretrial_pupil, na.rm = T),
            se = sd(pretrial_pupil, na.rm = T)/sqrt(n())) %>%
  na.omit()

e2_pretrial_bin_p = ggplot(e2_pretrial_bin, aes(x = rt_bin, y = mean, ymin = mean - se, ymax = mean + se)) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = .2) +
  labs(x = 'RT Quintile', y = 'Pretrial Pupil (normalized)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))

e2_pretrial_plots = plot_grid(plot_grid(e2_condition_mean_p, e2_condition_cv_p, ncol = 2, labels = 'AUTO'), 
                              plot_grid(NULL, e2_pretrial_bin_p, NULL, ncol = 3, rel_widths = c(1, 2, 1), labels = c("", "C", "")), 
                              ncol = 1)
e2_pretrial_plots
ggsave(e2_pretrial_plots, file = 'Figures/e2_pretrial.png', height = 10, width = 10, units = 'in', dpi = 600)

# Phasic ####
e2_phasic_files <- list.files(
  path = "Data/Experiment 2/phasic x trial",
  pattern = "^e2_phasic_trial_[0-9]+\\.tsv$",
  full.names = TRUE
)

e2_phasic_data <- read_tsv(e2_phasic_files)

e2_phasic_data = e2_phasic_data %>%
  filter(Subject %in% unique(e2_conditions$subject))

length(unique(e2_phasic_data$Subject))

table(e2_phasic_data$condition)

e2_phasic_data <- e2_phasic_data %>%
  select(-condition) %>%
  mutate(block = ceiling(TrialId / 28)) %>%
  rename(subject = Subject,
         trial = TrialId)

e2_phasic_data = e2_phasic_data %>%
  full_join(e2_conditions)

table(e2_phasic_data$condition)

# excluding missing samples within trials
e2_phasic_data = e2_phasic_data %>%
  group_by(subject, trial) %>%
  mutate(missing = ifelse(is.na(pupil_change), 1, 0),
         missing = mean(missing)) %>%
  filter(missing < .5)

# TEPR waveform

e2_waveform_data <- e2_phasic_data %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  group_by(subject, block, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(block, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  )

e2_TEPR_block <- ggplot(e2_waveform_data,
                        aes(x = bin,
                            y = mean_response,
                            group = factor(block),
                            color = factor(block))) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "Block"
  ) + ylim(-0.02, 0.14) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e2_TEPR_block

# create phasic df with condition
table(e2_phasic_data$condition)


# TEPR by condition
e2_tepr_condition_data <- e2_phasic_data %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  group_by(subject, goal, feedback, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(goal, feedback, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()

e2_tepr_condition_p <- ggplot(data = e2_tepr_condition_data,
                              aes(x = bin,
                                  y = mean_response,
                                  group = factor(feedback),
                                  color = factor(feedback))) +
  geom_line(linewidth = 1) +
  labs(color = "Feedback") +
  scale_color_grey(start = 0.2, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = NULL
  ) + ylim(-0.02, 0.15) +
  facet_grid(cols = vars(goal)) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.75, .85))

e2_tepr_condition_p

# maxmimum pupil dilation by condition

e2_maximum_pupil <- e2_phasic_data %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  select(subject, block, trial, pupil_change, goal, feedback, bin) %>%
  filter(bin >= 500, bin <= 800)

e2_maximum_data <- e2_maximum_pupil %>%
  filter(!is.na(pupil_change)) %>%
  filter(bin >= 500, bin <= 800) %>%
  group_by(subject, trial, block, goal, feedback) %>%
  summarise(
    max_pupil = max(pupil_change, na.rm = T),
    .groups = "drop"
  ) %>%
  group_by(subject, block, goal, feedback) %>%
  summarise(max_pupil = mean(max_pupil, na.rm = T))

# TEPR ANOVA
tepr_anova_afex <- aov_ez(
  id = "subject",
  dv = "max_pupil",
  data = e2_maximum_data,
  between = c("goal", "feedback"),
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
tepr_anova_afex <- as.data.frame(tepr_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
tepr_anova_afex

# max dilation summary by block, condition

e2_experiment_block_max = e2_maximum_data %>%
  group_by(goal, feedback, block) %>%
  summarise(mean = mean(max_pupil, na.rm  = T),
            se = sd(max_pupil, na.rm = T)/sqrt(n()))

e2_condition_max_p <- ggplot(e2_experiment_block_max, aes(x = block, y = mean, linetype = feedback,
                                                          ymin = mean - se,
                                                          ymax = mean + se,
                                                          group = feedback))  +
  geom_line(position = position_dodge(.4)) +
  scale_linetype_manual(
    name = "feedback",
    values = c(
      "feedback" = "dashed",
      "no feedback" = "solid"
    )
  ) +
  xlab("Block") +
  ylab("Maximum Dilation") +
  geom_errorbar(width =.2, position = position_dodge(0.4), linetype = 1) +
  ylim(0 , 0.24) +
  facet_grid(rows = vars(goal)) + 
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .65),
        legend.title = element_blank())
e2_condition_max_p

# RT Quintile analysis
e2_phasic_data = e2_phasic_data %>%
  full_join(e2_behavior_data)

e2_phasic_subject_rt_bin = e2_phasic_data %>%
  group_by(subject, rt_bin, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T))

e2_max_pupil_subject_rt_bin = e2_phasic_data %>%
  filter(between(bin, 500, 800)) %>%
  group_by(subject, rt_bin, trial) %>%
  summarise(max_pupil = max(pupil_change, na.rm = T)) %>%
  mutate(max_pupil = ifelse(is.infinite(max_pupil), NA, max_pupil)) %>%
  group_by(subject, rt_bin) %>%
  summarise(max_pupil = mean(max_pupil, na.rm = T)) %>%
  na.omit()

e2_phasic_rt_bin = e2_phasic_subject_rt_bin %>%
  group_by(rt_bin, bin) %>%
  summarise(mean = mean(pupil_change, na.rm = T),
            se = sd(pupil_change, na.rm = T)/sqrt(n())) %>%
  na.omit()

e2_tepr_quintile_p <- ggplot(data = e2_phasic_rt_bin,
                             aes(x = bin,
                                 y = mean,
                                 group = factor(rt_bin),
                                 color = factor(rt_bin))) +
  geom_line(linewidth = 1) +
  labs(color = "RT Quintile") +
  scale_color_grey(start = 0.2, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "RT Quintile"
  ) + ylim(-0.02, 0.15) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.2, .7))

e2_tepr_quintile_p

e2_tepr_plots = plot_grid(e2_TEPR_block, e2_tepr_condition_p, e2_condition_max_p,e2_tepr_quintile_p,
                          ncol = 2, labels = 'AUTO')
e2_tepr_plots
ggsave(e2_tepr_plots, file = 'Figures/e2_tepr.png', height = 10, width = 10, units = 'in', dpi = 600)

# TEPR ANOVA
tepr_anova_afex <- aov_ez(
  id = "subject",
  dv = "max_pupil",
  data = e2_max_pupil_subject_rt_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
tepr_anova_afex <- as.data.frame(tepr_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
tepr_anova_afex

# Quiet Eye ####
e2_qe_files <- list.files(
  path = "Data/Experiment 2/quiet eye",
  pattern = "^e2_wait_eye_subject_trial_bin_[0-9]+\\.csv$",
  full.names = T
)

e2_qe_data <- read_csv(e2_qe_files)

# excluding missing samples within trials

e2_qe_data <- e2_qe_data %>%
  group_by(Subject, TrialId) %>%
  mutate(missing = ifelse(is.na(right_gaze_x), 1, 0),
         missing = mean(missing)) %>%
  filter(missing < .5) %>%
  group_by(Subject) %>%
  mutate(n_trials = length(unique(TrialId))) %>%
  filter(n_trials > 70)

e2_qe_sd <- e2_qe_data %>%
  group_by(TrialId, Subject) %>%
  summarise(
    sd_gaze_x = sd(right_gaze_x, na.rm = T),
    sd_gaze_y = sd(right_gaze_y, na.rm = T),
    .groups = "drop"
  )

e2_sd_gaze <- e2_qe_sd %>%
  group_by(TrialId, Subject) %>%
  summarise(
    sd_gaze = (sd_gaze_x + sd_gaze_y)/2,
    .groups = "drop"
  )

e2_qe <- e2_sd_gaze %>%
  group_by(TrialId, Subject) %>%
  summarise(
    qe = log(1/sd_gaze),
    .groups = "drop"
  )

e2_qe <- e2_qe %>%
  mutate(block = ceiling(TrialId/28)) %>%
  rename(subject = Subject,
         trial = TrialId) %>%
  full_join(e2_behavior_data) %>%
  filter(subject %in% unique(e2_behavior_data$subject))

e2_qe_subject_block = e2_qe %>%
  mutate(goal = case_when(condition == 'goal + feedback' | condition == 'goal + no feedback' ~ 'goal',
                          condition == 'no goal + feedback' | condition == 'no goal + no feedback' ~ 'no goal'),
         feedback = case_when(condition == 'goal + feedback' | condition == 'no goal + feedback' ~ 'feedback',
                              condition == 'goal + no feedback' | condition == 'no goal + no feedback' ~ 'no feedback')) %>%
  group_by(subject, condition, goal, feedback, block, trial) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(subject, condition, goal, feedback, block) %>%
  summarise(qe = mean(qe, na.rm = T))

e2_average_qe = e2_qe_subject_block %>%
  group_by(block, condition, goal, feedback) %>%
  summarise(
    mean_qe = mean(qe, na.rm = T),
    se = sd(qe, na.rm = T)/sqrt(n()),
    .groups = "drop" 
  )
# e2 averge QE by condition

pd <- position_dodge(width = 0.15)

e2_qe_block_p <- ggplot(e2_average_qe,
                        aes(x = block,
                            y = mean_qe,
                            group = feedback,
                            linetype = feedback)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 1.5) +
  geom_errorbar(
    aes(
      ymin = mean_qe - se,
      ymax = mean_qe + se
    ),
    width = 0.1,
    position = pd,
    linetype = 1,
    linewidth = 0.6
  ) +
  scale_linetype_manual(
    name = "feedback",
    values = c(
      "feedback" = "dashed",
      "no feedback" = "solid"
    )
  ) +
  xlab("Block") +
  ylab("Gaze Stability (QE)") +
  theme_bw(base_size = 17) +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.75, .4),
    axis.text = element_text(color = 'black')
  ) +
  facet_grid(rows = vars(goal))

e2_qe_block_p

# ANOVA
e2_qe_anova_afex <- aov_ez(
  id = "subject",
  dv = "qe",
  data = e2_qe_subject_block,
  between = c("goal", "feedback"),
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)

e2_qe_anova_table <- as.data.frame(e2_qe_anova_afex$anova_table) %>% 
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~round(.x,3)))
e2_qe_anova_table

# RT Quintile Analyses

e2_qe_subject_rt_bin = e2_qe %>%
  group_by(subject, rt_bin) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  filter(!is.na(rt_bin))

e2_qe_rt_bin =e2_qe_subject_rt_bin %>%
  group_by(rt_bin) %>%
  summarise(mean = mean(qe, na.rm = T),
            se = sd(qe, na.rm = T)/sqrt(n())) %>%
  na.omit()

e2_qe_bin_p = ggplot(e2_qe_rt_bin, aes(x = rt_bin, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(stat = 'identity', width = .5, color = 'black', fill = 'grey') +
  geom_line() + 
  geom_point() +
  geom_errorbar(width = .2) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = 'RT Quintile', y = 'Gaze Stability (Quiet Eye)')

e2_qe_p = plot_grid(e2_qe_block_p, e2_qe_bin_p, ncol = 2, labels = 'AUTO')
e2_qe_p
ggsave(e2_qe_p, file = 'Figures/e2_qe.png', height = 6, width = 12, units = 'in', dpi = 600)

qe_anova_afex <- aov_ez(
  id = "subject",
  dv = "qe",
  data = e2_qe_subject_rt_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
qe_anova_table <- as.data.frame(qe_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
qe_anova_table

# Thought Probes ####
e2_clean_proberesp <- e2_behavior_data %>% filter(proberesp != "")

e2_mw_data <- e2_clean_proberesp %>%
  mutate(
    thought_state = case_when(
      proberesp == 1 ~ "on-task",
      proberesp == 4 ~ "intentional\nMW",
      proberesp == 5 ~ "unintentional\nMW",
      proberesp == 6 ~ 'mind-blanking',
      T ~ NA_character_
    )
  )

# RT

e2_rts <- e2_mw_data %>%
  mutate(rt = as.numeric(rt)) %>%
  filter(between(rt, 200, 3000))
e2_rts$thought_state = factor(e2_rts$thought_state, levels = c("on-task", "unintentional\nMW", "intentional\nMW", "mind-blanking"))

e2_mw_rt_model <- lmer(rt ~ thought_state + (thought_state|subject), data = e2_rts)
e2_mw_rt_model = lmer(rt ~ thought_state + (1|subject), data = e2_rts)
summary(e2_mw_rt_model)

e2_rt_thought_state = e2_rts %>%
  group_by(subject, thought_state) %>%
  summarise(rt = mean(rt, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarise(mean = mean(rt, na.rm = T),
            se = sd(rt, na.rm = T)/sqrt(n())) %>%
  na.omit()

e2_rt_mw_p = ggplot(e2_rt_thought_state, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Reaction Time (ms)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e2_rt_mw_p

# Pretrial
e2_pretrial_data <- e2_pretrial_data %>%
  group_by(subject) %>%
  mutate(z = scale(pretrial_pupil)[,1])

e2_pretrial_data = e2_pretrial_data %>%
  mutate(thought_state = case_when(
    proberesp == 1 ~ "on-task",
    proberesp == 4 ~ "intentional\nMW",
    proberesp == 5 ~ "unintentional\nMW",
    proberesp == 6 ~ 'mind-blanking',
    T ~ NA_character_))

e2_mw_pretrial = filter(e2_pretrial_data, !is.na(thought_state))

e2_mw_pretrial$thought_state = factor(e2_mw_pretrial$thought_state, levels = c("on-task", "unintentional\nMW", "intentional\nMW", "mind-blanking"))

e2_mw_pretrial_model <- lmer(z ~ thought_state + (thought_state|subject), data = e2_mw_pretrial)
e2_mw_pretrial_model = lmer(z ~ thought_state + (1|subject), data = e2_mw_pretrial)
summary(e2_mw_pretrial_model)

e2_pretrial_thought_state = e2_mw_pretrial %>%
  group_by(subject, thought_state) %>%
  summarise(z = mean(z, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarise(mean = mean(z, na.rm = T),
            se = sd(z, na.rm = T)/sqrt(n())) %>%
  na.omit()

e2_pretrial_mw_p = ggplot(e2_pretrial_thought_state, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Pretrial Pupil (normalized)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e2_pretrial_mw_p

# TEPR

e2_phasic_trial <- e2_phasic_data %>%
  mutate(thought_state = case_when(
    proberesp == 1 ~ "on-task",
    proberesp == 4 ~ "intentional\nMW",
    proberesp == 5 ~ "unintentional\nMW",
    proberesp == 6 ~ 'mind-blanking',
    T ~ NA_character_))

e2_phasic_state_bin = e2_phasic_trial %>%
  group_by(subject, thought_state, bin) %>%
  summarize(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(thought_state, bin) %>%
  summarize(mean = mean(pupil_change, na.rm = T)) %>%
  na.omit()

e2_phasic_state_bin$thought_state = factor(e2_phasic_state_bin$thought_state, levels = c("on-task", "unintentional\nMW", "intentional\nMW", "mind-blanking"))

# Plot
e2_tepr_mw_p = ggplot(e2_phasic_state_bin, aes(x = bin, y = mean, group = thought_state, color = thought_state, linetype = thought_state)) +
  geom_line(linewidth = 1) +
  labs(x = 'Time (ms)', y = 'Task-evoked Response (mm)', color = 'Probe Response', linetype = 'Probe Response') +
  theme_bw(base_size = 17) +
  scale_color_grey(start = 0.1, end = 0.8) +
  ylim(-.04, .14) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))
e2_tepr_mw_p

e2_phasic_trial = e2_phasic_trial %>%
  group_by(subject, trial, thought_state) %>%
  filter(between(bin, 500, 800)) %>%
  summarise(tepr = max(pupil_change, na.rm = T)) %>%
  mutate(tepr = ifelse(is.infinite(tepr), NA, tepr)) %>%
  na.omit()

e2_phasic_trial$thought_state = factor(e2_phasic_trial$thought_state, levels = c("on-task", "unintentional\nMW", "intentional\nMW", "mind-blanking"))

e2_mw_tepr_model <- lmer(tepr ~ thought_state + (1|subject), data = e2_phasic_trial)
round(summary(e2_mw_tepr_model)$coefficients, digits = 3)

# e2 QE lme

e2_qe_lme_data <- e2_qe %>%
  mutate(thought_state = case_when(
    proberesp == 1 ~ "on-task",
    proberesp == 4 ~ "intentional\nMW",
    proberesp == 5 ~ "unintentional\nMW",
    proberesp == 6 ~ 'mind-blanking',
    T ~ NA_character_))

e2_qe_lme_data$thought_state = factor(e2_qe_lme_data$thought_state, levels = c("on-task", "unintentional\nMW", "intentional\nMW", "mind-blanking"))

e2_mw_qe_model = lmer(qe ~ thought_state + (thought_state|subject), data = e2_qe_lme_data)
e2_mw_qe_model = lmer(qe ~ thought_state + (1|subject), data = e2_qe_lme_data)
summary(e2_mw_qe_model)

# Aggregate for plotting
e2_qe_thought_state = e2_qe_lme_data %>%
  group_by(subject, thought_state) %>%
  summarize(qe = mean(qe, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarize(mean = mean(qe, na.rm = T),
            se = sd(qe, na.rm = T)/sqrt(n())) %>%
  na.omit()

e2_qe_mw_p = ggplot(e2_qe_thought_state, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Gaze Stability (QE)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e2_qe_mw_p

# Combine probe response plots
e2_probe_plots = plot_grid(e2_rt_mw_p, e2_pretrial_mw_p, e2_tepr_mw_p, e2_qe_mw_p, ncol = 2, labels = 'AUTO') 
e2_probe_plots
ggsave(e2_probe_plots, file = 'Figures/e2_probe.png', height = 12, width = 12, units = 'in', dpi = 600)

# Individual differences ####
e2_behavior_subject = e2_behavior_data %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject) %>%
  summarize(rt_mean = mean(rt, na.rm = T))

e2_mot_alert_subject = e2_behavior_data %>%
  group_by(subject, block) %>%
  summarise(motivation = mean(motivation, na.rm = T),
            alertness = mean(alertness, na.rm = T)) %>%
  group_by(subject) %>%
  summarise(motivation = mean(motivation, na.rm = T),
            alertness = mean(alertness, na.rm = T))

e2_pretrial_subject = e2_pretrial_data %>%
  group_by(subject) %>%
  summarize(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/pretrial_mean)

e2_tepr_subject = e2_maximum_data %>%
  group_by(subject) %>%
  summarize(tepr = mean(max_pupil, na.rm = T))

e2_qe_subject = e2_qe_lme_data %>%
  group_by(subject) %>%
  summarize(qe = mean(qe, na.rm = T))

e2_subject = full_join(e2_behavior_subject, e2_pretrial_subject) %>%
  full_join(e2_tepr_subject) %>%
  full_join(e2_qe_subject) %>%
  full_join(e2_mot_alert_subject)

describe(select(e2_subject, rt_mean, motivation, alertness, pretrial_cv, tepr, qe)) %>%
  select(mean, sd)

corr.test(select(e2_subject, rt_mean, motivation, alertness, pretrial_cv, tepr, qe))

# Experiment 3 #####

# Behavior ####
e3_behavior <- read.csv("Data/Experiment 3/behavior/e3_rts.csv")
e3_behavior <- e3_behavior %>%
  mutate(
    experiment = str_remove(experiment, "PVT150"),
    condition2 = str_remove(experiment, "300TET"),
    condition = case_when(condition2 == "goalfeedback$TET" ~ 'no incentive',
                          condition2 == "goalfeedbackTET"  ~ 'cash incentive',
                          condition2 == "goalfeedbacktimeTET"  ~ 'time incentive')) %>%
  group_by(subject) %>%
  arrange(rt, .by_group = TRUE) %>%   # fastest to slowest
  mutate(rt_bin = ntile(rt, 5)) %>%      # 1 = fastest, 5 = slowest
  ungroup() %>%
  mutate(trial = (block - 1) * 28 + trial)

e3_rt_subject_block = e3_behavior %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject, condition, block) %>%
  summarize(rt = mean(rt, na.rm = T)) 

e3_rt_block_condition = e3_rt_subject_block %>%
  group_by(condition, block) %>%
  mutate(z = scale(rt)[,1]) %>%
  filter(abs(z) < 3) %>%
  summarize(mean = mean(rt, na.rm = T),
            se = sd(rt, na.rm = T)/sqrt(n()))

e3_average_rt_p <- ggplot(
  e3_rt_block_condition,
  aes(
    x = block,
    y = mean,
    ymin = mean - se, 
    ymax = mean + se,
    group = condition,
    color = condition
  )
) +
  geom_line(
    aes(linetype = condition),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = condition, color = condition),
    position = pd
  ) +
  geom_errorbar(
    width = 0.20,
    position = pd,
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "condition",
    values = c("time incentive" = 15,
               "cash incentive" = 16,
               "no incentive" = 17)
  ) +
  scale_linetype_manual(
    values = c(
      "time incentive" = "dotted",
      "cash incentive" = "dashed",
      "no incentive" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "time incentive" = "gray60",
      "cash incentive" = "gray35",
      "no incentive" = "black"
    )
  ) +
  xlab("Block") +
  ylab("Reaction Time (ms)") +
  theme_bw(base_size = 17) +
  theme(legend.position = 'inside',
        legend.position.inside = c(.2, .8),
        axis.text = element_text(color = 'black'))
e3_average_rt_p
ggsave(e3_average_rt_p, file = 'Figures/e3_rt.png', height = 6, width = 6, units = 'in', dpi = 600)

# Pretrial ####

e3_pretrial_files <- list.files(
  path = "Data/Experiment 3/pretrial x trial",
  pattern = "^e3_pretrial_trial_[0-9]+\\.tsv$",
  full.names = TRUE
)
e3_pretrial_data <- read_tsv(e3_pretrial_files)

e3_pretrial_data <- e3_pretrial_data %>%
  rename(trial = TrialId,
         subject = Subject) %>%
  mutate(block = ceiling(trial / 28)) %>%
  group_by(subject) %>%
  mutate(missing = ifelse(is.na(pretrial_pupil), 1, 0),
         missing = mean(missing, na.rm = T)) %>%
  filter(missing < .5) %>%# excluding participants missing 50% of data
  full_join(e3_behavior)

e3_summary <- e3_pretrial_data %>%
  full_join(e3_behavior) %>%
  group_by(subject, block, condition) %>%
  summarise(
    mean_pupil = mean(pretrial_pupil, na.rm = TRUE),
    sd_pupil = sd(pretrial_pupil, na.rm = TRUE),
    cv_pupil = sd(pretrial_pupil, na.rm = TRUE)/mean(pretrial_pupil, na.rm = TRUE),
    .groups = "drop"
  )

table(e3_summary$condition)

# average arousal by condition plot 

e3_average_arousal_data <- e3_summary %>%
  group_by(condition, block) %>%
  summarise(
    group_mean = mean(mean_pupil, na.rm = TRUE),
    se = sd(mean_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  na.omit()

pd <- position_dodge(width = 0.15)

e3_average_arousal_p <- ggplot(
  e3_average_arousal_data,
  aes(
    x = factor(block),
    y = group_mean,
    group = condition,
    color = condition
  )
) +
  geom_line(
    aes(linetype = condition),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = condition, color = condition),
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = group_mean - se,
      ymax = group_mean + se
    ),
    width = 0.20,
    position = pd,
    linewidth = 1
  ) +
  scale_shape_manual(
    name = "condition",
    values = c("no incentive" = 15,
               "cash incentive" = 16,
               "time incentive" = 17)
  ) +
  scale_linetype_manual(
    values = c(
      "no incentive" = "dotted",
      "cash incentive" = "dashed",
      "time incentive" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "no incentive" = "gray60",
      "cash incentive" = "gray35",
      "time incentive" = "black"
    )
  ) +
  xlab("Block") +
  ylab("Pretrial pupil diameter (mm)") +
  coord_cartesian(ylim = c(2.7, 3.3)) + 
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.75, .75))

e3_average_arousal_p

# coefficient of varition by condition

e3_cv_plot_data <- e3_summary %>%
  group_by(condition, block) %>%
  summarise(
    group_cv = mean(cv_pupil, na.rm = TRUE),
    se = sd(cv_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(condition %in% c("no incentive", "cash incentive", "time incentive"))

pd <- position_dodge(width = 0.18)

e3_cv_p <- ggplot(
  e3_cv_plot_data,
  aes(
    x = factor(block),
    y = group_cv,
    group = condition,
    linetype = condition,
    color = condition
  )
) +
  geom_line(
    position = pd,
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = condition, 
      shape = condition),
    position = pd) +
  geom_errorbar(
    aes(
      ymin = group_cv - se,
      ymax = group_cv + se,
      color = condition
    ),
    position = pd,
    width = 0.2,
    linewidth = 1,
    linetype = 1
  ) +
  scale_shape_manual(
    name = "condition",
    values = c("no incentive" = 15,
               "cash incentive" = 16,
               "time incentive" = 17)
  ) +
  scale_color_manual(
    values = c(
      "no incentive" = "gray60",
      "cash incentive" = "gray35",
      "time incentive" = "black"
    )) +
  scale_linetype_manual(
    name = "condition",
    values = c(
      "no incentive" = "dotted",
      "cash incentive" = "dashed",
      "time incentive" = "solid"
    )
  ) +
  labs(
    x = "Block",
    y = "Pretrial pupil variability"
  ) +
  coord_cartesian(ylim = c(0.04, 0.09)) + theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.title = element_blank(),
        legend.position = "none")

e3_cv_p

# ANOVAs
e3_pretrial_subject_block = e3_pretrial_data %>%
  group_by(subject, condition, block) %>%
  summarise(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/mean(pretrial_pupil, na.rm = T))


pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_mean",
  data = e3_pretrial_subject_block,
  between = "condition",
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_cv",
  data = e3_pretrial_subject_block,
  between = "condition",
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

# RT Bins
e3_pretrial_subject_bin = e3_pretrial_data %>%
  group_by(subject) %>%
  mutate(z = scale(pretrial_pupil)[,1]) %>%
  group_by(subject, condition, rt_bin) %>%
  summarise(pretrial_pupil = mean(z, na.rm = T)) %>%
  filter(!is.na(rt_bin))

pretrial_anova_afex <- aov_ez(
  id = "subject",
  dv = "pretrial_pupil",
  data = e3_pretrial_subject_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
pretrial_anova_table <- as.data.frame(pretrial_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
pretrial_anova_table

e3_pretrial_rt_bin = e3_pretrial_subject_bin %>%
  group_by(rt_bin) %>%
  summarise(mean = mean(pretrial_pupil, na.rm = T),
            se = sd(pretrial_pupil, na.rm = T)/sqrt(n()),
            n = n())

e3_pretrial_bin_p = ggplot(e3_pretrial_rt_bin, aes(x = rt_bin, y = mean, ymin = mean - se, ymax = mean + se)) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = .2) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = 'RT Quintile', y = 'Pretrial Pupil (normalized)')

e3_pretrial_p = plot_grid(plot_grid(e3_average_arousal_p, e3_cv_p, ncol = 2, labels = 'AUTO'),
                          plot_grid(NULL, e3_pretrial_bin_p, NULL, ncol = 3, rel_widths = c(1, 2, 1), labels = c("", "C", "")),
                          ncol = 1)
e3_pretrial_p
ggsave(e3_pretrial_p, file = 'Figures/e3_pretrial.png', height = 10, width = 10, units = 'in', dpi = 600)

# Phasic ####
e3_phasic_files <- list.files(
  path = "Data/Experiment 3/phasic x trial",
  pattern = "^e3_phasic_trial_[0-9]+\\.tsv$",
  full.names = TRUE
)
e3_phasic_data <- read_tsv(e3_phasic_files)

e3_phasic_data <- e3_phasic_data %>%
  rename(trial = TrialId,
         subject = Subject) %>%
  mutate(block = ceiling(trial / 28)) %>%
  full_join(e3_behavior)

waveform_data <- e3_phasic_data %>%
  group_by(subject, block, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = TRUE)) %>%
  group_by(block, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  )

e3_TEPR_block <- ggplot(waveform_data,
                        aes(x = bin,
                            y = mean_response,
                            group = factor(block),
                            color = factor(block))) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "Block"
  ) + ylim(-0.02, 0.14) +
  theme_bw() +
  theme(axis.text  = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e3_TEPR_block

waveform_data_condition <- e3_phasic_data %>%
  group_by(subject, condition, bin) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = TRUE)) %>%
  group_by(condition, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()

e3_TEPR_condition <- ggplot(waveform_data_condition,
                            aes(x = bin,
                                y = mean_response,
                                group = factor(condition),
                                color = factor(condition))) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "Condition"
  ) + ylim(-0.02, 0.14) +
  theme_bw() +
  theme(axis.text  = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e3_TEPR_condition

e3_max_pupil <- e3_phasic_data %>%
  filter(bin >= 500, bin <= 800) %>%
  filter(!is.na(pupil_change)) %>%
  group_by(subject, trial, condition, block) %>%
  summarise(
    max_pupil = max(pupil_change),
    .groups = "drop"
  )

e3_max_pupil = e3_max_pupil %>%
  group_by(subject, condition, block) %>%
  summarise(max_pupil = mean(max_pupil, na.rm = T))

tepr_anova_afex <- aov_ez(
  id = "subject",
  dv = "max_pupil",
  data = e3_max_pupil,
  within = "block",
  between = "condition",
  type = 3,
  anova_table = list(es = "pes")
)

tepr_anova_table <- as.data.frame(tepr_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
tepr_anova_table

plot_max_e3 <- e3_max_pupil %>%
  group_by(condition, block) %>%
  summarise(
    mean_max = mean(max_pupil, na.rm = TRUE),
    se = sd(max_pupil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(condition %in% c("no incentive", "cash incentive", "time incentive"))

pd <- position_dodge(width = 0.25)

e3_max_mean_p <- ggplot(plot_max_e3,
                        aes(x = block,
                            y = mean_max,
                            group = condition,
                            linetype = condition,
                            color = condition,
                            shape = condition,
                            ymin = mean_max - se,
                            ymax = mean_max + se)) +
  geom_line(position = pd, linewidth = 0.5) +
  geom_point(position = pd, size = 1) +
  geom_errorbar(
    position = pd,
    width = 0.15,
    linetype = 1
  ) +
  scale_linetype_manual(
    name = "condition",
    values = c(
      "no incentive" = "dotted",
      "cash incentive" = "dashed",
      "time incentive" = "solid"
    )
  ) +
  labs(
    x = "Block",
    y = "Maximum Dilation"
  ) +
  ylim(0, 0.2) +
  scale_color_grey(name = "condition", 
                   start = 0.1, end = 0.8) +
  theme_bw() +
  labs(color = "condition", shape = "condition", linetype = "condition")+
  theme(legend.position = 'inside',
        legend.position.inside = c(.7, .3),
        axis.text = element_text(color = 'black'))

e3_max_mean_p

e3_tepr_plots = plot_grid(plot_grid(e3_TEPR_block, e3_TEPR_condition, labels = 'AUTO', ncol = 2), plot_grid(NULL, e3_max_mean_p, NULL, rel_widths = c(1, 2, 1), labels = c("", "C", ""), ncol = 3), ncol = 1)
e3_tepr_plots
ggsave(e3_tepr_plots, file = 'Figures/e3_tepr.png', height = 8, width = 8, units = 'in', dpi = 600)

# RT Quintile
e3_rts <- e3_phasic_data %>%
  dplyr::select(subject, condition, trial, rt, pupil_change, bin, rt_bin) %>%
  filter(!is.na(rt), rt >= 200, rt <= 3000)

e3_rt_bins <- e3_rts

rt_waveform <- e3_rt_bins %>%
  group_by(subject, rt_bin, bin) %>%
  summarize(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(rt_bin, bin) %>%
  summarise(
    mean_response = mean(pupil_change, na.rm = TRUE),
    .groups = "drop"
  )

e3_bin_p = ggplot(rt_waveform,
                  aes(x = bin,
                      y = mean_response,
                      color = factor(rt_bin),
                  )) +
  geom_line(linewidth = 1) +
  scale_color_grey(start = 0.1, end = 0.8) +
  labs(
    x = "Time (ms)",
    y = "Task-evoked response (mm)",
    color = "RT quintile",
  ) + 
  ylim(-0.02, 0.14) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))

e3_tepr_plots = plot_grid(e3_TEPR_block, e3_TEPR_condition, e3_max_mean_p, e3_bin_p, labels = 'AUTO', ncol = 2)
e3_tepr_plots
ggsave(e3_tepr_plots, file = 'Figures/e3_tepr.png', height = 8, width = 8, units = 'in', dpi = 600)


# TEPR ANOVA
e3_tepr_subject_rt_bin = e3_rt_bins %>%
  group_by(subject, condition, rt_bin) %>%
  filter(between(bin, 500, 800)) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T))

tepr_anova_afex <- aov_ez(
  id = "subject",
  dv = "pupil_change",
  data = e3_tepr_subject_rt_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
tepr_anova_afex <- as.data.frame(tepr_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
tepr_anova_afex

# Quiet Eye ####

# e3 qe analysis

e3_quiet_eye_files <- list.files(
  path = "Data/Experiment 3/quiet eye",
  pattern = "^e2_wait_eye_subject_trial_bin_[0-9]+\\.csv$",
  full.names =  TRUE
)

e3_quiet_eye_data <- read_csv(e3_quiet_eye_files) %>%
  rename(subject = Subject,
         trial = TrialId)

# excluding missing samples within trials
e3_quiet_eye_data = e3_quiet_eye_data %>%
  mutate(missing = ifelse(is.na(right_gaze_x), 1, 0)) %>%
  group_by(subject, trial) %>%
  mutate(missing = mean(missing, na.rm = T)) %>%
  filter(missing < .5) %>%
  group_by(subject) %>%
  mutate(n_trials = length(unique(trial))) %>%
  filter(n_trials > 70)

# calculating qe from x and y

e3_quiet_eye_sd <- e3_quiet_eye_data %>%
  group_by(trial, subject) %>%
  summarise(
    sd_gaze_x = sd(right_gaze_x, na.rm = TRUE), 
    sd_gaze_y = sd(right_gaze_y, na.rm = T),
    .groups = "drop"
  )

e3_sd_gaze <- e3_quiet_eye_sd %>%
  group_by(trial, subject) %>%
  summarise(
    sd_gaze = (sd_gaze_x + sd_gaze_y)/2,
    .groups = "drop"
  )

e3_qe_data <- e3_sd_gaze %>%
  group_by(trial, subject) %>%
  summarise(
    qe = log(1/sd_gaze),
    .groups ="drop"
  )

e3_qe_data <- e3_qe_data %>%
  mutate(block = ceiling(trial/28)) %>%
  full_join(e3_behavior)

e3_condition_data <- e3_summary %>%
  dplyr::select(subject, condition) %>%
  group_by(subject, condition) %>%
  summarise()

e3_average_qe_data <- e3_qe_data %>%
  group_by(subject, condition, trial, block) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(subject, condition, block) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(block, condition) %>%
  summarise(
    mean_qe = mean(qe, na.rm = T),
    se = sd(qe, na.rm = T) / sqrt(n()),
    .groups = "drop"
  ) %>%
  na.omit()

e3_subject_block_qe_data <- e3_qe_data %>%
  group_by(subject, condition, trial, block) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(subject, condition, block) %>%
  summarise(qe = mean(qe, na.rm = T))

# ANOVA
e3_qe_anova_afex <- aov_ez(
  id = "subject",
  dv = "qe",
  data = e3_subject_block_qe_data,
  between = "condition",
  within = "block",
  type = 3,
  anova_table = list(es = "pes")
)

e3_qe_anova_table <- as.data.frame(e3_qe_anova_afex$anova_table) %>% 
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~round(.x,3)))
e3_qe_anova_table

# e3 average qe by goal plot

pd <- position_dodge(width = 0.15)

e3_qe_block_p <- ggplot(e3_average_qe_data,
                        aes(x = block,
                            y = mean_qe,
                            group = condition,
                            color = condition,
                            linetype = condition, 
                            shape = condition)) +
  geom_line(position = pd, linewidth = 0.9) +
  geom_point(position = pd, size = 1.5) +
  geom_errorbar(
    aes(
      ymin = mean_qe - se,
      ymax = mean_qe + se
    ),
    width = 0.2,
    position = pd,
    linewidth = 1,
    linetype = 1
  ) +
  scale_linetype_manual(
    values = c(
      "no incentive" = "dotted",
      "cash incentive" = "dashed",
      "time incentive" = "solid"
    )
  ) +
  scale_color_manual(
    values = c(
      "no incentive" = "gray60",
      "cash incentive" = "gray35",
      "time incentive" = "black"
    )
  ) +
  labs(x = "Block", 
       y = "Gaze Stability (QE)", 
       linetype = "condition",
       color = "condition",
       shape = "condition") +
  ylim(3.3, 4.1) +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.2, .2))
e3_qe_block_p

# RT Bins
e3_qe_subject_bin = e3_qe_data %>%
  full_join(e3_condition_data) %>%
  group_by(subject, condition, rt_bin) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  filter(!is.na(rt_bin))

length(unique(e3_qe_subject_bin$subject))

e3_qe_bin = e3_qe_subject_bin %>%
  group_by(rt_bin) %>%
  summarise(mean = mean(qe, na.rm = T),
            se = sd(qe, na.rm = T)/sqrt(n())) %>%
  na.omit()

e3_qe_bin_p = ggplot(e3_qe_bin, aes(x = rt_bin, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(stat = 'identity', width = .5, color = 'black', fill = 'grey') +
  geom_line() + 
  geom_point() +
  geom_errorbar(width = .2) +
  theme_bw(base_size = 17) +
  ylim(3.3, 4.1) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = 'RT Quintile', y = 'Gaze Stability (QE)')

e3_qe_p = plot_grid(e3_qe_block_p, e3_qe_bin_p, ncol = 2, labels = 'AUTO')
e3_qe_p
ggsave(e3_qe_p, file = 'Figures/e3_qe.png', height = 6, width = 12, units = 'in', dpi = 600)

qe_anova_afex <- aov_ez(
  id = "subject",
  dv = "qe",
  data = e3_qe_subject_bin,
  within = "rt_bin",
  type = 3,
  anova_table = list(es = "pes")
)
qe_anova_table <- as.data.frame(qe_anova_afex$anova_table) %>%
  rownames_to_column(" ") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
qe_anova_table

# Probe responses ####

# Filter to include only trials with probes
e3_clean_proberesp <- e3_behavior %>% filter(proberesp != "")

# Label responses
e3_mw_data <- e3_clean_proberesp %>%
  mutate(
    thought_state = case_when(
      proberesp == 1 ~ "on-task",
      proberesp == 4 ~ "intentional\nMW",
      proberesp == 5 ~ "unintentional\nMW",
      proberesp == 6 ~ 'mind-blanking',
      T ~ NA_character_
    )
  ) 

# make responses a factor
e3_mw_data <- e3_mw_data %>%
  mutate(
    thought_state = factor(thought_state, 
                           levels = c("on-task",
                                      "intentional\nMW",
                                      "unintentional\nMW",
                                      "mind-blanking")
    )
  ) %>%
  select(subject, trial, thought_state, block, rt)

# e3 RT lme

e3_rt_trial <- e3_rts %>%
  group_by(subject, trial, condition) %>%
  summarise(
    avg_rt = mean(rt, na.rm = T),
    .groups = "drop"
  )

e3_mw_rt_lme <- full_join(e3_mw_data, e3_rt_trial, by = c("subject", "trial"))

e3_mw_rt_model <- lmer(avg_rt ~ thought_state + (thought_state|subject), data = e3_mw_rt_lme)
e3_mw_rt_model = lmer(avg_rt ~ thought_state + (1|subject), data = e3_mw_rt_lme)
summary(e3_mw_rt_model)

# Aggregate for plot
e3_mw_rt = e3_mw_data %>%
  group_by(subject, thought_state) %>%
  summarise(rt = mean(rt, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarize(mean = mean(rt, na.rm = T),
            se = sd(rt, na.rm = T)/sqrt(n())) %>%
  na.omit()

# Plot

e3_rt_mw_p = ggplot(e3_mw_rt, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Reaction Time (ms)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e3_rt_mw_p

e3_pretrial_clean <- e3_pretrial_data %>%
  group_by(subject) %>%
  mutate(z = scale(pretrial_pupil)[,1])

e3_mw_pretrial_data <- full_join(e3_mw_data, e3_pretrial_clean, by = c("subject", "trial")) 

# Linear mixed effects model
e3_mw_pretrial_model <- lmer(z ~ thought_state + (1|subject), data = e3_mw_pretrial_data)
summary(e3_mw_pretrial_model)

# Aggregate for plot
e3_mw_pretrial = e3_mw_pretrial_data %>%
  group_by(subject, thought_state) %>%
  summarize(z = mean(z, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarize(mean = mean(z, na.rm = T),
            se = sd(z, na.rm = T)/sqrt(n())) %>%
  na.omit()

e3_pretrial_mw_p = ggplot(e3_mw_pretrial, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se))+
  #geom_bar(width = .5, color = 'black', fill = 'grey', stat = 'identity') +
  geom_point() +
  geom_errorbar(position = position_dodge(), width = .2) +
  labs(x = 'Probe Response', y = 'Pretrial Pupil (normalized)') +
  theme_bw(base_size = 17) +
  theme(axis.text = element_text(color = 'black'))
e3_pretrial_mw_p

# e3 TEPR lme
e3_tepr_mw_data = full_join(e3_mw_data, e3_phasic_data) %>%
  filter(!is.na(thought_state))

table(e3_tepr_mw_data$thought_state)

e3_tepr_mw = e3_tepr_mw_data %>%
  group_by(subject, thought_state, bin) %>%
  mutate(z = scale(pupil_change)[1]) %>%
  filter(abs(z) < 3) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T)) %>%
  mutate(z = scale(pupil_change)[1]) %>%
  filter(abs(z) < 3) %>%
  group_by(thought_state, bin) %>%
  summarise(mean = mean(pupil_change, na.rm = T),
            se = sd(pupil_change, na.rm = T)/sqrt(n()))

# Plot
e3_tepr_mw_p = ggplot(e3_tepr_mw, aes(x = bin, y = mean, group = thought_state, color = thought_state, linetype = thought_state)) +
  geom_line(linewidth = 1) +
  labs(x = 'Time (ms)', y = 'Task-evoked Response (mm)', color = 'Probe Response', linetype = 'Probe Response') +
  theme_bw(base_size = 17) +
  scale_color_grey(start = 0.1, end = 0.8) +
  ylim(-.02, .14) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))
e3_tepr_mw_p

e3_tepr_mw_lme = e3_tepr_mw_data %>%
  filter(between(bin, 500, 800)) %>%
  group_by(subject, thought_state, trial) %>%
  summarise(pupil_change = mean(pupil_change, na.rm = T)) %>%
  group_by(subject, thought_state) %>%
  summarise(tepr = mean(pupil_change, na.rm = T))

e3_mw_tepr_model = lmer(tepr ~ thought_state + (1|subject), data = e3_tepr_mw_lme)
summary(e3_mw_tepr_model)

# e3 QE lme

e3_qe_clean <- e3_qe_data %>%
  group_by(subject, trial) %>%
  summarise(qe = mean(qe, na.rm = T))

e3_mw_qe_lme <- full_join(e3_mw_data, e3_qe_clean) %>%
  filter(!is.na(thought_state))

e3_mw_qe_model = lmer(qe ~ thought_state + (thought_state|subject), data = e3_mw_qe_lme)
summary(e3_mw_qe_model)

# Aggregate for plot
e3_qe_mw = e3_mw_qe_lme %>%
  group_by(subject, thought_state) %>%
  summarise(qe = mean(qe, na.rm = T)) %>%
  group_by(thought_state) %>%
  summarise(mean = mean(qe, na.rm = T),
            se = sd(qe, na.rm = T)/sqrt(n()))

e3_qe_mw_p = ggplot(e3_qe_mw, aes(x = thought_state, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_bar(stat = 'identity', width = .5, color = 'black', fill = 'grey') +
  geom_point() +
  geom_errorbar(width = .2, position = position_dodge(.2)) +
  labs(x = 'Probe Response', y = 'Gaze Stability (Quiet Eye)') +
  theme_bw(base_size = 17) +
  scale_color_grey(start = 0.1, end = 0.8) +
  theme(axis.text = element_text(color = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(.25, .75))
e3_qe_mw_p

e3_probe_plots = plot_grid(e3_rt_mw_p, e3_pretrial_mw_p, e3_tepr_mw_p, e3_qe_mw_p, ncol = 2, labels = 'AUTO')
e3_probe_plots
ggsave(e3_probe_plots, file = 'Figures/e3_probe.png', height = 12, width = 12, units = 'in', dpi = 600)

# Individual Differences ####
e3_behavior_subject = e3_behavior %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject) %>%
  summarize(rt_mean = mean(rt, na.rm = T))

e3_mot_alert_subject = e3_behavior %>%
  group_by(subject, block) %>%
  summarise(motivation = mean(motivation, na.rm = T),
            alertness = mean(alertness, na.rm = T)) %>%
  group_by(subject) %>%
  summarise(motivation = mean(motivation, na.rm = T),
            alertness = mean(alertness, na.rm = T))

e3_pretrial_subject = e3_pretrial_data %>%
  group_by(subject) %>%
  summarize(pretrial_mean = mean(pretrial_pupil, na.rm = T),
            pretrial_cv = sd(pretrial_pupil, na.rm = T)/pretrial_mean)

e3_tepr_subject = e3_max_pupil %>%
  group_by(subject) %>%
  summarize(tepr = mean(max_pupil, na.rm = T))

e3_qe_subject = e3_qe_data %>%
  group_by(subject) %>%
  summarize(qe = mean(qe, na.rm = T))

e3_subject = full_join(e3_behavior_subject, e3_pretrial_subject) %>%
  full_join(e3_tepr_subject) %>%
  full_join(e3_qe_subject) %>%
  full_join(e3_mot_alert_subject)

corr.test(select(e3_subject, rt_mean, motivation, alertness, pretrial_cv, tepr, qe))
describe(select(e3_subject, rt_mean, motivation, alertness, pretrial_cv, tepr, qe)) %>%
  select(mean, sd)


# Combine experiments
e1_subject$experiment = 'experiment 1'
e2_subject$experiment = 'experiment 2'
e3_subject$experiment = 'experiment 3'

all_subject = full_join(e1_subject, e2_subject) %>%
  full_join(e3_subject) %>%
  select(rt_mean, pretrial_cv, tepr, qe, motivation, alertness) %>%
  group_by() %>%
  mutate_if(is.numeric, scale)

summary(lm(rt_mean ~ pretrial_cv + tepr + qe, data = all_subject))

# Add motivation and alertness
summary(lm(rt_mean ~ pretrial_cv + tepr + qe + motivation + alertness, data = all_subject))

# Trial-level covariance ####

# Experiment 1
e1_trial_level = e1_behavior %>%
  full_join(e1_pretrial_data) %>%
  full_join(e1_max_pupil) %>%
  full_join(e1_qe)

e1_trial_level = e1_trial_level %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject) %>%
  mutate(rt_z = scale(rt)[,1],
         pretrial_z = scale(pretrial_pupil)[,1],
         tepr_z = scale(max_pupil)[,1],
         qe_z = scale(qe)[,1]) %>%
  filter(abs(rt_z) < 3,
         abs(pretrial_z) < 3,
         abs(tepr_z) < 3,
         abs(qe_z) < 3)

# Experiment 2

e2_trial_level = e2_behavior_data %>%
  full_join(e2_pretrial_data) %>%
  full_join(e2_maximum_pupil) %>%
  full_join(e2_qe_lme_data)

e2_trial_level = e2_trial_level %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject) %>%
  mutate(rt_z = scale(rt)[,1],
         pretrial_z = scale(pretrial_pupil)[,1],
         tepr_z = scale(pupil_change)[,1],
         qe_z = scale(qe)[,1]) %>%
  filter(abs(rt_z) < 3,
         abs(pretrial_z) < 3,
         abs(tepr_z) < 3,
         abs(qe_z) < 3) %>%
  group_by(subject, trial) %>%
  mutate(n = seq(1:n())) %>%
  filter(n == 1)

# Experiment 3
e3_trial_level = e3_behavior %>%
  full_join(e3_pretrial_data) %>%
  full_join(e3_max_pupil) %>%
  full_join(e3_qe_data)

e3_trial_level = e3_trial_level %>%
  filter(between(rt, 200, 3000)) %>%
  group_by(subject) %>%
  mutate(rt_z = scale(rt)[,1],
         pretrial_z = scale(pretrial_pupil)[,1],
         tepr_z = scale(max_pupil)[,1],
         qe_z = scale(qe)[,1]) %>%
  filter(abs(rt_z) < 3,
         abs(pretrial_z) < 3,
         abs(tepr_z) < 3,
         abs(qe_z) < 3) %>%
  group_by(subject, trial) %>%
  mutate(n = seq(1:n())) %>%
  filter(n == 1)

# Join
trial_level_data = full_join(e1_trial_level, e2_trial_level) %>%
  full_join(e3_trial_level)

length(unique(trial_level_data$subject))


# Trial-level covariance within subjects
within_corrs = trial_level_data %>%
  group_by(subject) %>%
  summarise(pretrial_tepr_cor = cor(pretrial_z, tepr_z, use = 'pairwise'),
            pretrial_qe_cor = cor(pretrial_z, qe_z, use = 'pairwise'),
            tepr_qe_cor = cor(tepr_z, qe_z, use = 'pairwise')) %>%
  mutate(pretrial_tepr_cor_z = fisherz(pretrial_tepr_cor),
         pretrial_qe_cor_z = fisherz(pretrial_qe_cor),
         tepr_qe_cor_z = fisherz(tepr_qe_cor))

t.test(within_corrs$pretrial_tepr_cor_z)
mean(within_corrs$pretrial_tepr_cor_z, na.rm = T)
sd(within_corrs$pretrial_tepr_cor_z, na.rm = T)

t.test(within_corrs$pretrial_qe_cor)
mean(within_corrs$pretrial_qe_cor, na.rm = T)
sd(within_corrs$pretrial_qe_cor, na.rm = T)

mean(within_corrs$tepr_qe_cor, na.rm = T)
sd(within_corrs$tepr_qe_cor, na.rm = T)
t.test(within_corrs$tepr_qe_cor)

# LME
model = lmer(rt_z ~ pretrial_z + tepr_z + qe_z + (pretrial_z + tepr_z + qe_z|subject), data = trial_level_data)
summary(model)

lme_coef = data.frame(coefficients(summary(model)))
write_csv(lme_coef, file = 'Data/lme_coefficients.csv')





