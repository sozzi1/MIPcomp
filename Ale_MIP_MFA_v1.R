library(tidyverse)
library(janitor)
library(FactoMineR)
library(factoextra)

# Pretty labels for plot text: "_" -> space, Title Case each word, but force
# known acronyms (phase-of-play codes, physical metrics) to all upper case.
acronym_tokens <- c(
  "ip1", "ip2", "ip3",
  "oop1", "oop2", "oop3",
  "mip", "td", "hsr", "pc1"
)

pretty_label <- function(x) {
  vapply(
    x,
    function(label) {
      words <- str_split(str_replace_all(label, "_", " "), "\\s+")[[1]]
      words <- vapply(
        words,
        function(w) {
          if (str_to_lower(w) %in% acronym_tokens) {
            str_to_upper(w)
          } else if (str_to_lower(w) == "m") {
            "(m)"
          } else {
            str_to_title(w)
          }
        },
        character(1)
      )
      str_c(words, collapse = " ")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Colour scheme used across all plots/maps.
# Cluster colours (named by cluster so mapping is order-independent).
cluster_palette <- c(
  "1" = "#005EB8",  # blue
  "2" = "#F39200",  # orange
  "3" = "#E30613",  # red
  "4" = "#009640"   # green
)
cluster_dark <- "#101820"  # spare 5th colour from the scheme

# Ordered palette for maps that need more groups than there are clusters
# (e.g. the position map has 7 levels). First 5 are the scheme; last 2 extend it.
brand_palette <- c(
  "#005EB8",  # blue
  "#F39200",  # orange
  "#E30613",  # red
  "#009640",  # green
  "#101820",  # dark navy
  "#7FB2E3",  # light blue
  "#9B59B6"   # purple
)

mfa_output_dir <- "C:/Users/Alessandro/Edge Hill University/Daniel Weaving - Ale Sozzi MRes Folder/MFA Output"
dir.create(mfa_output_dir, recursive = TRUE, showWarnings = FALSE)

mfa_out <- function(filename) {
  file.path(mfa_output_dir, filename)
}

save_mfa_plot <- function(filename, plot, width, height, dpi = 300, ...) {
  ggsave(
    mfa_out(filename),
    plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    ...
  )
  print(plot)
}

MIP_data <- read_csv("ALL_mip_tactical.csv", show_col_types = FALSE) %>%
  clean_names()

# Cohort: starters (sub = 0) or substitutes who came on strictly before 70:00
# (mirrors GPS_Stats.Rmd). Late substitutes are excluded.
starter_early_sub_cutoff_min <- 70

parse_mmss_to_min <- function(x) {
  x <- str_remove(trimws(as.character(x)), "^'")
  parts <- str_split(x, ":", simplify = TRUE)
  if (ncol(parts) < 2) return(suppressWarnings(as.numeric(x)))
  mm <- suppressWarnings(as.numeric(parts[, 1]))
  ss <- suppressWarnings(as.numeric(parts[, 2]))
  mm + ss / 60
}

filter_starter_early_sub <- function(df, cutoff_min = starter_early_sub_cutoff_min) {
  df %>%
    mutate(
      .sub = as.integer(sub),
      .sub_time_min = parse_mmss_to_min(sub_time)
    ) %>%
    filter(
      .sub == 0L |
        (!is.na(.sub_time_min) & .sub_time_min > 0 & .sub_time_min < cutoff_min)
    ) %>%
    select(-.sub, -.sub_time_min)
}

mip_interp_excl_path <- "E:/Work/R Projects/GPS/Exclusion_Data/ALL_mip_exclusion.csv"
mip_interp_pct_max <- 15
mip_interp_pct_run <- 10
mip_interp_max_run <- 5

filter_mip_interp_quality <- function(df, excl_path = mip_interp_excl_path) {
  if (!file.exists(excl_path)) {
    warning("MIP interpolation exclusion file not found: ", excl_path)
    return(df)
  }
  excl <- read_csv(excl_path, show_col_types = FALSE) %>%
    transmute(
      match_id,
      exclude_interp = pct_interpolated > mip_interp_pct_max |
        (pct_interpolated > mip_interp_pct_run & max_consecutive_interpolated > mip_interp_max_run)
    )
  n_before <- nrow(df)
  out <- df %>%
    left_join(excl, by = "match_id") %>%
    filter(is.na(exclude_interp) | !exclude_interp) %>%
    select(-exclude_interp)
  n_drop <- n_before - nrow(out)
  if (n_drop > 0) {
    message(
      "MIP GPS quality filter: dropped ", n_drop, " / ", n_before,
      " rows (pct_interp > ", mip_interp_pct_max,
      ", or pct_interp > ", mip_interp_pct_run,
      " with max consecutive interp > ", mip_interp_max_run, ")"
    )
  }
  out
}

# Aggregate raw playing positions into groups (mirrors GPS_Stats.Rmd)
position_group_map <- c(
  RM = "W", LM = "W",
  RW = "W", LW = "W",
  RF = "F", LF = "F",
  RCB = "CB", LCB = "CB",
  RWB = "WB", LWB = "WB",
  LCM = "CM", RCM = "CM",
  WM = "W"
)

position_order <- c("CB", "WB", "CM", "CAM", "F", "W", "ST")

MIP_data <- MIP_data %>%
  filter_starter_early_sub() %>%
  filter_mip_interp_quality() %>%
  mutate(
    position = str_trim(str_to_upper(as.character(position))),
    position = recode(position, !!!position_group_map, .default = position),
    position = factor(
      position,
      levels = c(
        intersect(position_order, position),
        setdiff(unique(position), position_order)
      )
    )
  )

# Convert phase-of-play durations to proportions (overwrites ip*/oop* in place).
# Transitions, long ball and counter press are kept as raw values.
MIP_data <- MIP_data %>%
  mutate(
    context_total =
      ip1 + ip2 + ip3 +
      oop1 + oop2 + oop3
  ) %>%
  mutate(
    across(
      c(
        ip1, ip2, ip3,
        oop1, oop2, oop3
      ),
      ~ if_else(context_total > 0, .x / context_total, NA_real_)
    )
  )

# Define MFA variable groups
action_vars <- c(
  "pressing",
  "interception",
  "covering",
  "recovery_run",
  "penetrate_box",
  "attack_space_behind",
  "over_underlap",
  "carry_ball",
  "push_up_pitch",
  "move_to_receive"
)

context_vars <- c(
  "ip1",
  "ip2",
  "ip3",
  "oop1",
  "oop2",
  "oop3",
  "long_ball",
  "attacking_transition",
  "counter_press",
  "defensive_transition"
)

# Active MFA dataset
mfa_active <- MIP_data %>%
  select(
    all_of(action_vars),
    all_of(context_vars)
  )

# Remove zero-variance variables
zero_var <- names(mfa_active)[
  map_lgl(mfa_active, ~ sd(.x, na.rm = TRUE) == 0)
]

mfa_active <- mfa_active %>%
  select(-any_of(zero_var))

action_vars <- intersect(action_vars, names(mfa_active))
context_vars <- intersect(context_vars, names(mfa_active))

# Order MFA data by groups (pretty column names so plots/outputs read cleanly)
mfa_ordered <- mfa_active %>%
  select(
    all_of(action_vars),
    all_of(context_vars)
  ) %>%
  rename_with(pretty_label)

# MFA group structure
group_sizes <- c(
  length(action_vars),
  length(context_vars)
)

group_names <- c(
  "Actions",
  "Context"
)

# Variable factor map with labels offset beyond arrow tips (avoids text on arrows).
plot_mfa_quanti_var_map <- function(
    res,
    vars_keep,
    label_scale = 1.24,
    action_labels = pretty_label(action_vars)
) {
  coord <- res$quanti.var$coord[vars_keep, c("Dim.1", "Dim.2"), drop = FALSE]
  df <- tibble(
    variable = rownames(coord),
    x = coord[, "Dim.1"],
    y = coord[, "Dim.2"]
  )

  # Manual label offsets for variables that crowd arrow tips or axes
  label_nudges <- tibble::tribble(
    ~variable,              ~nudge_x, ~nudge_y,
    "Covering",             -0.06,     0.16,
    "IP2",                  -0.04,    -0.00,
    "IP3",                  -0.04,     0.02,
    "OOP2",                 -0.14,    -0.10,
    "Penetrate Box",         0.12,    -0.12,
    "Defensive Transition",  0.16,     0.02,
    "Recovery Run",         -0.16,     0.08
  )

  df <- df %>%
    left_join(label_nudges, by = "variable") %>%
    mutate(
      nudge_x = replace_na(nudge_x, 0),
      nudge_y = replace_na(nudge_y, 0),
      lx = x * label_scale + nudge_x,
      ly = y * label_scale + nudge_y,
      group = if_else(variable %in% action_labels, "Actions", "Context")
    )

  grid_breaks <- seq(-1, 1, by = 0.25)
  arrow_colours <- c(
    Actions = brand_palette[[1]],  # blue
    Context = brand_palette[[3]]   # red
  )

  ggplot(df, aes(x = x, y = y, colour = group)) +
    geom_hline(yintercept = grid_breaks, colour = "grey90", linewidth = 0.25) +
    geom_vline(xintercept = grid_breaks, colour = "grey90", linewidth = 0.25) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.45) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.45) +
    geom_segment(
      aes(x = 0, y = 0, xend = x, yend = y),
      linewidth = 0.55,
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    geom_text(
      aes(x = lx, y = ly, label = variable),
      inherit.aes = FALSE,
      data = df,
      colour = "grey15",
      size = 3.2
    ) +
    scale_colour_manual(values = arrow_colours) +
    scale_x_continuous(breaks = grid_breaks, limits = c(-1, 1)) +
    scale_y_continuous(breaks = grid_breaks, limits = c(-1, 1)) +
    coord_equal(xlim = c(-1, 1), ylim = c(-1, 1), clip = "off", expand = FALSE) +
    labs(
      title = "Variable factor map",
      subtitle = "Variables with |coord| > 0.35 on Dim.1 or Dim.2",
      x = "Dim.1",
      y = "Dim.2",
      colour = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
}

# Run MFA
res_mfa <- MFA(
  mfa_ordered,
  group = group_sizes,
  type = rep("s", length(group_sizes)),
  name.group = group_names,
  graph = FALSE
)

# Eigenvalues
eig <- get_eigenvalue(res_mfa)
print(eig)

save_mfa_plot(
  "mfa_screeplot.png",
  fviz_screeplot(
    res_mfa,
    addlabels = TRUE,
    barfill = cluster_palette[["1"]],
    barcolor = cluster_palette[["1"]]
  ),
  width = 7,
  height = 5
)

# Group contribution plot
save_mfa_plot(
  "mfa_group_contribution_map.png",
  fviz_mfa_var(
    res_mfa,
    choice = "group",
    palette = brand_palette
  ),
  width = 7,
  height = 6
)

# Variable factor map: keep only variables with |coord| > 0.4 on Dim.1 or Dim.2
quanti_coord <- res_mfa$quanti.var$coord
vars_keep_map <- rownames(quanti_coord)[
  abs(quanti_coord[, "Dim.1"]) > 0.35 | abs(quanti_coord[, "Dim.2"]) > 0.35
]

save_mfa_plot(
  "mfa_variable_factor_map.png",
  plot_mfa_quanti_var_map(
    res_mfa,
    vars_keep = vars_keep_map
  ),
  width = 8,
  height = 8
)

# Individual map coloured by position
save_mfa_plot(
  "mfa_individual_map_position.png",
  fviz_mfa_ind(
    res_mfa,
    habillage = as.factor(MIP_data$position),
    addEllipses = TRUE,
    palette = brand_palette,
    repel = TRUE
  ),
  width = 9,
  height = 7
)

# Individual map coloured by starter/substitute
save_mfa_plot(
  "mfa_individual_map_sub.png",
  fviz_mfa_ind(
    res_mfa,
    habillage = as.factor(MIP_data$sub),
    addEllipses = TRUE,
    palette = brand_palette,
    repel = TRUE
  ),
  width = 9,
  height = 7
)

# Extract coordinates and contributions
ind_coords <- as_tibble(res_mfa$ind$coord) %>%
  bind_cols(
    MIP_data %>%
      select(
        player_name,
        player_id,
        match_id,
        position,
        mip_start_in_match_mmss,
        mip_end_in_match_mmss,
        mip_pc1_score,
        mip_td_m,
        mip_hsr_m,
        mip_accel_dist_m,
        mip_decel_dist_m,
        sub
      )
  )

var_coords <- as_tibble(
  res_mfa$quanti.var$coord,
  rownames = "variable"
)

var_contrib <- as_tibble(
  res_mfa$quanti.var$contrib,
  rownames = "variable"
)

group_contrib <- as_tibble(
  res_mfa$group$contrib,
  rownames = "group"
)

print(var_coords)
print(var_contrib)
print(group_contrib)

# Top contributing variables by dimension
var_contrib_long <- var_contrib %>%
  pivot_longer(
    cols = starts_with("Dim"),
    names_to = "dimension",
    values_to = "contribution"
  ) %>%
  arrange(
    dimension,
    desc(contribution)
  )

top_var_contrib <- var_contrib_long %>%
  group_by(dimension) %>%
  slice_max(contribution, n = 10) %>%
  ungroup()

print(top_var_contrib)

# HCPC clustering
set.seed(123)

hcpc <- HCPC(
  res_mfa,
  nb.clust = -1,
  max = 4,
  graph = FALSE
)

# Per-cluster defining variables (v.test ranked, significant vs overall mean).
# catdes() make.names()-ifies labels (spaces -> dots); restore spaces for display.
cluster_var_description <- imap_dfr(
  hcpc$desc.var$quanti,
  ~ as_tibble(.x, rownames = "variable") %>%
    mutate(cluster = .y, .before = 1)
) %>%
  mutate(variable = str_replace_all(variable, "\\.", " "))

print(cluster_var_description)

write_csv(
  cluster_var_description,
  mfa_out("mfa_cluster_descriptive_variables.csv")
)

clustered_windows <- ind_coords %>%
  mutate(
    cluster = factor(hcpc$data.clust$clust)
  )

# Cluster counts by position
cluster_position_table <- clustered_windows %>%
  count(cluster, position) %>%
  group_by(cluster) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup()

print(cluster_position_table)

# Cluster profiles using original variables
cluster_profiles <- MIP_data %>%
  mutate(
    cluster = factor(hcpc$data.clust$clust)
  ) %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    
    across(
      c(
        pressing,
        interception,
        covering,
        recovery_run,
        penetrate_box,
        attack_space_behind,
        over_underlap,
        carry_ball,
        push_up_pitch,
        move_to_receive,
        
        ip1,
        ip2,
        ip3,
        oop1,
        oop2,
        oop3,
        long_ball,
        attacking_transition,
        counter_press,
        defensive_transition,
        
        mip_pc1_score,
        mip_td_m,
        mip_hsr_m,
        mip_accel_dist_m,
        mip_decel_dist_m
      ),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    
    .groups = "drop"
  )

print(cluster_profiles)

# Cluster sizes (n) for labelling legends and headers
cluster_n <- setNames(cluster_profiles$n, as.character(cluster_profiles$cluster))
cluster_fill_label <- function(cl) {
  paste0(cl, " (n = ", cluster_n[as.character(cl)], ")")
}

# Cluster map
save_mfa_plot(
  "mfa_cluster_map.png",
  fviz_cluster(
    list(
      data = ind_coords %>%
        select(Dim.1, Dim.2),
      cluster = hcpc$data.clust$clust
    ),
    geom = "point",
    ellipse.type = "convex",
    palette = unname(cluster_palette),
    repel = TRUE
  ),
  width = 9,
  height = 7
)

# Convert cluster profiles to long format
cluster_profiles_long <- cluster_profiles %>%
  pivot_longer(
    cols = -c(cluster, n),
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  mutate(
    variable = str_remove(variable, "^mean_"),
    # coord_flip(): reverse cluster order so Cluster 1 is on top, 4 on bottom
    cluster = factor(cluster, levels = rev(levels(cluster)))
  )

# Tactical action profiles
p_action_profiles <- cluster_profiles_long %>%
  filter(variable %in% action_vars) %>%
  ggplot(
    aes(
      x = variable,
      y = mean_value,
      fill = cluster
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels = pretty_label) +
  scale_fill_manual(values = cluster_palette, labels = cluster_fill_label) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = NULL,
    y = "Mean frequency per 3-min MIP",
    fill = "Cluster"
  ) +
  theme_minimal()
save_mfa_plot("mfa_cluster_action_profiles.png", p_action_profiles, width = 10, height = 6)

# Context profiles
raw_context_vars <- c(
  "ip1",
  "ip2",
  "ip3",
  "oop1",
  "oop2",
  "oop3",
  "long_ball",
  "attacking_transition",
  "counter_press",
  "defensive_transition"
)

p_context_profiles <- cluster_profiles_long %>%
  filter(variable %in% raw_context_vars) %>%
  ggplot(
    aes(
      x = variable,
      y = mean_value,
      fill = cluster
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels = pretty_label) +
  scale_fill_manual(values = cluster_palette, labels = cluster_fill_label) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = NULL,
    y = "Mean duration/frequency per 3-min MIP",
    fill = "Cluster"
  ) +
  theme_minimal()
save_mfa_plot("mfa_cluster_context_profiles.png", p_context_profiles, width = 10, height = 6)

# Physical profiles (descriptive only)
physical_vars <- c(
  "mip_td_m",
  "mip_hsr_m",
  "mip_accel_dist_m",
  "mip_decel_dist_m"
)

p_physical_profiles <- cluster_profiles_long %>%
  filter(variable %in% physical_vars) %>%
  ggplot(
    aes(
      x = variable,
      y = mean_value,
      fill = cluster
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels = pretty_label) +
  scale_fill_manual(values = cluster_palette, labels = cluster_fill_label) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = NULL,
    y = "Mean physical value",
    fill = "Cluster"
  ) +
  theme_minimal()
save_mfa_plot("mfa_cluster_physical_profiles.png", p_physical_profiles, width = 9, height = 5)

# =============================================================================
# Publication-style heatmap of tactical MIP cluster profiles (within-variable z)
# =============================================================================

# One row per MIP, with its cluster assignment
df <- MIP_data %>%
  mutate(cluster = factor(hcpc$data.clust$clust))

# Variables for the heatmap (fixed display order)
heatmap_action_vars <- c(
  "recovery_run", "push_up_pitch", "pressing", "penetrate_box",
  "over_underlap", "move_to_receive", "interception",
  "covering", "carry_ball", "attack_space_behind"
)

heatmap_context_vars <- c(
  "attacking_transition", "counter_press", "defensive_transition",
  "ip1", "ip2", "ip3", "long_ball", "oop1", "oop2", "oop3"
)

heatmap_vars <- c(heatmap_action_vars, heatmap_context_vars)

# "Who tends to be in it" label: positions (desc. share) until cumulative >= 75%.
# Each position on its own line so it stays tied to its cluster column.
cluster_position_label <- cluster_position_table %>%
  arrange(cluster, desc(prop)) %>%
  group_by(cluster) %>%
  filter(cumsum(prop) - prop < 0.75) %>%
  summarise(
    pos_label = paste(
      sprintf("%s %.0f%%", position, 100 * prop),
      collapse = "\n"
    ),
    .groups = "drop"
  )

# 1-3) Cluster means -> long format -> per-variable z-score across cluster means
cluster_heatmap_data <- df %>%
  group_by(cluster) %>%
  summarise(
    across(all_of(heatmap_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = all_of(heatmap_vars),
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  group_by(variable) %>%
  mutate(
    z_score = {
      s <- sd(mean_value)
      if (is.na(s) || s == 0) rep(0, n()) else (mean_value - mean(mean_value)) / s
    }
  ) %>%
  ungroup() %>%
  mutate(
    group = if_else(variable %in% heatmap_action_vars, "Actions", "Context"),
    # 6) clean labels; keep requested order (first listed sits at the top)
    variable_label = factor(
      pretty_label(variable),
      levels = rev(pretty_label(heatmap_vars))
    )
  ) %>%
  # attach the dominant-position label to each cluster column
  left_join(cluster_position_label, by = "cluster") %>%
  mutate(
    cluster_label = paste0(
      "Cluster ", cluster, " (n = ", cluster_n[as.character(cluster)], ")",
      "\n", pos_label
    )
  )

# 8) Optional cell labels with rounded z-scores
show_cell_labels <- TRUE

cell_label_layers <- if (show_cell_labels) {
  list(
    geom_text(
      aes(label = sprintf("%.1f", z_score), colour = abs(z_score) > 1),
      size = 3,
      show.legend = FALSE
    ),
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"))
  )
} else {
  NULL
}

# 5) Symmetric diverging limits centred at 0
z_abs_max <- max(abs(cluster_heatmap_data$z_score), na.rm = TRUE)
if (!is.finite(z_abs_max) || z_abs_max == 0) z_abs_max <- 1

# 4 & 7) Heatmap, faceted to separate Actions vs Context
p_cluster_heatmap <- ggplot(
  cluster_heatmap_data,
  aes(x = cluster_label, y = variable_label, fill = z_score)
) +
  geom_tile(colour = "white", linewidth = 0.4) +
  cell_label_layers +
  facet_grid(
    group ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_fill_gradient2(
    low = cluster_palette[["1"]],
    mid = "white",
    high = cluster_palette[["3"]],
    midpoint = 0,
    limits = c(-z_abs_max, z_abs_max),
    name = "Z-score"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    x = NULL,
    y = NULL,
    title = "Tactical MIP cluster profiles",
    subtitle = "Within-variable z-scores of cluster means (3-min MIP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x.top = element_text(face = "bold", size = 8, lineheight = 0.9),
    strip.text.y.left = element_text(face = "bold", angle = 0),
    strip.placement = "outside",
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

save_mfa_plot(
  "mfa_cluster_heatmap.png",
  p_cluster_heatmap,
  width = 8,
  height = 9,
  dpi = 600
)

ggsave(
  mfa_out("mfa_cluster_heatmap.tiff"),
  p_cluster_heatmap,
  width = 8,
  height = 9,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# Heatmap source table: raw cluster means alongside within-variable z-scores
cluster_heatmap_table <- cluster_heatmap_data %>%
  transmute(
    cluster,
    group,
    variable = pretty_label(variable),
    raw_mean = mean_value,
    z_score
  ) %>%
  arrange(cluster, group, variable)

print(cluster_heatmap_table)

write_csv(
  cluster_heatmap_table,
  "mfa_cluster_heatmap_zscores.csv"
)

write_csv(
  cluster_position_label,
  "mfa_cluster_position_label.csv"
)

# Export outputs
write_csv(
  ind_coords,
  "mfa_individual_coordinates.csv"
)

write_csv(
  var_coords,
  "mfa_variable_coordinates.csv"
)

write_csv(
  var_contrib,
  "mfa_variable_contributions.csv"
)

write_csv(
  group_contrib,
  "mfa_group_contributions.csv"
)

write_csv(
  clustered_windows,
  "mfa_clustered_windows.csv"
)

write_csv(
  cluster_profiles,
  "mfa_cluster_profiles.csv"
)

write_csv(
  cluster_position_table,
  "mfa_cluster_position_table.csv"
)