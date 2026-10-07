# =============================================================================
# PCA_Group_Analysis.R
# Group-level summaries and figures from per-player PCA exports in Data_PCA.
# Supports .csv and .xlsx files with Section = PC_Summary / PC_Summ / Loadings.
# Run: source("PCA_Group_Analysis.R"); run_pca_group_analysis()
#   or: Rscript PCA_Group_Analysis.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

# Guard against dplyr verbs being masked by other attached packages
# (e.g. MASS::select / stats::filter pulled in via fitdistrplus). These global
# bindings are found before the package search path by functions defined here.
select <- dplyr::select
filter <- dplyr::filter

# -----------------------------------------------------------------------------
# Paths (override by passing pca_dir to run_pca_group_analysis)
# -----------------------------------------------------------------------------
.default_pca_dir <- "E:/Work/R Projects/GPS/Data_PCA"

# -----------------------------------------------------------------------------
# Discover and read one player's PCA export (.csv or .xlsx)
# -----------------------------------------------------------------------------
list_pca_player_files <- function(pca_dir) {
  files <- list.files(
    pca_dir,
    pattern = "_pca\\.(csv|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  files[!grepl("^~\\$|desktop\\.ini$", basename(files), ignore.case = TRUE)]
}

player_id_from_path <- function(path) {
  nm <- tools::file_path_sans_ext(basename(path))
  sub("_pca$", "", nm, ignore.case = TRUE)
}

# Normalise Section labels from GPS.Rmd exports or alternate sheet layouts
normalise_pca_sections <- function(df) {
  if (!"Section" %in% names(df)) {
    stop("PCA file must contain a 'Section' column (PC_Summary / Loadings).")
  }
  df %>%
    mutate(
      Section = str_trim(as.character(Section)),
      Section = case_when(
        str_detect(Section, regex("^PC[_ ]?Summ", ignore_case = TRUE)) ~ "PC_Summary",
        str_detect(Section, regex("^Loading", ignore_case = TRUE)) ~ "Loadings",
        TRUE ~ Section
      )
    )
}

read_pca_export <- function(path) {
  ext <- tolower(tools::file_ext(path))
  raw <- switch(
    ext,
    csv = read_csv(path, show_col_types = FALSE, na = c("", "NA", "N/A", "na")),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Install readxl to read .xlsx PCA files: install.packages('readxl')")
      }
      readxl::read_excel(path) %>% as_tibble()
    },
    stop("Unsupported PCA file type: ", ext, " (", path, ")")
  )
  normalise_pca_sections(raw)
}

# Split one player's table into PC summary and loadings
parse_player_pca <- function(path) {
  player_name <- player_id_from_path(path)
  raw <- read_pca_export(path)

  pc_summary <- raw %>%
    filter(Section == "PC_Summary") %>%
    transmute(
      player_name = player_name,
      source_file = basename(path),
      PC = as.character(PC),
      variance_explained = as.numeric(variance_explained),
      eigenvalue = as.numeric(eigenvalue)
    )

  loadings <- raw %>%
    filter(Section == "Loadings") %>%
    transmute(
      player_name = player_name,
      source_file = basename(path),
      PC = as.character(PC),
      Variable = as.character(Variable),
      loading = as.numeric(loading)
    )

  if (nrow(pc_summary) == 0) {
    warning("No PC_Summary rows in: ", path)
  }
  if (nrow(loadings) == 0) {
    warning("No Loadings rows in: ", path)
  }

  list(pc_summary = pc_summary, loadings = loadings)
}

# -----------------------------------------------------------------------------
# Combine all players
# -----------------------------------------------------------------------------
read_all_player_pca <- function(pca_dir = .default_pca_dir) {
  files <- list_pca_player_files(pca_dir)
  if (length(files) == 0) {
    stop("No *_pca.csv or *_pca.xlsx files found in: ", pca_dir)
  }

  parsed <- lapply(files, parse_player_pca)
  pc_all <- bind_rows(lapply(parsed, `[[`, "pc_summary"))
  loadings_all <- bind_rows(lapply(parsed, `[[`, "loadings"))

  # Stable numeric player IDs for plotting (alphabetical by player_name)
  player_levels <- pc_all %>%
    distinct(player_name) %>%
    arrange(player_name) %>%
    mutate(player_id = row_number())

  pc_all <- pc_all %>% left_join(player_levels, by = "player_name")
  loadings_all <- loadings_all %>% left_join(player_levels, by = "player_name")

  list(
    files = files,
    n_players = nrow(player_levels),
    player_levels = player_levels,
    pc_summary = pc_all,
    loadings = loadings_all
  )
}

# -----------------------------------------------------------------------------
# Group-level PC summary (mean / SD / min / max per PC)
# -----------------------------------------------------------------------------
summarise_group_pc <- function(pc_all) {
  pc_all %>%
    group_by(PC) %>%
    summarise(
      n_players = n_distinct(player_name),
      mean_variance_explained = mean(variance_explained, na.rm = TRUE),
      sd_variance_explained = sd(variance_explained, na.rm = TRUE),
      min_variance_explained = min(variance_explained, na.rm = TRUE),
      max_variance_explained = max(variance_explained, na.rm = TRUE),
      mean_eigenvalue = mean(eigenvalue, na.rm = TRUE),
      sd_eigenvalue = sd(eigenvalue, na.rm = TRUE),
      min_eigenvalue = min(eigenvalue, na.rm = TRUE),
      max_eigenvalue = max(eigenvalue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(PC_num = as.integer(str_extract(PC, "\\d+"))) %>%
    arrange(PC_num) %>%
    select(-PC_num)
}

# Tidy display table: two rows per PC (Variance explained %, Eigenvalue),
# sharing Mean / SD / Min / Max columns. Variance shown as % to 1 dp;
# eigenvalues to 2 dp.
format_group_pc_table <- function(group_pc_summary) {
  variance_rows <- group_pc_summary %>%
    transmute(
      PC,
      Measure = "Variance explained (%)",
      Mean = sprintf("%.1f", mean_variance_explained * 100),
      SD = sprintf("%.1f", sd_variance_explained * 100),
      Min = sprintf("%.1f", min_variance_explained * 100),
      Max = sprintf("%.1f", max_variance_explained * 100)
    )

  eigen_rows <- group_pc_summary %>%
    transmute(
      PC,
      Measure = "Eigenvalue",
      Mean = sprintf("%.2f", mean_eigenvalue),
      SD = sprintf("%.2f", sd_eigenvalue),
      Min = sprintf("%.2f", min_eigenvalue),
      Max = sprintf("%.2f", max_eigenvalue)
    )

  # Group by measure so variance and eigenvalue form two clear sections
  bind_rows(variance_rows, eigen_rows) %>%
    mutate(
      Measure = factor(Measure, levels = c("Variance explained (%)", "Eigenvalue")),
      PC_num = as.integer(str_extract(PC, "\\d+"))
    ) %>%
    arrange(Measure, PC_num) %>%
    select(Measure, PC, Mean, SD, Min, Max)
}

# Long-format data with a section column used as gt row groups
build_group_pc_gt_data <- function(group_pc_table) {
  group_pc_table %>%
    mutate(
      Section = factor(
        as.character(Measure),
        levels = c("Variance explained (%)", "Eigenvalue")
      )
    ) %>%
    arrange(Section, as.integer(str_extract(PC, "\\d+"))) %>%
    select(Section, PC, Mean, SD, Min, Max)
}

render_group_pc_table_gt <- function(group_pc_table) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Install gt for PCA tables: install.packages('gt')")
  }

  tbl <- build_group_pc_gt_data(group_pc_table)
  row_var_end <- max(which(tbl$Section == "Variance explained (%)"))
  row_pc1 <- which(tbl$PC == "PC1")
  tbl_id <- "pca-group-summary"

  # Booktabs-style: horizontal section labels as row-group rows; inherit Rmd font
  gt::gt(tbl, groupname_col = "Section", row_group_as_column = FALSE, id = tbl_id) %>%
    gt::tab_options(
      table.border.top.style = "solid",
      table.border.top.width = gt::px(1),
      table.border.top.color = "#333333",
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(1),
      table.border.bottom.color = "#333333",
      table.border.left.style = "none",
      table.border.right.style = "none",
      table_body.hlines.style = "none",
      table_body.vlines.style = "none",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#333333",
      column_labels.vlines.style = "none",
      row_group.background.color = "#ffffff",
      row_group.font.weight = "bold",
      row_group.border.top.style = "none",
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(2),
      data_row.padding = gt::px(2),
      table.align = "center"
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold", align = "center"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold", align = "left"),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#e8f4fc"),
      locations = gt::cells_body(rows = row_pc1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_body(columns = PC)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(align = "right"),
      locations = gt::cells_body(columns = c(Mean, SD, Min, Max))
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "bottom", color = "#cccccc", weight = gt::px(1)),
      locations = gt::cells_body(rows = row_var_end)
    ) %>%
    gt::opt_css(
      css = paste0(
        "#", tbl_id, " { font-family: inherit; font-size: inherit; }",
        "#", tbl_id, " .gt_table { font-family: inherit; }",
        "#", tbl_id, " .gt_row_group { font-family: inherit; }"
      )
    )
}

# Keep y = 0 as the x-axis baseline (no padding below zero)
theme_axis_at_zero <- function() {
  theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.line.x = element_line(colour = "black", linewidth = 0.5),
      axis.line.y = element_line(colour = "black", linewidth = 0.5)
    )
}

scale_y_from_zero <- function(y_max = NULL) {
  if (is.null(y_max)) {
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  } else {
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.05)))
  }
}

# -----------------------------------------------------------------------------
# PC1 loadings (long table: one row per player × variable)
# -----------------------------------------------------------------------------
extract_pc1_loadings <- function(loadings_all) {
  loadings_all %>%
    filter(PC == "PC1", !is.na(Variable), Variable != "", Variable != "NA") %>%
    mutate(
      Variable = as.character(Variable),
      # Sign of a PCA loading is arbitrary; report absolute magnitude
      loading = abs(as.numeric(loading))
    ) %>%
    filter(!is.na(loading))
}

# Pretty variable labels and distinct shapes (publication-style legend)
variable_label_map <- c(
  TD_m = "Total distance",
  HSR_m = "HSR distance",
  AccelDist_m = "Accel distance",
  DecelDist_m = "Decel distance"
)

variable_shape_map <- c(
  AccelDist_m = 4,   # x
  DecelDist_m = 0,   # square
  TD_m = 19,         # filled circle
  HSR_m = 17         # filled triangle
)

prepare_pc1_plot_data <- function(pc1_loadings) {
  vars_present <- unique(pc1_loadings$Variable)
  shape_map <- variable_shape_map[names(variable_shape_map) %in% vars_present]
  if (length(shape_map) < length(vars_present)) {
    extra <- setdiff(vars_present, names(shape_map))
    shape_map <- c(shape_map, setNames(seq(0, 5, length.out = length(extra))[seq_along(extra)], extra))
  }

  pc1_loadings %>%
    mutate(
      variable_label = coalesce(unname(variable_label_map[Variable]), Variable),
      variable_label = factor(
        variable_label,
        levels = coalesce(unname(variable_label_map[vars_present]), vars_present)
      ),
      point_shape = shape_map[Variable]
    )
}

# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------
plot_pc1_loadings_by_player <- function(pc1_plot_df, title = "PC1 Loadings by Player") {
  y_vals <- pc1_plot_df$loading
  y_max <- max(1.0, ceiling(max(y_vals, na.rm = TRUE) * 10) / 10)

  shape_vals <- pc1_plot_df %>%
    distinct(variable_label, point_shape) %>%
    arrange(variable_label)

  ggplot(pc1_plot_df, aes(x = player_id, y = loading)) +
    geom_point(aes(shape = variable_label), size = 2.8, stroke = 0.9, colour = "black") +
    scale_shape_manual(values = setNames(shape_vals$point_shape, shape_vals$variable_label)) +
    scale_x_continuous(breaks = sort(unique(pc1_plot_df$player_id))) +
    scale_y_from_zero(y_max) +
    labs(
      title = title,
      x = "Player",
      y = "PC1 Loading",
      shape = NULL
    ) +
    theme_axis_at_zero() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}

plot_group_scree <- function(group_pc_summary) {
  scree_df <- group_pc_summary %>%
    mutate(PC_num = as.integer(str_extract(as.character(PC), "\\d+")))

  ggplot(scree_df, aes(x = PC, y = mean_eigenvalue)) +
    geom_col(fill = "grey70", colour = "black", linewidth = 0.4, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, mean_eigenvalue - sd_eigenvalue), ymax = mean_eigenvalue + sd_eigenvalue),
      width = 0.2,
      linewidth = 0.6,
      colour = "black"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey30", linewidth = 0.6) +
    scale_y_from_zero() +
    labs(
      title = "Group scree plot (mean eigenvalue ± SD)",
      x = "PC",
      y = "Mean eigenvalue"
    ) +
    theme_axis_at_zero()
}

# -----------------------------------------------------------------------------
# Main pipeline
# -----------------------------------------------------------------------------
run_pca_group_analysis <- function(
    pca_dir = .default_pca_dir,
    save_outputs = TRUE
) {
  if (!dir.exists(pca_dir)) {
    stop("PCA directory not found: ", pca_dir)
  }

  message("Reading PCA files from: ", pca_dir)
  combined <- read_all_player_pca(pca_dir)
  message("Players found: ", combined$n_players)

  group_pc_summary <- summarise_group_pc(combined$pc_summary)
  group_pc_table <- format_group_pc_table(group_pc_summary)
  group_pc_table_gt <- render_group_pc_table_gt(group_pc_table)
  pc1_loadings <- extract_pc1_loadings(combined$loadings)
  pc1_plot_df <- prepare_pc1_plot_data(pc1_loadings)

  p_loadings <- plot_pc1_loadings_by_player(pc1_plot_df)
  p_scree <- plot_group_scree(group_pc_summary)

  if (isTRUE(save_outputs)) {
    write_csv(group_pc_summary, file.path(pca_dir, "group_pc_summary.csv"))
    write_csv(group_pc_table, file.path(pca_dir, "group_pc_summary_table.csv"))
    write_csv(pc1_loadings, file.path(pca_dir, "pc1_loadings_by_player.csv"))
    write_csv(combined$player_levels, file.path(pca_dir, "player_id_lookup.csv"))

    ggsave(
      file.path(pca_dir, "pc1_loadings_by_player.png"),
      p_loadings,
      width = 10,
      height = 6,
      dpi = 300,
      bg = "white"
    )
    ggsave(
      file.path(pca_dir, "group_scree_plot.png"),
      p_scree,
      width = 7,
      height = 5,
      dpi = 300,
      bg = "white"
    )

    message("Saved group_pc_summary.csv, pc1_loadings_by_player.csv, player_id_lookup.csv")
    message("Saved pc1_loadings_by_player.png, group_scree_plot.png")
  }

  invisible(list(
    n_players = combined$n_players,
    player_levels = combined$player_levels,
    pc_summary = combined$pc_summary,
    loadings = combined$loadings,
    group_pc_summary = group_pc_summary,
    group_pc_table = group_pc_table,
    group_pc_table_gt = group_pc_table_gt,
    pc1_loadings = pc1_loadings,
    plots = list(pc1_loadings = p_loadings, scree = p_scree)
  ))
}

# Run when executed via Rscript (sys.nframe() == 0); skip when sourced from R Markdown
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  pca_dir <- if (length(args) >= 1) args[1] else .default_pca_dir
  run_pca_group_analysis(pca_dir = pca_dir)
}
