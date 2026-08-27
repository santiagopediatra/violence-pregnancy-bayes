
## -----------------------------------------------------------------------------
## Conceptual DAG: registry-recorded gestational violence, neonatal outcomes
## Geometric routing fix (Zero collision guarantee)
## -----------------------------------------------------------------------------

library(tidyverse)

## --- Color palette ------------------------------------------------------------
pal <- list(
  amber = list(fill = "#FAEEDA", stroke = "#BA7517", text = "#633806"),
  blue  = list(fill = "#E6F1FB", stroke = "#185FA5", text = "#0C447C"),
  green = list(fill = "#EAF3DE", stroke = "#3B6D11", text = "#27500A"),
  gray  = list(fill = "#F1EFE8", stroke = "#5F5E5A", text = "#444441")
)

## --- Nodes --------------------------------------------------------------------
boxes <- tribble(
  ~id,          ~xmin, ~xmax, ~ymin, ~ymax, ~color,  ~title,                                  ~subtitle,
  "calendar",    6.0,  11.0, 10.20, 11.80, "gray",  "Calendar period and\ndocumentation context", "Sensitivity analysis factor",
  "covariates",  0.2,   5.0,  7.20,  9.00, "amber", "Baseline maternal and\nobstetric covariates", "Age, education, ethnicity, partnership,\nparity, prenatal care",
  "violence",    6.0,  11.0,  7.20,  9.00, "blue",  "Registry-recorded\ngestational violence",   "Exposure documented by trimester",
  "outcome1",    0.2,   5.4,  4.20,  5.80, "green", "Preterm birth and\nsmall for gestational age", "Neonatal outcomes",
  "outcome2",   11.6,  16.8,  4.20,  5.80, "green", "1-minute Apgar score <7 and\nexclusive breastfeeding at discharge", "Neonatal outcomes",
  "selection",   6.0,  11.0,  1.20,  2.80, "gray",  "Neonatal hospitalization",            "Conditioned cohort-selection variable"
)

selection_outer <- tibble(xmin = 5.85, xmax = 11.15, ymin = 1.05, ymax = 2.95)

## --- Solid Direct Arrows (No collisions) -------------------------------------
arrows_direct <- tribble(
  ~x,     ~y,    ~xend, ~yend,
  # Covariates
  5.00,   8.10,   6.00,  8.10,  # Covariates -> Violence
  2.60,   7.20,   2.60,  5.80,  # Covariates -> Outcome 1 (Directa vertical)

  # Calendar
  8.50,  10.20,   8.50,  9.00,  # Calendar -> Violence (Directa vertical)

  # Violence
  6.50,   7.20,   5.00,  5.80,  # Violence -> Outcome 1
  10.50,  7.20,  12.00,  5.80,  # Violence -> Outcome 2

  # Outcomes -> Selection
  4.50,   4.20,   6.50,  2.80,  # Outcome 1 -> Selection
  12.50,   4.20,  10.50,  2.80   # Outcome 2 -> Selection
)

## --- Orthogonal / Bypassing Paths (Evitan cajas intermedias) ------------------

# 1. Covariates -> Outcome 2 (Bordea por abajo de Covariates/Violence)
path_cov_out2 <- tibble(
  x = c(5.00, 5.70, 11.60),
  y = c(7.50, 6.50,  5.50)
)

# 2. Covariates -> Selection (Bordea por la izquierda externa)
path_cov_sel <- tibble(
  x = c(0.20, -0.60, -0.60, 6.00),
  y = c(8.10,  8.10,  2.00, 2.00)
)

# 3. Calendar -> Outcome 1 (Bordea por la izquierda de Violence)
path_cal_out1 <- tibble(
  x = c(7.00, 5.70, 5.70, 4.00),
  y = c(10.20, 9.60, 6.50, 5.80)
)

# 4. Calendar -> Outcome 2 (Bordea por la derecha de Violence)
path_cal_out2 <- tibble(
  x = c(11.00, 12.50, 13.50),
  y = c(10.80, 10.80,  5.80)
)

# 5. Calendar -> Selection (Bordea por la derecha externa completa)
path_cal_sel <- tibble(
  x = c(11.00, 17.50, 17.50, 11.00),
  y = c(11.20, 11.20,  2.00,  2.00)
)

# 6. Violence -> Selection (Curva punteada por la izquierda)
bezier_q <- function(p0, p1, p2, n = 80) {
  t <- seq(0, 1, length.out = n)
  tibble(
    x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
    y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2]
  )
}
curve_violence_selection <- bezier_q(
  p0 = c(6.00, 7.80),
  p1 = c(5.30, 4.80),
  p2 = c(6.00, 2.50)
)

## --- Build Plot --------------------------------------------------------------
fill_vec   <- map_chr(boxes$color, ~ pal[[.x]]$fill)
stroke_vec <- map_chr(boxes$color, ~ pal[[.x]]$stroke)
text_vec   <- map_chr(boxes$color, ~ pal[[.x]]$text)

p <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white", color = NA) +
  # Selection Outer Box
  geom_rect(data = selection_outer, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = pal$gray$stroke, linewidth = 0.45) +
  # Nodes
  geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = fill_vec, color = stroke_vec, linewidth = 0.45) +
  # Titles & Subtitles
  geom_text(data = boxes, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2 + 0.28, label = title),
            color = text_vec, size = 2.8, fontface = "bold", lineheight = 0.95) +
  geom_text(data = boxes, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2 - 0.38, label = subtitle),
            color = text_vec, size = 2.1, lineheight = 0.90) +

  # Direct Arrows
  geom_segment(data = arrows_direct, aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 0.45, color = "grey30",
               arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +

  # Routed Paths (Avoiding intersections)
  geom_path(data = path_cov_out2, aes(x = x, y = y), linewidth = 0.45, color = "grey30",
            arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  geom_path(data = path_cov_sel, aes(x = x, y = y), linewidth = 0.45, color = "grey30",
            arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  geom_path(data = path_cal_out1, aes(x = x, y = y), linewidth = 0.45, color = "grey30",
            arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  geom_path(data = path_cal_out2, aes(x = x, y = y), linewidth = 0.45, color = "grey30",
            arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  geom_path(data = path_cal_sel, aes(x = x, y = y), linewidth = 0.45, color = "grey30",
            arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +

  # Dashed Selection Curve
  geom_path(data = curve_violence_selection, aes(x = x, y = y),
            linewidth = 0.38, color = "grey45", linetype = "dashed",
            arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +

  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

## --- Legend & Framing --------------------------------------------------------
p <- p +
  geom_segment(aes(x = 0.35, y = -0.2, xend = 1.15, yend = -0.2), linewidth = 0.45, color = "grey30",
               arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  annotate("text", x = 1.30, y = -0.2, label = "Prespecified hypothesized relation", hjust = 0, size = 2.5, color = "grey20") +
  geom_segment(aes(x = 0.35, y = -0.6, xend = 1.15, yend = -0.6), linewidth = 0.38, color = "grey45", linetype = "dashed",
               arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  annotate("text", x = 1.30, y = -0.6, label = "Plausible additional selection pathway", hjust = 0, size = 2.5, color = "grey20") +
  annotate("rect", xmin = 0.35, xmax = 1.15, ymin = -1.1, ymax = -0.9, fill = NA, color = pal$gray$stroke, linewidth = 0.40) +
  annotate("text", x = 1.30, y = -1.0, label = "Conditioned selection variable", hjust = 0, size = 2.5, color = "grey20") +
  coord_fixed(xlim = c(-1.2, 18.2), ylim = c(-1.3, 12.2), expand = FALSE)

## --- Export ------------------------------------------------------------------
ggsave("Figure1_DAG_gestational_violence_fixed.tiff", p, width = 22, height = 14, units = "cm", dpi = 600, compression = "lzw", bg = "white")
ggsave("Figure1_DAG_gestational_violence_fixed.png", p, width = 22, height = 14, units = "cm", dpi = 300, bg = "white")
