# Declarative dependencies. This file does not install or update packages.

required_packages <- c(
  "bayesplot",
  "boot",
  "broom",
  "brms",
  "dagitty",
  "EValue",
  "ggdag",
  "logistf",
  "posterior",
  "tidyverse",
  "WeightIt"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
