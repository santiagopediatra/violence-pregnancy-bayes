#!/usr/bin/env Rscript

canonical_scripts <- c(
  "Analisis_maestro.R",
  "12_ANÁLISIS_DE_MEDIACIÓN__Violencia___Outcomes_neonatales.R",
  "DAG_corrected_white_background.R",
  "requirements.R"
)

required_release_files <- c(
  canonical_scripts,
  "README.md",
  "LICENSE",
  "CITATION.cff",
  ".zenodo.json",
  "Figure1_DAG_gestational_violence_fixed.tiff",
  "Figure1_DAG_gestational_violence_fixed.png",
  "A1_perfil_materno_tabla2.csv",
  "A2_perfil_materno_diagnosticos_bayes.csv",
  "B1_tabla3_desenlaces_todos_metodos.csv",
  "B2_tabla3_basal_todos_metodos.csv",
  "C1_tabla4_atenuacion.csv",
  "D1_tabla5_QBA.csv",
  "E1_tabla6_IPW.csv",
  "F1_bootstrap_prem_T3.csv",
  "G1_evalue_prem_T3.csv",
  "G1b_evalue_prem_T3_extendido.csv",
  "J1_tendencia_temporal.csv",
  "J1c_tendencia_missingness_cruda.csv",
  "L1_predicted_probabilities_T3preterm.csv",
  "M1b_smd_ipw_con_anio_fix.csv",
  "M2_resultado_ipw_con_anio.csv",
  "S8_tendencia_temporal_finalv2.csv"
)

missing_files <- required_release_files[!file.exists(required_release_files)]
if (length(missing_files)) {
  stop("Missing required release files: ", paste(missing_files, collapse = ", "))
}

parse_errors <- character()
for (script in canonical_scripts) {
  tryCatch(parse(file = script), error = function(e) {
    parse_errors <<- c(parse_errors, paste(script, conditionMessage(e), sep = ": "))
  })
}
if (length(parse_errors)) stop(paste(parse_errors, collapse = "\n"))

prohibited <- c(
  "BASE_HASTA_2024.csv",
  "VIOLENCIA_HASTA_2024.csv",
  "PERFIL_MADRE_VIOLENTADA_COMPLETO.xlsx",
  "objetos_analisis_violencia_corregido.rds"
)
present_prohibited <- prohibited[file.exists(prohibited)]
if (length(present_prohibited)) {
  stop("Clinical/derived data files must not be included in the public release: ",
       paste(present_prohibited, collapse = ", "))
}

cat("Release validation: OK\n")
cat("Parsed scripts:", length(canonical_scripts), "\n")
cat("Required files:", length(required_release_files), "\n")
cat("No model fitting or numerical reproduction was performed.\n")
