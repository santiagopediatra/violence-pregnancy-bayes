[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22072677.svg)](https://doi.org/10.5281/zenodo.22072677)

# Registered gestational violence and neonatal outcomes at HGOIA

Reproducible code and derived aggregate results for analyses of **registry-recorded gestational violence and neonatal outcomes** at Hospital Gineco-Obstétrico Isidro Ayora (HGOIA), Quito, Ecuador, 2009–2024.

The exposure is violence documented in the clinical registry. It must not be interpreted as the true prevalence of gestational violence. Under-recording, missingness and selection into the neonatal cohort are addressed through sensitivity analyses.

| 🧬 **Estudio Clínico / Salud Global** | [Frontiers in Global Women's Health](https://doi.org/10.3389/fgwh.2026.1920014) |
| :--- | :--- |
| **Título** | Recorded gestational violence and neonatal risks: A 15-year study in hospitalized deliveries |
| **Enfoque** | Violencia gestacional, riesgos neonatales, parto prematuro y uso de sustancias |
| **Metodología** | Análisis de datos perinatales históricos (15 años de seguimiento) |
| **Enlace Directo** | [![DOI Frontiers](https://shields.io)](https://doi.org/10.3389/fgwh.2026.1920014) |


[![DOI:10.3389/fgwh.2026.1920014](https://shields.io)](https://doi.org/10.3389/fgwh.2026.1920014)

---

## Repository contents

| File group | Purpose |
|---|---|
| `Analisis_maestro.R` | Canonical consolidated analysis after peer review |
| `12_ANÁLISIS_DE_MEDIACIÓN__Violencia___Outcomes_neonatales.R` | Extended sensitivity/mediation analysis |
| `DAG_corrected_white_background.R` | Reproducible source for the conceptual DAG |
| `A1_*.csv`–`M2_*.csv`, `S8_*.csv` | Aggregate tables and sensitivity results; `M1b_smd_ipw_con_anio_fix.csv` is the canonical balance table |
| `Table*.docx` | Final formatted main and supplementary tables |
| `Figure1_DAG_gestational_violence_fixed.*` | Final conceptual DAG in PNG and TIFF publication formats |
| `requirements.R` | Declarative package list; does not install software |
| `validate_release.R` | Structural validation without clinical data or model fitting |

Superseded scripts and files with the suffix `(1)` were removed. The repository retains only the version 2 master analysis, the separate mediation analysis and final named outputs.

## Data availability and confidentiality

No individual-level clinical data are included in this repository or in the Zenodo package. Full reproduction requires an authorized copy of:

```text
BASE_HASTA_2024.csv
```

The data contain sensitive health information concerning gestational violence and neonatal care. Access, use and redistribution remain subject to HGOIA institutional authorization and the applicable ethics and confidentiality requirements. Do not attempt re-identification.

The code assumes the original variable names, including UTF-8 names such as `Número.Consultas.prenatales` and `Año`. A data dictionary and a checksum for the source dataset were not available in the materials audited for this release; they should be added only after institutional verification.

## Software requirements

- R 4.x.
- A working C++/Stan toolchain for `brms`.
- Packages listed in `requirements.R`.

The Bayesian models are computationally intensive. The master script reports approximately 4–6 hours on a multicore workstation, but runtime varies substantially by hardware and Stan configuration.

## Validation without data

From this directory:

```bash
Rscript validate_release.R
```

This parses the canonical R scripts, checks required release files and verifies that no prohibited clinical-data filename is present. It does not fit models or validate the numerical results against source data.

## Full analysis

Place an authorized `BASE_HASTA_2024.csv` in this directory and run from a clean R session:

```r
source("Analisis_maestro.R")
```

The script uses seed `2024` and writes aggregate CSV results to the working directory. Model specifications, covariates, priors and sensitivity analyses are documented inline.

To regenerate the conceptual DAG:

```bash
Rscript DAG_corrected_white_background.R
```

## Interpretation safeguards

- Exposure means **registry-recorded violence**, not latent or true violence.
- Low recorded prevalence is compatible with under-recording.
- The cohort is conditioned on neonatal hospitalization, so estimates are not automatically generalizable to all pregnancies or births at HGOIA.
- The 2021–2022 period has markedly higher missingness in violence fields and is examined separately.
- QBA, IPW, prior sensitivity and E-values quantify assumptions; they do not eliminate bias.

## Reproducibility status

The scripts parse successfully and the distributed result files are internally readable. A complete re-fit from a clean environment was not performed during release preparation because the authorized clinical dataset was unavailable and the Stan models are computationally expensive. This release therefore supports code and result auditing, with full numerical reproduction conditional on authorized data access.

## Citation

Use `CITATION.cff`. After Zenodo assigns a DOI, cite the version-specific DOI shown on the Zenodo record.

## License

Code, documentation and distributed aggregate outputs are provided under the [Creative Commons Attribution 4.0 International License](LICENSE). This license does not grant access to or permission to redistribute individual-level clinical data.

## Contact

Santiago Vasco-Morales<br>
Hospital Gineco-Obstétrico Isidro Ayora, Quito, Ecuador<br>
`snvasco@uce.edu.ec` · `santiago.vasco@hgoia.gob.ec`
