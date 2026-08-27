# Release notes — 1.0.0

Date prepared: 2026-08-23

## Included

- Consolidated version 2 master analysis and the separate mediation analysis.
- Aggregate CSV results and formatted DOCX tables.
- Conceptual DAG source and publication-resolution image files.
- Reproducibility, citation, licensing and Zenodo metadata.

## Curation decisions

- `Analisis_maestro.R` is designated as the canonical consolidated pipeline.
- The historical master `Analisis .R` and the superseded modular scripts `01`,
  `02`, `03`, `04`, `07` and `10` were removed because their analyses are
  consolidated in `Analisis_maestro.R` version 2.
- Exact copies carrying the suffix `(1)` are omitted.
- `M1_smd_ipw_con_anio.csv` was byte-identical to the corrected
  `M1b_smd_ipw_con_anio_fix.csv`; only the corrected canonical filename is kept.
- Individual-level data, RDS objects and fitted Stan models are omitted because
  they contain or derive from sensitive clinical data and are reproducible only
  with authorized access.

## Validation performed

- All five distributed R source files parsed successfully with R 4.5.0.
- Required release artifacts were present.
- CSV files were confirmed to contain small aggregate tables rather than
  individual-level records.
- DOCX containers and image formats were structurally valid.
- No complete model refit was performed because the source dataset was not
  available in the publication workspace.

## Known limitations

- Package versions are declared but not frozen with `renv.lock`.
- Dataset checksum and a verified institutional data dictionary were not
  available.
- Creator order, ORCID identifiers, ethics wording and final license require
  confirmation by the responsible research team before publication.
