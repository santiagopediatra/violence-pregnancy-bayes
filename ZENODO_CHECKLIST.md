# Zenodo publication checklist

- [ ] Confirm the final software title.
- [ ] Confirm the complete creator list and order.
- [ ] Add ORCID identifiers and complete affiliations where applicable.
- [ ] Confirm that version `1.0.0` is appropriate.
- [ ] Confirm CC BY 4.0 with all rights holders.
- [ ] Confirm the ethics approval and data-access wording against the manuscript.
- [ ] Run `Rscript validate_release.R` from a clean checkout.
- [ ] Verify the ZIP and `MANIFEST_SHA256.txt`.
- [ ] Confirm that no individual-level data, RDS models or credentials are present.
- [ ] Upload the ZIP to Zenodo as **Software**.
- [ ] Reserve or publish the DOI only after human review of metadata.
- [ ] Add the assigned DOI to `CITATION.cff`, `.zenodo.json` and `README.md`.
- [ ] Create a matching GitHub release/tag and archive the exact same version.

Publication in Zenodo is externally visible and versioned. The prepared archive
should not be published until authorship, licensing and ethics wording have been
confirmed by the responsible research team.
