# Sys_mcs

## No-DOI PDF recovery upload

The reproducible handoff for agent-identified no-DOI PDFs is:

- Agent manifest: `data-raw/agent_identified_pdf_manifest.csv`
- Wrapper: `data-raw/fetch_agent_identified_pdfs.R`
- Local PDFs: `analysis/pdf_download/<record_index>.pdf`
- Derived report: `analysis/data-derived/agent_identified_pdf_status.csv`

The manifest records which PDFs were identified and verified by agentic search
and which records remained missing, with `missing_reason` retained for
not-found rows. By default the wrapper only validates the CSV and writes the
derived report; Drive reads require `--run`, and Drive writes require the
separate `SYS_MCS_AGENT_PDF_ALLOW_DEST_UPLOAD=true` gate.

```bash
Rscript data-raw/fetch_agent_identified_pdfs.R --dry-run
```

To download staged PDFs from Google Drive, provide a source staging folder ID
or exact name:

```bash
SYS_MCS_AGENT_PDF_SOURCE_FOLDER_ID="<staging-folder-id>" \
Rscript data-raw/fetch_agent_identified_pdfs.R --run
```

Recovered no-DOI PDFs are staged in the same local folder used by the DOI workflow:

- Local PDFs: `analysis/pdf_download/<record_index>.pdf`
- Upload manifest: `analysis/data-derived/no_doi_pdf_manifest.csv`
- Dry-run outputs: `analysis/data-derived/no_doi_drive_manifest.csv`, `analysis/data-derived/no_doi_sheet_update_preview.csv`, and `analysis/data-derived/no_doi_upload_summary.json`

Dry-run validation, with no Google Drive or Sheet writes:

```r
Rscript analysis/10_upload_no_doi_pdfs_to_drive.R
```

Live upload/update requires explicit gates and the existing Sheet ID:

```bash
SYS_MCS_NODOI_ALLOW_DRIVE_UPLOAD=true \
SYS_MCS_NODOI_ALLOW_SHEET_UPDATE=true \
SYS_MCS_NODOI_ALLOW_SHARE_ANYONE=true \
SYS_MCS_NODOI_GOOGLE_SHEET_ID="<spreadsheet-id>" \
SYS_MCS_NODOI_GOOGLE_SHEET_NAME="studies" \
Rscript analysis/10_upload_no_doi_pdfs_to_drive.R
```

The script reuses existing Drive files with matching filenames, uploads missing ones to `Causes_of_Mortality_Review/Retrieved_PDFs`, and updates rows by `record_index` using the existing `pdf_link`, `pdf_found`, and `screening_status` columns.
