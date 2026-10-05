# Sys_mcs

## Reviewed PDF recovery and country correction

These workflows copy verified PDFs already in Drive into the main `Retrieved_PDFs` folder and update exact screening records. **Both default to a dry run. Production changes have not been applied; running `--apply` requires the user's approval of the proposed changes.**

Reviewed starting state, 5 October 2026:

| Input | Ready action | Exceptions |
| --- | --- | --- |
| Gambia/Djibouti correction | Restore 9 records to full-text screening: 6 PDFs found, 3 missing | Final article **39727** replaces preprint **124095** explicitly; other reviewed exclusions/holds are not appended |
| 395 recovered no-DOI files | Copy/reuse 354 verified PDFs and update 336 existing records; 18 absent records get no new rows | 41 identity holds: 37 existing and 4 absent records; neither copied nor updated |
| 126 unresolved no-DOI records | Separate retrieval/outcome follow-up | Not part of these upload manifests |

Country restoration is **not final review inclusion**. New screening decisions, assignments and comments remain blank. Crisis geography, HRP timing and other eligibility checks remain for full-text screening. The three missing country PDFs are 62533, 98605 and 108251; the poster schedule for 108251 does not count as its PDF.

Inputs: `data-raw/hrp_country_correction_records.csv`, `data-raw/hrp_country_correction_review.csv`, `data-raw/recovered_pdf_inventory.csv` and `data-raw/recovered_pdf_identity_reviews.csv`. PDF identity is pinned to source file ID, folder, filename, size and checksum. Source `124095.pdf` is explicitly copied as `39727.pdf`, using the final article's bibliography.

## Setup and access

Run commands from the repository root. Install the runtime packages once:

```bash
Rscript -e 'install.packages(c("googledrive", "googlesheets4", "httr", "jsonlite"), repos="https://cloud.r-project.org")'
```

The scripts authenticate through `googledrive` and `googlesheets4`, opening a browser when required. The account needs source-folder read/download access, destination-folder write access and target-sheet edit access for apply. Keep the OAuth cache outside the repository.

Defaults are:

| Resource | ID |
| --- | --- |
| Live screening sheet, `studies` tab | `1foHMPBCWYzWX8975UKjdacE2i_j_48n8uTMdAQ-ETcg` |
| Recovery source folder | `1m03L684qekbvcQQlKE40_6KIJjdoQDO4` |
| Additional country PDF source folder | `1CWNX7lgG5_aj8BPDUsfFa83LAGsUt8WK` |
| Main destination `Retrieved_PDFs` | `1C7ufZYeYVyvzULW7zisp_PKlEYUyqYIU` |

Source-folder overrides recorded per manifest row are honoured. Neither workflow changes sharing permissions.

## Run and review

First read current Drive/sheet state and save plans without remote writes:

```bash
Rscript analysis/10_hrp_correction.R --dry-run
Rscript analysis/10_upload_no_doi_pdfs_to_drive.R --dry-run
```

Inspect the printed receipt directory under `analysis/recovery_runs/`: `plan.json`, `files.csv`, `cells.csv`, `exceptions.csv` and `receipt.json`. Identity holds have a separate ledger. Historical `analysis/data-derived/no_doi_*` upload outputs were dry-run records and are not evidence of completed uploads.

After approval, apply the reviewed workflows:

```bash
Rscript analysis/10_hrp_correction.R --apply
Rscript analysis/10_upload_no_doi_pdfs_to_drive.R --apply
```

Each apply rereads the sheet and Drive, checks identity/checksums, copies or reuses PDFs, writes exact cells, and verifies readback. It recomputes row positions; conflicting identities, links, duplicate filenames or concurrent edits stop the run with a receipt.

The two physical `pdf_link` columns are handled explicitly: the first receives the main-folder URL and the second mirrors it. Existing valid mirrors are preserved. Only PDF fields and blank/`PDF missing` retrieval statuses change on existing rows; screening decisions, assignments, comments, notes and validation remain protected. Google may add the expected hyperlink formatting to a newly inserted destination URL. Country additions receive blank screening fields and copied formatting/validation. Missing country PDFs are marked `FALSE`/`PDF missing`; existing found PDFs are never downgraded.

Reruns reuse matching files and records. Inspect partial/uncertain receipts before rerunning; copied files remain available for recovery. Run only one apply process at a time.

Use `--help` for options. For an authorised disposable integration target, override both resources:

```bash
Rscript analysis/10_hrp_correction.R --dry-run --sheet-id TEST_SHEET_ID --destination-folder TEST_FOLDER_ID
```

`--snapshot-dir PATH` supports offline dry runs using `sheet.json`, `source_files.json` and `destination_files.json`; it cannot be combined with `--apply`. Legacy `SYS_MCS_NODOI_*` and `DESTINATION_MODE` environment-variable gates are superseded by this CLI.

## Validation

```bash
Rscript tests/test_recovery_sync.R
Rscript tests/test_recovery_review.R
Rscript tests/test_recovery_cli.R
Rscript tests/test_recovery_google_adapter.R
```

These local tests simulate state and mock Google/auth/HTTP calls; the adapter test skips if runtime packages are absent. A separate disposable Google connector integration passed readback for 5 PDF copies, 8 existing-cell updates and 3 country appends (2 found, 1 missing). It checked 121,716 existing cells and their metadata, preserving reviewer fields, notes and validation while allowing the expected destination autolinks. New reviewer fields were blank. This verifies real Google operations separately from the mocked R API tests; it does not claim an authenticated end-to-end R-adapter run or production application.
