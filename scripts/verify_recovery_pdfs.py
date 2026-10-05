#!/usr/bin/env python3
"""Build/check the exact-byte PDF inventory used by the recovery sync.

Read-only for Drive and Sheets. Building requires local synced PDFs, the original
recovery manifest, a Drive inventory CSV and reviewed, hash-bound exceptions.
Title matching is conservative (no fuzzy title acceptance). A matching title is
identity evidence, not a claim that a conference abstract is a full paper or that
it passes the screening protocol. Program-only title/author listings and reviewed
related publications remain on hold.

Examples:
  python scripts/verify_recovery_pdfs.py build --pdf-dir /path/to/recovery --drive-inventory analysis/audit_2026-10-05/recovery_drive_inventory_395.csv --date 2026-10-05
  python scripts/verify_recovery_pdfs.py check --pdf-dir /path/to/recovery
"""
from __future__ import annotations
import argparse
import concurrent.futures
import csv
import hashlib
import json
import re
import subprocess
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FOLDER = "1m03L684qekbvcQQlKE40_6KIJjdoQDO4"


def rows(path):
    with Path(path).open(newline="", encoding="utf-8-sig") as stream:
        return list(csv.DictReader(stream))


def indexed(items, field="record_index"):
    result = {}
    for row in items:
        key = row[field]
        if not key or key in result:
            raise ValueError(f"Blank or duplicate {field}: {key!r}")
        result[key] = row
    return result


def normalized(text):
    return re.sub(r"[^a-z0-9]", "", unicodedata.normalize("NFKD", text).lower().encode("ascii", "ignore").decode())


def title_variant(title):
    # Only remove catalogue language/issue annotations, never substantive words.
    title = re.sub(r"\s*\[(French|Spanish|Russian|English|Persian)\]\.?$", "", title, flags=re.I)
    return re.sub(r"\. \((?:Special [Ii]ssue|Supplement|Health, Nutrition|FR[0-9]|Saude de|Ebola outbreak).*$", "", title)


def digest(path):
    md5, sha = hashlib.md5(), hashlib.sha256()
    with path.open("rb") as stream:
        prefix = stream.read(5)
        if prefix != b"%PDF-":
            raise ValueError(f"Not a PDF: {path}")
        md5.update(prefix); sha.update(prefix)
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            md5.update(chunk); sha.update(chunk)
    return path.stat().st_size, md5.hexdigest(), sha.hexdigest()


def extract(path):
    texts = []
    for order in ("-layout", "-raw"):
        result = subprocess.run(["pdftotext", order, str(path), "-"], capture_output=True, check=True, timeout=180)
        texts.append(result.stdout.decode("utf-8", errors="replace"))
    return texts


def document_type(row):
    s = (row["access_type"] + " " + row["source"]).lower()
    if "generated from" in s: return "generated_text_pdf"
    if "factsheet" in s or "project information" in s: return "project_factsheet"
    if "program pdf" in s or "program book" in s: return "conference_program"
    if "poster" in s: return "conference_poster"
    if "abstract" in s or "conference" in s or "proceedings" in s: return "conference_abstract_or_book"
    if "protocol" in s or "clinicaltrials.gov" in s or "regulator" in s: return "study_protocol_or_registry_document"
    if "preprint" in s: return "preprint"
    if "report" in s or "who iris" in s or "dhs program" in s: return "report_or_journal_issue"
    return "article_or_repository_manuscript"


def build(args):
    manifest = indexed(r for r in rows(args.manifest) if r["status"] == "found_pdf")
    drive = indexed(rows(args.drive_inventory))
    reviews = indexed(rows(args.reviews))
    if set(manifest) != set(drive):
        raise ValueError("Recovered manifest IDs and Drive inventory IDs differ")
    if not set(reviews) <= set(manifest):
        raise ValueError("Review contains IDs absent from recovered manifest")
    source_ids = {reviews.get(i, {}).get("source_record_index") or i for i in manifest}
    unknown = source_ids - set(drive)
    if unknown: raise ValueError(f"Unknown reviewed source IDs: {sorted(unknown)}")
    def inspect(i):
        filename = drive[i]["filename"]
        if Path(filename).name != filename: raise ValueError("Unsafe source filename")
        path = args.pdf_dir / filename
        size, md5, sha = digest(path)
        if size != int(drive[i]["drive_size_bytes"]):
            raise ValueError(f"Drive snapshot/local size mismatch: {filename}")
        texts = extract(path)
        return i, (size, md5, sha, texts)
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        files = dict(pool.map(inspect, sorted(source_ids, key=int)))
    output = []
    for i in sorted(manifest, key=int):
        row, review = manifest[i], reviews.get(i, {})
        source_i = review.get("source_record_index") or i
        size, md5, sha, texts = files[source_i]
        title = normalized(title_variant(row["title"]))
        hits = []
        for order, text in zip(("layout", "raw"), texts):
            for page, content in enumerate(text.split("\f"), 1):
                if title and title in normalized(content):
                    hits.append((order, page))
                    break
        status = "verified" if hits else "hold"
        relation = "exact_record" if hits else "unconfirmed_identity"
        kind = document_type(manifest[source_i])
        evidence = (f"Normalized catalogue title found in PDF text ({hits[0][0]} order, PDF page {hits[0][1]})." if hits else "No conservative title match; manual review required.")
        # A programme can establish a title/author listing without supplying the
        # indexed abstract. A title hit alone must never mark it retrieved.
        if kind == "conference_program":
            status, relation = "hold", "title_author_listing_only"
            evidence = "Conference program/title listing only; require the actual abstract, poster or full report before PDF-found updating."
        if review:
            if review["expected_source_sha256"] != sha:
                raise ValueError(f"Reviewed PDF bytes changed for record {i}")
            status, relation, kind = review["validation_status"], review["identity_relation"], review["document_type"]
            evidence = review["evidence"]
        if status not in ("verified", "hold"): raise ValueError("Invalid review status")
        if kind == "conference_program" and status == "verified":
            raise ValueError(f"Program-only listing cannot be verified as a retrieved abstract/report: {i}")
        original_path = args.pdf_dir / drive[i]["filename"]
        original_size, original_md5, original_sha = digest(original_path)
        output.append(dict(
            record_index=i, title=row["title"], pdf_filename=f"{i}.pdf",
            source_record_index=source_i, source_drive_file_id=drive[source_i]["drive_file_id"],
            source_folder_id=FOLDER, source_filename=drive[source_i]["filename"],
            source_size_bytes=size, source_md5=md5, source_sha256=sha,
            validation_status=status, document_type=kind, identity_relation=relation,
            evidence=evidence, source_url=manifest[source_i]["candidate_pdf_url"],
            landing_page_url=manifest[source_i]["landing_page_url"],
            original_source_drive_file_id=drive[i]["drive_file_id"],
            original_source_filename=drive[i]["filename"], original_source_sha256=original_sha,
            prior_recovery_evidence=row["evidence"],
            live_present_at_audit=drive[i]["live_present"], verified_on=args.date,
            byte_evidence="Local synced PDF; size matches Drive snapshot. Runtime must recheck remote MD5 before copying."
        ))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, output[0], lineterminator="\n"); writer.writeheader(); writer.writerows(output)
    from collections import Counter
    print(json.dumps({"records":len(output), "validation_status":dict(Counter(r["validation_status"] for r in output)),
        "document_type":dict(Counter(r["document_type"] for r in output)),
        "remapped_source_count":sum(r["source_record_index"] != r["record_index"] for r in output)}, indent=2))


def check(args):
    inventory = indexed(rows(args.inventory))
    errors = []
    cache = {}
    for i, row in inventory.items():
        filename = row["source_filename"]
        if Path(filename).name != filename:
            errors.append(f"{i}: unsafe filename"); continue
        try:
            if filename not in cache: cache[filename] = digest(args.pdf_dir / filename)
            size, md5, sha = cache[filename]
            if (str(size), md5, sha) != (row["source_size_bytes"], row["source_md5"], row["source_sha256"]):
                errors.append(f"{i}: source bytes differ from committed inventory")
        except (OSError, ValueError) as error: errors.append(f"{i}: {error}")
    report = {"records_checked":len(inventory), "distinct_files_checked":len(cache), "errors":errors}
    print(json.dumps(report, indent=2))
    if errors: raise SystemExit(1)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    build_parser = sub.add_parser("build")
    build_parser.add_argument("--pdf-dir", type=Path, required=True)
    build_parser.add_argument("--manifest", type=Path, default=ROOT / "data-raw/agent_identified_pdf_manifest.csv")
    build_parser.add_argument("--drive-inventory", type=Path, required=True)
    build_parser.add_argument("--reviews", type=Path, default=ROOT / "data-raw/recovered_pdf_identity_reviews.csv")
    build_parser.add_argument("--output", type=Path, default=ROOT / "data-raw/recovered_pdf_inventory.csv")
    build_parser.add_argument("--date", required=True, help="Review date YYYY-MM-DD")
    build_parser.add_argument("--workers", type=int, default=4)
    build_parser.set_defaults(func=build)
    check_parser = sub.add_parser("check")
    check_parser.add_argument("--pdf-dir", type=Path, required=True)
    check_parser.add_argument("--inventory", type=Path, default=ROOT / "data-raw/recovered_pdf_inventory.csv")
    check_parser.set_defaults(func=check)
    args = parser.parse_args(); args.func(args)

if __name__ == "__main__": main()
