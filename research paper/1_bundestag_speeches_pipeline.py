# Author: Claude Sonnet 4.6
# Base prompt: "Create a pipeline that uses following api https://dip.bundestag.de/documents/informationsblatt_zur_dip_api.pdf to get all speeches from the beginning of the 20th period of the German Bundestag. If possible, include metadata for the date, speaker and party. Do this using either R or python."
# Conversation: Iteration - uploaded output of the code with instructions to improve/change the script to include/exclude different data.


"""
Bundestag DIP API Pipeline — Live Speeches with Full Text
==========================================================
Fetches all live Reden (aktivitaetsart = "Rede") from the 20th Wahlperiode
of the German Bundestag.

Two-phase approach:
  Phase 1 — DIP API  : fetch metadata for every "Rede" aktivitaet
                       (date, speaker, party, session reference, page range)
  Phase 2 — XML files: download each unique plenary protocol XML once,
                       find every <rede> block by matching speaker name,
                       and concatenate paragraphs into "speech_text".

Bug fixes vs previous version:
  - Filter f.aktivitaetsart=Rede now works correctly (PAGE_SIZE=10 as
    required by the aktivitaet endpoint; old version used 100 and fetched
    all 127k records regardless of filter)
  - XML matching now uses vorname+nachname instead of person_id. The DIP
    API's person_id is a short internal integer (e.g. "7244") while the
    plenary XML uses the long Bundestag MdB ID (e.g. "11004097"). They are
    different ID systems. Matching by name is reliable and avoids a costly
    extra API call per speaker to resolve the ID mapping.

Output columns:
  aktivitaet_id   — DIP internal ID
  datum           — date of the session
  dokumentnummer  — session number (e.g. "20/42")
  speaker_name    — full name parsed from the DIP "titel" field
  party           — Fraktion / party parsed from the DIP "titel" field
  topic           — agenda item(s) the speech relates to
  anfangsseite    — start page in the printed protocol
  endseite        — end page in the printed protocol
  pdf_url         — page-anchored PDF link
  speech_text     — full transcribed speech text (from protocol XML)

Requirements:
  pip install requests pandas lxml

Usage:
  python bundestag_speeches_pipeline.py                   # full run (~19k speeches)
  python bundestag_speeches_pipeline.py --max-pages 5    # test: 50 speeches
  python bundestag_speeches_pipeline.py --apikey YOUR_KEY

API key:
  The embedded key is the public demo key (valid until end of 05/2026).
  For production use request a personal key at:
  parlamentsdokumentation@bundestag.de
"""

import argparse
import json
import re
import sys
import time
from collections import defaultdict
from pathlib import Path

import pandas as pd
import requests
from lxml import etree

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

BASE_URL    = "https://search.dip.bundestag.de/api/v1"
PUBLIC_KEY  = "OSOegLs.PR2lwJ1dwCeje9vTj7FPOt3hvpYKtwKkhw"  # valid until end 05/2026
WAHLPERIODE = 20
# The aktivitaet endpoint is hard-capped at 10 results per page by the API.
# Using any larger value still returns only 10, so we set it explicitly.
PAGE_SIZE   = 10
SLEEP       = 0.3   # seconds between requests

KNOWN_PARTIES = {
    "SPD", "CDU/CSU", "CDU", "CSU", "AfD", "FDP",
    "BÜNDNIS 90/DIE GRÜNEN", "Die Linke", "Die Linke (Gruppe)",
    "BSW", "BSW (Gruppe)", "Gruppe BSW", "fraktionslos",
}

ACADEMIC_TITLES = re.compile(
    r"^(Dr\.-Ing\.|Prof\. Dr\.|Prof\.|Dr\.) ", re.IGNORECASE
)

# ---------------------------------------------------------------------------
# Phase 1: DIP API — speech metadata
# ---------------------------------------------------------------------------

def make_session(api_key: str) -> requests.Session:
    s = requests.Session()
    s.headers.update({"Authorization": f"ApiKey {api_key}", "Accept": "application/json"})
    return s


def paginate(session, base_params, max_pages=None):
    """Cursor-based pagination over the aktivitaet endpoint."""
    params = {**base_params, "format": "json", "num": PAGE_SIZE}
    cursor, prev_cursor = None, None
    all_docs, page = [], 0

    while True:
        if cursor:
            params["cursor"] = cursor

        print(
            f"  page {page + 1}"
            + (f" [cursor …{cursor[-10:]}]" if cursor else "")
            + " … ",
            end="", flush=True,
        )
        resp = session.get(f"{BASE_URL}/aktivitaet", params=params, timeout=30)
        resp.raise_for_status()
        data = resp.json()

        docs = data.get("documents", [])
        all_docs.extend(docs)
        print(f"{len(docs)} records  (total {len(all_docs)} / {data.get('numFound', '?')})")

        new_cursor = data.get("cursor")
        if not new_cursor or new_cursor == prev_cursor or not docs:
            break
        prev_cursor, cursor = cursor, new_cursor
        page += 1

        if max_pages is not None and page >= max_pages:
            print(f"  [--max-pages {max_pages} reached, stopping early]")
            break

        time.sleep(SLEEP)

    return all_docs


def fetch_speech_metadata(session, wahlperiode=WAHLPERIODE, max_pages=None):
    print(f"\n{'='*60}")
    print(f"Phase 1 — Fetching Rede metadata (Wahlperiode {wahlperiode})")
    print(f"{'='*60}")
    records = paginate(
        session,
        base_params={
            "f.wahlperiode": wahlperiode,
            "f.aktivitaetsart": "Rede",   # live speeches only
            "f.zuordnung": "BT",
        },
        max_pages=max_pages,
    )
    print(f"\n✓ {len(records)} Rede records fetched.")
    return records


# ---------------------------------------------------------------------------
# Name parsing helpers
# ---------------------------------------------------------------------------

def strip_academic_title(name: str) -> str:
    """Remove leading academic titles like 'Dr.', 'Prof. Dr.' etc."""
    return ACADEMIC_TITLE.sub("", name).strip() if False else ACADEMIC_TITLES.sub("", name).strip()


def parse_titel(titel: str) -> tuple[str, str, str, str]:
    """
    Parse DIP 'titel' into (full_name, vorname, nachname, party).

    Common formats:
      "Friedrich Merz, MdB, CDU/CSU"
      "Dr. Sahra Wagenknecht, MdB, BSW (Gruppe)"
      "Olaf Scholz, Bundeskanzler"
      "Bärbel Bas, Bundestagspräs."
    """
    if not titel:
        return "", "", "", ""

    parts = [p.strip() for p in titel.split(",")]
    raw_name = parts[0]
    clean_name = strip_academic_title(raw_name)
    name_parts = clean_name.split()
    vorname  = name_parts[0] if len(name_parts) > 1 else ""
    nachname = name_parts[-1] if name_parts else ""

    party = ""
    for part in parts[1:]:
        if part in KNOWN_PARTIES:
            party = part
            break
        for known in KNOWN_PARTIES:
            if known in part:
                party = known
                break
        if party:
            break

    return raw_name, vorname, nachname, party


# ---------------------------------------------------------------------------
# Phase 2: XML — extract full speech text
# ---------------------------------------------------------------------------

_xml_cache: dict = {}


def fetch_xml(xml_url: str, session: requests.Session):
    """Download and parse a plenary protocol XML; cache to avoid re-fetching."""
    if xml_url in _xml_cache:
        return _xml_cache[xml_url]
    try:
        resp = session.get(xml_url, timeout=60)
        resp.raise_for_status()
        root = etree.fromstring(resp.content)
        _xml_cache[xml_url] = root
        return root
    except Exception as exc:
        print(f"\n  [WARN] Could not fetch {xml_url}: {exc}")
        _xml_cache[xml_url] = None
        return None


def build_xml_name_index(xml_root) -> dict[str, list]:
    """
    Pre-index all <rede> blocks by (vorname, nachname) for fast lookup.

    The XML <redner> block looks like:
      <redner id="11004097">
        <name>
          <vorname>Friedrich</vorname>
          <nachname>Merz</nachname>
          <fraktion>CDU/CSU</fraktion>
        </name>
      </redner>

    Note: the XML uses long Bundestag MdB IDs (e.g. "11004097"), not the
    short DIP person_id integers (e.g. "7244"). We match by name instead.

    Returns dict mapping (vorname, nachname) -> list of <rede> elements.
    """
    index = defaultdict(list)
    if xml_root is None:
        return index
    for rede in xml_root.iter("rede"):
        for redner in rede.iter("redner"):
            vn_el = redner.find("name/vorname")
            nn_el = redner.find("name/nachname")
            if vn_el is not None and nn_el is not None:
                key = (vn_el.text.strip(), nn_el.text.strip())
                index[key].append(rede)
            break  # only first redner per rede
    return index


def extract_speech_text(rede_elements: list) -> str:
    """
    Concatenate all text paragraphs from a list of <rede> elements.
    Skips the speaker-header <p klasse="redner"> line.
    """
    paragraphs = []
    for rede in rede_elements:
        for p in rede.findall("p"):
            if p.get("klasse") == "redner":
                continue
            text = "".join(p.itertext()).strip()
            if text:
                paragraphs.append(text)
    return "\n\n".join(paragraphs)


# ---------------------------------------------------------------------------
# Flatten one record
# ---------------------------------------------------------------------------

def build_row(meta: dict, speech_text: str) -> dict:
    fundstelle  = meta.get("fundstelle") or {}
    vorgangsbez = meta.get("vorgangsbezug") or []
    topic = "; ".join(v.get("titel", "") for v in vorgangsbez)
    full_name, vorname, nachname, party = parse_titel(meta.get("titel", ""))

    return {
        "aktivitaet_id":  meta["id"],
        "datum":          meta.get("datum", ""),
        "dokumentnummer": fundstelle.get("dokumentnummer", ""),
        "speaker_name":   full_name,
        "party":          party,
        "topic":          topic,
        "anfangsseite":   fundstelle.get("anfangsseite", ""),
        "endseite":       fundstelle.get("endseite", ""),
        "pdf_url":        fundstelle.get("pdf_url", ""),
        "speech_text":    speech_text,
    }


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

def run(api_key, wahlperiode, max_pages, out_dir):
    session = make_session(api_key)

    # Phase 1 — metadata
    try:
        metadata = fetch_speech_metadata(session, wahlperiode, max_pages)
    except requests.HTTPError as e:
        print(f"\n✗ HTTP error: {e}")
        print("  Check your API key. Request one at: parlamentsdokumentation@bundestag.de")
        sys.exit(1)

    # Phase 2 — full text from XML
    print(f"\n{'='*60}")
    print("Phase 2 — Downloading protocol XML files & extracting text")
    print(f"{'='*60}")

    by_xml: dict[str, list[dict]] = defaultdict(list)
    no_xml: list[dict] = []
    for m in metadata:
        xml_url = (m.get("fundstelle") or {}).get("xml_url")
        if xml_url:
            by_xml[xml_url].append(m)
        else:
            no_xml.append(m)

    n_protocols = len(by_xml)
    print(f"  {n_protocols} unique protocols to fetch "
          f"({len(no_xml)} speeches have no XML URL)\n")

    rows = []
    for i, (xml_url, speeches) in enumerate(by_xml.items(), 1):
        session_ref = (speeches[0].get("fundstelle") or {}).get("dokumentnummer", "?")
        print(
            f"  [{i:>3}/{n_protocols}] {session_ref}  ({len(speeches)} speeches) … ",
            end="", flush=True,
        )
        xml_root  = fetch_xml(xml_url, session)
        name_idx  = build_xml_name_index(xml_root)

        found = 0
        for m in speeches:
            _, vorname, nachname, _ = parse_titel(m.get("titel", ""))
            rede_els = name_idx.get((vorname, nachname), [])
            text = extract_speech_text(rede_els)
            if text:
                found += 1
            rows.append(build_row(m, text))

        print(f"text found for {found}/{len(speeches)}")
        time.sleep(SLEEP)

    for m in no_xml:
        rows.append(build_row(m, ""))

    # Build DataFrame and save
    print(f"\n{'='*60}")
    print("Phase 3 — Saving output")
    print(f"{'='*60}\n")

    df = pd.DataFrame(rows)
    df["datum"] = pd.to_datetime(df["datum"], errors="coerce")
    df.sort_values(["datum", "dokumentnummer"], inplace=True)
    df.reset_index(drop=True, inplace=True)

    with_text = (df["speech_text"].str.len() > 0).sum()
    print(f"  Total speeches  : {len(df)}")
    print(f"  With full text  : {with_text}")
    print(f"  Date range      : {df['datum'].min().date()} → {df['datum'].max().date()}")
    print(f"  Unique speakers : {df['speaker_name'].nunique()}")
    print(f"\n  Speeches per party:")
    for party, n in df["party"].value_counts().items():
        print(f"    {(party or '(role/no party)'):45s} {n:5d}")

    out_dir.mkdir(parents=True, exist_ok=True)
    csv_path  = out_dir / f"bundestag_{wahlperiode}wp_speeches.csv"
    json_path = out_dir / f"bundestag_{wahlperiode}wp_speeches.json"

    df.to_csv(csv_path, index=False, encoding="utf-8-sig")
    df.to_json(json_path, orient="records", force_ascii=False, indent=2, date_format="iso")

    print(f"\n✓ CSV  → {csv_path}")
    print(f"✓ JSON → {json_path}")
    print("\nDone.")


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def parse_args():
    p = argparse.ArgumentParser(
        description="Fetch live Bundestag Reden with full speech text."
    )
    p.add_argument("--apikey",      default=PUBLIC_KEY,  help="DIP API key")
    p.add_argument("--wahlperiode", default=WAHLPERIODE, type=int)
    p.add_argument("--max-pages",   default=None,        type=int, metavar="N",
                   help="Limit to N pages (10 speeches each) — useful for testing")
    p.add_argument("--out-dir",     default=".",         help="Output directory")
    return p.parse_args()


if __name__ == "__main__":
    args = parse_args()
    run(
        api_key=args.apikey,
        wahlperiode=args.wahlperiode,
        max_pages=args.max_pages,
        out_dir=Path(args.out_dir),
    )
