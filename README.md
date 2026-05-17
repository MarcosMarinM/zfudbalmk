# ŽFudbalMK — Macedonian Women's Football Data & Website

Automated data pipeline that scrapes match data from the Football Federation of Macedonia (FFM) website and generates a bilingual (Macedonian/Albanian) static website with Spanish fallback.

**Live site:** [zfudbalmk-mmm.web.app](https://zfudbalmk-mmm.web.app)

---

## How It Works

### 1. Scraping (`Rscript scrape.R`)

The scraper reads ID ranges from `rangos_ids.txt` and fetches match data (najava + partido pages) from `ffm.mk` for each ID. It uses a two-tier cache system:

- **`actas_cache.rds`** — Stores raw scraped match data keyed by FFM match ID
- **`tracking.rds`** — Tracks the state of each match (Archived, Live_Post, Scheduled, Cancelado, etc.)

On each run, the scraper:
1. Loads the cache and tracking files
2. Skips IDs that are already marked as "Archived" in tracking
3. Scrapes only new/unprocessed IDs
4. Saves the updated cache and tracking

### 2. Building (`Rscript buildhtml.R`)

Processes the scraped data and generates a complete static HTML site in `docs/`:

- Competition hubs with round-by-round match schedules
- Team profile pages with rosters and schedules
- Player profile pages with statistics
- Referee profile pages
- Match detail pages with lineups and events
- Multilingual support (Macedonian, Albanian, Spanish, English)

### 3. Deployment (GitHub Actions)

Every push to `main` (and scheduled runs during the season) triggers the workflow `.github/workflows/update_and_deploy.yml`:

1. **Checkout** — clones the repo (with full git history via `fetch-depth: 0`)
2. **Cache restore** — tries `actions/cache` for a fast `.rds` restore
3. **Fallback** (if cache miss) — 4-tier fallback:
   - **Tier 0:** `.rds` files already present in workspace (from git checkout)
   - **Tier 1:** Download from GitHub Release (`actas-cache` asset)
   - **Tier 2:** Restore from git history (last commit before `.rds` deletion)
   - **Tier 3:** Scrape from scratch
4. **Scrape** — `Rscript scrape.R`
5. **Save to cache** — uploads updated `.rds` to `actions/cache`
6. **Upload to release** — creates `rds-cache.tar.gz` and uploads to the `actas-cache` GitHub Release (replaces previous asset)
7. **Build HTML** — `Rscript buildhtml.R`
8. **CDN push** — pushes `search_data.json` to orphan `data-cdn` branch for jsDelivr CDN
9. **Deploy** — deploys to Firebase Hosting

#### Cache Persistence

| Mechanism | Purpose | Speed |
|---|---|---|
| `actions/cache` (v2) | Fast restore between runs | ~1s |
| GitHub Release (`actas-cache`) | Long-term backup (7-day cache expiry) | ~3s |
| Git tracking (committed `.rds`) | Last-resort seed | instant |

The `.rds` files are committed to git **once** as a seed. The workflow never commits them back — they travel via cache and release. This keeps the repository lightweight.

---

## File Structure

### Core Scripts

| File | Purpose |
|---|---|
| `scrape.R` | Scrapes match data from FFM website |
| `buildhtml.R` | Generates the static HTML site |
| `consultar.R` | Interactive R console helpers for ad-hoc queries and debugging |
| `test_fix.R` | Ad-hoc fixes and data patching |

### R Pipeline Modules (`R/`)

| File | Purpose |
|---|---|
| `01_setup.R` | Libraries, paths, helpers |
| `02_load_reference.R` | Loads reference data (translations, clubs, etc.) |
| `03_load_cache.R` | Loads `actas_cache.rds` and `tracking.rds` |
| `04_scrape.R` | Web scraping logic for FFM pages |
| `05_parse.R` | Parses raw HTML into structured match data |
| `06_process.R` | Data cleaning and normalization |
| `07_ranking.R` | League standings calculations |
| `08_players.R` | Player statistics aggregation |
| `09_teams.R` | Team profiles and schedules |
| `10_referees.R` | Referee data processing |
| `11_cache.R` | Cache save/load and tracking management |
| `12_assets.R` | CSS, JavaScript, and asset generation |
| `13_html_generation.R` | HTML page builders |
| `14_output.R` | File writing and site assembly |

### Reference Data Files

| File | Purpose |
|---|---|
| `rangos_ids.txt` | FFM match ID ranges per competition |
| `competitions.txt` | Competition names and metadata |
| `translations.txt` | Multilingual UI strings (MK, SQ, ES, EN) |
| `abbreviations.txt` | Club name abbreviations |
| `clubs_latin.txt` | Latin-script club names (non-trivial transliterations) |
| `name_corrections.txt` | Player/club name corrections |
| `entity_corrections.txt` | General entity corrections |
| `country_codes.txt` | Country code mappings |
| `country_translations.txt` | Country name translations |
| `official_results.txt` | Conceded/official results (e.g. 3-0) |
| `cancelled_matches.txt` | Matches cancelled or removed from schedules |
| `referee_notes.txt` | Referee annotations |
| `competitions_disambiguation.txt` | Ambiguous competition name resolution |
| `sanctions.txt` | Player/club sanctions |
| `conversions.txt` | Data normalisation rules |
| `gradovi.txt` | City/town names |
| `classification_styles.txt` | League classification CSS styles |
| `comps_ffm.xlsx` | Competition metadata spreadsheet (source mapping) |
| `igracki.xlsx` | Player registry spreadsheet with biographical data |

### Cache Files (gitignored, tracked as seed)

| File | Purpose |
|---|---|
| `actas_cache.rds` | Scraped match data (keyed by FFM ID) |
| `tracking.rds` | Match state tracking (Archived, Live_Post, etc.) |
| `cache_info.rds` | Cache metadata and timestamps |
| `official_results_cache.rds` | Cached official results |
| `cancelled_matches_cache.rds` | Cached cancelled matches |
| `competition_log.rds` | Competition processing log |

### Output (`docs/`)

Generated static site deployed to Firebase Hosting.

---

## Running Locally

### Prerequisites

- R (≥ 4.0)
- System dependencies (Ubuntu/Debian):
  ```bash
  sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev
  ```

### Quick Start

```bash
# Scrape new data
Rscript scrape.R

# Build the static site
Rscript buildhtml.R
```

> **Note:** The first scrape fetches all IDs from `rangos_ids.txt` (~2,100 matches) and takes ~30 minutes due to rate limiting. Subsequent runs only fetch new/unprocessed data.

The generated site will be in `docs/`. Open `docs/index.html` in a browser to preview.

---

## Scheduling

The GitHub Actions workflow runs on a schedule aligned with the Macedonian women's football season:

- **High season** (Feb–May, Aug–Nov): Multiple times daily
  - Mon–Thu: Twice daily (15:09, 21:09 UTC)
  - Fri–Sun: Every 30 min during match hours (08:00–21:00 UTC)
- **Off season** (Jun–Jul, Dec–Jan): Once daily (21:09 UTC) to keep caches warm

---

## Translations

The site supports 4 languages, defined in `translations.txt`:

| Code | Language |
|---|---|
| `mk` | Macedonian (default) |
| `sq` | Albanian |
| `es` | Spanish (fallback) |
| `en` | English |

---

## License

Data sourced from [ffm.mk](https://ffm.mk). This project is not affiliated with the Football Federation of Macedonia.
