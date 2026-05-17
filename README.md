# zfudbalmk — Macedonian women's football data & website

Automated data pipeline that scrapes match data from the Football Federation of Macedonia (FFM) website and generates a static website in Macedonian, Albanian, Spanish and English.

**Live site:** [zfudbalmk.web.app](https://zfudbalmk.web.app)

---

## How it works

### 1. Scraping (`Rscript scrape.R`)

The scraper reads ID ranges from `rangos_ids.txt` and fetches match data (najava + partido pages) from `ffm.mk` for each ID. It uses a two-tier cache system:

- **`actas_cache.rds`** — stores raw scraped match data keyed by FFM match ID.
- **`tracking.rds`** — tracks the state of each match (archived, live_post, scheduled, cancelled, etc.).

On each run, the scraper:

1. Loads the cache and tracking files.
2. Skips IDs already marked as archived.
3. Scrapes only new or unprocessed IDs.
4. Saves the updated cache and tracking.

### 2. Building (`Rscript buildhtml.R`)

Processes the scraped data and generates a complete static HTML site in `docs/`:

- Competition hubs with round-by-round match schedules.
- Team profile pages with rosters and schedules.
- Player profile pages with statistics.
- Referee profile pages.
- Match detail pages with lineups and events.
- Multilingual support (Macedonian, Albanian, Spanish, English).

### 3. Deployment (GitHub Actions)

Every push to `main` (and scheduled runs during the season) triggers the workflow in `.github/workflows/update_and_deploy.yml`:

1. **Checkout** — clones the repo with full git history (`fetch-depth: 0`).
2. **Cache restore** — tries `actions/cache` for a fast `.rds` restore.
3. **Fallback** (if cache miss) — four tiers:
   - Tier 0: `.rds` files already in workspace (from git checkout).
   - Tier 1: download from GitHub release (`actas-cache` asset).
   - Tier 2: restore from git history (last commit before `.rds` deletion).
   - Tier 3: scrape from scratch.
4. **Scrape** — `Rscript scrape.R`.
5. **Save to cache** — uploads updated `.rds` to `actions/cache`.
6. **Upload to release** — creates `rds-cache.tar.gz` and uploads it to the `actas-cache` GitHub release (replaces the previous asset).
7. **Build HTML** — `Rscript buildhtml.R`.
8. **CDN push** — pushes `search_data.json` to an orphan `data-cdn` branch for jsDelivr CDN.
9. **Deploy** — deploys to Firebase hosting.

#### Cache persistence

| Mechanism | Purpose | Speed |
|---|---|---|
| `actions/cache` (v2) | Fast restore between runs | ~1s |
| GitHub release (`actas-cache`) | Long-term backup (7-day cache expiry) | ~3s |
| Git tracking (committed `.rds`) | Last-resort seed | instant |

The `.rds` files are committed to git once as a seed. The workflow never commits them back — they travel via cache and release. This keeps the repository lightweight.

---

## File structure

### Core scripts

| File | Purpose |
|---|---|
| `scrape.R` | Scrapes match data from FFM website |
| `buildhtml.R` | Generates the static HTML site |
| `consultar.R` | Interactive R console helpers for ad-hoc queries and debugging |

### R pipeline modules (`R/`)

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

### Reference data files

| File | Purpose |
|---|---|
| `rangos_ids.txt` | FFM match ID ranges per competition |
| `competitions.txt` | Competition names and metadata |
| `translations.txt` | Multilingual UI strings (mk, sq, es, en) |
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
| `comps_ffm.xlsx` | Competition metadata spreadsheet |
| `igracki.xlsx` | Player registry spreadsheet with biographical data |

### Cache files (gitignored, tracked as seed)

| File | Purpose |
|---|---|
| `actas_cache.rds` | Scraped match data (keyed by FFM ID) |
| `tracking.rds` | Match state tracking (archived, live_post, etc.) |
| `cache_info.rds` | Cache metadata and timestamps |
| `official_results_cache.rds` | Cached official results |
| `cancelled_matches_cache.rds` | Cached cancelled matches |
| `competition_log.rds` | Competition processing log |

### Output (`docs/`)

Generated static site deployed to Firebase hosting.

---

## Running locally

### Prerequisites

- R (>= 4.0)
- System dependencies (Ubuntu/Debian):

```bash
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev
```

### Quick start

```bash
# scrape new data
Rscript scrape.R

# build the static site
Rscript buildhtml.R
```

> **Note:** The first scrape fetches all IDs from `rangos_ids.txt` (~2,100 matches) and takes ~30 minutes due to rate limiting. Subsequent runs only fetch new or unprocessed data.

The generated site will be in `docs/`. Open `docs/index.html` in a browser to preview.

---

## Scheduling

The GitHub Actions workflow runs on a schedule aligned with the Macedonian women's football season:

- **High season** (February–May, August–November): multiple times daily
  - Monday–Thursday: twice daily (15:09, 21:09 UTC)
  - Friday–Sunday: every 30 minutes during match hours (08:00–21:00 UTC)
- **Off season** (June–July, December–January): once daily (21:09 UTC) to keep caches warm

The workflow can also be triggered manually from the GitHub Actions tab.

---

## Translations

The site supports four languages, defined in `translations.txt`:

| Code | Language |
|---|---|
| `mk` | Macedonian |
| `sq` | Albanian |
| `es` | Spanish |
| `en` | English |

---

## License

Data sourced from [ffm.mk](https://ffm.mk). This project is not affiliated with the Football Federation of Macedonia.
