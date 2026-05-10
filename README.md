Project for collecting Macedonian men's football data and building a website.

This repository contains the core scripts and reference data needed to scrape sources, process records and generate static html pages.

How it works:

- `scrape.R` is used to fetch raw data from the source sites.
- the `R/` folder holds the processing scripts that clean, merge and transform collected data.
- `buildhtml.R` runs the build pipeline and writes the generated html output.
- the workflow relies on plain text data files and a spreadsheet to define the sources, names and competition structure.

Reference data files:

- `abbreviations.txt`: maps shortened names and abbreviations to full forms.
- `cancelled_matches.txt`: lists matches that were cancelled or removed from official timetables.
- `clubs_latin.txt`: contains latin-script club names for use in output (those which are not straightforward with the usual transliteration from Macedonian).
- `competitions_disambiguation.txt`: resolves ambiguous competition names and labels.
- `competitions.txt`: translates the competition names.
- `conversions.txt`: defines conversions and normalisations applied during processing.
- `name_corrections.txt`: contains corrected player and club name variants.
- `official_results.txt`: stores conceded results (i.e. 3-0)
- `rangos_ids.txt`: lists id ranges and related mapping information.
- `translations.txt`: provides text translations for the generated pages.
- `comps_ffm.xlsx`: spreadsheet with competition metadata and source mapping used by the build.
- `Logos/`: folder of club logo images used by the generated site.