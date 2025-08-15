
## How to use

#### 1. Prerequisites
-   **R**
-   **RStudio**: Recommended for an easier workflow.

#### 2. Setup
1.  Clone this repository to your local machine.
2.  Open the project in RStudio.
3.  Run the following command in the R console to install all required packages automatically. The script uses `pacman` to manage dependencies.
    ```r
    if (!require("pacman")) install.packages("pacman")
    ```

#### 3. Prepare your data
This is the most important step. You need to populate the data folders and configuration files:

1.  **`Actas/`**: Place all your PDF match reports from Comet in this folder.
2.  **`Calendarios/`**: (Optional) Add `.xlsx` files with future match schedules. This allows the website to show upcoming games.
3.  **`Logos/`**: Add team logos as `.png` files. The filename should match the team's name in Macedonian (e.g., `ЖФК Каменица Саса.png`). Include a `NOLOGO.png` as a fallback.
4.  **`igraci.xlsx`**: Fill this Excel file with player demographic data (position, date of birth, nationality).
5.  **Configure `.txt` files**: Go through the `.txt` files (`conversions.txt`, `id_unification.txt`, etc.) and adapt them to your data. See the "Configuration Files" section below for details.

#### 4. Run the scripts
The process is divided into two main scripts. **You must run them in order.**

1.  **Run `01_parse_reports.R`**: This script reads all the PDFs, processes the data, and creates a cache (`actas_cache.rds`). It only processes new or modified files on subsequent runs.
2.  **Run `02_generate_website.R`**: This script reads the cached data, calculates all statistics, and generates the complete multilingual website in the `docs/` folder.

After running both scripts, your website is ready! You can view it by opening `docs/index.html` in your browser.

---

## Configuration files explained

These files give you full control over the data and translations. They are designed to be edited easily in any text editor.

-   **`translations.txt`**: The master file for all UI translations. Add new columns for new languages (e.g., `es` for Spanish).
    ```csv
    key,mk,sq,en,es
    ```

-   **`conversions.txt`**: For global name corrections. Useful for fixing typos or too long names found across all PDFs.
    ```csv
    original,corregido
    "ЖФК Асс Унитед","АС Јунајтед"
    "ЖФК Каменица Саса","Каменица Саса"
    ```

-   **`id_unification.txt`**: Crucial for merging player profiles that have different IDs or name variations across different reports.
    ```csv
    id_canonico,id_a_unificar,nombre_canonico
    12345,67890,"Даскалова, Улса"
    ```

-   **`entity_corrections.txt` / `name_corrections.txt`**: Provide manual translations or transliterations for entities (teams, referees) and players. This overrides the automatic transliteration.
    ```csv
    original_mk,latin_sq,latin_es,latin_en
    АС Јунајтед,AS United,AS United,AS United
    ```

-   **`estilos_clasificacion.txt`**: Defines the color-coding for league tables (e.g., promotion spots, relegation).
    ```
    [COMPETICION: Прва Женска Лига 23/24]
    1, #4CAF50, league_champions_league
    2, #2196F3, league_europa_league
    8, #F44336, league_relegation
    ```

## How it works: the two-script pipeline

The project is split into two scripts to optimize performance and separate concerns.

1.  **`extraerinfopdf.R` (The parser)**:
    -   Scans the `Actas/` folder.
    -   Compares the files against a cache (`actas_cache.rds`) to identify new or changed PDFs.
    -   Parses only the necessary files, extracting all relevant data.
    -   Saves the raw results into the cache and logs the changes in `cache_info.rds`.
    -   This script can be run frequently without high computational cost.

2.  **`buildhtml.R` (The generator)**:
    -   Reads the change log (`cache_info.rds`). If there are no changes, it does nothing.
    -   If changes are detected (or if it's the first run), it loads all data from the cache.
    -   It merges player IDs, calculates all league-wide and career statistics, and prepares the data for every language.
    -   It then builds every single HTML page (competitions, matches, players, etc.) and saves them in the `docs/` folder, organized by language.
    -   On subsequent runs, if only a few PDFs were added, it intelligently regenerates only the affected pages (e.g., the competition page, the two team pages, and the player pages involved). A full rebuild is only triggered if files are deleted.

This architecture ensures that updating the website with a new week's worth of match reports is a matter of seconds, not minutes.