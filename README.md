# dswrZentra

Processing pipeline for METER Z6 data logger data downloaded from
[Zentra Cloud](https://zentracloud.com/) — UVM Dairy Soil Water Regeneration /
Soil Health Institute project.

---

## Directory layout

```
dswrZentra/
├── R/
│   └── batch_concat.R          # utility functions used by concat_z6_data.R
├── data/
│   └── usr/
│       ├── inputs/
│       │   ├── Zentra/         # ← user copies Zentra Cloud exports here
│       │   │   └── z6-XXXXX(z6-XXXXX)-<id>/
│       │   │       ├── z6-XXXXX-Configuration 1-<id>.csv
│       │   │       ├── z6-XXXXX-Metadata-<id>.csv
│       │   │       └── ...
│       │   ├── serials.txt     # ← one z6 serial number per line
│       │   └── z6_info_wide.csv  # ← site/plot context for each logger
│       └── outputs/
│           ├── concat_data/    # intermediate: one CSV per serial (Step 2 output)
│           └── mapped_data/    # final: one CSV per sensor port  (Step 3 output)
├── concat_z6_data.R            # Step 2 script
├── map_z6_info_and_save_files.R # Step 3 script
└── README.md
```

### User-maintained files (the only files you need to edit)

| File | Location | Purpose |
|---|---|---|
| `serials.txt` | `data/usr/inputs/serials.txt` | One Z6 serial (e.g. `z6-14354`) per line |
| `z6_info_wide.csv` | `data/usr/inputs/z6_info_wide.csv` | Maps each serial to a site, plot, and lists which sensor is on each port |

All other mappings (Zentra Cloud header strings → standardised column names,
sensor product names → short codes) are kept in **R lists at the top of
`concat_z6_data.R`** (`param_lookup`, `sensor_lookup`). They change only when
METER releases a new sensor or renames a unit, and are easier to review/
diff as code than as YAML/CSV (the keys contain Unicode characters such as
`°`, `³`, `/`).

---

## Workflow

### Step 1 — Download data from Zentra Cloud

1. Log in to [zentracloud.com](https://zentracloud.com/).
2. Export data for the desired date range.
3. Copy the downloaded export folder(s) into:

   ```
   data/usr/inputs/Zentra/
   ```

   Each Z6 logger produces a sub-folder named like
   `z6-14355(z6-14355)-1767886067/` that contains several
   `Configuration*.csv` and a `Metadata*.csv`.

### Step 2 — Concatenate raw Zentra exports (`concat_z6_data.R`)

Opens every `Configuration*.csv` for each serial listed in `serials.txt`,
renames columns to standardised names, and writes one combined CSV per logger
to `data/usr/outputs/concat_data/`.

```r
source("concat_z6_data.R")
```

**Key user inputs** (top of the script):

| Variable | Default | Description |
|---|---|---|
| `folder_path` | `data/usr/inputs/Zentra/` | Root of Zentra Cloud exports |
| `serial_numbers` | read from `data/usr/inputs/serials.txt` | Which loggers to process |

### Step 3 — Map site/plot info and split by sensor (`map_z6_info_and_save_files.R`)

Reads the concatenated files from Step 2 and, **for every `Port{N}_{Sensor}`
column group actually present in the data**, writes one CSV to
`data/usr/outputs/mapped_data/`. Site and Plot are looked up from
`z6_info_wide.csv` keyed by `(Serial, Port)`.

Sensor identity comes from the data columns themselves (which were named by
`concat_z6_data.R` from the Zentra Cloud headers), so port re-wiring or
sensor swaps are handled automatically — the user only needs to keep the
Site/Plot column accurate. If a port carries multiple sensors, multiple
files are produced for that port.

Output filenames follow the pattern:
`<Site>_<Plot>_<Serial>_<Port>_<Sensor>.csv`

Each file begins with a metadata header:
```
# Site: VB
# Treatment: daf low
# Serial: z6-14354
# Port: Port1
# Sensor: TEROS11
# Data begins on line: 7
```

```r
source("map_z6_info_and_save_files.R")
```

**Key user inputs** (bottom of the script):

| Variable | Default | Description |
|---|---|---|
| `input_dir` | `data/usr/outputs/concat_data` | Output from Step 2 |
| `info_wide_file` | `data/usr/inputs/z6_info_wide.csv` | Site/plot info |
| `output_dir` | `data/usr/outputs/mapped_data` | Where to save results |
| `zentra_dir` | `data/usr/inputs/Zentra` | Used only for sensor-assignment verification (set `NULL` to skip) |

---

## Updating `z6_info_wide.csv`

Edit `data/usr/inputs/z6_info_wide.csv` when loggers are moved or sensors are
swapped.  The format is:

```
# zentra z6 data loggers — UVM DSWR project.
# updated YYYY-MM-DD
Site,Plot,Serial,Port_1,Port_2,Port_3,Port_4,Port_5,Port_6,Notes
VB,DAF low,z6-14354,TEROS 11,TEROS 11,SO-411,SO-411,,,
...
```

- `Site` — short site code (e.g. `VB`, `VA`)
- `Plot` — descriptive plot/treatment name
- `Port_1`–`Port_6` — sensor on each port; leave blank if unused. **These
  cells are descriptive only** — actual sensor identity is taken from the
  data columns. They are still useful for sanity-checking against the
  Zentra metadata (`zentra_dir` argument enables this check).
- `Notes` — free-text notes

The script automatically drops blank port cells and derives the long-form
mapping, so **no manual editing of `z6_info_long.csv` is needed**.

---

## Steps to complete (implementation progress)

- [x] Step A: Reorganise data directory — user files moved to `data/usr/inputs/`;
  `data/usr/inputs/Zentra/` created as the canonical drop zone for Zentra Cloud exports.
- [x] Step B: Fix `concat_z6_data.R` — add Unicode parameter names (°C, m³/m³)
  for 2025+ Zentra exports; add `kPa VPD` for ATMOS 14; fix input paths.
- [x] Step C: Refactor `map_z6_info_and_save_files.R` — use `z6_info_wide.csv`
  (wide-to-long pivot done in-script); include Site + Plot in output filenames;
  add optional Zentra metadata verification.
- [x] Step D: Update README with workflow documentation.
