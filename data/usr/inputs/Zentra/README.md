# Zentra/ — Zentra Cloud export drop zone

Copy your Zentra Cloud export folder(s) here. `data/user/inputs/Zentra/

Zentra users: Molly Ratliff (Admin) and Adrian Wiegman, Joshua Faulkner Dan Lipskin 

Down load all data for all time ranges then extract the files. 

Each logger export is a sub-folder named like:

```
z6-14355(z6-14355)-1767886067/
  z6-14355(z6-14355)-Configuration 1-1767886067.700015.csv
  z6-14355(z6-14355)-Configuration 2-1767886067.700015.csv
  z6-14355(z6-14355)-Metadata-1767886067.700015.csv
  ...
```

After copying data here, run `concat_z6_data.R` to produce per-serial
concatenated files in `data/usr/outputs/concat_data/`.
