import csv
import yaml

# Load the GPS data from the YAML file
with open('z6_GPS.yaml', 'r') as yaml_file:
    gps_data = yaml.safe_load(yaml_file)

# Create a dictionary to map serial numbers to their GPS coordinates
gps_dict = {entry['Serial']: (entry['Latitude'], entry['Longitude']) for entry in gps_data}

# Read the CSV file and add the GPS coordinates
with open('z6_info_wide.csv', 'r') as csv_file:
    reader = csv.reader(csv_file)
    rows = list(reader)

# Add headers for Latitude and Longitude
header = rows[0]
header.extend(['Latitude', 'Longitude'])

# Add GPS data to each row
for row in rows[1:]:
    serial = row[2]
    if serial in gps_dict:
        latitude, longitude = gps_dict[serial]
        row.extend([latitude, longitude])
    else:
        row.extend(['', ''])  # Add empty values if no GPS data is found

# Write the updated rows to a new CSV file
with open('z6_info_wide_updated.csv', 'w', newline='') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerows(rows)

print("Updated CSV file has been written to 'z6_info_wide_updated.csv'")