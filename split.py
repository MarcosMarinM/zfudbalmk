import os

os.makedirs('R', exist_ok=True)
with open('buildhtml.R', 'r', encoding='utf-8') as f:
    lines = f.readlines()

def write_chunk(name, start, end):
    with open('R/' + name, 'w', encoding='utf-8') as f:
        f.writelines(lines[start:end])

write_chunk('07_setup.R', 0, 17)
write_chunk('08_functions.R', 17, 842)
write_chunk('09_data_loading.R', 842, 1207)
write_chunk('10_data_processing.R', 1207, 1670)
write_chunk('11_aggregated_datasets.R', 1670, 2236)
write_chunk('12_assets.R', 2236, 3504)
write_chunk('13_html_generation.R', 3504, 5239)
write_chunk('14_finalization.R', 5239, len(lines))

main_script = """# buildhtml.R - Master build script
source("R/07_setup.R", encoding = "UTF-8")
source("R/08_functions.R", encoding = "UTF-8")
source("R/09_data_loading.R", encoding = "UTF-8")
source("R/10_data_processing.R", encoding = "UTF-8")
source("R/11_aggregated_datasets.R", encoding = "UTF-8")
source("R/12_assets.R", encoding = "UTF-8")
source("R/13_html_generation.R", encoding = "UTF-8")
source("R/14_finalization.R", encoding = "UTF-8")
"""

# Backup the original
os.rename('buildhtml.R', 'buildhtml_backup.R')

# Write the new buildhtml.R
with open('buildhtml.R', 'w', encoding='utf-8') as f:
    f.write(main_script)
