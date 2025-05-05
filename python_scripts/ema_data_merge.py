# Necessary imports
import csv
import re
from io import StringIO
from collections import defaultdict
from datetime import datetime, timedelta
import pandas as pd  # Import pandas

# --- Part 1: Process the first CSV (temp.txt) to get extracted_data ---
csv_data_first_file = '../input_data/cortisolMelatonin/2025-03-30_all_ema_data.csv'
# Column names for the first file
id_col_f1 = "EcoSleep_ID"
instrument_col_f1 = "redcap_repeat_instrument"
date_col_f1 = "field_response_time_16"
melatonin_col_f1 = "field_k61io07p_9ha2qzr"
cortisol_col_f1 = "field_60subewzohiw5d6m"
target_instrument_f1 = "module_cicucg9xtdd62lml"

# Regex to find hh:mm patterns
time_regex = re.compile(r'\b(\d{1,2}:\d{2})\b')

# Dictionary to hold the extracted data from the first file
extracted_data = defaultdict(lambda: defaultdict(lambda: {'melatonin': [], 'cortisol': []}))

# Process the first file's data
with open(csv_data_first_file, mode='r', newline='', encoding='utf-8') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        if row.get(instrument_col_f1) == target_instrument_f1:
            eco_id = row.get(id_col_f1)
            date_str_full = row.get(date_col_f1, "")
            date_str = date_str_full.split('T')[0] if 'T' in date_str_full else None
            if not eco_id or not date_str: continue
            try:
                current_date_obj = datetime.strptime(date_str, '%Y-%m-%d').date()
            except ValueError:
                continue
            melatonin_times_str = row.get(melatonin_col_f1, "")
            cortisol_times_str = row.get(cortisol_col_f1, "")
            melatonin_times = time_regex.findall(melatonin_times_str)
            cortisol_times = time_regex.findall(cortisol_times_str)
            existing_melatonin = extracted_data[eco_id][current_date_obj]['melatonin']
            existing_cortisol = extracted_data[eco_id][current_date_obj]['cortisol']
            existing_melatonin.extend(melatonin_times)
            existing_cortisol.extend(cortisol_times)
            #extracted_data[eco_id][current_date_obj]['melatonin'] = sorted(list(existing_melatonin))
            #extracted_data[eco_id][current_date_obj]['cortisol'] = sorted(list(existing_cortisol))
            extracted_data[eco_id][current_date_obj]['melatonin'] = existing_melatonin
            extracted_data[eco_id][current_date_obj]['cortisol'] = existing_cortisol

# --- Part 2: Process the second file (Excel) and map times using pandas ---

# <<< --- Configuration for Excel files --- >>>
# <<< Replace with the actual path to your input Excel file >>>
excel_input_path = '../output_data/merged_file_reversed.xlsx'
# <<< Replace with the desired path for your output Excel file >>>
excel_output_path = '../output_data/output_updated_data.xlsx'
# <<< Replace with the sheet name if it's not the first one (or use None for first sheet) >>>
excel_sheet_name = 0  # 0 for the first sheet, or provide sheet name as string e.g., 'Sheet1'

# Column names expected in the Excel file
id_col_f2 = "ID"
date_col_f2 = "date"
instance_col_f2 = "instance"
target_col_f2 = "time_ema_automated"  # The column to update
time_check_column = "time_check"

# Date comparison window
date_window = timedelta(days=7)

processed_count = 0
mapped_count = 0

try:
    # Read the Excel file into a pandas DataFrame
    print(f"Reading Excel file: {excel_input_path}...")
    # Keep 'ID' as string to match keys from extracted_data
    df_f2 = pd.read_excel(excel_input_path, sheet_name=excel_sheet_name, dtype={id_col_f2: str})
    print(f"Read {len(df_f2)} rows.")

    # Ensure the target column exists, add if not (filled with None or '')
    if target_col_f2 not in df_f2.columns:
        df_f2[target_col_f2] = ''  # Or pd.NA

    if time_check_column not in df_f2.columns:
        df_f2[time_check_column] = ''  # Or pd.NA

    # Convert date column to datetime objects, coercing errors to NaT (Not a Time)
    # This helps handle various date formats Excel might have
    df_f2['date_obj'] = pd.to_datetime(df_f2[date_col_f2], errors='coerce').dt.date

    # Iterate through the DataFrame rows
    print("Processing rows and mapping times...")
    for index, row_f2 in df_f2.iterrows():
        processed_count += 1
        # Access data using column names - ensure they match your Excel file!
        # .get() is not standard for pandas rows, access directly or check for NaNs
        current_row_id = row_f2[id_col_f2] if pd.notna(row_f2[id_col_f2]) else None
        current_row_date_obj = row_f2['date_obj'] if pd.notna(row_f2['date_obj']) else None
        current_row_instance = str(row_f2[instance_col_f2]) if pd.notna(
            row_f2[instance_col_f2]) else None  # Ensure instance is string

        # Basic validation
        if not current_row_id or not current_row_date_obj or not current_row_instance:
            continue  # Skip row if essential info is missing or invalid date

        time_to_map = None  # Reset time for this row

        # Check if the ID exists in the extracted data
        # Note: IDs from CSV/text might be strings, ensure comparison works (already handled by reading Excel ID as string)
        if current_row_id in extracted_data:
            # Iterate through the dates recorded for this ID in file 1
            for source_date_obj, source_times in extracted_data[current_row_id].items():
                # Check if the date from file 2 is within +/- 7 days of the date from file 1
                if abs(current_row_date_obj - source_date_obj) <= date_window:
                    # Found a matching date window, now parse the instance
                    instance_index = -1
                    time_type = None

                    if current_row_instance.startswith('e'):
                        time_type = 'melatonin'
                        try:
                            instance_index = int(current_row_instance[1:]) - 1
                        except ValueError:
                            pass
                    elif current_row_instance.startswith('m'):
                        time_type = 'cortisol'
                        try:
                            instance_index = int(current_row_instance[1:]) - 1
                        except ValueError:
                            pass

                    # If instance parsed correctly, try to get the time
                    if time_type and instance_index >= 0:
                        source_time_list = source_times.get(time_type, [])
                        if 0 <= instance_index < len(source_time_list):
                            time_to_map = source_time_list[instance_index]
                            break  # Found the time for this row_f2, stop checking dates

        # Update the target column in the DataFrame if a time was found
        if time_to_map:
            # Use .loc to update the DataFrame value by index and column name
            df_f2.loc[index, target_col_f2] = time_to_map
            if(df_f2.loc[index, "time_ema"] != "" and pd.notna(df_f2.loc[index, "time_ema"])):
                t1 = datetime.strptime(str(df_f2.loc[index, "time_ema"]), "%H:%M:%S").time()
                t2 = datetime.strptime(str(df_f2.loc[index, target_col_f2]), "%H:%M").time()
                df_f2.loc[index, time_check_column] = (t1.hour == t2.hour) and (t1.minute == t2.minute)
            mapped_count += 1

    print(f"Processed {processed_count} rows.")
    print(f"Mapped {mapped_count} time entries.")

    # --- Part 3: Export the updated DataFrame to a new Excel file ---
    print(f"Exporting updated data to Excel file: {excel_output_path}...")
    # Drop the temporary date_obj column before exporting
    df_f2.drop(columns=['date_obj'], inplace=True)

    # Write to Excel, without including the pandas index column
    df_f2.to_excel(excel_output_path, index=False, sheet_name='Updated_Data')
    print("Export complete.")

except FileNotFoundError:
    print(f"Error: The Excel file '{excel_input_path}' was not found.")
except ImportError:
    print("Error: The 'pandas' or 'openpyxl' library is required. Please install it (pip install pandas openpyxl).")
except KeyError as e:
    print(f"Error: A required column name was not found in the Excel file: {e}")
    print("Please ensure the column names (ID, date, instance, time_ema) match your Excel file exactly.")
except Exception as e:
    print(f"An unexpected error occurred: {e}")