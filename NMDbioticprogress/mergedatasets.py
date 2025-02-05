# getmission.py - Read all snapshots from every mission in NMDbiotic
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

'''
>>> snapshots.keys()
Index(['missiontypename', 'year', 'platform', 'delivery', 'snapshot'], dtype='object')
>>> cruiseseries.keys()
Index(['cruise_series', 'sample_time', 'cruise_number', 'ship_name'], dtype='object')
>>> cruises.keys()
Index(['name', 'startyear', 'cruiseNumber', 'startTime', 'stopTime',
       'specificArea', 'departurePort', 'arrivalPort', 'cruiseCode',
       'cruiseType', 'cruiseStatus', 'purpose', 'platformCodes', 'datasets'],
      dtype='object')
'''

#"old_col2": "new_col2"})

snapshots_columns = {"delivery": "cruise_number", "year": "sample_time"}
cruises_columns = {"cruiseCode": "cruise_number"}

snapshots = pd.read_parquet("snapshots.parquet").rename(columns=snapshots_columns)
cruises = pd.read_parquet("cruises.parquet").rename(columns=cruises_columns)
cruiseseries = pd.read_parquet("cruiseseries.parquet")

snapshots.keys()
snapshots.head()
snapshots['cruise_number'].unique()
cruiseseries.keys()
cruiseseries.head()
cruises.keys()
cruises.head()
cruises['cruise_number'].unique()

# Merge snapshots with cruiseseries on 'cruise_number' (no snapshot in cruiseseries)
merged_df = snapshots.merge(cruiseseries, on="cruise_number", how="left")

# Merge the result with cruises on 'cruise_number' (no snapshot in cruises)
merged_df = merged_df.merge(cruises, on="cruise_number", how="left")

merged_df["startTime"] = pd.to_datetime(merged_df["startTime"],
                                        utc=True)
merged_df["stopTime"] = pd.to_datetime(merged_df["stopTime"],
                                        utc=True)
merged_df["snapshot"] = pd.to_datetime(merged_df["snapshot"],
                                       format = "%Y-%m-%dT%H.%M.%S.%fZ",
                                       utc=True)

merged_df["time_diff_days"] = (merged_df["snapshot"] - merged_df["stopTime"])
#.dt.total_seconds() / (3600*24)

# Define the cutoff date for filtering
cutoff_date = pd.Timestamp("2016-01-01", tz="UTC")  # Ensure the timezone matches

# Apply filters
'''
filtered_df = merged_df[
    (merged_df["sample_time_x"].astype('int')>2015) &
    (merged_df["startTime"] >= cutoff_date) &  # Keep startTime from 2016 onwards
    (merged_df["time_diff_days"].dt.total_seconds() >= 0)        # Remove negative time differences
]
'''

filtered_df = merged_df[
    ((merged_df["missiontypename"] == 'Forskningsfartøy') |
    (merged_df["missiontypename"] == 'Leiefartøy'))&
    (merged_df["time_diff_days"].dt.total_seconds() >= 0)
]

'''
filtered_df = merged_df[
    (merged_df["cruise_series"] == 'Barents Sea NOR-RUS demersal fish cruise in winter')
]
'''
filtered_df.keys()

merged_df["cruise_series"].unique()
filtered_df['snapshot']
filtered_df['startTime'].describe()



mintimelag = filtered_df.groupby('cruise_number')['time_diff_days'].min().reset_index()
starttime = filtered_df.groupby('cruise_number')['startTime'].median().reset_index()
time_lag = mintimelag.merge(starttime, on="cruise_number", how="left")
#time_lag = time_lag.merge(cruiseseries, on="cruise_number", how="left")

time_lag["time_diff_days"] = time_lag["time_diff_days"].dt.total_seconds() / (24 * 3600)

plt.figure(figsize=(10, 5))
plt.scatter(time_lag["startTime"],
         time_lag["time_diff_days"], marker="o")

plt.xlabel("Start Time")
plt.ylabel("Time Difference (days)")
plt.title("Start Time vs. Time Difference in Days")
plt.grid(True)

plt.show()


