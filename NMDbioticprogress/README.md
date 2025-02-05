# NMDbioticprogress

Python scripts to access the NMD api for NMDbiotic and NMDreference and list the snapshots and serial numbers available in NMDbiotic.


## Read all snapshots from biotic

getmission.py - Read all snapshots from every mission in NMDbiotic

df.to_parquet("snapshots.parquet")

## Read cruises series

cruiseseries.py - Read all cruise series and cruises

df.to_parquet("cruiseseries.parquet")

## Read cruises

cruises.py - Read all cruise information

df.to_parquet("cruises.parquet")

## Combine and analyze

mergedatasets.py - combine the datasets in a pd
