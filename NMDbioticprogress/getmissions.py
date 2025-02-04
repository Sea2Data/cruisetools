import requests
import xml.etree.ElementTree as ET
import pandas as pd

# Define the namespace
namespace = {"ns": "http://www.imr.no/formats/nmdcommon/v2"}

# URL to misiontypes
url = "https://biotic-api.hi.no/apis/nmdapi/biotic/v3"

# Get the missionytpes
root = ET.fromstring(requests.get(url).text)
missiontypenames = root.findall(".//ns:element[@name='missiontypename']",
                                namespace)

# Initialize variables
snapshots = []
#_missiontypenames = [missiontypenames[1], missiontypenames[17]]

for missiontypename in _missiontypenames:
    print(missiontypename.text)
    
# Mission type name
for  missiontypename in missiontypenames:
    # Get the years from (missiontype)
    years_url = (
        "https://biotic-api.hi.no/apis/nmdapi/biotic/v3/"+
        missiontypename.text
        )
    years_xml = ET.fromstring(requests.get(years_url).text)
    years = years_xml.findall(".//ns:element[@name='year']", namespace)    

    for year in years:
        # Get the platforms from (year, years and missiontype)
        platformpath_url = (
            'https://biotic-api.hi.no/apis/nmdapi/biotic/v3/' +
            missiontypename.text + '/' +
            year.text.replace(" ", ""))
        platformpath_xml = ET.fromstring(requests.get(platformpath_url).text)
        platformpaths = platformpath_xml.findall(".//ns:element[@name='platformpath']", namespace)    
        
        for platformpath in platformpaths:
            # Get the delivery from (platform, platforms, year, years and missiontype)
            delivery_url = (
                'https://biotic-api.hi.no/apis/nmdapi/biotic/v3/' +
                missiontypename.text + '/' +
                year.text.replace(" ", "") + '/' +
                platformpath.text)
            delivery_xml = ET.fromstring(requests.get(delivery_url).text)
            deliveries = delivery_xml.findall(".//ns:element[@name='delivery']", namespace)

            for delivery in deliveries:
                # Get the platform from (year, years and missiontype)
                # Has snapshots?
                
                # List snapshots
                snapshot_url = (
                    'https://biotic-api.hi.no/apis/nmdapi/biotic/v3/' +
                    missiontypename.text + '/' +
                    year.text.replace(" ", "") + '/' +
                    platformpath.text + '/' +
                    delivery.text + '/snapshot')
                snapshot_reponse = requests.get(snapshot_url)
                if snapshot_reponse.status_code == 200:
                    snapshot_xml = ET.fromstring(requests.get(snapshot_url).text)
                    _snapshots = snapshot_xml.findall(
                        ".//ns:element[@name='snapshot time']", namespace)
                    for _snapshot in _snapshots:
                        dat = {'missiontypename': missiontypename.text,
                               'year': year.text.replace(" ", ""),
                               'platform': platformpath.text,
                               'delivery': delivery.text,
                               'snapshot': _snapshot.text}
                        snapshots.append(dat)

# Display DataFrame
df = pd.DataFrame(snapshots)
print(df)
df.to_parquet("snapshots.parquet")
