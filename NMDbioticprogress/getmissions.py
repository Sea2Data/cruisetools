import requests
import xml.etree.ElementTree as ET

# Define the namespace
namespace = {"ns": "http://www.imr.no/formats/nmdcommon/v2"}

# URL to misiontypes
url = "https://biotic-api.hi.no/apis/nmdapi/biotic/v3"

# Get the missionytpes
root = ET.fromstring(requests.get(url).text)
missiontypenames = root.findall(".//ns:element[@name='missiontypename']", namespace)

# Extract and print the text of each missiontypename
for elem in missiontypenames:
    print(elem.text)

data = []
snapshots = []
# Mission type name
for  missiontypename in missiontypenames:
    # Year
    year_url = "https://biotic-api.hi.no/apis/nmdapi/biotic/v3/"+missiontypename.text
    #print(year_url)
    
    # Get the year content
    year_xml = ET.fromstring(requests.get(year_url).text)
    years = year_xml.findall(".//ns:element[@name='year']", namespace)    

    # Extract and print the text of each missiontypename
    for year in years:

        # Year
        platformpath_url = (
            'https://biotic-api.hi.no/apis/nmdapi/biotic/v3/' +
            missiontypename.text + '/' +
            year.text.replace(" ", ""))
        #print(platformpath_url)
        
        # Get the year content
        platformpath_xml = ET.fromstring(requests.get(platformpath_url).text)
        platformpaths = platformpath_xml.findall(".//ns:element[@name='platformpath']", namespace)    
        
        for platformpath in platformpaths:
            
            # Delivery
            delivery_url = (
                'https://biotic-api.hi.no/apis/nmdapi/biotic/v3/' +
                missiontypename.text + '/' +
                year.text.replace(" ", "") + '/' +
                platformpath.text)
            #print(delivery_url)
            
            # Get the delivery content
            delivery_xml = ET.fromstring(requests.get(delivery_url).text)
            deliveries = delivery_xml.findall(".//ns:element[@name='delivery']", namespace)

            for delivery in deliveries:
                # Has snapshots?
                
                # List snapshots
                snapshot_url = (
                    'https://biotic-api.hi.no/apis/nmdapi/biotic/v3/' +
                    missiontypename.text + '/' +
                    year.text.replace(" ", "") + '/' +
                    platformpath.text + '/' +
                    delivery.text + '/snapshot')
                print(snapshot_url)

                # Get the snapshots content
                try:
                    snapshot_xml = ET.fromstring(requests.get(snapshot_url).text)
                    _snapshots = snapshot_xml.findall(
                        ".//ns:element[@name='snapshot time']", namespace)
                    snapshots.append({snapshot_url : [_snapshot.text for _snapshot in _snapshots]})
                except:
                    print('Failed for :'+snapshot_xml)


# Read cruises (Forskningsfartøy)

# Check if cruise is present in biotic

# Check last day of cruise

# Check snapshot date *after* last day of cruise

# Build pd where the 
