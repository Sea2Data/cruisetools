import requests
import xml.etree.ElementTree as ET
import pandas as pd

api = 'https://reference-api.hi.no/apis/nmdapi/reference/v2/model/cruiseseries' 
namespaces_reference = {'ns': 'http://www.imr.no/formats/nmdreference/v2.1'}
namespaces_biotic = {'ns', 'biotixmlns="http://www.imr.no/formats/nmdcommon/v2'}

# Get the cruise series
cs_r = ET.fromstring(requests.get(api+'?version=2.1').text)

cruiseseries = []

# Iterate over cruiseseries
for row in cs_r.findall('ns:row', namespaces_reference):
    code = row.find('ns:code', namespaces_reference).text
    name = row.find('ns:name', namespaces_reference).text

    # Get the sample times
    smpls = ET.fromstring(requests.get(
        api+'/'+code+'/samples?version=2.1').text)

    # Iterate over sample time
    for row in smpls.findall('ns:row', namespaces_reference):
        time = row.find('ns:sampleTime', namespaces_reference).text

        # Get the cruise numbers for this year
        cruise = ET.fromstring(requests.get(
            api+'/'+code+'/samples/'+time+'/cruises?version=2.1').text)

        # Iterate over the cruises
        for row in cruise.findall('ns:row', namespaces_reference):
            cruisenr = row.find('ns:cruisenr', namespaces_reference).text

            # Get the cruise information from the cruise api
            _ship_name = row.find('ns:shipName', namespaces_reference)
            if _ship_name is not None:
                ship_name = _ship_name.text
            else:
                ship_name = None

            # Print extracted values
            dat = {"cruise_series": name,
                   "sample_time": time,
                   "cruise_number": cruisenr,
                   "ship_name": ship_name}
            cruiseseries.append(dat)

# Display DataFrame
df = pd.DataFrame(cruiseseries)
print(df)
df.to_parquet("cruiseseries.parquet")
