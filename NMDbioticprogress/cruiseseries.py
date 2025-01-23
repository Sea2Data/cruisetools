import requests
import xml.etree.ElementTree as ET

api = 'https://reference-api.hi.no/apis/nmdapi/reference/v2/model/cruiseseries' 
namespaces_reference = {'ns': 'http://www.imr.no/formats/nmdreference/v2.1'}
namespaces_biotic = {'ns', 'biotixmlns="http://www.imr.no/formats/nmdcommon/v2'}

# Get the cruise series
cs_r = ET.fromstring(requests.get(api+'?version=2.1').text)

# Iterate over cruiseseries
for row in cs_r.findall('ns:row', namespaces_reference)[1:2]:
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
            platform = row.find('ns:shipName', namespaces_reference).text

            # Print extracted values
            print("Cruise Series:", name)
            print("sampleTime:", time)
            print("Cruise Number:", cruisenr)
            print("Ship Name:", ship_name)

#url = "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/Forskningsfartøy/2017/G.O.Sars_LMEL/2017150/snapshot?version=3.0'
cruise = '2019103'
platform = 'G.O.Sars_LMEL'
year = '2019'
url = 'http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/Forskningsfartøy/'+year+'/'+platform+'/'+cruisenr+'/snapshot?version=3.0'
nils = requests.get(url).text
