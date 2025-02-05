import requests
import xml.etree.ElementTree as ET
import pandas as pd


def get_text(element, tag, namespace):
    found = element.find(f"nmd:{tag}", namespace)
    return found.text if found is not None else None


def parse_cruise(cruise_root, namespace):
    cruise_data = {
        "name": get_text(cruise_root, "name", namespace),
        "startyear": get_text(cruise_root, "startyear", namespace),
        "cruiseNumber": get_text(cruise_root, "cruiseNumber", namespace),
        "startTime": get_text(cruise_root, "startTime", namespace),
        "stopTime": get_text(cruise_root, "stopTime", namespace),
        "specificArea": get_text(cruise_root, "specificArea", namespace),
        "departurePort": get_text(cruise_root, "departurePort", namespace),
        "arrivalPort": get_text(cruise_root, "arrivalPort", namespace),
        "cruiseCode": get_text(cruise_root, "cruiseCode", namespace),
        "cruiseType": get_text(cruise_root, "cruiseType", namespace),
        "cruiseStatus": get_text(cruise_root, "cruiseStatus", namespace),
    }

    # Parse purpose
    purpose_elem = root.find("nmd:purpose/nmd:purpose/nmd:purpose", namespace)
    cruise_data["purpose"] = purpose_elem.text if purpose_elem is not None else None

    # Parse platform codes
    cruise_data["platformCodes"] = [
        {
            "platformCode": get_text(platform, "platformCode", namespace),
            "sysCode": get_text(platform, "sysCode", namespace),
            "sysName": get_text(platform, "sysName", namespace),
        }
        for platform in root.findall("nmd:platformCodes/nmd:platformCode", namespace)
    ]

    # Parse datasets
    cruise_data["datasets"] = [
        {
            "dataTypeCode": get_text(dataset, "dataTypeCode", namespace),
            "dataType": get_text(dataset, "dataType", namespace),
            "collectedCode": get_text(dataset, "collectedCode", namespace),
            "collected": get_text(dataset, "collected", namespace),
        }
        for dataset in root.findall("nmd:datasets/nmd:dataset", namespace)
    ]
    return cruise_data


# Cruise api url
cruise_api = 'https://cruise-api.hi.no/apis/nmdapi/cruise/v2/'

# Define the namespace from the XML response
namespace = {"nmd": "http://www.imr.no/formats/nmdcommon/v2"}
namespace_cruise = {"nmd": "http://www.imr.no/formats/nmdcruise/v2.0"}

# Parse the XML
root = ET.fromstring(requests.get(cruise_api + "?version=2_0").text)

# Mission types
mission_types = [
    elem.text for elem in root.findall(
        ".//nmd:element[@name='missiontype']", namespace)]

# Initialize
cruise_data = []

# Extract elements
for mission_type in mission_types:
    print(mission_type)
    # /{missiontype}
    root2 = ET.fromstring(
        requests.get(
            cruise_api + '/' + mission_type + "?version=2_0").text)

    # Years with data for this mission type
    years = [elem.text for elem in root2.findall(
        ".//nmd:element[@name='year']", namespace)]

    for year in years:
        # /{missiontype}/{year}
        root3 = ET.fromstring(
            requests.get(
                cruise_api + '/' +
                mission_type + '/' +
                year.replace(" ", "") + "?version=2_0").text)

        # Platform paths for this year and missiontype
        paths = [elem.text for elem in root3.findall(
            ".//nmd:element[@name='path']", namespace)]

        for path in paths:
            # /{missiontype}/{year}/{platformpath}
            root4 = ET.fromstring(
                requests.get(
                    cruise_api + '/' +
                    mission_type + '/' +
                    year.replace(" ", "") + '/' +
                    path +
                    "?version=2_0").text)
            # Cruises for this platformpath, year and missiontype
            cruises = [elem.text for elem in root4.findall(
                ".//nmd:element[@name='delivery']", namespace)]

            for cruise in cruises:
                cruise_url = (
                    cruise_api + '/' +
                    mission_type + '/' +
                    year.replace(" ", "") + '/' +
                    path + '/' +
                    cruise +
                    "/dataset?version=2.0"
                )
                print(cruise_url)
                cruise_request = requests.get(cruise_url)
                cruise_root = ET.fromstring(cruise_request.text)
                cruise_data.append(parse_cruise(cruise_root, namespace_cruise))

# Save to parquet
df = pd.DataFrame(cruise_data)
print(df)
df.to_parquet("cruises.parquet")
