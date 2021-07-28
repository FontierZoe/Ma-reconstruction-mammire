import os
import csv
import http
import logging
import time

import pandas as pd
import requests


logging.basicConfig(level=logging.INFO)

TIME_SLEEP = 0.1  # TODO: change this if need be
PATH_TO_INPUT_FILE = os.path.join(os.environ["HOME"], "Downloads", "pairs_400.csv")    # TODO: change this if need be
PATH_TO_OUTPUT_FILE = os.path.join(os.environ["HOME"], "Downloads", "results_400.csv") # TODO: change this if need be


def main():
    # Read candidate pairs
    df = pd.read_csv(
        PATH_TO_INPUT_FILE,
        dtype=dict(finess_geo=str, code_commune=str)
    )
    df = df.drop_duplicates(subset=["finess_geo", "code_commune"])

    # Open result csv in append mode
    with open(PATH_TO_OUTPUT_FILE, "a") as csv_file:
        writer = csv.writer(csv_file, delimiter=",", quoting=csv.QUOTE_ALL)
        
        start_index = 650000 # TODO: change start index
	
        for i, row in df.iloc[start_index:].iterrows():
            res = requests.get(
                url="https://openrouteservice.curie.net/ors/v2/directions/driving-car",  # Only at Curie with ethernet
                params=dict(
                    start="{0},{1}".format(row["longitude_com"],
                                           row["latitude_com"]),
                    end="{0},{1}".format(row["longitude_fi"],
                                         row["latitude_fi"])
                )
            )

            if res.status_code == http.HTTPStatus.OK:
                res_json = res.json()
                summary = res_json["features"][0]["properties"]["summary"]

                distance = summary.get("distance")
                duration = summary.get("duration")

                logging.info("{0}/{1} {2} -> {3} OK"
                             .format(i+1, 
                                     len(df), 
                                     row["code_commune"], 
                                     row["finess_geo"]))

                result = (row["code_commune"], 
                          row["finess_geo"], 
                          distance / 1000, 
                          duration / 60)
            else:
                logging.info("{0}/{1} {2} -> {3} Error"
                             .format(i+1, 
                                     len(df), 
                                     row["code_commune"], 
                                     row["finess_geo"]))

                result = (row["code_commune"],
                          row["finess_geo"],
                          None,
                          None)
                
            writer.writerow(result)
            time.sleep(TIME_SLEEP)


if __name__ == "__main__":
    main()
