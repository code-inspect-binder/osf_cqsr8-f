import os
import urllib.request
import urllib.error
import urllib.parse
from socket import timeout
import requests
import glob


# Modify input directory and output file here:
input_gpx_directory = "D:/daten/fcd/2019-02_DelayCalc/tracks/20190508/withouttimezone"
output_csv_filename = "d:/tmp/output8.csv"


class GraphiumStopDetectionApi:

    def __init__(self):
        pass

    def process_post_call(self, url, file):
        try:
            with open(file, 'r') as f:
                api_response = requests.post(url, files={'file': f})
        except urllib.error.HTTPError as e:
            self.report_error("HTTP error", True)
            return {"error": {"msg": "HTTP error: " + e.code}}
        except urllib.error.URLError as e:
            self.report_error("Network error", True)
            return {"error": {"msg": "Network error: " + e.reason.args[1]}}
        except timeout:
            self.report_error("Timeout", True)
            return {"error": {"msg": "Timeout"}}

        if api_response.status_code == 200:
            return api_response
        else:
            return {"error": {"msg": api_response.reason}}

    @staticmethod
    def report_error(message, fatal_error=False):
        print(('Fatal error: ' if fatal_error else 'Error: ') + message)

    @staticmethod
    def report_info(message):
        print('Info: ' + message)

    def do_stop_detection(self, graphium_server_url, track, graph_high_level, graph_low_level=None):
        if graph_low_level:
            url = graphium_server_url + '/graphium/detectstops?graphHighLevel=' + graph_high_level + \
                  "&graphLowLevel=" + graph_low_level
        else:
            url = graphium_server_url + '/graphium/graphs/' + graph_high_level + '/detectstops'

        return self.process_post_call(url, track)


api = GraphiumStopDetectionApi()

if os.path.isdir(input_gpx_directory) is False:
    print("Warning: Directory '" + input_gpx_directory + "' does not exist. Use another input directory!")
    exit()

# Create new output file
try:
    csv_file = open(output_csv_filename, "x")
    csv_file.close()
except FileExistsError as e:
    print("Warning: File '" + output_csv_filename + "' already exists. Use another output file!")
    exit()

header = False
for gpx_file in glob.glob(input_gpx_directory + "/*.gpx"):
    print("Processing... " + str(gpx_file))
    response = api.do_stop_detection('http://localhost:7474', gpx_file, 'osm_at_highlevel', 'osm_at_lowlevel')
    if type(response) is not dict:
        csv_file = open(output_csv_filename, "a")
        if header is False:
            csv_file.write(response.text)
            header = True
        else:
            csv_file.write(response.text.split("\n", 1)[1])
        csv_file.close()
    else:
        print("Could not process GPX file (" + response['error']['msg'] + ")")
