#!/usr/bin/env python3

import base64
import requests
import time
from sys import argv
from datetime import datetime

PORT = 3000

impath = argv[1]  # "net/images/left_sample.png"
camside = argv[2]

with open(impath, "rb") as image:
    encoded = base64.b64encode(image.read())

while True:
    tpl = tuple(map(int, datetime.now().__repr__().split('e')[-1][1:-1].split(', ')))
    timestamp = ("%04d.%02d.%02d-%02d:%02d:%02d.%06d"%tpl)[:-3]
    print(timestamp)
    requests.post("http://localhost:%d/"%PORT, data=encoded,
                  headers={"camside": camside, "timestamp": timestamp})
    time.sleep(1)

