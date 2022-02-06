#!/bin/bash

# re-tag latest built image to GCR-friendly naming convention
docker tag \
    site-nginx:latest \
    gcr.io/fir-sandbox-326008/site-nginx:latest

docker push gcr.io/fir-sandbox-326008/site-nginx:latest

gcloud run deploy \
    --image gcr.io/fir-sandbox-326008/site-nginx:latest \
    --platform managed
