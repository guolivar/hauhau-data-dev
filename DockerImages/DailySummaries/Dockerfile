# Base image centos:centos8.2.2004 plus extra packages

FROM uaqh/base:latest
# Remember to build uaqh/base

# Create the right folders
RUN mkdir -p /plots

# copy files
COPY daily_summary_HAUHAU.R /daily_summary_HAUHAU.R
COPY secret_aws.txt /secret_aws.txt
COPY secret_hauhauapikey.txt /secret_hauhauapikey.txt
COPY hauhau-devices.csv /hauhau-devices.csv
COPY summary_wikidot.html /summary_wikidot.html

# Run the R-packages
CMD Rscript daily_summary_HAUHAU.R