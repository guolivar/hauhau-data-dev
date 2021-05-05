# Ugly but useful QR code generator for HauHau Daily summary pages
# Based on info found here:
# https://www.linux-magazine.com/Online/Features/Generating-QR-Codes-in-Linux
library(readr)

# Read the list of web addresses
web_addresses <- read_csv(paste0("./wikidot_pages.txt"),
                          col_names = FALSE)$X1

for (web_address in web_addresses){
  # Extract number of the HauHau
  hauhau_nr <- unlist(strsplit(web_address,":"))[3]
  qr_code_file <- paste0("qr-code-",
                         hauhau_nr,
                         ".png")
  qrencode_options <- "-s 2 -m 2 -l M -o"
  system(paste("qrencode",
                qrencode_options,
                qr_code_file,
               web_address))
}