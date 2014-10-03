source('0_settings.R')

espa_email <- 'azvoleff@conservation.org'

###############################################################################
# All Sites
download_folder <- 'I:/Landsat_Originals'

stopifnot(file_test('-d', download_folder))

# espa_download(espa_email, '5142014-155630', download_folder) # 0-2% cover
# espa_download(espa_email, '5142014-155558', download_folder) # 2-5% cover
# espa_download(espa_email, '5142014-15558', download_folder) # 5-10% cover
# espa_download(espa_email, '5142014-153954', download_folder) # 10-20% cover
# espa_download(espa_email, '6162014-8585', download_folder) # 10-20% cover
library(stringr)
options(RCurlOptions=list(cainfo=system.file("CurlSSL", "cacert.pem", 
                                             package="RCurl")))
curl=getCurlHandle()
login_page <- unlist(strsplit(getURL('https://espa.cr.usgs.gov/login/', curl=curl), '\n'))
csrfmiddlewaretoken <- login_page[grepl("csrfmiddlewaretoken", login_page)]
csrfmiddlewaretoken <- gsub("(value=)|(')", '',
                            str_extract(csrfmiddlewaretoken, 
                                        "value='[a-zA-Z0-9]*'"))
params <- list('username'="azvoleff",
               'password'="0ZmNcTDR1ZtSs85",
               'submit'="Log In",
               'next'="",
               'csrfmiddlewaretoken'=csrfmiddlewaretoken)
post_res <- postForm('https://espa.cr.usgs.gov/login',
                     .params=params, style="POST", curl=curl)
espa_page <- getURL("http://espa.cr.usgs.gov/ordering/status/azvoleff%40conservation.org-782014-164815", curl=curl)

espa_download(espa_email, '782014-164815', download_folder) # 10-20% cover

