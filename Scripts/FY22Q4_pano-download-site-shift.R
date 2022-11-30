# PROJECT:  groundhog_day
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  download all Site Shift datasets
# LICENSE:  MIT
# DATE:     2022-11-30
# UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(gagglr)
  library(grabr)

# GLOBAL VARIABLES --------------------------------------------------------

  #store Pano creds (first time only)
  # set_pano()
  
  #establish session
  sess <- pano_session(username = pano_user(), 
                       password = pano_pwd())


# MUNGE -------------------------------------------------------------------

  #downloads address - "https://pepfar-panorama.org/forms/downloads/"
  #pull paths on Pano to Site shift datasets
  paths <- pano_extract("mer", unpack = TRUE) %>% 
    filter(str_detect(item, "SITE_IM_SHIFT")) %>% 
    pull()

  #download all the datasets
  walk(paths,
       ~pano_download(item_url = .x,
                      session = sess,
                      dest = si_path()
                      ))