######################
# REQUIREMENTS       #
######################
if(FALSE %in% 
   do.call(c,lapply(c("tidyverse", "rvest"), function(pkg){
     if(! require(pkg, character.only = TRUE)) install.packages(pkg, depend = TRUE)
     library(pkg, character.only = TRUE, logical.return = TRUE, quietly = TRUE, warn.conflicts = FALSE)
   }))
) stop("Cannot resolve some dependencies; exiting now...")


library(rvest)
grdplayers.home <- read_html("http://www.gdrplayers.it/blog/")

gdrplayers.posts <- grdplayers.home %>% html_nodes(".post-excerpt")
gdrplayers.posts.links <- gdrplayers.posts %>% html_node("h3 > a") %>% html_attr("href")
gdrplayers.posts.location <- gdrplayers.posts %>% html_node("h3 > a") %>% html_attr("href")
