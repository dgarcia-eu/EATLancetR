    Sys.setlocale("LC_ALL", 'en_US.UTF-8')
    library(dplyr)
    library(knitr)
    library(kableExtra)
    library(ggplot2)
    library(wordcloud)
    library(tm)

    knitr::opts_chunk$set(
      echo = FALSE,
      message = FALSE,
      warning = FALSE,
      collapse = TRUE,
      comment = "#>"
    )

    load("data/biofreqs.rda")

    wordcloud(words=biofreqs$words[biofreqs$community=="blue"], freq = biofreqs$freq[biofreqs$community=="blue"], min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors="blue")

![](02_Network_files/figure-markdown_strict/WC1-1.png)

    wordcloud(words=biofreqs$words[biofreqs$community=="red"], freq = biofreqs$freq[biofreqs$community=="red"], min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors="red")

![](02_Network_files/figure-markdown_strict/WC2-1.png)

    wordcloud(words=biofreqs$words[biofreqs$community=="yellow"], freq = biofreqs$freq[biofreqs$community=="yellow"], min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors="darkorange")

![](02_Network_files/figure-markdown_strict/WC3-1.png)

    wordcloud(words=biofreqs$words[biofreqs$community=="green"], freq = biofreqs$freq[biofreqs$community=="green"], min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors="darkgreen")

![](02_Network_files/figure-markdown_strict/WC4-1.png)
