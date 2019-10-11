Percentage of tweets with each sentiment label for eatlancet and
yes2meat:

    load("data/eatlancet_tweets.rda")
    df <- read.csv("data/sampleTweets_Annotated.csv", colClasses=rep("character", 7))
    names(df) <- c("id", "URL", "Positive", "Negative", "Neutral", "Irrelevant", "NE")
    df <- inner_join(df, eatlancet_tweets, by=c("id"="id_str"))

    df %>% filter(NE != "X") -> df

    tabledf <- data.frame(
      Term=c("EAT-Lancet", "yes2meat"),
      Positive=c(round(sum(df$Positive[df$EATLancet]=="X")/sum(df$EATLancet)*100,2),round(sum(df$Positive[df$yes2meat]=="X")/sum(df$yes2meat)*100,2)),
      Negative=c(round(sum(df$Negative[df$EATLancet]=="X")/sum(df$EATLancet)*100,2),round(sum(df$Negative[df$yes2meat]=="X")/sum(df$yes2meat)*100,2)),
      Neutral=c(round(sum(df$Neutral[df$EATLancet]=="X")/sum(df$EATLancet)*100,2),round(sum(df$Neutral[df$yes2meat]=="X")/sum(df$yes2meat)*100,2)),
      Irrelevant=c(round(sum(df$Irrelevant[df$EATLancet]=="X")/sum(df$EATLancet)*100,2),round(sum(df$Irrelevant[df$yes2meat]=="X")/sum(df$yes2meat)*100,2)))

    kable(tabledf, caption="Sentiment by term") %>%
      kable_styling(full_width = F)

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Sentiment by term
</caption>
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:right;">
Positive
</th>
<th style="text-align:right;">
Negative
</th>
<th style="text-align:right;">
Neutral
</th>
<th style="text-align:right;">
Irrelevant
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
EAT-Lancet
</td>
<td style="text-align:right;">
28.72
</td>
<td style="text-align:right;">
31.91
</td>
<td style="text-align:right;">
38.30
</td>
<td style="text-align:right;">
1.06
</td>
</tr>
<tr>
<td style="text-align:left;">
yes2meat
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
40.98
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
55.74
</td>
</tr>
</tbody>
</table>

    sum(df$Negative[df$yes2meat]=="X")/(sum(df$Positive[df$yes2meat]=="X") + sum(df$Neutral[df$yes2meat]=="X"))
    #> [1] 12.5

Sentiment statistics per community:

    tdf <- read.csv("data/CommunitySelectedTweets_Annotated.csv", colClasses=rep("character", 6))
    names(tdf) <- c("id", "Positive", "Negative", "Neutral", "Irrelevant", "NE")

    load("data/eatlancet_tweets.rda")
    eatlancet_tweets %>% distinct(id_str, .keep_all = T) -> tweets  # We remove duplicates by tweet id
    tweets$Date <- as.Date(substr(tweets$created_at,1,10))
    #We analyze two weeks after the report was released
    tweets %>% filter(Date >= "2019-01-16" & Date <= "2019-01-31") -> tweets

    tdf <- left_join(tdf, tweets, by=c("id"="id_str"))

    load("data/communitydf.rda")
    tdf <- inner_join(tdf, communitydf, by=c("USER_id_str"="v_name"))

    tabledf <- data.frame(Sentiment=c("Positive", "Negative", "Neutral", "Irrelevant"))
    for (color in c("blue", "red", "yellow", "green"))
    {
      Pos <- sum(tdf$Positive[tdf$community==color]=="X")/sum(tdf$community==color)*100
      Neg <- sum(tdf$Negative[tdf$community==color]=="X")/sum(tdf$community==color)*100
      Neu <- sum(tdf$Neutral[tdf$community==color]=="X")/sum(tdf$community==color)*100
      Irr <- sum(tdf$Irrelevant[tdf$community==color]=="X")/sum(tdf$community==color)*100
      tabledf[[color]] <- c(Pos, Neg, Neu, Irr)
    }


    kable(tabledf, caption="Sentiment by community") %>%
      kable_styling(full_width = F)

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Sentiment by community
</caption>
<thead>
<tr>
<th style="text-align:left;">
Sentiment
</th>
<th style="text-align:right;">
blue
</th>
<th style="text-align:right;">
red
</th>
<th style="text-align:right;">
yellow
</th>
<th style="text-align:right;">
green
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Positive
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
Negative
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Neutral
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Irrelevant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
48
</td>
</tr>
</tbody>
</table>
