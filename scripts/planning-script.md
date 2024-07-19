Planning Script
================
Last Update: July 17, 2024

# Package Loading

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.1      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(forcats)
library(lazyeval)
```

    ## 
    ## Attaching package: 'lazyeval'
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     is_atomic, is_formula

``` r
library(ggpubr)
library(ggridges)
```

# Data Loading

``` r
transaction_volume <- read.csv("/Users/kenjinchang/github/multimodal-dining-study/data/parent-data/transaction-volume-082023121623.csv")
```

# Data Cleaning

``` r
transaction_volume_by_minute <- transaction_volume %>%
  select(Hour,X15.Minutes,Business.Date,Transactions) %>%
  rename(hour=Hour,interval=X15.Minutes,date=Business.Date,count=Transactions) %>%
  unite(minute_hour,c(hour,interval),sep=":",remove=FALSE) %>%
  unite(date_time,c(date,minute_hour),sep=", ", remove=FALSE)
```

adding day

``` r
transaction_volume_by_minute <- transaction_volume_by_minute %>%
  mutate(day=case_when(date=="08/21/2023"~"Monday",date=="08/22/2023"~"Tuesday",date=="08/23/2023"~"Wednesday",date=="08/24/2023"~"Thursday",date=="08/25/2023"~"Friday",date=="08/28/2023"~"Monday",date=="08/29/2023"~"Tuesday",date=="08/30/2023"~"Wednesday",date=="08/31/2023"~"Thursday",date=="09/01/2023"~"Friday",date=="09/05/2023"~"Tuesday",date=="09/06/2023"~"Wednesday",date=="09/07/2023"~"Thursday",date=="09/08/2023"~"Friday",date=="09/11/2023"~"Monday",date=="09/12/2023"~"Tuesday",date=="09/13/2023"~"Wednesday",date=="09/14/2023"~"Thursday",date=="09/15/2023"~"Friday",date=="09/18/2023"~"Monday",date=="09/19/2023"~"Tuesday",date=="09/20/2023"~"Wednesday",date=="09/21/2023"~"Thursday",date=="09/22/2023"~"Friday",date=="09/25/2023"~"Monday",date=="09/26/2023"~"Tuesday",date=="09/27/2023"~"Wednesday",date=="09/28/2023"~"Thursday",date=="09/29/2023"~"Friday",date=="10/02/2023"~"Monday",date=="10/03/2023"~"Tuesday",date=="10/04/2023"~"Wednesday",date=="10/05/2023"~"Thursday",date=="10/06/2023"~"Friday",date=="10/11/2023"~"Wednesday",date=="10/12/2023"~"Thursday",date=="10/13/2023"~"Friday",date=="10/16/2023"~"Monday",date=="10/17/2023"~"Tuesday",date=="10/18/2023"~"Wednesday",date=="10/19/2023"~"Thursday",date=="10/20/2023"~"Friday",date=="10/23/2023"~"Monday",date=="10/24/2023"~"Tuesday",date=="10/25/2023"~"Wednesday",date=="10/26/2023"~"Thursday",date=="10/27/2023"~"Friday",date=="10/30/2023"~"Monday",date=="10/31/2023"~"Tuesday",date=="11/01/2023"~"Wednesday",date=="11/02/2023"~"Thursday",date=="11/03/2023"~"Friday",date=="11/06/2023"~"Monday",date=="11/07/2023"~"Tuesday",date=="11/08/2023"~"Wednesday",date=="11/09/2023"~"Thursday",date=="11/10/2023"~"Friday",date=="11/13/2023"~"Monday",date=="11/14/2023"~"Tuesday",date=="11/15/2023"~"Wednesday",date=="11/16/2023"~"Thursday",date=="11/17/2023"~"Friday",date=="11/18/2023"~"Saturday",date=="11/20/2023"~"Monday",date=="11/21/2023"~"Tuesday",date=="11/27/2023"~"Monday",date=="11/28/2023"~"Tuesday",date=="11/29/2023"~"Wednesday",date=="11/30/2023"~"Thursday",date=="12/01/2023"~"Friday",date=="12/04/2023"~"Monday",date=="12/05/2023"~"Tuesday",date=="12/06/2023"~"Wednesday",date=="12/07/2023"~"Thursday",date=="12/08/2023"~"Friday",date=="12/11/2023"~"Monday",date=="12/12/2023"~"Tuesday",date=="12/13/2023"~"Wednesday",date=="12/14/2023"~"Thursday",date=="12/15/2023"~"Friday"))
```

abbreviate date format

``` r
transaction_volume_by_minute <- transaction_volume_by_minute %>%
  mutate(date_abbrev=case_when(date=="08/21/2023"~"08.21",date=="08/22/2023"~"08.22",date=="08/23/2023"~"08.23",date=="08/24/2023"~"08.24",date=="08/25/2023"~"08.25",date=="08/28/2023"~"08.28",date=="08/29/2023"~"08.29",date=="08/30/2023"~"08.30",date=="08/31/2023"~"08.31",date=="09/01/2023"~"09.01",date=="09/05/2023"~"09.05",date=="09/06/2023"~"09.06",date=="09/07/2023"~"09.07",date=="09/08/2023"~"09.08",date=="09/11/2023"~"09.11",date=="09/12/2023"~"09.12",date=="09/13/2023"~"09.13",date=="09/14/2023"~"09.14",date=="09/15/2023"~"09.15",date=="09/18/2023"~"09.18",date=="09/19/2023"~"09.19",date=="09/20/2023"~"09.20",date=="09/21/2023"~"09.21",date=="09/22/2023"~"09.22",date=="09/25/2023"~"09.25",date=="09/26/2023"~"09.26",date=="09/27/2023"~"09.27",date=="09/28/2023"~"09.28",date=="09/29/2023"~"09.29",date=="10/02/2023"~"10.02",date=="10/03/2023"~"10.03",date=="10/04/2023"~"10.04",date=="10/05/2023"~"10.05",date=="10/06/2023"~"10.06",date=="10/11/2023"~"10.11",date=="10/12/2023"~"10.12",date=="10/13/2023"~"10.13",date=="10/16/2023"~"10.16",date=="10/17/2023"~"10.17",date=="10/18/2023"~"10.18",date=="10/19/2023"~"10.19",date=="10/20/2023"~"10.20",date=="10/23/2023"~"10.23",date=="10/24/2023"~"10.24",date=="10/25/2023"~"10.25",date=="10/26/2023"~"10.26",date=="10/27/2023"~"10.27",date=="10/30/2023"~"10.30",date=="10/31/2023"~"10.31",date=="11/01/2023"~"11.01",date=="11/02/2023"~"11.02",date=="11/03/2023"~"11.03",date=="11/06/2023"~"11.06",date=="11/07/2023"~"11.07",date=="11/08/2023"~"11.08",date=="11/09/2023"~"11.09",date=="11/10/2023"~"11.10",date=="11/13/2023"~"11.13",date=="11/14/2023"~"11.14",date=="11/15/2023"~"11.15",date=="11/16/2023"~"11.16",date=="11/17/2023"~"11.17",date=="11/18/2023"~"11.18",date=="11/20/2023"~"11.20",date=="11/21/2023"~"11.21",date=="11/27/2023"~"11.27",date=="11/28/2023"~"11.28",date=="11/29/2023"~"11.29",date=="11/30/2023"~"11.30",date=="12/01/2023"~"12.01",date=="12/04/2023"~"12.04",date=="12/05/2023"~"12.05",date=="12/06/2023"~"12.06",date=="12/07/2023"~"12.07",date=="12/08/2023"~"12.08",date=="12/11/2023"~"12.11",date=="12/12/2023"~"12.12",date=="12/13/2023"~"12.13",date=="12/14/2023"~"12.14",date=="12/15/2023"~"12.15"))
```

remove Saturday

``` r
transaction_volume_by_minute <- transaction_volume_by_minute %>%
  filter_at(vars(day),all_vars(!. %in% "Saturday"))
```

create version that collapses across day (disregards hour and 15-minute
intervals)

``` r
transaction_volume_by_day <- transaction_volume_by_minute %>%
  group_by(date) %>%
  summarise(sum(count)) %>%
  rename(count="sum(count)") %>%
  mutate(day=case_when(date=="08/21/2023"~"Monday",date=="08/22/2023"~"Tuesday",date=="08/23/2023"~"Wednesday",date=="08/24/2023"~"Thursday",date=="08/25/2023"~"Friday",date=="08/28/2023"~"Monday",date=="08/29/2023"~"Tuesday",date=="08/30/2023"~"Wednesday",date=="08/31/2023"~"Thursday",date=="09/01/2023"~"Friday",date=="09/05/2023"~"Tuesday",date=="09/06/2023"~"Wednesday",date=="09/07/2023"~"Thursday",date=="09/08/2023"~"Friday",date=="09/11/2023"~"Monday",date=="09/12/2023"~"Tuesday",date=="09/13/2023"~"Wednesday",date=="09/14/2023"~"Thursday",date=="09/15/2023"~"Friday",date=="09/18/2023"~"Monday",date=="09/19/2023"~"Tuesday",date=="09/20/2023"~"Wednesday",date=="09/21/2023"~"Thursday",date=="09/22/2023"~"Friday",date=="09/25/2023"~"Monday",date=="09/26/2023"~"Tuesday",date=="09/27/2023"~"Wednesday",date=="09/28/2023"~"Thursday",date=="09/29/2023"~"Friday",date=="10/02/2023"~"Monday",date=="10/03/2023"~"Tuesday",date=="10/04/2023"~"Wednesday",date=="10/05/2023"~"Thursday",date=="10/06/2023"~"Friday",date=="10/11/2023"~"Wednesday",date=="10/12/2023"~"Thursday",date=="10/13/2023"~"Friday",date=="10/16/2023"~"Monday",date=="10/17/2023"~"Tuesday",date=="10/18/2023"~"Wednesday",date=="10/19/2023"~"Thursday",date=="10/20/2023"~"Friday",date=="10/23/2023"~"Monday",date=="10/24/2023"~"Tuesday",date=="10/25/2023"~"Wednesday",date=="10/26/2023"~"Thursday",date=="10/27/2023"~"Friday",date=="10/30/2023"~"Monday",date=="10/31/2023"~"Tuesday",date=="11/01/2023"~"Wednesday",date=="11/02/2023"~"Thursday",date=="11/03/2023"~"Friday",date=="11/06/2023"~"Monday",date=="11/07/2023"~"Tuesday",date=="11/08/2023"~"Wednesday",date=="11/09/2023"~"Thursday",date=="11/10/2023"~"Friday",date=="11/13/2023"~"Monday",date=="11/14/2023"~"Tuesday",date=="11/15/2023"~"Wednesday",date=="11/16/2023"~"Thursday",date=="11/17/2023"~"Friday",date=="11/18/2023"~"Saturday",date=="11/20/2023"~"Monday",date=="11/21/2023"~"Tuesday",date=="11/27/2023"~"Monday",date=="11/28/2023"~"Tuesday",date=="11/29/2023"~"Wednesday",date=="11/30/2023"~"Thursday",date=="12/01/2023"~"Friday",date=="12/04/2023"~"Monday",date=="12/05/2023"~"Tuesday",date=="12/06/2023"~"Wednesday",date=="12/07/2023"~"Thursday",date=="12/08/2023"~"Friday",date=="12/11/2023"~"Monday",date=="12/12/2023"~"Tuesday",date=="12/13/2023"~"Wednesday",date=="12/14/2023"~"Thursday",date=="12/15/2023"~"Friday")) %>%
  mutate(date_abbrev=case_when(date=="08/21/2023"~"08.21",date=="08/22/2023"~"08.22",date=="08/23/2023"~"08.23",date=="08/24/2023"~"08.24",date=="08/25/2023"~"08.25",date=="08/28/2023"~"08.28",date=="08/29/2023"~"08.29",date=="08/30/2023"~"08.30",date=="08/31/2023"~"08.31",date=="09/01/2023"~"09.01",date=="09/05/2023"~"09.05",date=="09/06/2023"~"09.06",date=="09/07/2023"~"09.07",date=="09/08/2023"~"09.08",date=="09/11/2023"~"09.11",date=="09/12/2023"~"09.12",date=="09/13/2023"~"09.13",date=="09/14/2023"~"09.14",date=="09/15/2023"~"09.15",date=="09/18/2023"~"09.18",date=="09/19/2023"~"09.19",date=="09/20/2023"~"09.20",date=="09/21/2023"~"09.21",date=="09/22/2023"~"09.22",date=="09/25/2023"~"09.25",date=="09/26/2023"~"09.26",date=="09/27/2023"~"09.27",date=="09/28/2023"~"09.28",date=="09/29/2023"~"09.29",date=="10/02/2023"~"10.02",date=="10/03/2023"~"10.03",date=="10/04/2023"~"10.04",date=="10/05/2023"~"10.05",date=="10/06/2023"~"10.06",date=="10/11/2023"~"10.11",date=="10/12/2023"~"10.12",date=="10/13/2023"~"10.13",date=="10/16/2023"~"10.16",date=="10/17/2023"~"10.17",date=="10/18/2023"~"10.18",date=="10/19/2023"~"10.19",date=="10/20/2023"~"10.20",date=="10/23/2023"~"10.23",date=="10/24/2023"~"10.24",date=="10/25/2023"~"10.25",date=="10/26/2023"~"10.26",date=="10/27/2023"~"10.27",date=="10/30/2023"~"10.30",date=="10/31/2023"~"10.31",date=="11/01/2023"~"11.01",date=="11/02/2023"~"11.02",date=="11/03/2023"~"11.03",date=="11/06/2023"~"11.06",date=="11/07/2023"~"11.07",date=="11/08/2023"~"11.08",date=="11/09/2023"~"11.09",date=="11/10/2023"~"11.10",date=="11/13/2023"~"11.13",date=="11/14/2023"~"11.14",date=="11/15/2023"~"11.15",date=="11/16/2023"~"11.16",date=="11/17/2023"~"11.17",date=="11/18/2023"~"11.18",date=="11/20/2023"~"11.20",date=="11/21/2023"~"11.21",date=="11/27/2023"~"11.27",date=="11/28/2023"~"11.28",date=="11/29/2023"~"11.29",date=="11/30/2023"~"11.30",date=="12/01/2023"~"12.01",date=="12/04/2023"~"12.04",date=="12/05/2023"~"12.05",date=="12/06/2023"~"12.06",date=="12/07/2023"~"12.07",date=="12/08/2023"~"12.08",date=="12/11/2023"~"12.11",date=="12/12/2023"~"12.12",date=="12/13/2023"~"12.13",date=="12/14/2023"~"12.14",date=="12/15/2023"~"12.15"))
```

# Data Analysis

``` r
transaction_volume_by_minute %>%
  group_by(date) %>%
  summarise(sum(count)) 
```

    ## # A tibble: 79 × 2
    ##    date       `sum(count)`
    ##    <chr>             <int>
    ##  1 08/21/2023         2141
    ##  2 08/22/2023         1971
    ##  3 08/23/2023         2129
    ##  4 08/24/2023         2048
    ##  5 08/25/2023         1494
    ##  6 08/28/2023         2123
    ##  7 08/29/2023         2184
    ##  8 08/30/2023         2222
    ##  9 08/31/2023         2147
    ## 10 09/01/2023         1461
    ## # … with 69 more rows

``` r
volume_by_date <- transaction_volume_by_day %>%
  mutate(day=fct_relevel(day,"Monday","Tuesday","Wednesday","Thursday","Friday")) %>%
  ggplot(aes(x=date_abbrev,y=count,fill=day)) + 
  geom_col(color="black",linewidth=0.1,alpha=0.8) +
  xlab("Date") + 
  ylab("Transaction Volume") + 
  scale_fill_brewer(palette="Set2",name="") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.position="right",legend.justification="center",legend.box.spacing=unit(10,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
transaction_volume_by_day %>%
  group_by(day) %>%
  summarise(sum(count),mean(count),sd(count)) %>%
  rename(sum="sum(count)",mean="mean(count)",sd="sd(count)")
```

    ## # A tibble: 5 × 4
    ##   day         sum  mean    sd
    ##   <chr>     <int> <dbl> <dbl>
    ## 1 Friday    19363 1210.  359.
    ## 2 Monday    27327 1822.  319.
    ## 3 Thursday  28871 1804.  411.
    ## 4 Tuesday   28782 1799.  494.
    ## 5 Wednesday 30312 1894.  444.

``` r
transaction_volume_by_day %>%
  group_by(day) %>%
  summarise(sum(count),mean(count),sd(count)) %>%
  rename(sum="sum(count)",mean="mean(count)",sd="sd(count)") %>%
  summarise(mean(mean))
```

    ## # A tibble: 1 × 1
    ##   `mean(mean)`
    ##          <dbl>
    ## 1        1706.

``` r
mean_by_day <- transaction_volume_by_day %>%
  group_by(day) %>%
  summarise(sum(count),mean(count),sd(count)) %>% 
  rename(sum="sum(count)",mean="mean(count)",sd="sd(count)") %>%
  mutate(day=fct_relevel(day,"Monday","Tuesday","Wednesday","Thursday","Friday")) %>%
  ggplot(aes(x=day,y=mean,fill=day,color=day)) + 
  geom_col(color="black",linewidth=0.1,alpha=0.8) + 
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.1,size=0.5) +
  xlab("Day") + 
  ylab("Transaction Volume") + 
  scale_fill_brewer(palette="Set2",name="Day") +
  scale_color_brewer(palette="Set2",name="Day") +
  geom_hline(yintercept=1705.96,linetype="dashed",size=0.3) +
  theme(legend.position="none",legend.justification="center",legend.box.spacing=unit(10,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.

``` r
distribution_by_day <- transaction_volume_by_day %>%
  mutate(day=fct_relevel(day,"Monday","Tuesday","Wednesday","Thursday","Friday")) %>%
  ggplot(aes(y=count,x=day,fill=day,color=day)) + 
  geom_violin(draw_quantiles=0.5,adjust=1,alpha=0.8,size=0.5) + 
  scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette="Set2") + 
  geom_hline(yintercept=1705.96,linetype="dashed",size=0.3) +
  xlab("Day") + 
  ylab("Transaction Volume") + 
  stat_summary(fun.y=mean,geom="point",shape=20,size=3,color="black",fill="white") +
  theme(legend.position="none",legend.justification="right",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

    ## Warning: The `fun.y` argument of `stat_summary()` is deprecated as of ggplot2 3.3.0.
    ## ℹ Please use the `fun` argument instead.

``` r
density_by_day <- transaction_volume_by_day %>%
  mutate(day=fct_relevel(day,"Monday","Tuesday","Wednesday","Thursday","Friday")) %>%
  ggplot(aes(x=count,y=day,color=day)) +
  stat_density_ridges(aes(fill=day),alpha=0.6,linewidth=0.5,scale=2,quantile_lines=TRUE,quantiles=c(0.025,0.975)) +
  scale_fill_brewer(palette="Set2") +
  scale_color_brewer(palette="Set2") +
  geom_vline(xintercept=1705.96,linetype="dashed",size=0.3) +
  scale_y_discrete(limits=rev) +
  xlab("Transaction Volume") + 
  ylab("Day") + 
  theme(legend.position="none",legend.justification="right",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
ggarrange(volume_by_date,
          ggarrange(mean_by_day,distribution_by_day,density_by_day,ncol=3,labels=c("B","C","D")),
          nrow=2, 
          labels="A",
          common.legend=TRUE,
          legend="bottom") 
```

    ## Picking joint bandwidth of 93.3

![](planning-script_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
sf1 <- ggarrange(volume_by_date,
          ggarrange(mean_by_day,distribution_by_day,density_by_day,ncol=3,labels=c("B","C","D")),
          nrow=2, 
          labels="A",
          common.legend=TRUE,
          legend="bottom") 
```

    ## Picking joint bandwidth of 93.3

``` r
ggsave("sf1.png",path="/Users/kenjinchang/github/multimodal-dining-study/figures/sf",plot=sf1,width=30,height=20,units="cm",dpi=150)
```

insert holidays?

``` r
transaction_volume_by_minute %>%
  group_by(hour) %>%
  summarise(sum(count))
```

    ## # A tibble: 10 × 2
    ##     hour `sum(count)`
    ##    <int>        <int>
    ##  1     6            2
    ##  2     7           44
    ##  3     8         6810
    ##  4     9         9052
    ##  5    10         7196
    ##  6    11        32813
    ##  7    12        36453
    ##  8    13        28962
    ##  9    14        13026
    ## 10    15          297

``` r
transaction_volume_by_minute %>%
  group_by(minute_hour) %>%
  summarise(sum(count))
```

    ## # A tibble: 35 × 2
    ##    minute_hour `sum(count)`
    ##    <chr>              <int>
    ##  1 10:00-14            3690
    ##  2 10:15-29            1904
    ##  3 10:30-44             580
    ##  4 10:45-59            1022
    ##  5 11:00-14            8022
    ##  6 11:15-29            7485
    ##  7 11:30-44            9757
    ##  8 11:45-59            7549
    ##  9 12:00-14            9406
    ## 10 12:15-29           11132
    ## # … with 25 more rows

``` r
transaction_volume_by_minute %>%
  group_by(date_time) %>%
  summarise(sum(count))
```

    ## # A tibble: 2,262 × 2
    ##    date_time            `sum(count)`
    ##    <chr>                       <int>
    ##  1 08/21/2023, 10:00-14           47
    ##  2 08/21/2023, 10:15-29           35
    ##  3 08/21/2023, 10:30-44           18
    ##  4 08/21/2023, 10:45-59           22
    ##  5 08/21/2023, 11:00-14          132
    ##  6 08/21/2023, 11:15-29          126
    ##  7 08/21/2023, 11:30-44          129
    ##  8 08/21/2023, 11:45-59          126
    ##  9 08/21/2023, 12:00-14          171
    ## 10 08/21/2023, 12:15-29          182
    ## # … with 2,252 more rows

``` r
write.csv(transaction_volume,"~/github/multimodal-dining-study/data/output/historical-transaction-volume.csv")
```

write.csv(university_impact_model,“~/github/university-impact-model/data/model-output/university-impact-model.csv”)
