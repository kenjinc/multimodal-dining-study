Cleaning Script
================

# Cleaning Script

## Package Loading

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggpubr)
```

## Sales Data (10.16-12.06)

``` r
sales_data <- read.csv("/Users/kenjinchang/github/multimodal-dining-study/data/parent-data/sales-data.csv")
```

In the interest of avoiding double counting downstream, we will first
remove the rows that represent aggregate groups of items from the
dataset.

To accomplish this, we create the following string

``` r
aggregates=c("Total","Unit Prepared","Grill","Asian","Breakfast","Italian","Mexican","Soup","Salad Bar","Beverages","Grab N Go","Open Misc","Salads","Convenience","Beverages","Dairy","NutritionalSnack","Food","Salty Snacks","Baked Goods","Ice Cream","Grocery","Candy Confection","Int Commissary","Straight Bakery","Take Us Home","Ext Commissary","Sushi","York Street Comm")
sales_data <- sales_data %>%
  filter(!(item %in% aggregates))
```

Next, we will create a variable pairing each item with the corresponding
station it is sold at.

``` r
sales_data <- sales_data %>%
  mutate("station"=case_when(item=="Grilled Hamburger"~"grill",
                             item=="Fried Chicken Tenders"~"grill",
                             item=="French Fries"~"grill",
                             item=="Trillium Grill Impossible Burger"~"grill",
                             item=="Grilled Chicken Breast Sandwich"~"grill",
                             item=="Seared Salmon Burger"~"grill",
                             item=="Black Bean Burger"~"grill",
                             item=="Sweet Potato Fries"~"grill",
                             item=="Add Sausage 2 Patty"~"grill",
                             item=="ADD Beef Patty $2.99"~"grill",
                             item=="ADD Cheese"~"grill",
                             item=="Add Egg $.99"~"grill",
                             item=="Bowl Ramen Tofu"~"ramen",
                             item=="Bowl Ramen Chicken"~"ramen",
                             item=="Quesadilla Deluxe Trillium"~"quesadilla",
                             item=="Burrito Una Mano Trillium BYO"~"quesadilla",
                             item=="Quesadilla Cheese"~"quesadilla",
                             item=="1 Entree + 1 Side"~"wok",
                             item=="1 Entree + 2 Side"~"wok",
                             item=="2 Entrees + 2 Sides"~"wok",
                             item=="1 Wok Entree"~"wok",
                             item=="Side Vegetarian Lo Mein"~"wok",
                             item=="Side Vegetables"~"wok",
                             item=="Side Vegetable Spring Rolls"~"wok",
                             item=="Side Vegetarian Fried Rice with"~"wok",
                             item=="Side White or Brown Rice"~"wok"
                             )) 
```
