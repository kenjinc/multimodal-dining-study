Cleaning Script
================

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
aggregates=c("Total","Unit Prepared","Grill","Asian","Breakfast","Italian","Mexican","Soup","Salad Bar","Beverages","Grab N Go","Open Misc","Salads","Convenience","Beverages","Dairy","NutritionalSnack","Food","Salty Snacks","Baked Goods","Ice Cream","Grocery","Candy Confection","Int Commissary","Commissary","Straight Bakery","Take Us Home","Ext Commissary","Sushi","York Street Comm","GF Commissary")
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
                             item=="ADD Chicken Breast"~"grill",
                             item=="ADD Burger Salmon Grilled"~"grill",
                             item=="Add Egg $.99"~"grill",
                             item=="Bowl Ramen Tofu"~"ramen",
                             item=="Bowl Ramen Chicken"~"ramen",
                             item=="Quesadilla Deluxe Trillium"~"mexican",
                             item=="Burrito Una Mano Trillium BYO"~"mexican",
                             item=="Quesadilla Cheese"~"mexican",
                             item=="1 Entree + 1 Side"~"wok",
                             item=="1 Entree + 2 Side"~"wok",
                             item=="2 Entrees + 2 Sides"~"wok",
                             item=="Add Extra Toppings"~"ramen",
                             item=="1 Wok Entree"~"wok",
                             item=="Side Vegetarian Lo Mein"~"wok",
                             item=="Side Fried Spring Roll"~"wok",
                             item=="Side Vegetables"~"wok",
                             item=="Side Vegetable Spring Rolls"~"wok",
                             item=="Side Vegetarian Fried Rice with"~"wok",
                             item=="Side White or Brown Rice"~"wok",
                             item=="Create Your Pasta Bowl MEAT"~"italian",
                             item=="Pizza with Toppings"~"italian",
                             item=="Create Your Pasta Bowl VEG"~"italian",
                             item=="Pizza Cheese"~"italian",
                             item=="Add Extra Meat"~"italian",
                             item=="Side Bread Pasta Station"~"italian",
                             item=="Burrito Bowl BYO"~"mexican",
                             item=="Single Taco"~"mexican",
                             item=="Side Guacamole"~"mexican",
                             item=="Add Extra Toppings Una Mano"~"mexican",
                             item=="Side Salsa"~"mexican",
                             item=="Salad by the Pound"~"salad bar",
                             item=="Soup 12 oz"~"salad bar",
                             item=="8 oz Soup"~"salad bar",
                             item=="BowlMexicanChick"~"grab 'n' go",
                             item=="Tempura Crunch Roll"~"grab 'n' go",
                             item=="Golden Dragon Roll"~"grab 'n' go",
                             item=="Avocado Roll"~"grab 'n' go",
                             item=="California Roll"~"grab 'n' go",
                             item=="TSA Roll"~"grab 'n' go",
                             item=="Spicy Tuna Roll"~"grab 'n' go",
                             item=="Hawaiian Volcano Roll"~"grab 'n' go",
                             item=="Hawaiian Sunset"~"grab 'n' go",
                             item=="Alaskan Roll"~"grab 'n' go",
                             item=="Tempura Crunch Rooll"~"grab 'n' go",
                             item=="Tempura Shrimp Roll"~"grab 'n' go",
                             item=="Salmon Roll"~"grab 'n' go",
                             item=="Salad Chicken Caesar"~"grab 'n' go",
                             item=="Sandwich Corned Beef & Swiss"~"grab 'n' go",
                             item=="Sandwich Crispy Chicken Milanese"~"grab 'n' go",
                             item=="Wrap Chicken Caesar"~"grab 'n' go",
                             item=="Sandwich Black Forrest Ham & Swi"~"grab 'n' go",
                             item=="Sandwich Prosciutto & Mozzarella"~"grab 'n' go",
                             item=="Wrap Buffalo Chicken"~"grab 'n' go",
                             item=="BowlSesAsianNood"~"grab 'n' go",
                             item=="BowlChickAlfrPen"~"grab 'n' go",
                             item=="BowlMedProtein"~"grab 'n' go",
                             item=="BowlSouthChick"~"grab 'n' go",
                             item=="GF ChickCaesarSld"~"grab 'n' go",
                             item=="GF SunButterJelly"~"grab 'n' go",
                             item=="GFSunButterJelly"~"grab 'n' go",
                             item=="GF"~"grab 'n' go",
                             item=="GF Turkey Sand"~"grab 'n' go",
                             item=="GF ChicCaesarSld"~"grab 'n' go",
                             item=="PBJ on Wheat"~"grab 'n' go"
                             )) 
```

``` r
sales_data
```

    ##                                  item count  date     station
    ## 1          Quesadilla Deluxe Trillium   114 12.06     mexican
    ## 2                   Grilled Hamburger    76 12.06       grill
    ## 3       Burrito Una Mano Trillium BYO    50 12.06     mexican
    ## 4               Fried Chicken Tenders    63 12.06       grill
    ## 5                        French Fries   113 12.06       grill
    ## 6                   Quesadilla Cheese    12 12.06     mexican
    ## 7    Trillium Grill Impossible Burger     8 12.06       grill
    ## 8     Grilled Chicken Breast Sandwich     8 12.06       grill
    ## 9                Seared Salmon Burger     6 12.06       grill
    ## 10                  Black Bean Burger     5 12.06       grill
    ## 11                 Sweet Potato Fries     7 12.06       grill
    ## 12                Add Sausage 2 Patty     4 12.06       grill
    ## 13               ADD Beef Patty $2.99     2 12.06       grill
    ## 14                         ADD Cheese     5 12.06       grill
    ## 15                       Add Egg $.99     2 12.06       grill
    ## 16                  1 Entree + 1 Side   137 12.06         wok
    ## 17                  1 Entree + 2 Side    61 12.06         wok
    ## 18                 Bowl Ramen Chicken    58 12.06       ramen
    ## 19                2 Entrees + 2 Sides    23 12.06         wok
    ## 20                    Bowl Ramen Tofu    18 12.06       ramen
    ## 21                       1 Wok Entree     5 12.06         wok
    ## 22            Side Vegetarian Lo Mein     5 12.06         wok
    ## 23                    Side Vegetables     1 12.06         wok
    ## 24        Side Vegetable Spring Rolls     1 12.06         wok
    ## 25    Side Vegetarian Fried Rice with     1 12.06         wok
    ## 26           Side White or Brown Rice     1 12.06         wok
    ## 27                  Burrito Breakfast    72 12.06        <NA>
    ## 28                Small French Omelet    49 12.06        <NA>
    ## 29               Grand Slam Breakfast    15 12.06        <NA>
    ## 30   Egg Cheese Sausage Breakfast San    26 12.06        <NA>
    ## 31   Egg Cheese Bacon Breakfast Sandw    20 12.06        <NA>
    ## 32                          Add Bacon    27 12.06        <NA>
    ## 33                           Two Eggs    18 12.06        <NA>
    ## 34                     Pancake Single     4 12.06        <NA>
    ## 35                Trillium Home Fries     3 12.06        <NA>
    ## 36                     2 Slices Toast     5 12.06        <NA>
    ## 37                              Toast     1 12.06        <NA>
    ## 38                   PC Peanut Butter     1 12.06        <NA>
    ## 39        Create Your Pasta Bowl MEAT    83 12.06     italian
    ## 40                Pizza with Toppings    28 12.06     italian
    ## 41         Create Your Pasta Bowl VEG    12 12.06     italian
    ## 42                       Pizza Cheese    19 12.06     italian
    ## 43                     Add Extra Meat    26 12.06     italian
    ## 44                   Burrito Bowl BYO    66 12.06     mexican
    ## 45                        Single Taco     6 12.06     mexican
    ## 46                         Soup 12 oz    34 12.06   salad bar
    ## 47                          8 oz Soup    30 12.06   salad bar
    ## 48                 Salad by the Pound    34 12.06   salad bar
    ## 49                Soda Fountain 24 oz    35 12.06        <NA>
    ## 50                    Coffee 16 oz SB    23 12.06        <NA>
    ## 51                Soda Fountain 16 oz    30 12.06        <NA>
    ## 52                    Coffee 12 oz SB    16 12.06        <NA>
    ## 53                      Hot Tea 20 oz    10 12.06        <NA>
    ## 54                          Hot Cocoa     3 12.06        <NA>
    ## 55                   Side Potato Tots    21 12.06        <NA>
    ## 56                 Open Miscellaneous     4 12.06        <NA>
    ## 57               Water Aquafina 20 oz    14 12.06        <NA>
    ## 58              Soda Pepsi Diet 20 Oz    12 12.06        <NA>
    ## 59   Tea Golden Oolong Unsweet Ito En     7 12.06        <NA>
    ## 60   Juice Ocean Spray Cranbe Grape 1    11 12.06        <NA>
    ## 61                   Soda Pepsi 20 Oz     7 12.06        <NA>
    ## 62       Soda Pepsi Wild Cherry 20 Oz     7 12.06        <NA>
    ## 63       Water Life WTR Immune 700 ML     5 12.06        <NA>
    ## 64   Tea Green Matcha Milk Ito En 11.     4 12.06        <NA>
    ## 65   Tea Jasmine Green Unsweet Ito En     4 12.06        <NA>
    ## 66            Soda Orange Crush 20 Oz     6 12.06        <NA>
    ## 67   Juice Orange Premium Topicana 11     5 12.06        <NA>
    ## 68   Gatorade Propel Grape Water 1 lt     5 12.06        <NA>
    ## 69     Frappuccino Vanilla SB 13.7 Oz     3 12.06        <NA>
    ## 70          Juice Naked Green Machine     3 12.06        <NA>
    ## 71           Juice Naked Mighty Mango     3 12.06        <NA>
    ## 72                  Water Aquafina 1L     5 12.06        <NA>
    ## 73    Soda Ginger Ale Schweppes 20 Oz     5 12.06        <NA>
    ## 74            Soda Rootbeer Mug 20 oz     5 12.06        <NA>
    ## 75     Tea Iced Rasberry Lipton 16 oz     5 12.06        <NA>
    ## 76    Tea Peach Pure Leaf Lipton 18.5     5 12.06        <NA>
    ## 77   Tea Unsweetened Pure Leaf Lipton     5 12.06        <NA>
    ## 78   Yerba Mate Enlighten Mint 15.5 o     3 12.06        <NA>
    ## 79        Yerba Mate Tropical 15.5 oz     3 12.06        <NA>
    ## 80      Tea Black Milk Ito En 11.8 oz     3 12.06        <NA>
    ## 81   Cold Brew Vanilla Sweet Cream St     3 12.06        <NA>
    ## 82   Juice Joes Lemonade  Red Jacket1     3 12.06        <NA>
    ## 83   Energy Blue Raz Lemonade Celsius     3 12.06        <NA>
    ## 84    Energy Kiwi Guava Celsius 12 oz     3 12.06        <NA>
    ## 85   Energy Mango PassFruit Celsius 1     3 12.06        <NA>
    ## 86   Muscle Milk PROSRS 40 Intense Va     2 12.06        <NA>
    ## 87      Water Aquafina Alumitek 16 oz     5 12.06        <NA>
    ## 88    Soda Rootbeer Zero Sugar Mug 20     4 12.06        <NA>
    ## 89   Tea Tea & Lemon Pure Leaf Lipton     4 12.06        <NA>
    ## 90       Cider Apple Red Jacket 12 oz     3 12.06        <NA>
    ## 91            Juice Naked Berry Blast     2 12.06        <NA>
    ## 92           Juice Naked Blue Machine     2 12.06        <NA>
    ## 93         Juice Naked PNCLDA 15.2 Oz     2 12.06        <NA>
    ## 94      Naked Strawberry Banana Juice     2 12.06        <NA>
    ## 95     Kombucha Ginger Kevita 15.2 oz     2 12.06        <NA>
    ## 96    Kombucha Pineapple Peach Kevita     2 12.06        <NA>
    ## 97                Gatorade Blue 28 oz     3 12.06        <NA>
    ## 98   Juice Orange Homestyle Tropicana     3 12.06        <NA>
    ## 99   Gatorade Propel Berry Water 1 lt     3 12.06        <NA>
    ## 100  Gatorade Gatorlyte Strawberry Ki     2 12.06        <NA>
    ## 101     Yerba Mate Bluephoria 15.5 oz     2 12.06        <NA>
    ## 102          Yerba Mate Lemon 15.5 oz     2 12.06        <NA>
    ## 103  Yerba Mate Peach Revival 15.5 oz     2 12.06        <NA>
    ## 104    Yerba Mate Revel Berry 15.5 oz     2 12.06        <NA>
    ## 105  Cold Brew Chocolate Cream Starbu     2 12.06        <NA>
    ## 106  Juice Fuji Apple Red Jacket 12oz     2 12.06        <NA>
    ## 107             Soda Pepsi  Zero 20oz     3 12.06        <NA>
    ## 108  Tea Blackberry Pure Leaf 18.5 oz     3 12.06        <NA>
    ## 109  Tea Sweetened With Lemon Brisk 2     3 12.06        <NA>
    ## 110    Energy Artic Vibe Celsius 12oz     2 12.06        <NA>
    ## 111   Energy Fast Twitch Cool Blue 12     2 12.06        <NA>
    ## 112    Energy Fast Twitch Grape 12 oz     2 12.06        <NA>
    ## 113  Tea Green Peach Mango Celsius 12     2 12.06        <NA>
    ## 114   Tea Green Rasp Acai  Celsius 12     2 12.06        <NA>
    ## 115       Gatorade Berry Zero G 28 oz     2 12.06        <NA>
    ## 116              Gatorade Orange 28oz     2 12.06        <NA>
    ## 117     Glacier Freeze Gatorade 28 oz     2 12.06        <NA>
    ## 118  Juice Raspberry Lemonade Tropica     2 12.06        <NA>
    ## 119    Soda Tamarind Jarritos 12.5 oz     2 12.06        <NA>
    ## 120          Muscle Milk Choc PB 14oz     1 12.06        <NA>
    ## 121        Lipton Pure Leaf Sweet Tea     2 12.06        <NA>
    ## 122      Soda Mountain Dew Zero 20 Oz     2 12.06        <NA>
    ## 123  Tea Sweet W/ Le Pure Leaf Lipton     2 12.06        <NA>
    ## 124          Juice Apple Dole 15.2 oz     2 12.06        <NA>
    ## 125         Juice Orange Dole 15.2 oz     2 12.06        <NA>
    ## 126          Juice Protein Zone Naked     1 12.06        <NA>
    ## 127           Naked Red Machine Juice     1 12.06        <NA>
    ## 128    Soda Mandarin Jarritos 12.5 oz     2 12.06        <NA>
    ## 129   Kombucha Rasp Lemon Kevita 15.2     1 12.06        <NA>
    ## 130  Gatorade Gatorlyte Cherry Lime 2     1 12.06        <NA>
    ## 131  Gatorade Gatorlyte MixBerry 20 o     1 12.06        <NA>
    ## 132  Gatorade Gatorlyte Zero Fruit Pu     1 12.06        <NA>
    ## 133  Energy 222 Pineapple Mango Odyss     1 12.06        <NA>
    ## 134   Energy Blue Crush Celsius 16 oz     1 12.06        <NA>
    ## 135  Energy Mango Tango Celsius 16 oz     1 12.06        <NA>
    ## 136  Energy Passion Orange Guva Odyss     1 12.06        <NA>
    ## 137    Energy StrawLemon Celsius 12oz     1 12.06        <NA>
    ## 138  Mountain Dew Kickstart Black Che     1 12.06        <NA>
    ## 139        Gatorade Fruit Punch 28 oz     1 12.06        <NA>
    ## 140  Gatorade Galcier Freeze Zero G 2     1 12.06        <NA>
    ## 141         Gatorade Lemon Lime 28 oz     1 12.06        <NA>
    ## 142         Gatorade Zero Grape 28 oz     1 12.06        <NA>
    ## 143     Glacier Cherry Gatorade 28 oz     1 12.06        <NA>
    ## 144  Juice Zero Summer Splash Punch 1     1 12.06        <NA>
    ## 145  Kickstart Strawberry Start-up 16     1 12.06        <NA>
    ## 146  Gatorade Propel  Kiwi Straw Wate     1 12.06        <NA>
    ## 147              Water Gatorade 700ml     1 12.06        <NA>
    ## 148       Soda Mango Jarritos 12.5 oz     1 12.06        <NA>
    ## 149           Soda Mountain Dew 20 Oz     1 12.06        <NA>
    ## 150      Tea Light Peach Lipton 20 oz     1 12.06        <NA>
    ## 151  Tea Pure Leaf Zero Sugar Sweet T     1 12.06        <NA>
    ## 152  Tea Unsweet Black W Lemon 18.5 o     1 12.06        <NA>
    ## 153  Tea Unsweet Green Lipton 18.5 oz     1 12.06        <NA>
    ## 154          Juice Lemonade Dole 20oz     1 12.06        <NA>
    ## 155    Cornell Low Fat Chocolate Milk     8 12.06        <NA>
    ## 156  Yogurt Mixed Berry Chobani Drink     4 12.06        <NA>
    ## 157  Yogurt Straw Banana Chobani Drin     3 12.06        <NA>
    ## 158    Milk Chocolate LF Cornell 8 Oz     6 12.06        <NA>
    ## 159   Yogurt Flip Peanut Butter Dream     3 12.06        <NA>
    ## 160        Yogurt Peach Greek Chobani     3 12.06        <NA>
    ## 161        Yogurt Plain Greek Chobani     3 12.06        <NA>
    ## 162  Yogurt Flip Almond Coco Loco Cho     2 12.06        <NA>
    ## 163        Cornell 2% Milk (70000381)     3 12.06        <NA>
    ## 164   Yogurt Mango Chobani Drink 7 oz     1 12.06        <NA>
    ## 165                Cornell Whole Milk     2 12.06        <NA>
    ## 166        Cornell 2% Milk (70000380)     1 12.06        <NA>
    ## 167    Yogurt Blueberry Greek Chobani     1 12.06        <NA>
    ## 168    Yogurt Raspberry Greek Chobani     1 12.06        <NA>
    ## 169  Yogurt Strawberry Banana Greek C     1 12.06        <NA>
    ## 170   Yogurt Strawberry Greek Chobani     1 12.06        <NA>
    ## 171  Jelly Konjac Mango Pineapple Tas     4 12.06        <NA>
    ## 172  Jelly Konjac Apple Grape Tastell     2 12.06        <NA>
    ## 173  Jelly Konjac Double Berry Tastel     2 12.06        <NA>
    ## 174        Bar Chocolate Chip Clifbar     2 12.06        <NA>
    ## 175  Bar Choc Chip Cookie Dough Quest     1 12.06        <NA>
    ## 176  Cookie Birthday Cake Lenny & Lar     1 12.06        <NA>
    ## 177  Dark Choc Cherry Cashew Plus Kin     1 12.06        <NA>
    ## 178    Bar Blueberry Crisp Clif 2.4oz     1 12.06        <NA>
    ## 179  Bar That's It Apple+Blueberry 1.     1 12.06        <NA>
    ## 180  Bar That's It Apple+Mango 1.2 oz     1 12.06        <NA>
    ## 181         Oat And Honey Granola Bar     1 12.06        <NA>
    ## 182                       Fruit Whole    46 12.06        <NA>
    ## 183            Chips Dirty Sea Salted     4 12.06        <NA>
    ## 184  Chip Potato Dirty BBQ Mesquite 2     2 12.06        <NA>
    ## 185        Chip Maui Onion Dirty 2 oz     1 12.06        <NA>
    ## 186   Chips Cracked Pepper & Sea Salt     1 12.06        <NA>
    ## 187        Jalapeno Heat Chips Kosher     1 12.06        <NA>
    ## 188          Utz Barbecue Potato Chip     1 12.06        <NA>
    ## 189                  Utz Regular Chip     1 12.06        <NA>
    ## 190        Chip Baked Jax  Utz 1.5 oz     1 12.06        <NA>
    ## 191       Chewy Marshmallow GF 2.1 oz     4 12.06        <NA>
    ## 192  GF Sweet Street Choloate Brownie     3 12.06        <NA>
    ## 193                  MochiCookCrm1.5o     2 12.06        <NA>
    ## 194   Ice Cream Mochi Sweet Mango 1.5     1 12.06        <NA>
    ## 195     Fresh Cut Pineapple Fruit Cup     1 12.06        <NA>
    ## 196              Orbit Wintermint Gum     1 12.06        <NA>
    ## 197                      Muffin Jumbo    16 12.06        <NA>
    ## 198                            Cookie    19 12.06        <NA>
    ## 199                    Croissant Choc     4 12.06        <NA>
    ## 200                 Croissant Blue CC     3 12.06        <NA>
    ## 201                Croissant Straw CC     3 12.06        <NA>
    ## 202                   Plain Croissant     2 12.06        <NA>
    ## 203                       Coffee Cake     2 12.06        <NA>
    ## 204       Combo 16 oz Coffee & Muffin     1 12.06        <NA>
    ## 205                      Cinnamon Bun     1 12.06        <NA>
    ## 206                  BowlMexicanChick     2 12.06 grab 'n' go
    ## 207               Tempura Crunch Roll     1 12.06 grab 'n' go
    ## 208                Golden Dragon Roll     1 12.06 grab 'n' go
    ## 209                      Avocado Roll     1 12.06 grab 'n' go
    ## 210                   California Roll     1 12.06 grab 'n' go
    ## 211              Salad Chicken Caesar     1 12.06 grab 'n' go
    ## 212      Sandwich Corned Beef & Swiss     1 12.06 grab 'n' go
    ## 213               Wrap Chicken Caesar     1 12.06 grab 'n' go
    ## 214        Quesadilla Deluxe Trillium   163 12.05     mexican
    ## 215                 Grilled Hamburger   118 12.05       grill
    ## 216             Fried Chicken Tenders   116 12.05       grill
    ## 217     Burrito Una Mano Trillium BYO    82 12.05     mexican
    ## 218                      French Fries   179 12.05       grill
    ## 219   Grilled Chicken Breast Sandwich    22 12.05       grill
    ## 220  Trillium Grill Impossible Burger    16 12.05       grill
    ## 221                 Quesadilla Cheese    20 12.05     mexican
    ## 222                Sweet Potato Fries    42 12.05       grill
    ## 223              Seared Salmon Burger    11 12.05       grill
    ## 224                 Black Bean Burger     6 12.05       grill
    ## 225              ADD Beef Patty $2.99    15 12.05       grill
    ## 226                ADD Chicken Breast     4 12.05       grill
    ## 227               Add Sausage 2 Patty     3 12.05       grill
    ## 228                        ADD Cheese     9 12.05       grill
    ## 229                      Add Egg $.99     3 12.05       grill
    ## 230                 1 Entree + 1 Side   212 12.05         wok
    ## 231                 1 Entree + 2 Side    85 12.05         wok
    ## 232                Bowl Ramen Chicken    70 12.05       ramen
    ## 233               2 Entrees + 2 Sides    41 12.05         wok
    ## 234                   Bowl Ramen Tofu    22 12.05       ramen
    ## 235           Side Vegetarian Lo Mein    14 12.05         wok
    ## 236                      1 Wok Entree     8 12.05         wok
    ## 237          Side White or Brown Rice    10 12.05         wok
    ## 238            Side Fried Spring Roll     2 12.05         wok
    ## 239                   Side Vegetables     2 12.05         wok
    ## 240       Side Vegetable Spring Rolls     1 12.05         wok
    ## 241       Create Your Pasta Bowl MEAT   113 12.05     italian
    ## 242        Create Your Pasta Bowl VEG    32 12.05     italian
    ## 243               Pizza with Toppings    37 12.05     italian
    ## 244                      Pizza Cheese    23 12.05     italian
    ## 245                    Add Extra Meat    23 12.05     italian
    ## 246          Side Bread Pasta Station     1 12.05     italian
    ## 247                 Burrito Breakfast    69 12.05        <NA>
    ## 248               Small French Omelet    36 12.05        <NA>
    ## 249              Grand Slam Breakfast    18 12.05        <NA>
    ## 250  Egg Cheese Sausage Breakfast San    24 12.05        <NA>
    ## 251  Egg Cheese Bacon Breakfast Sandw    20 12.05        <NA>
    ## 252                         Add Bacon    28 12.05        <NA>
    ## 253                          Two Eggs    15 12.05        <NA>
    ## 254               Trillium Home Fries     2 12.05        <NA>
    ## 255                    2 Slices Toast     2 12.05        <NA>
    ## 256                             Toast     1 12.05        <NA>
    ## 257                  Burrito Bowl BYO    93 12.05     mexican
    ## 258                       Single Taco     4 12.05     mexican
    ## 259                    Side Guacamole     2 12.05     mexican
    ## 260       Add Extra Toppings Una Mano     2 12.05     mexican
    ## 261                         8 oz Soup    66 12.05   salad bar
    ## 262                        Soup 12 oz    36 12.05   salad bar
    ## 263                Salad by the Pound    47 12.05   salad bar
    ## 264               Soda Fountain 24 oz    37 12.05        <NA>
    ## 265                   Coffee 16 oz SB    26 12.05        <NA>
    ## 266               Soda Fountain 16 oz    27 12.05        <NA>
    ## 267                   Coffee 12 oz SB    17 12.05        <NA>
    ## 268                     Hot Tea 20 oz     7 12.05        <NA>
    ## 269                  Side Potato Tots    11 12.05        <NA>
    ## 270                Open Miscellaneous     9 12.05        <NA>
    ## 271           Add Extra Protein $2.99     2 12.05        <NA>
    ## 272              Water Aquafina 20 oz    15 12.05        <NA>
    ## 273          Juice Naked Mighty Mango     7 12.05        <NA>
    ## 274             Soda Pepsi Diet 20 Oz    12 12.05        <NA>
    ## 275  Energy Mango PassFruit Celsius 1     8 12.05        <NA>
    ## 276   Kombucha Pineapple Peach Kevita     6 12.05        <NA>
    ## 277  Gatorade Galcier Freeze Zero G 2     8 12.05        <NA>
    ## 278  Juice Orange Homestyle Tropicana     8 12.05        <NA>
    ## 279  Juice Orange Premium Topicana 11     8 12.05        <NA>
    ## 280          Juice Naked Blue Machine     5 12.05        <NA>
    ## 281  Yerba Mate Peach Revival 15.5 oz     5 12.05        <NA>
    ## 282    Yerba Mate Revel Berry 15.5 oz     5 12.05        <NA>
    ## 283  Tea Green Matcha Milk Ito En 11.     5 12.05        <NA>
    ## 284  Tea Jasmine Green Unsweet Ito En     5 12.05        <NA>
    ## 285    Tea Iced Rasberry Lipton 16 oz     8 12.05        <NA>
    ## 286  Cold Brew Vanilla Sweet Cream St     5 12.05        <NA>
    ## 287    Energy Artic Vibe Celsius 12oz     5 12.05        <NA>
    ## 288             Soda Pepsi  Zero 20oz     7 12.05        <NA>
    ## 289   Tea Peach Pure Leaf Lipton 18.5     7 12.05        <NA>
    ## 290  Yerba Mate Enlighten Mint 15.5 o     4 12.05        <NA>
    ## 291       Yerba Mate Tropical 15.5 oz     4 12.05        <NA>
    ## 292  Tea Golden Oolong Unsweet Ito En     4 12.05        <NA>
    ## 293                 Water Aquafina 1L     6 12.05        <NA>
    ## 294   Energy Blue Crush Celsius 16 oz     4 12.05        <NA>
    ## 295                  Soda Pepsi 20 Oz     6 12.05        <NA>
    ## 296  Tea Sweet W/ Le Pure Leaf Lipton     6 12.05        <NA>
    ## 297               Gatorade Blue 28 oz     5 12.05        <NA>
    ## 298   Starbucks Doubleshot Energy Van     3 12.05        <NA>
    ## 299  Gatorade Propel Berry Water 1 lt     5 12.05        <NA>
    ## 300  Energy Fuji Apple Pear Celsius 1     4 12.05        <NA>
    ## 301    Energy StrawLemon Celsius 12oz     4 12.05        <NA>
    ## 302  Tea Green Peach Mango Celsius 12     4 12.05        <NA>
    ## 303      Frappuccino Mocha 13.7 Oz SB     3 12.05        <NA>
    ## 304    Frappuccino Vanilla SB 13.7 Oz     3 12.05        <NA>
    ## 305   Kombucha Rasp Lemon Kevita 15.2     3 12.05        <NA>
    ## 306  Tea Blackberry Pure Leaf 18.5 oz     5 12.05        <NA>
    ## 307  Gatorade Gatorlyte Cherry Lime 2     3 12.05        <NA>
    ## 308  Gatorade Gatorlyte Zero Fruit Pu     3 12.05        <NA>
    ## 309         Gatorade Lemon Lime 28 oz     4 12.05        <NA>
    ## 310     Glacier Freeze Gatorade 28 oz     4 12.05        <NA>
    ## 311        Juice AppleTropicana 11 oz     4 12.05        <NA>
    ## 312   Juice Lively Lemonade Tropicana     4 12.05        <NA>
    ## 313  Juice Raspberry Lemonade Tropica     4 12.05        <NA>
    ## 314  Energy Passion Orange Guva Odyss     3 12.05        <NA>
    ## 315  Sparkling lemonade Strawberry Ke     4 12.05        <NA>
    ## 316     Muscle Milk KO Chocolate 14oz     2 12.05        <NA>
    ## 317  Muscle Milk P40 Strawberry Cream     2 12.05        <NA>
    ## 318      Water Life WTR Immune 700 ML     3 12.05        <NA>
    ## 319            Water Gatorade 33.8 oz     3 12.05        <NA>
    ## 320        Lipton Pure Leaf Sweet Tea     4 12.05        <NA>
    ## 321           Soda Orange Crush 20 Oz     4 12.05        <NA>
    ## 322           Soda Rootbeer Mug 20 oz     4 12.05        <NA>
    ## 323      Tea Light Peach Lipton 20 oz     4 12.05        <NA>
    ## 324  Tea Pure Leaf Zero Sugar Sweet T     4 12.05        <NA>
    ## 325  Tea Unsweetened Pure Leaf Lipton     4 12.05        <NA>
    ## 326    Frappuccino Caramel SB 13.7 oz     2 12.05        <NA>
    ## 327         Juice Naked Green Machine     2 12.05        <NA>
    ## 328          Juice Protein Zone Naked     2 12.05        <NA>
    ## 329     Naked Strawberry Banana Juice     2 12.05        <NA>
    ## 330  Juice Ocean Spray Cranbe Grape 1     4 12.05        <NA>
    ## 331  Gatorade Propel Grape Water 1 lt     3 12.05        <NA>
    ## 332  Gatorade Gatorlyte Zero Lemon Li     2 12.05        <NA>
    ## 333          Yerba Mate Lemon 15.5 oz     2 12.05        <NA>
    ## 334      Starbucks Double Shot 6.5 Oz     2 12.05        <NA>
    ## 335  Energy Fruit Burst Celsius 16 oz     2 12.05        <NA>
    ## 336  Energy Mango Tango Celsius 16 oz     2 12.05        <NA>
    ## 337  Juice Fuji Apple Red Jacket 12oz     2 12.05        <NA>
    ## 338   Soda Ginger Ale Schweppes 20 Oz     3 12.05        <NA>
    ## 339      Soda Mountain Dew Zero 20 Oz     3 12.05        <NA>
    ## 340   Soda Rootbeer Zero Sugar Mug 20     3 12.05        <NA>
    ## 341  Energy Blue Raz Lemonade Celsius     2 12.05        <NA>
    ## 342   Energy Fast Twitch Cool Blue 12     2 12.05        <NA>
    ## 343   Energy Kiwi Guava Celsius 12 oz     2 12.05        <NA>
    ## 344   Tea Green Rasp Acai  Celsius 12     2 12.05        <NA>
    ## 345    Soda Mandarin Jarritos 12.5 oz     3 12.05        <NA>
    ## 346  Mountain Dew Kickstart Orange 16     2 12.05        <NA>
    ## 347      Cider Apple Red Jacket 12 oz     2 12.05        <NA>
    ## 348     Water Aquafina Alumitek 16 oz     3 12.05        <NA>
    ## 349         Gatorade Zero Grape 28 oz     2 12.05        <NA>
    ## 350        Juice Grape Tropicana 11oz     2 12.05        <NA>
    ## 351  Gatorade Propel  Kiwi Straw Wate     2 12.05        <NA>
    ## 352              Water Gatorade 700ml     2 12.05        <NA>
    ## 353    Muscle Milk PP Chocolate 14 oz     1 12.05        <NA>
    ## 354           Soda Mountain Dew 20 Oz     2 12.05        <NA>
    ## 355      Soda Pepsi Wild Cherry 20 Oz     2 12.05        <NA>
    ## 356      Soda Starry Lemon Lime 20 oz     2 12.05        <NA>
    ## 357  Tea Sweetened With Lemon Brisk 2     2 12.05        <NA>
    ## 358  Tea Tea & Lemon Pure Leaf Lipton     2 12.05        <NA>
    ## 359  Tea Unsweet Black W Lemon 18.5 o     2 12.05        <NA>
    ## 360  Tea Unsweet Green Lipton 18.5 oz     2 12.05        <NA>
    ## 361  Starbucks Doubleshot Ener Coffee     1 12.05        <NA>
    ## 362          Juice Apple Dole 15.2 oz     2 12.05        <NA>
    ## 363          Juice Lemonade Dole 20oz     2 12.05        <NA>
    ## 364         Juice Orange Dole 15.2 oz     2 12.05        <NA>
    ## 365  Frappuccino Oatmilk Caramel Whit     1 12.05        <NA>
    ## 366           Juice Naked Berry Blast     1 12.05        <NA>
    ## 367        Juice Naked PNCLDA 15.2 Oz     1 12.05        <NA>
    ## 368    Kombucha Ginger Kevita 15.2 oz     1 12.05        <NA>
    ## 369  Gatorade Gatorlyte Glacier Freez     1 12.05        <NA>
    ## 370   Gatorade Gatorlyte Orange 20 oz     1 12.05        <NA>
    ## 371  Gatorade Gatorlyte Strawberry Ki     1 12.05        <NA>
    ## 372     Yerba Mate Bluephoria 15.5 oz     1 12.05        <NA>
    ## 373     Tea Black Milk Ito En 11.8 oz     1 12.05        <NA>
    ## 374  Cold Brew Chocolate Cream Starbu     1 12.05        <NA>
    ## 375  Energy 222 Blue Raspberry Odysse     1 12.05        <NA>
    ## 376  Energy Dragonberry Celsius 16 oz     1 12.05        <NA>
    ## 377  Energy Orangesicle Celsius 16 oz     1 12.05        <NA>
    ## 378  Energy Revive Prickly Pear Odyss     1 12.05        <NA>
    ## 379  Juice Joes Lemonade  Red Jacket1     1 12.05        <NA>
    ## 380     Rockstar Pure Zero Silver Ice     1 12.05        <NA>
    ## 381  Mountain Dew Kickstart Black Che     1 12.05        <NA>
    ## 382       Gatorade Berry Zero G 28 oz     1 12.05        <NA>
    ## 383              Gatorade Orange 28oz     1 12.05        <NA>
    ## 384  Juice Zero Summer Splash Punch 1     1 12.05        <NA>
    ## 385  Kickstart Strawberry Start-up 16     1 12.05        <NA>
    ## 386       Soda Mango Jarritos 12.5 oz     1 12.05        <NA>
    ## 387   Soda Pineapple Jarritos 12.5 oz     1 12.05        <NA>
    ## 388    Ocean Spray Cranberry Cocktail     1 12.05        <NA>
    ## 389    Cornell Low Fat Chocolate Milk     7 12.05        <NA>
    ## 390  Yogurt 0% Fat Vanilla Greek Oiko     8 12.05        <NA>
    ## 391  Yogurt Mixed Berry Chobani Drink     4 12.05        <NA>
    ## 392  Yogurt Straw Banana Chobani Drin     4 12.05        <NA>
    ## 393    Milk Chocolate LF Cornell 8 Oz     7 12.05        <NA>
    ## 394                   Cornell 2% Milk     6 12.05        <NA>
    ## 395                Cornell Whole Milk     5 12.05        <NA>
    ## 396    Yogurt Blueberry Greek Chobani     4 12.05        <NA>
    ## 397  Yogurt Flip Almond Coco Loco Cho     3 12.05        <NA>
    ## 398        Yogurt Mango Greek Chobani     3 12.05        <NA>
    ## 399    Yogurt Raspberry Greek Chobani     3 12.05        <NA>
    ## 400   Yogurt Flip Peanut Butter Dream     2 12.05        <NA>
    ## 401  Yogurt Black Cherry Greek Choban     2 12.05        <NA>
    ## 402        Yogurt Plain Greek Chobani     2 12.05        <NA>
    ## 403   Yogurt Mango Chobani Drink 7 oz     1 12.05        <NA>
    ## 404  Yogurt Strawberry Banana Greek C     1 12.05        <NA>
    ## 405  Jelly Konjac Mango Pineapple Tas     9 12.05        <NA>
    ## 406       Jelly Konjac Peach Tastelli     8 12.05        <NA>
    ## 407    Fruit And Nut Delight Kind Bar     4 12.05        <NA>
    ## 408         Bar S'Mores Quest 2.12 oz     2 12.05        <NA>
    ## 409  Bar That's It Apple+Strawberry 1     3 12.05        <NA>
    ## 410  Bar CHOC Mint Builders Clif bar2     2 12.05        <NA>
    ## 411   Bar Peanut Butter Builders Clif     2 12.05        <NA>
    ## 412         Oat And Honey Granola Bar     4 12.05        <NA>
    ## 413  Jelly Konjac Double Berry Tastel     2 12.05        <NA>
    ## 414        Bar Chocolate Chip Clifbar     2 12.05        <NA>
    ## 415    Kind Cranberry Almond Plus Bar     2 12.05        <NA>
    ## 416  Bar Choc Chip Cookie Dough Quest     1 12.05        <NA>
    ## 417  Bar Cookie & Cream Quest 2.12 oz     1 12.05        <NA>
    ## 418   Bar Frosted Birthday Cake Quest     1 12.05        <NA>
    ## 419  Cookie Peanut Butter Lenny & Lar     1 12.05        <NA>
    ## 420  Jelly Konjac Apple Grape Tastell     1 12.05        <NA>
    ## 421    Bar Crunchy Peanut Butter Clif     1 12.05        <NA>
    ## 422  Bar White Chocolate Macadamia Cl     1 12.05        <NA>
    ## 423  Bar That's It Apple+Mango 1.2 oz     1 12.05        <NA>
    ## 424   Oats And Dark Chocolate Granola     1 12.05        <NA>
    ## 425                       Fruit Whole    69 12.05        <NA>
    ## 426                  Utz Regular Chip     4 12.05        <NA>
    ## 427            Chips Dirty Sea Salted     2 12.05        <NA>
    ## 428        Jalapeno Heat Chips Kosher     2 12.05        <NA>
    ## 429  Nuts Almonds Honey 1.5 oz Sahale     1 12.05        <NA>
    ## 430  Chip Potato Kettle Honey BBQ 2oz     1 12.05        <NA>
    ## 431  Chip Potato Dirty BBQ Mesquite 2     1 12.05        <NA>
    ## 432   Chips Cracked Pepper & Sea Salt     1 12.05        <NA>
    ## 433   Chip Sour Cream And Onion Dirty     1 12.05        <NA>
    ## 434          Utz Barbecue Potato Chip     1 12.05        <NA>
    ## 435        Utz GoodHealth Veggie Chip     1 12.05        <NA>
    ## 436           Utz Salt N Vinegar Chip     1 12.05        <NA>
    ## 437       Chewy Marshmallow GF 2.1 oz     6 12.05        <NA>
    ## 438  GF Sweet Street Choloate Brownie     4 12.05        <NA>
    ## 439              Orbit Wintermint Gum     9 12.05        <NA>
    ## 440    Candy CHOC Cara Caff Bar Awake     1 12.05        <NA>
    ## 441     Fresh Cut Pineapple Fruit Cup     1 12.05        <NA>
    ## 442  Ice Cream Mochi Double Chocolate     1 12.05        <NA>
    ## 443                   California Roll     3 12.05 grab 'n' go
    ## 444                          TSA Roll     2 12.05 grab 'n' go
    ## 445                   Spicy Tuna Roll     2 12.05 grab 'n' go
    ## 446             Hawaiian Volcano Roll     1 12.05 grab 'n' go
    ## 447                      Alaskan Roll     1 12.05 grab 'n' go
    ## 448               Tempura Crunch Roll     1 12.05 grab 'n' go
    ## 449               Tempura Shrimp Roll     1 12.05 grab 'n' go
    ## 450                Golden Dragon Roll     1 12.05 grab 'n' go
    ## 451                       Salmon Roll     1 12.05 grab 'n' go
    ## 452                      Avocado Roll     1 12.05 grab 'n' go
    ## 453               Wrap Chicken Caesar     6 12.05 grab 'n' go
    ## 454  Sandwich Black Forrest Ham & Swi     1 12.05 grab 'n' go
    ## 455      Sandwich Corned Beef & Swiss     1 12.05 grab 'n' go
    ## 456              Wrap Buffalo Chicken     1 12.05 grab 'n' go
    ## 457                      Muffin Jumbo    26 12.05        <NA>
    ## 458                            Cookie    19 12.05        <NA>
    ## 459                      Cinnamon Bun     5 12.05        <NA>
    ## 460                Croissant Straw CC     4 12.05        <NA>
    ## 461                    Croissant Choc     3 12.05        <NA>
    ## 462                   Plain Croissant     3 12.05        <NA>
    ## 463                 Croissant Blue CC     2 12.05        <NA>
    ## 464       Combo 16 oz Coffee & Muffin     2 12.05        <NA>
    ## 465                       Coffee Cake     2 12.05        <NA>
    ## 466                           Brownie     1 12.05        <NA>
    ## 467                  BowlSesAsianNood     3 12.05 grab 'n' go
    ## 468                  BowlChickAlfrPen     1 12.05 grab 'n' go
    ## 469                    BowlMedProtein     1 12.05 grab 'n' go
    ## 470                    BowlSouthChick     1 12.05 grab 'n' go
    ## 471                  GF ChicCaesarSld     1 12.05 grab 'n' go
    ## 472                  GFSunButterJelly     2 12.05 grab 'n' go
    ## 473                    GF Turkey Sand     1 12.05 grab 'n' go
    ## 474        Quesadilla Deluxe Trillium   168 12.04     mexican
    ## 475                 Grilled Hamburger   117 12.04       grill
    ## 476             Fried Chicken Tenders    83 12.04       grill
    ## 477     Burrito Una Mano Trillium BYO    60 12.04     mexican
    ## 478                      French Fries   126 12.04       grill
    ## 479   Grilled Chicken Breast Sandwich    16 12.04       grill
    ## 480                 Quesadilla Cheese    17 12.04     mexican
    ## 481                Sweet Potato Fries    28 12.04       grill
    ## 482  Trillium Grill Impossible Burger     7 12.04       grill
    ## 483              Seared Salmon Burger     5 12.04       grill
    ## 484                ADD Chicken Breast    10 12.04       grill
    ## 485              ADD Beef Patty $2.99     7 12.04       grill
    ## 486               Add Sausage 2 Patty     4 12.04       grill
    ## 487                 Black Bean Burger     1 12.04       grill
    ## 488                        ADD Cheese     5 12.04       grill
    ## 489                      Add Egg $.99     3 12.04       grill
    ## 490                 1 Entree + 1 Side   202 12.04         wok
    ## 491                 1 Entree + 2 Side    85 12.04         wok
    ## 492                Bowl Ramen Chicken    84 12.04       ramen
    ## 493               2 Entrees + 2 Sides    35 12.04         wok
    ## 494                   Bowl Ramen Tofu    13 12.04       ramen
    ## 495           Side Vegetarian Lo Mein    16 12.04         wok
    ## 496                      1 Wok Entree     5 12.04         wok
    ## 497          Side White or Brown Rice     9 12.04         wok
    ## 498                   Side Vegetables     2 12.04         wok
    ## 499            Side Fried Spring Roll     1 12.04         wok
    ## 500   Side Vegetarian Fried Rice with     1 12.04         wok
    ## 501       Create Your Pasta Bowl MEAT   142 12.04     italian
    ## 502        Create Your Pasta Bowl VEG    27 12.04     italian
    ## 503               Pizza with Toppings    22 12.04     italian
    ## 504                      Pizza Cheese    22 12.04     italian
    ## 505                    Add Extra Meat    36 12.04     italian
    ## 506          Side Bread Pasta Station     2 12.04     italian
    ## 507                 Burrito Breakfast    91 12.04        <NA>
    ## 508               Small French Omelet    58 12.04        <NA>
    ## 509              Grand Slam Breakfast    19 12.04        <NA>
    ## 510  Egg Cheese Sausage Breakfast San    30 12.04        <NA>
    ## 511  Egg Cheese Bacon Breakfast Sandw    28 12.04        <NA>
    ## 512                         Add Bacon    34 12.04        <NA>
    ## 513                          Two Eggs    18 12.04        <NA>
    ## 514               Trillium Home Fries     5 12.04        <NA>
    ## 515                    Pancake Single     2 12.04        <NA>
    ## 516                    2 Slices Toast     4 12.04        <NA>
    ## 517                             Toast     1 12.04        <NA>
    ## 518                  Burrito Bowl BYO    84 12.04     mexican
    ## 519                       Single Taco    10 12.04     mexican
    ## 520                    Side Guacamole     2 12.04     mexican
    ## 521       Add Extra Toppings Una Mano     2 12.04     mexican
    ## 522                        Soup 12 oz    58 12.04   salad bar
    ## 523                         8 oz Soup    59 12.04   salad bar
    ## 524                Salad by the Pound    61 12.04   salad bar
    ## 525               Soda Fountain 16 oz    48 12.04        <NA>
    ## 526                   Coffee 16 oz SB    32 12.04        <NA>
    ## 527               Soda Fountain 24 oz    36 12.04        <NA>
    ## 528                   Coffee 12 oz SB    17 12.04        <NA>
    ## 529                     Hot Tea 20 oz     6 12.04        <NA>
    ## 530                  Side Potato Tots    11 12.04        <NA>
    ## 531                Open Miscellaneous     5 12.04        <NA>
    ## 532           Add Extra Protein $2.99     2 12.04        <NA>
    ## 533             Soda Pepsi Diet 20 Oz    18 12.04        <NA>
    ## 534              Water Aquafina 20 oz    19 12.04        <NA>
    ## 535   Energy Kiwi Guava Celsius 12 oz     8 12.04        <NA>
    ## 536          Juice Naked Mighty Mango     6 12.04        <NA>
    ## 537      Water Life WTR Immune 700 ML     8 12.04        <NA>
    ## 538  Yerba Mate Enlighten Mint 15.5 o     6 12.04        <NA>
    ## 539    Yerba Mate Revel Berry 15.5 oz     6 12.04        <NA>
    ## 540       Yerba Mate Tropical 15.5 oz     6 12.04        <NA>
    ## 541  Energy Mango PassFruit Celsius 1     6 12.04        <NA>
    ## 542  Muscle Milk P40 Strawberry Cream     4 12.04        <NA>
    ## 543                 Water Aquafina 1L     8 12.04        <NA>
    ## 544                  Soda Pepsi 20 Oz     8 12.04        <NA>
    ## 545   Starbucks Doubleshot Energy Van     4 12.04        <NA>
    ## 546  Gatorade Gatorlyte Glacier Freez     4 12.04        <NA>
    ## 547          Yerba Mate Lemon 15.5 oz     4 12.04        <NA>
    ## 548     Tea Black Milk Ito En 11.8 oz     4 12.04        <NA>
    ## 549  Tea Jasmine Green Unsweet Ito En     4 12.04        <NA>
    ## 550          Muscle Milk Choc PB 14oz     3 12.04        <NA>
    ## 551     Muscle Milk KO Chocolate 14oz     3 12.04        <NA>
    ## 552  Muscle Milk PROSRS 40 Intense Va     3 12.04        <NA>
    ## 553  Red Jacket Strawberry Apple Juic     4 12.04        <NA>
    ## 554  Tea Unsweetened Pure Leaf Lipton     6 12.04        <NA>
    ## 555        Juice AppleTropicana 11 oz     5 12.04        <NA>
    ## 556  Gatorade Propel  Kiwi Straw Wate     5 12.04        <NA>
    ## 557  Sparkling lemonade Strawberry Ke     5 12.04        <NA>
    ## 558    Energy Fast Twitch Grape 12 oz     4 12.04        <NA>
    ## 559          Juice Naked Blue Machine     3 12.04        <NA>
    ## 560          Juice Protein Zone Naked     3 12.04        <NA>
    ## 561     Naked Strawberry Banana Juice     3 12.04        <NA>
    ## 562    Kombucha Ginger Kevita 15.2 oz     3 12.04        <NA>
    ## 563   Kombucha Rasp Lemon Kevita 15.2     3 12.04        <NA>
    ## 564        Lipton Pure Leaf Sweet Tea     5 12.04        <NA>
    ## 565   Soda Ginger Ale Schweppes 20 Oz     5 12.04        <NA>
    ## 566      Soda Mountain Dew Zero 20 Oz     5 12.04        <NA>
    ## 567           Soda Orange Crush 20 Oz     5 12.04        <NA>
    ## 568   Tea Peach Pure Leaf Lipton 18.5     5 12.04        <NA>
    ## 569  Tea Sweet W/ Le Pure Leaf Lipton     5 12.04        <NA>
    ## 570  Gatorade Gatorlyte Zero Fruit Pu     3 12.04        <NA>
    ## 571     Yerba Mate Bluephoria 15.5 oz     3 12.04        <NA>
    ## 572  Yerba Mate Peach Revival 15.5 oz     3 12.04        <NA>
    ## 573               Gatorade Blue 28 oz     4 12.04        <NA>
    ## 574  Gatorade Galcier Freeze Zero G 2     4 12.04        <NA>
    ## 575  Juice Orange Homestyle Tropicana     4 12.04        <NA>
    ## 576  Juice Orange Premium Topicana 11     4 12.04        <NA>
    ## 577  Juice Raspberry Lemonade Tropica     4 12.04        <NA>
    ## 578   Energy Blue Crush Celsius 16 oz     3 12.04        <NA>
    ## 579    Energy Artic Vibe Celsius 12oz     3 12.04        <NA>
    ## 580  Energy Blue Raz Lemonade Celsius     3 12.04        <NA>
    ## 581   Energy Fast Twitch Cool Blue 12     3 12.04        <NA>
    ## 582  Energy Fast Twitch Watermelon St     3 12.04        <NA>
    ## 583  Energy Fuji Apple Pear Celsius 1     3 12.04        <NA>
    ## 584            Water Gatorade 33.8 oz     3 12.04        <NA>
    ## 585           Soda Mountain Dew 20 Oz     4 12.04        <NA>
    ## 586      Soda Pepsi Wild Cherry 20 Oz     4 12.04        <NA>
    ## 587             Soda Pepsi  Zero 20oz     4 12.04        <NA>
    ## 588           Soda Rootbeer Mug 20 oz     4 12.04        <NA>
    ## 589    Tea Iced Rasberry Lipton 16 oz     4 12.04        <NA>
    ## 590  Tea Tea & Lemon Pure Leaf Lipton     4 12.04        <NA>
    ## 591  Starbucks Doubleshot Energy Moch     2 12.04        <NA>
    ## 592          Juice Apple Dole 15.2 oz     4 12.04        <NA>
    ## 593    Frappuccino Caramel SB 13.7 oz     2 12.04        <NA>
    ## 594      Frappuccino Mocha 13.7 Oz SB     2 12.04        <NA>
    ## 595    Frappuccino Vanilla SB 13.7 Oz     2 12.04        <NA>
    ## 596         Juice Naked Green Machine     2 12.04        <NA>
    ## 597        Juice Naked PNCLDA 15.2 Oz     2 12.04        <NA>
    ## 598   Kombucha Pineapple Peach Kevita     2 12.04        <NA>
    ## 599   Juice Lively Lemonade Tropicana     3 12.04        <NA>
    ## 600  Gatorade Propel Berry Water 1 lt     3 12.04        <NA>
    ## 601  Gatorade Propel Grape Water 1 lt     3 12.04        <NA>
    ## 602  Tea Golden Oolong Unsweet Ito En     2 12.04        <NA>
    ## 603  Tea Green Matcha Milk Ito En 11.     2 12.04        <NA>
    ## 604  Cold Brew Chocolate Cream Starbu     2 12.04        <NA>
    ## 605  Energy Passion Orange Guva Odyss     2 12.04        <NA>
    ## 606  Energy Revive Prickly Pear Odyss     2 12.04        <NA>
    ## 607  Juice Fuji Apple Red Jacket 12oz     2 12.04        <NA>
    ## 608   Soda Rootbeer Zero Sugar Mug 20     3 12.04        <NA>
    ## 609  Tea Blackberry Pure Leaf 18.5 oz     3 12.04        <NA>
    ## 610      Tea Light Peach Lipton 20 oz     3 12.04        <NA>
    ## 611  Tea Pure Leaf Zero Sugar Sweet T     3 12.04        <NA>
    ## 612    Ocean Spray Cranberry Cocktail     3 12.04        <NA>
    ## 613    Energy StrawLemon Celsius 12oz     2 12.04        <NA>
    ## 614  Tea Green Peach Mango Celsius 12     2 12.04        <NA>
    ## 615   Tea Green Rasp Acai  Celsius 12     2 12.04        <NA>
    ## 616  Mountain Dew Kickstart Black Che     2 12.04        <NA>
    ## 617     Glacier Freeze Gatorade 28 oz     2 12.04        <NA>
    ## 618        Juice Grape Tropicana 11oz     2 12.04        <NA>
    ## 619  Juice Zero Summer Splash Punch 1     2 12.04        <NA>
    ## 620              Water Gatorade 700ml     2 12.04        <NA>
    ## 621   Soda Pineapple Jarritos 12.5 oz     2 12.04        <NA>
    ## 622    Muscle Milk PP Chocolate 14 oz     1 12.04        <NA>
    ## 623      Soda Starry Lemon Lime 20 oz     2 12.04        <NA>
    ## 624  Tea Unsweet Black W Lemon 18.5 o     2 12.04        <NA>
    ## 625  Tea Unsweet Green Lipton 18.5 oz     2 12.04        <NA>
    ## 626  Gatorade Gatorlyte MixBerry 20 o     1 12.04        <NA>
    ## 627   Gatorade Gatorlyte Orange 20 oz     1 12.04        <NA>
    ## 628  Gatorade Gatorlyte Strawberry Ki     1 12.04        <NA>
    ## 629  Gatorade Gatorlyte Zero Lemon Li     1 12.04        <NA>
    ## 630     Water Aquafina Alumitek 16 oz     2 12.04        <NA>
    ## 631  Energy Cherry Limeade Celsius 16     1 12.04        <NA>
    ## 632  Energy Dragonberry Celsius 16 oz     1 12.04        <NA>
    ## 633  Energy Mango Tango Celsius 16 oz     1 12.04        <NA>
    ## 634  Energy Orangesicle Celsius 16 oz     1 12.04        <NA>
    ## 635  Mountain Dew Kickstart Orange 16     1 12.04        <NA>
    ## 636        Gatorade Fruit Punch 28 oz     1 12.04        <NA>
    ## 637         Gatorade Lemon Lime 28 oz     1 12.04        <NA>
    ## 638              Gatorade Orange 28oz     1 12.04        <NA>
    ## 639       Soda Guava Jarritos 12.5 oz     1 12.04        <NA>
    ## 640       Soda Mango Jarritos 12.5 oz     1 12.04        <NA>
    ## 641    Soda Tamarind Jarritos 12.5 oz     1 12.04        <NA>
    ## 642  Tea Sweetened With Lemon Brisk 2     1 12.04        <NA>
    ## 643          Juice Lemonade Dole 20oz     1 12.04        <NA>
    ## 644         Juice Orange Dole 15.2 oz     1 12.04        <NA>
    ## 645  Juice Ocean Spray Cranbe Grape 1     1 12.04        <NA>
    ## 646  Yogurt Straw Banana Chobani Drin     6 12.04        <NA>
    ## 647    Cornell Low Fat Chocolate Milk     7 12.04        <NA>
    ## 648        Cornell 2% Milk (70000380)     5 12.04        <NA>
    ## 649        Cornell 2% Milk (70000381)     8 12.04        <NA>
    ## 650    Milk Chocolate LF Cornell 8 Oz     8 12.04        <NA>
    ## 651   Yogurt Flip Peanut Butter Dream     5 12.04        <NA>
    ## 652    Yogurt Blueberry Greek Chobani     5 12.04        <NA>
    ## 653   Yogurt Mango Chobani Drink 7 oz     3 12.04        <NA>
    ## 654  Yogurt 0% Fat Vanilla Greek Oiko     4 12.04        <NA>
    ## 655                Cornell Whole Milk     5 12.04        <NA>
    ## 656  Yogurt Flip Almond Coco Loco Cho     3 12.04        <NA>
    ## 657  Yogurt Mixed Berry Chobani Drink     2 12.04        <NA>
    ## 658        Yogurt Mango Greek Chobani     2 12.04        <NA>
    ## 659        Yogurt Plain Greek Chobani     2 12.04        <NA>
    ## 660    Yogurt Raspberry Greek Chobani     2 12.04        <NA>
    ## 661  Yogurt Strawberry Banana Greek C     2 12.04        <NA>
    ## 662             8 oz Vanilla Soy Silk     2 12.04        <NA>
    ## 663  Yogurt Black Cherry Greek Choban     1 12.04        <NA>
    ## 664  Jelly Konjac Mango Pineapple Tas     4 12.04        <NA>
    ## 665   Bar Peanut Butter Builders Clif     3 12.04        <NA>
    ## 666  Bar Cookie & Cream Quest 2.12 oz     2 12.04        <NA>
    ## 667         Bar S'Mores Quest 2.12 oz     2 12.04        <NA>
    ## 668  Bar CHOC Mint Builders Clif bar2     2 12.04        <NA>
    ## 669  Jelly Konjac Double Berry Tastel     2 12.04        <NA>
    ## 670        Bar Chocolate Chip Clifbar     2 12.04        <NA>
    ## 671    Bar Blueberry Crisp Clif 2.4oz     2 12.04        <NA>
    ## 672  Bar That's It Apple+Strawberry 1     2 12.04        <NA>
    ## 673  Bar Choc Chip Cookie Dough Quest     1 12.04        <NA>
    ## 674  Cookie Birthday Cake Lenny & Lar     1 12.04        <NA>
    ## 675       Jelly Konjac Peach Tastelli     1 12.04        <NA>
    ## 676  Dark Choc Cherry Cashew Plus Kin     1 12.04        <NA>
    ## 677    Kind Cranberry Almond Plus Bar     1 12.04        <NA>
    ## 678  Bar That's It Apple+Mango 1.2 oz     1 12.04        <NA>
    ## 679                       Fruit Whole    43 12.04        <NA>
    ## 680         Fresh Cut Melon Fruit Cup     4 12.04        <NA>
    ## 681    Fresh Cut Watermelon Fruit Cup     3 12.04        <NA>
    ## 682     Fresh Cut Pineapple Fruit Cup     1 12.04        <NA>
    ## 683    Chip Voodoo Limited Zapps 2 oz     3 12.04        <NA>
    ## 684  Nuts Almonds Honey 1.5 oz Sahale     1 12.04        <NA>
    ## 685  Chip Potato Kettle Honey BBQ 2oz     1 12.04        <NA>
    ## 686      Chip Funky Fusion Dirty 2 oz     1 12.04        <NA>
    ## 687        Chip Maui Onion Dirty 2 oz     1 12.04        <NA>
    ## 688  Chip Potato Dirty BBQ Mesquite 2     1 12.04        <NA>
    ## 689  Chip Salt & Malt Vinegar Dirty 2     1 12.04        <NA>
    ## 690   Chips Cracked Pepper & Sea Salt     1 12.04        <NA>
    ## 691        Utz GoodHealth Veggie Chip     1 12.04        <NA>
    ## 692                  Utz Regular Chip     1 12.04        <NA>
    ## 693       Chewy Marshmallow GF 2.1 oz     3 12.04        <NA>
    ## 694  GF Sweet Street Choloate Brownie     2 12.04        <NA>
    ## 695                  MochiCookCrm1.5o     5 12.04        <NA>
    ## 696  Candy Dark Chocolate Awake Bar 1     1 12.04        <NA>
    ## 697              Orbit Wintermint Gum     1 12.04        <NA>
    ## 698                      Muffin Jumbo    34 12.04        <NA>
    ## 699                            Cookie    36 12.04        <NA>
    ## 700                    Croissant Choc     4 12.04        <NA>
    ## 701                Croissant Straw CC     4 12.04        <NA>
    ## 702                 Croissant Blue CC     2 12.04        <NA>
    ## 703                      Cinnamon Bun     2 12.04        <NA>
    ## 704                   Plain Croissant     2 12.04        <NA>
    ## 705                       Coffee Cake     2 12.04        <NA>
    ## 706                    BowlMedProtein     3 12.04 grab 'n' go
    ## 707                    BowlSouthChick     3 12.04 grab 'n' go
    ## 708                  BowlChickAlfrPen     1 12.04 grab 'n' go
    ## 709                  BowlMexicanChick     1 12.04 grab 'n' go
    ## 710                  GF ChicCaesarSld     3 12.04 grab 'n' go
    ## 711                    GF Turkey Sand     3 12.04 grab 'n' go
    ## 712                  GFSunButterJelly     3 12.04 grab 'n' go
    ## 713                      PBJ on Wheat    12 12.04 grab 'n' go
    ## 714                      Alaskan Roll     2 12.04 grab 'n' go
    ## 715                Golden Dragon Roll     2 12.04 grab 'n' go
    ## 716             Hawaiian Volcano Roll     1 12.04 grab 'n' go
    ## 717               Tempura Crunch Roll     1 12.04 grab 'n' go
    ## 718               Tempura Shrimp Roll     1 12.04 grab 'n' go
    ## 719                          TSA Roll     1 12.04 grab 'n' go
    ## 720                   Hawaiian Sunset     1 12.04 grab 'n' go
    ## 721                      Avocado Roll     1 12.04 grab 'n' go
    ## 722                   California Roll     1 12.04 grab 'n' go
    ## 723  Sandwich Prosciutto & Mozzarella     4 12.04 grab 'n' go
    ## 724              Wrap Buffalo Chicken     3 12.04 grab 'n' go
    ## 725  Sandwich Black Forrest Ham & Swi     2 12.04 grab 'n' go
    ## 726               Wrap Chicken Caesar     2 12.04 grab 'n' go
    ## 727      Sandwich Corned Beef & Swiss     1 12.04 grab 'n' go
    ## 728        Quesadilla Deluxe Trillium   184 12.03     mexican
    ## 729                 Grilled Hamburger    97 12.03       grill
    ## 730             Fried Chicken Tenders    97 12.03       grill
    ## 731     Burrito Una Mano Trillium BYO    64 12.03     mexican
    ## 732                      French Fries   134 12.03       grill
    ## 733                 Quesadilla Cheese    17 12.03     mexican
    ## 734   Grilled Chicken Breast Sandwich    14 12.03       grill
    ## 735  Trillium Grill Impossible Burger     9 12.03       grill
    ## 736                Sweet Potato Fries    26 12.03       grill
    ## 737              Seared Salmon Burger     7 12.03       grill
    ## 738              ADD Beef Patty $2.99    15 12.03       grill
    ## 739                 Black Bean Burger     4 12.03       grill
    ## 740                ADD Chicken Breast     9 12.03       grill
    ## 741         ADD Burger Salmon Grilled     2 12.03       grill
    ## 742                        ADD Cheese    13 12.03       grill
    ## 743                      Add Egg $.99     4 12.03       grill
    ## 744                 1 Entree + 1 Side   199 12.03         wok
    ## 745                 1 Entree + 2 Side    81 12.03         wok
    ## 746                Bowl Ramen Chicken    76 12.03       ramen
    ## 747               2 Entrees + 2 Sides    22 12.03         wok
    ## 748                   Bowl Ramen Tofu    21 12.03       ramen
    ## 749           Side Vegetarian Lo Mein     8 12.03         wok
    ## 750       Side Vegetable Spring Rolls     4 12.03         wok
    ## 751          Side White or Brown Rice     6 12.03         wok
    ## 752   Side Vegetarian Fried Rice with     3 12.03         wok
    ## 753            Side Fried Spring Roll     1 12.03         wok
    ## 754                   Side Vegetables     1 12.03         wok
    ## 755                Add Extra Toppings     1 12.03       ramen
    ## 756       Create Your Pasta Bowl MEAT   116 12.03     italian
    ## 757        Create Your Pasta Bowl VEG    28 12.03     italian
    ## 758               Pizza with Toppings    39 12.03     italian
    ## 759                      Pizza Cheese    20 12.03     italian
    ## 760                    Add Extra Meat    21 12.03     italian
    ## 761                 Burrito Breakfast    72 12.03        <NA>
    ## 762               Small French Omelet    57 12.03        <NA>
    ## 763  Egg Cheese Sausage Breakfast San    36 12.03        <NA>
    ## 764  Egg Cheese Bacon Breakfast Sandw    33 12.03        <NA>
    ## 765              Grand Slam Breakfast    13 12.03        <NA>
    ## 766                         Add Bacon    22 12.03        <NA>
    ## 767                          Two Eggs    11 12.03        <NA>
    ## 768               Trillium Home Fries     2 12.03        <NA>
    ## 769                    Pancake Single     2 12.03        <NA>
    ## 770                    2 Slices Toast     2 12.03        <NA>
    ## 771                             Toast     1 12.03        <NA>
    ## 772                          PC Jelly     1 12.03        <NA>
    ## 773                  Burrito Bowl BYO   115 12.03     mexican
    ## 774                       Single Taco     4 12.03     mexican
    ## 775                    Side Guacamole     6 12.03     mexican
    ## 776       Add Extra Toppings Una Mano     3 12.03     mexican
    ## 777                        Side Salsa     1 12.03     mexican
    ## 778                        Soup 12 oz    53 12.03   salad bar
    ## 779                         8 oz Soup    51 12.03   salad bar
    ## 780                Salad by the Pound    55 12.03   salad bar
    ## 781                   Coffee 16 oz SB    38 12.03        <NA>
    ## 782               Soda Fountain 24 oz    37 12.03        <NA>
    ## 783                   Coffee 12 oz SB    27 12.03        <NA>
    ## 784               Soda Fountain 16 oz    25 12.03        <NA>
    ## 785                     Hot Tea 20 oz     4 12.03        <NA>
    ## 786                  Side Potato Tots    17 12.03        <NA>
    ## 787                Open Miscellaneous     9 12.03        <NA>
    ## 788           Add Extra Protein $2.99     1 12.03        <NA>
    ## 789              Water Aquafina 20 oz    21 12.03        <NA>
    ## 790  Yerba Mate Enlighten Mint 15.5 o     9 12.03        <NA>
    ## 791             Soda Pepsi  Zero 20oz    14 12.03        <NA>
    ## 792             Soda Pepsi Diet 20 Oz    13 12.03        <NA>
    ## 793      Water Life WTR Immune 700 ML     9 12.03        <NA>
    ## 794  Energy Blue Raz Lemonade Celsius     8 12.03        <NA>
    ## 795   Tea Green Rasp Acai  Celsius 12     8 12.03        <NA>
    ## 796  Tea Jasmine Green Unsweet Ito En     7 12.03        <NA>
    ## 797          Juice Naked Mighty Mango     5 12.03        <NA>
    ## 798  Gatorade Propel  Kiwi Straw Wate     8 12.03        <NA>
    ## 799    Frappuccino Vanilla SB 13.7 Oz     4 12.03        <NA>
    ## 800  Gatorade Galcier Freeze Zero G 2     6 12.03        <NA>
    ## 801  Energy Mango PassFruit Celsius 1     5 12.03        <NA>
    ## 802     Yerba Mate Bluephoria 15.5 oz     4 12.03        <NA>
    ## 803  Yerba Mate Peach Revival 15.5 oz     4 12.03        <NA>
    ## 804     Muscle Milk KO Chocolate 14oz     3 12.03        <NA>
    ## 805  Tea Sweet W/ Le Pure Leaf Lipton     6 12.03        <NA>
    ## 806         Gatorade Lemon Lime 28 oz     5 12.03        <NA>
    ## 807  Juice Orange Homestyle Tropicana     5 12.03        <NA>
    ## 808    Energy Artic Vibe Celsius 12oz     4 12.03        <NA>
    ## 809  Energy Fuji Apple Pear Celsius 1     4 12.03        <NA>
    ## 810    Energy StrawLemon Celsius 12oz     4 12.03        <NA>
    ## 811  Tea Green Peach Mango Celsius 12     4 12.03        <NA>
    ## 812           Juice Naked Berry Blast     3 12.03        <NA>
    ## 813         Juice Naked Green Machine     3 12.03        <NA>
    ## 814          Juice Protein Zone Naked     3 12.03        <NA>
    ## 815   Kombucha Rasp Lemon Kevita 15.2     3 12.03        <NA>
    ## 816                  Soda Pepsi 20 Oz     5 12.03        <NA>
    ## 817           Soda Rootbeer Mug 20 oz     5 12.03        <NA>
    ## 818    Tea Iced Rasberry Lipton 16 oz     5 12.03        <NA>
    ## 819   Tea Peach Pure Leaf Lipton 18.5     5 12.03        <NA>
    ## 820  Tea Unsweet Green Lipton 18.5 oz     5 12.03        <NA>
    ## 821       Yerba Mate Tropical 15.5 oz     3 12.03        <NA>
    ## 822     Tea Black Milk Ito En 11.8 oz     3 12.03        <NA>
    ## 823               Gatorade Blue 28 oz     4 12.03        <NA>
    ## 824   Juice Lively Lemonade Tropicana     4 12.03        <NA>
    ## 825  Juice Orange Premium Topicana 11     4 12.03        <NA>
    ## 826  Cold Brew Chocolate Cream Starbu     3 12.03        <NA>
    ## 827  Red Jacket Strawberry Apple Juic     3 12.03        <NA>
    ## 828  Sparkling lemonade Strawberry Ke     4 12.03        <NA>
    ## 829       Soda Mango Jarritos 12.5 oz     4 12.03        <NA>
    ## 830                 Water Aquafina 1L     4 12.03        <NA>
    ## 831      Soda Mountain Dew Zero 20 Oz     4 12.03        <NA>
    ## 832  Tea Blackberry Pure Leaf 18.5 oz     4 12.03        <NA>
    ## 833  Tea Sweetened With Lemon Brisk 2     4 12.03        <NA>
    ## 834          Juice Apple Dole 15.2 oz     4 12.03        <NA>
    ## 835          Juice Naked Blue Machine     2 12.03        <NA>
    ## 836           Naked Red Machine Juice     2 12.03        <NA>
    ## 837     Naked Strawberry Banana Juice     2 12.03        <NA>
    ## 838    Kombucha Ginger Kevita 15.2 oz     2 12.03        <NA>
    ## 839   Kombucha Pineapple Peach Kevita     2 12.03        <NA>
    ## 840       Gatorade Berry Zero G 28 oz     3 12.03        <NA>
    ## 841              Gatorade Orange 28oz     3 12.03        <NA>
    ## 842     Glacier Freeze Gatorade 28 oz     3 12.03        <NA>
    ## 843        Juice AppleTropicana 11 oz     3 12.03        <NA>
    ## 844  Gatorade Propel Berry Water 1 lt     3 12.03        <NA>
    ## 845  Gatorade Gatorlyte Zero Fruit Pu     2 12.03        <NA>
    ## 846    Yerba Mate Revel Berry 15.5 oz     2 12.03        <NA>
    ## 847      Starbucks Double Shot 6.5 Oz     2 12.03        <NA>
    ## 848       Soda Guava Jarritos 12.5 oz     3 12.03        <NA>
    ## 849     Water Aquafina Alumitek 16 oz     4 12.03        <NA>
    ## 850  Energy Fruit Burst Celsius 16 oz     2 12.03        <NA>
    ## 851  Energy Mango Tango Celsius 16 oz     2 12.03        <NA>
    ## 852  Energy Passion Orange Guva Odyss     2 12.03        <NA>
    ## 853        Lipton Pure Leaf Sweet Tea     3 12.03        <NA>
    ## 854           Soda Orange Crush 20 Oz     3 12.03        <NA>
    ## 855      Soda Pepsi Wild Cherry 20 Oz     3 12.03        <NA>
    ## 856  Tea Unsweetened Pure Leaf Lipton     3 12.03        <NA>
    ## 857   Energy Fast Twitch Cool Blue 12     2 12.03        <NA>
    ## 858  Energy Fast Twitch Watermelon St     2 12.03        <NA>
    ## 859  Juice Ocean Spray Cranbe Grape 1     3 12.03        <NA>
    ## 860            Water Gatorade 33.8 oz     2 12.03        <NA>
    ## 861        Gatorade Fruit Punch 28 oz     2 12.03        <NA>
    ## 862     Glacier Cherry Gatorade 28 oz     2 12.03        <NA>
    ## 863  Juice Raspberry Lemonade Tropica     2 12.03        <NA>
    ## 864  Gatorade Propel Grape Water 1 lt     2 12.03        <NA>
    ## 865   Soda Pineapple Jarritos 12.5 oz     2 12.03        <NA>
    ## 866    Soda Tamarind Jarritos 12.5 oz     2 12.03        <NA>
    ## 867          Muscle Milk Choc PB 14oz     1 12.03        <NA>
    ## 868  Muscle Milk P40 Strawberry Cream     1 12.03        <NA>
    ## 869  Muscle Milk PROSRS 40 Intense Va     1 12.03        <NA>
    ## 870      Soda Starry Lemon Lime 20 oz     2 12.03        <NA>
    ## 871  Tea Tea & Lemon Pure Leaf Lipton     2 12.03        <NA>
    ## 872  Starbucks Doubleshot Energy Moch     1 12.03        <NA>
    ## 873   Starbucks Doubleshot Energy Van     1 12.03        <NA>
    ## 874          Juice Lemonade Dole 20oz     2 12.03        <NA>
    ## 875    Frappuccino Caramel SB 13.7 oz     1 12.03        <NA>
    ## 876     Frappuccino Coffee 13.7 oz SB     1 12.03        <NA>
    ## 877        Juice Naked PNCLDA 15.2 Oz     1 12.03        <NA>
    ## 878    Soda Mandarin Jarritos 12.5 oz     2 12.03        <NA>
    ## 879   Energy Triple Shot Dark Caramel     1 12.03        <NA>
    ## 880  Gatorade Gatorlyte Cherry Lime 2     1 12.03        <NA>
    ## 881  Gatorade Gatorlyte Glacier Freez     1 12.03        <NA>
    ## 882  Gatorade Gatorlyte MixBerry 20 o     1 12.03        <NA>
    ## 883  Gatorade Gatorlyte Zero Lemon Li     1 12.03        <NA>
    ## 884          Yerba Mate Lemon 15.5 oz     1 12.03        <NA>
    ## 885  Tea Golden Oolong Unsweet Ito En     1 12.03        <NA>
    ## 886  Cold Brew Vanilla Sweet Cream St     1 12.03        <NA>
    ## 887  Energy 222 Blue Raspberry Odysse     1 12.03        <NA>
    ## 888   Energy Blue Crush Celsius 16 oz     1 12.03        <NA>
    ## 889  Energy Cherry Limeade Celsius 16     1 12.03        <NA>
    ## 890  Energy Orangesicle Celsius 16 oz     1 12.03        <NA>
    ## 891  Juice Joes Lemonade  Red Jacket1     1 12.03        <NA>
    ## 892     Rockstar Pure Zero Silver Ice     1 12.03        <NA>
    ## 893    Energy Fast Twitch Grape 12 oz     1 12.03        <NA>
    ## 894  Mountain Dew Kickstart Orange 16     1 12.03        <NA>
    ## 895      Cider Apple Red Jacket 12 oz     1 12.03        <NA>
    ## 896         Gatorade Zero Grape 28 oz     1 12.03        <NA>
    ## 897  Juice Zero Summer Splash Punch 1     1 12.03        <NA>
    ## 898              Water Gatorade 700ml     1 12.03        <NA>
    ## 899   Soda Ginger Ale Schweppes 20 Oz     1 12.03        <NA>
    ## 900           Soda Mountain Dew 20 Oz     1 12.03        <NA>
    ## 901   Soda Rootbeer Zero Sugar Mug 20     1 12.03        <NA>
    ## 902      Tea Light Peach Lipton 20 oz     1 12.03        <NA>
    ## 903  Tea Pure Leaf Zero Sugar Sweet T     1 12.03        <NA>
    ## 904  Tea Unsweet Black W Lemon 18.5 o     1 12.03        <NA>
    ## 905    Milk Chocolate LF Cornell 8 Oz    12 12.03        <NA>
    ## 906  Yogurt 0% Fat Vanilla Greek Oiko     5 12.03        <NA>
    ## 907  Yogurt Straw Banana Chobani Drin     3 12.03        <NA>
    ## 908        Cornell 2% Milk (70000380)     3 12.03        <NA>
    ## 909    Cornell Low Fat Chocolate Milk     3 12.03        <NA>
    ## 910    Yogurt Blueberry Greek Chobani     4 12.03        <NA>
    ## 911   Yogurt Flip Peanut Butter Dream     3 12.03        <NA>
    ## 912    Yogurt Raspberry Greek Chobani     3 12.03        <NA>
    ## 913  Yogurt Flip Almond Coco Loco Cho     2 12.03        <NA>
    ## 914        Yogurt Plain Greek Chobani     2 12.03        <NA>
    ## 915             8 oz Vanilla Soy Silk     2 12.03        <NA>
    ## 916   Yogurt Mango Chobani Drink 7 oz     1 12.03        <NA>
    ## 917  Yogurt Mixed Berry Chobani Drink     1 12.03        <NA>
    ## 918        Cornell 2% Milk (70000381)     2 12.03        <NA>
    ## 919  Yogurt Black Cherry Greek Choban     1 12.03        <NA>
    ## 920        Yogurt Peach Greek Chobani     1 12.03        <NA>
    ## 921           Silk Chocolate Soy Milk     1 12.03        <NA>
    ## 922        Cornell Dairy Mango Yogurt     1 12.03        <NA>
    ## 923                Cornell Whole Milk     1 12.03        <NA>
    ## 924    Bar Crunchy Peanut Butter Clif     3 12.03        <NA>
    ## 925  Bar Choc Chip Cookie Dough Quest     1 12.03        <NA>
    ## 926   Bar Frosted Birthday Cake Quest     1 12.03        <NA>
    ## 927         Bar S'Mores Quest 2.12 oz     1 12.03        <NA>
    ## 928  Cookie Birthday Cake Lenny & Lar     1 12.03        <NA>
    ## 929  Cookie Peanut Butter Lenny & Lar     1 12.03        <NA>
    ## 930  Cookie Wht/Choc Mac Lenny & Larr     1 12.03        <NA>
    ## 931   Bar Peanut Butter Builders Clif     1 12.03        <NA>
    ## 932  Jelly Konjac Double Berry Tastel     1 12.03        <NA>
    ## 933       Jelly Konjac Peach Tastelli     1 12.03        <NA>
    ## 934  Dark Choc Cherry Cashew Plus Kin     1 12.03        <NA>
    ## 935    Kind Cranberry Almond Plus Bar     1 12.03        <NA>
    ## 936    Bar Blueberry Crisp Clif 2.4oz     1 12.03        <NA>
    ## 937  Bar That's It Apple+Strawberry 1     1 12.03        <NA>
    ## 938                       Fruit Whole    52 12.03        <NA>
    ## 939   Chip Sour Cream And Onion Dirty     4 12.03        <NA>
    ## 940        Chip Baked Jax  Utz 1.5 oz     4 12.03        <NA>
    ## 941     Chip Kettle Salt Vinegar 2 oz     2 12.03        <NA>
    ## 942  Chip Potato Kettle Honey BBQ 2oz     2 12.03        <NA>
    ## 943            Chips Dirty Sea Salted     2 12.03        <NA>
    ## 944  Chip Salt & Malt Vinegar Dirty 2     1 12.03        <NA>
    ## 945   Chips Cracked Pepper & Sea Salt     1 12.03        <NA>
    ## 946        Jalapeno Heat Chips Kosher     1 12.03        <NA>
    ## 947          Utz Barbecue Potato Chip     1 12.03        <NA>
    ## 948        Utz GoodHealth Veggie Chip     1 12.03        <NA>
    ## 949           Utz Salt N Vinegar Chip     1 12.03        <NA>
    ## 950    Fresh Cut Watermelon Fruit Cup     5 12.03        <NA>
    ## 951         Fresh Cut Melon Fruit Cup     3 12.03        <NA>
    ## 952       Chewy Marshmallow GF 2.1 oz     5 12.03        <NA>
    ## 953  GF Sweet Street Choloate Brownie     4 12.03        <NA>
    ## 954              Orbit Wintermint Gum     3 12.03        <NA>
    ## 955  Candy Dark Chocolate Awake Bar 1     2 12.03        <NA>
    ## 956    Candy CHOC Cara Caff Bar Awake     1 12.03        <NA>
    ## 957  Ice Cream Mochi Double Chocolate     1 12.03        <NA>
    ## 958               Wrap Chicken Caesar     7 12.03 grab 'n' go
    ## 959  Sandwich Crispy Chicken Milanese     4 12.03 grab 'n' go
    ## 960  Sandwich Black Forrest Ham & Swi     3 12.03 grab 'n' go
    ## 961              Wrap Buffalo Chicken     3 12.03 grab 'n' go
    ## 962              Salad Chicken Caesar     2 12.03 grab 'n' go
    ## 963      Sandwich Corned Beef & Swiss     1 12.03 grab 'n' go
    ## 964                          TSA Roll     2 12.03 grab 'n' go
    ## 965                Golden Dragon Roll     2 12.03 grab 'n' go
    ## 966                   Spicy Tuna Roll     2 12.03 grab 'n' go
    ## 967             Hawaiian Volcano Roll     1 12.03 grab 'n' go
    ## 968               Tempura Crunch Roll     1 12.03 grab 'n' go
    ## 969               Tempura Shrimp Roll     1 12.03 grab 'n' go
    ## 970                   Hawaiian Sunset     1 12.03 grab 'n' go
    ## 971                            Cookie    31 12.03        <NA>
    ## 972                      Muffin Jumbo    24 12.03        <NA>
    ## 973                Croissant Straw CC     4 12.03        <NA>
    ## 974                    Croissant Choc     2 12.03        <NA>
    ## 975                      Cinnamon Bun     2 12.03        <NA>
    ## 976                   Plain Croissant     2 12.03        <NA>
    ## 977                 Croissant Blue CC     1 12.03        <NA>
    ## 978                       Coffee Cake     1 12.03        <NA>
    ## 979                    BowlSouthChick     3 12.03 grab 'n' go
    ## 980                  BowlMexicanChick     2 12.03 grab 'n' go
    ## 981                  BowlSesAsianNood     1 12.03 grab 'n' go
    ## 982                      PBJ on Wheat     5 12.03 grab 'n' go
    ## 983                  GF ChicCaesarSld     1 12.03 grab 'n' go
    ## 984                    GF Turkey Sand     1 12.03 grab 'n' go
    ## 985        Quesadilla Deluxe Trillium   144 12.02     mexican
    ## 986                 Grilled Hamburger    97 12.02       grill
    ## 987     Burrito Una Mano Trillium BYO    48 12.02     mexican
    ## 988             Fried Chicken Tenders    60 12.02       grill
    ## 989                      French Fries   101 12.02       grill
    ## 990                 Quesadilla Cheese    16 12.02     mexican
    ## 991              Seared Salmon Burger     9 12.02       grill
    ## 992                Sweet Potato Fries    23 12.02       grill
    ## 993              ADD Beef Patty $2.99    19 12.02       grill
    ## 994                 Black Bean Burger     6 12.02       grill
    ## 995   Grilled Chicken Breast Sandwich     6 12.02       grill
    ## 996                ADD Chicken Breast     8 12.02       grill
    ## 997                        ADD Cheese    10 12.02       grill
    ## 998               Add Sausage 2 Patty     2 12.02       grill
    ## 999                 1 Entree + 1 Side   169 12.02         wok
    ## 1000               Bowl Ramen Chicken    89 12.02       ramen
    ## 1001                1 Entree + 2 Side    69 12.02         wok
    ## 1002              2 Entrees + 2 Sides    16 12.02         wok
    ## 1003                  Bowl Ramen Tofu    19 12.02       ramen
    ## 1004                     1 Wok Entree     9 12.02         wok
    ## 1005                  Bowl Ramen Pork     5 12.02        <NA>
    ## 1006          Side Vegetarian Lo Mein     9 12.02         wok
    ## 1007         Side White or Brown Rice     9 12.02         wok
    ## 1008                  Side Vegetables     1 12.02         wok
    ## 1009      Create Your Pasta Bowl MEAT   128 12.02     italian
    ## 1010       Create Your Pasta Bowl VEG    24 12.02     italian
    ## 1011                     Pizza Cheese    32 12.02     italian
    ## 1012              Pizza with Toppings    22 12.02     italian
    ## 1013                   Add Extra Meat    21 12.02     italian
    ## 1014         Side Bread Pasta Station     1 12.02     italian
    ## 1015                Burrito Breakfast    79 12.02        <NA>
    ## 1016              Small French Omelet    44 12.02        <NA>
    ## 1017             Grand Slam Breakfast    10 12.02        <NA>
    ## 1018 Egg Cheese Sausage Breakfast San    13 12.02        <NA>
    ## 1019 Egg Cheese Bacon Breakfast Sandw    11 12.02        <NA>
    ## 1020                        Add Bacon    15 12.02        <NA>
    ## 1021                         Two Eggs    13 12.02        <NA>
    ## 1022              Trillium Home Fries     3 12.02        <NA>
    ## 1023                   Pancake Single     3 12.02        <NA>
    ## 1024                   2 Slices Toast     3 12.02        <NA>
    ## 1025                 PC Peanut Butter     1 12.02        <NA>
    ## 1026                 Burrito Bowl BYO    87 12.02     mexican
    ## 1027                      Single Taco     4 12.02     mexican
    ## 1028                   Side Guacamole     1 12.02     mexican
    ## 1029                       Soup 12 oz    56 12.02   salad bar
    ## 1030                        8 oz Soup    40 12.02   salad bar
    ## 1031               Salad by the Pound    51 12.02   salad bar
    ## 1032              Soda Fountain 24 oz    35 12.02        <NA>
    ## 1033                  Coffee 12 oz SB    31 12.02        <NA>
    ## 1034              Soda Fountain 16 oz    31 12.02        <NA>
    ## 1035                  Coffee 16 oz SB    22 12.02        <NA>
    ## 1036                    Hot Tea 20 oz     8 12.02        <NA>
    ## 1037                        Hot Cocoa     3 12.02        <NA>
    ## 1038                 Side Potato Tots    13 12.02        <NA>
    ## 1039               Open Miscellaneous     7 12.02        <NA>
    ## 1040          Add Extra Protein $2.99     1 12.02        <NA>
    ## 1041   Yerba Mate Revel Berry 15.5 oz    13 12.02        <NA>
    ## 1042            Soda Pepsi Diet 20 Oz    13 12.02        <NA>
    ## 1043             Water Aquafina 20 oz    12 12.02        <NA>
    ## 1044                 Soda Pepsi 20 Oz    11 12.02        <NA>
    ## 1045 Tea Jasmine Green Unsweet Ito En     6 12.02        <NA>
    ## 1046 Juice Orange Premium Topicana 11     8 12.02        <NA>
    ## 1047  Kombucha Pineapple Peach Kevita     5 12.02        <NA>
    ## 1048              Gatorade Blue 28 oz     7 12.02        <NA>
    ## 1049 Tea Golden Oolong Unsweet Ito En     5 12.02        <NA>
    ## 1050   Energy Artic Vibe Celsius 12oz     5 12.02        <NA>
    ## 1051 Tea Green Peach Mango Celsius 12     5 12.02        <NA>
    ## 1052 Gatorade Propel Berry Water 1 lt     6 12.02        <NA>
    ## 1053 Yerba Mate Enlighten Mint 15.5 o     4 12.02        <NA>
    ## 1054   Tea Iced Rasberry Lipton 16 oz     6 12.02        <NA>
    ## 1055  Juice Lively Lemonade Tropicana     5 12.02        <NA>
    ## 1056 Energy Fast Twitch Watermelon St     4 12.02        <NA>
    ## 1057   Energy StrawLemon Celsius 12oz     4 12.02        <NA>
    ## 1058        Juice Naked Green Machine     3 12.02        <NA>
    ## 1059         Juice Naked Mighty Mango     3 12.02        <NA>
    ## 1060     Water Life WTR Immune 700 ML     4 12.02        <NA>
    ## 1061                Water Aquafina 1L     5 12.02        <NA>
    ## 1062  Soda Ginger Ale Schweppes 20 Oz     5 12.02        <NA>
    ## 1063 Gatorade Gatorlyte Zero Fruit Pu     3 12.02        <NA>
    ## 1064         Yerba Mate Lemon 15.5 oz     3 12.02        <NA>
    ## 1065    Tea Black Milk Ito En 11.8 oz     3 12.02        <NA>
    ## 1066        Gatorade Lemon Lime 28 oz     4 12.02        <NA>
    ## 1067 Juice Orange Homestyle Tropicana     4 12.02        <NA>
    ## 1068 Cold Brew Vanilla Sweet Cream St     3 12.02        <NA>
    ## 1069 Gatorade Propel  Kiwi Straw Wate     4 12.02        <NA>
    ## 1070 Sparkling lemonade Strawberry Ke     4 12.02        <NA>
    ## 1071 Juice Ocean Spray Cranbe Grape 1     5 12.02        <NA>
    ## 1072    Muscle Milk KO Chocolate 14oz     2 12.02        <NA>
    ## 1073   Muscle Milk PP Chocolate 14 oz     2 12.02        <NA>
    ## 1074 Muscle Milk PROSRS 40 Intense Va     2 12.02        <NA>
    ## 1075     Soda Pepsi Wild Cherry 20 Oz     4 12.02        <NA>
    ## 1076  Tea Peach Pure Leaf Lipton 18.5     4 12.02        <NA>
    ## 1077  Starbucks Doubleshot Energy Van     2 12.02        <NA>
    ## 1078         Juice Apple Dole 15.2 oz     4 12.02        <NA>
    ## 1079   Ocean Spray Cranberry Cocktail     4 12.02        <NA>
    ## 1080   Frappuccino Caramel SB 13.7 oz     2 12.02        <NA>
    ## 1081     Frappuccino Mocha 13.7 Oz SB     2 12.02        <NA>
    ## 1082          Juice Naked Berry Blast     2 12.02        <NA>
    ## 1083       Juice Naked PNCLDA 15.2 Oz     2 12.02        <NA>
    ## 1084         Juice Protein Zone Naked     2 12.02        <NA>
    ## 1085  Kombucha Rasp Lemon Kevita 15.2     2 12.02        <NA>
    ## 1086 Juice Raspberry Lemonade Tropica     3 12.02        <NA>
    ## 1087 Gatorade Gatorlyte Glacier Freez     2 12.02        <NA>
    ## 1088 Gatorade Gatorlyte MixBerry 20 o     2 12.02        <NA>
    ## 1089 Gatorade Gatorlyte Zero Lemon Li     2 12.02        <NA>
    ## 1090    Yerba Mate Bluephoria 15.5 oz     2 12.02        <NA>
    ## 1091      Yerba Mate Tropical 15.5 oz     2 12.02        <NA>
    ## 1092 Tea Green Matcha Milk Ito En 11.     2 12.02        <NA>
    ## 1093 Energy 222 Blue Raspberry Odysse     2 12.02        <NA>
    ## 1094  Energy Blue Crush Celsius 16 oz     2 12.02        <NA>
    ## 1095 Red Jacket Strawberry Apple Juic     2 12.02        <NA>
    ## 1096       Lipton Pure Leaf Sweet Tea     3 12.02        <NA>
    ## 1097          Soda Mountain Dew 20 Oz     3 12.02        <NA>
    ## 1098     Soda Starry Lemon Lime 20 oz     3 12.02        <NA>
    ## 1099 Tea Blackberry Pure Leaf 18.5 oz     3 12.02        <NA>
    ## 1100 Tea Sweetened With Lemon Brisk 2     3 12.02        <NA>
    ## 1101 Tea Sweet W/ Le Pure Leaf Lipton     3 12.02        <NA>
    ## 1102 Tea Unsweetened Pure Leaf Lipton     3 12.02        <NA>
    ## 1103    Rockstar Pure Zero Silver Ice     2 12.02        <NA>
    ## 1104         Juice Lemonade Dole 20oz     3 12.02        <NA>
    ## 1105 Energy Blue Raz Lemonade Celsius     2 12.02        <NA>
    ## 1106 Energy Fuji Apple Pear Celsius 1     2 12.02        <NA>
    ## 1107 Energy Mango PassFruit Celsius 1     2 12.02        <NA>
    ## 1108           Water Gatorade 33.8 oz     2 12.02        <NA>
    ## 1109    Water Aquafina Alumitek 16 oz     3 12.02        <NA>
    ## 1110 Gatorade Galcier Freeze Zero G 2     2 12.02        <NA>
    ## 1111             Gatorade Orange 28oz     2 12.02        <NA>
    ## 1112    Glacier Freeze Gatorade 28 oz     2 12.02        <NA>
    ## 1113       Juice AppleTropicana 11 oz     2 12.02        <NA>
    ## 1114 Gatorade Propel Grape Water 1 lt     2 12.02        <NA>
    ## 1115      Soda Guava Jarritos 12.5 oz     2 12.02        <NA>
    ## 1116         Muscle Milk Choc PB 14oz     1 12.02        <NA>
    ## 1117 Muscle Milk P40 Strawberry Cream     1 12.02        <NA>
    ## 1118          Soda Orange Crush 20 Oz     2 12.02        <NA>
    ## 1119            Soda Pepsi  Zero 20oz     2 12.02        <NA>
    ## 1120  Soda Rootbeer Zero Sugar Mug 20     2 12.02        <NA>
    ## 1121     Tea Light Peach Lipton 20 oz     2 12.02        <NA>
    ## 1122 Tea Unsweet Black W Lemon 18.5 o     2 12.02        <NA>
    ## 1123   Frappuccino Vanilla SB 13.7 Oz     1 12.02        <NA>
    ## 1124         Juice Naked Blue Machine     1 12.02        <NA>
    ## 1125          Naked Red Machine Juice     1 12.02        <NA>
    ## 1126    Naked Strawberry Banana Juice     1 12.02        <NA>
    ## 1127   Kombucha Ginger Kevita 15.2 oz     1 12.02        <NA>
    ## 1128 Gatorade Gatorlyte Strawberry Ki     1 12.02        <NA>
    ## 1129     Starbucks Double Shot 6.5 Oz     1 12.02        <NA>
    ## 1130 Energy Dragonberry Celsius 16 oz     1 12.02        <NA>
    ## 1131 Energy Mango Tango Celsius 16 oz     1 12.02        <NA>
    ## 1132 Energy Passion Orange Guva Odyss     1 12.02        <NA>
    ## 1133 Juice Joes Lemonade  Red Jacket1     1 12.02        <NA>
    ## 1134   Energy Fast Twitch Grape 12 oz     1 12.02        <NA>
    ## 1135 Mountain Dew Kickstart Black Che     1 12.02        <NA>
    ## 1136 Mountain Dew Kickstart Orange 16     1 12.02        <NA>
    ## 1137       Gatorade Fruit Punch 28 oz     1 12.02        <NA>
    ## 1138        Gatorade Zero Grape 28 oz     1 12.02        <NA>
    ## 1139    Glacier Cherry Gatorade 28 oz     1 12.02        <NA>
    ## 1140       Juice Grape Tropicana 11oz     1 12.02        <NA>
    ## 1141 Juice Zero Summer Splash Punch 1     1 12.02        <NA>
    ## 1142      Soda Mango Jarritos 12.5 oz     1 12.02        <NA>
    ## 1143  Soda Pineapple Jarritos 12.5 oz     1 12.02        <NA>
    ## 1144   Soda Tamarind Jarritos 12.5 oz     1 12.02        <NA>
    ## 1145 Tea Pure Leaf Zero Sugar Sweet T     1 12.02        <NA>
    ## 1146 Tea Tea & Lemon Pure Leaf Lipton     1 12.02        <NA>
    ## 1147 Tea Unsweet Green Lipton 18.5 oz     1 12.02        <NA>
    ## 1148        Juice Orange Dole 15.2 oz     1 12.02        <NA>
    ## 1149   Soda Mandarin Jarritos 12.5 oz     1 12.02        <NA>
    ## 1150  Yogurt Mango Chobani Drink 7 oz     6 12.02        <NA>
    ## 1151 Yogurt Flip Almond Coco Loco Cho     5 12.02        <NA>
    ## 1152  Yogurt Strawberry Greek Chobani     4 12.02        <NA>
    ## 1153 Yogurt 0% Fat Vanilla Greek Oiko     3 12.02        <NA>
    ## 1154 Yogurt Straw Banana Chobani Drin     2 12.02        <NA>
    ## 1155       Yogurt Mango Greek Chobani     2 12.02        <NA>
    ## 1156       Yogurt Peach Greek Chobani     2 12.02        <NA>
    ## 1157            8 oz Vanilla Soy Silk     2 12.02        <NA>
    ## 1158 Yogurt Mixed Berry Chobani Drink     1 12.02        <NA>
    ## 1159   Milk Chocolate LF Cornell 8 Oz     2 12.02        <NA>
    ## 1160   Cornell Low Fat Chocolate Milk     1 12.02        <NA>
    ## 1161  Yogurt Flip Peanut Butter Dream     1 12.02        <NA>
    ## 1162 Yogurt Black Cherry Greek Choban     1 12.02        <NA>
    ## 1163   Yogurt Blueberry Greek Chobani     1 12.02        <NA>
    ## 1164       Yogurt Plain Greek Chobani     1 12.02        <NA>
    ## 1165 Yogurt Strawberry Banana Greek C     1 12.02        <NA>
    ## 1166          Silk Chocolate Soy Milk     1 12.02        <NA>
    ## 1167       Cornell Dairy Mango Yogurt     1 12.02        <NA>
    ## 1168 Bar CHOC Mint Builders Clif bar2     2 12.02        <NA>
    ## 1169 Jelly Konjac Mango Pineapple Tas     2 12.02        <NA>
    ## 1170      Jelly Konjac Peach Tastelli     2 12.02        <NA>
    ## 1171   Bar Blueberry Crisp Clif 2.4oz     2 12.02        <NA>
    ## 1172 Bar Cookie & Cream Quest 2.12 oz     1 12.02        <NA>
    ## 1173 Jelly Konjac Apple Grape Tastell     1 12.02        <NA>
    ## 1174 Jelly Konjac Double Berry Tastel     1 12.02        <NA>
    ## 1175 Dark Choc Cherry Cashew Plus Kin     1 12.02        <NA>
    ## 1176   Kind Cranberry Almond Plus Bar     1 12.02        <NA>
    ## 1177 Bar That's It Apple+Strawberry 1     1 12.02        <NA>
    ## 1178 Nature Valley Peanut Butter Gran     1 12.02        <NA>
    ## 1179                      Fruit Whole    44 12.02        <NA>
    ## 1180             Orbit Sweet Mint Gum     4 12.02        <NA>
    ## 1181   Candy CHOC Cara Caff Bar Awake     2 12.02        <NA>
    ## 1182 Candy Milk Choc Caff Bar Awake 1     2 12.02        <NA>
    ## 1183             Orbit Wintermint Gum     2 12.02        <NA>
    ## 1184 Candy Dark Chocolate Awake Bar 1     1 12.02        <NA>
    ## 1185        Fresh Cut Melon Fruit Cup     3 12.02        <NA>
    ## 1186   Fresh Cut Watermelon Fruit Cup     2 12.02        <NA>
    ## 1187 Chip Salt & Malt Vinegar Dirty 2     2 12.02        <NA>
    ## 1188                 Utz Regular Chip     2 12.02        <NA>
    ## 1189       Chip Maui Onion Dirty 2 oz     1 12.02        <NA>
    ## 1190           Chips Dirty Sea Salted     1 12.02        <NA>
    ## 1191          Utz Salt N Vinegar Chip     1 12.02        <NA>
    ## 1192      Chewy Marshmallow GF 2.1 oz     1 12.02        <NA>
    ## 1193                     Muffin Jumbo    20 12.02        <NA>
    ## 1194                           Cookie    24 12.02        <NA>
    ## 1195                   Croissant Choc     5 12.02        <NA>
    ## 1196                Croissant Blue CC     2 12.02        <NA>
    ## 1197                     Cinnamon Bun     2 12.02        <NA>
    ## 1198                  Plain Croissant     2 12.02        <NA>
    ## 1199               Croissant Straw CC     1 12.02        <NA>
    ## 1200      Combo 16 oz Coffee & Muffin     1 12.02        <NA>
    ## 1201                      Coffee Cake     1 12.02        <NA>
    ## 1202                   BowlMedProtein     4 12.02 grab 'n' go
    ## 1203                 BowlMexicanChick     2 12.02 grab 'n' go
    ## 1204                 BowlChickAlfrPen     1 12.02 grab 'n' go
    ## 1205                 BowlSesAsianNood     1 12.02 grab 'n' go
    ## 1206                   BowlSouthChick     1 12.02 grab 'n' go
    ## 1207                     PBJ on Wheat     9 12.02 grab 'n' go
    ## 1208             Wrap Buffalo Chicken     5 12.02 grab 'n' go
    ## 1209 Sandwich Black Forrest Ham & Swi     2 12.02 grab 'n' go
    ## 1210 Sandwich Crispy Chicken Milanese     2 12.02 grab 'n' go
    ## 1211 Sandwich Prosciutto & Mozzarella     2 12.02 grab 'n' go
    ## 1212              Wrap Chicken Caesar     2 12.02 grab 'n' go
    ## 1213               Golden Dragon Roll     2 12.02 grab 'n' go
    ## 1214            Hawaiian Volcano Roll     1 12.02 grab 'n' go
    ## 1215              Tempura Crunch Roll     1 12.02 grab 'n' go
    ## 1216                         TSA Roll     1 12.02 grab 'n' go
    ## 1217                  California Roll     1 12.02 grab 'n' go
    ## 1218       Quesadilla Deluxe Trillium   121 11.26     mexican
    ## 1219                Grilled Hamburger    52 11.26       grill
    ## 1220            Fried Chicken Tenders    42 11.26       grill
    ## 1221    Burrito Una Mano Trillium BYO    31 11.26     mexican
    ## 1222                     French Fries    71 11.26       grill
    ## 1223             Seared Salmon Burger    12 11.26       grill
    ## 1224                Quesadilla Cheese     8 11.26     mexican
    ## 1225               Sweet Potato Fries    20 11.26       grill
    ## 1226 Trillium Grill Impossible Burger     4 11.26       grill
    ## 1227  Grilled Chicken Breast Sandwich     4 11.26       grill
    ## 1228                Black Bean Burger     3 11.26       grill
    ## 1229               ADD Chicken Breast     2 11.26       grill
    ## 1230             ADD Beef Patty $2.99     2 11.26       grill
    ## 1231                       ADD Cheese     5 11.26       grill
    ## 1232                1 Entree + 1 Side    77 11.26         wok
    ## 1233                1 Entree + 2 Side    46 11.26         wok
    ## 1234               Bowl Ramen Chicken    45 11.26       ramen
    ## 1235              2 Entrees + 2 Sides    17 11.26         wok
    ## 1236                  Bowl Ramen Tofu     6 11.26       ramen
    ## 1237          Side Vegetarian Lo Mein     3 11.26         wok
    ## 1238                     1 Wok Entree     1 11.26         wok
    ## 1239         Side White or Brown Rice     3 11.26         wok
    ## 1240                  Side Vegetables     1 11.26         wok
    ## 1241                Burrito Breakfast    41 11.26        <NA>
    ## 1242              Small French Omelet    15 11.26        <NA>
    ## 1243             Grand Slam Breakfast     7 11.26        <NA>
    ## 1244                        Add Bacon    12 11.26        <NA>
    ## 1245 Egg Cheese Sausage Breakfast San     4 11.26        <NA>
    ## 1246                         Two Eggs     5 11.26        <NA>
    ## 1247                   Pancake Single     1 11.26        <NA>
    ## 1248                   2 Slices Toast     1 11.26        <NA>
    ## 1249                            Toast     1 11.26        <NA>
    ## 1250      Create Your Pasta Bowl MEAT    29 11.26     italian
    ## 1251              Pizza with Toppings    14 11.26     italian
    ## 1252       Create Your Pasta Bowl VEG     6 11.26     italian
    ## 1253                     Pizza Cheese     6 11.26     italian
    ## 1254                   Add Extra Meat     2 11.26     italian
    ## 1255                 Burrito Bowl BYO    42 11.26     mexican
    ## 1256                      Single Taco     3 11.26     mexican
    ## 1257      Add Extra Toppings Una Mano     2 11.26     mexican
    ## 1258               Salad by the Pound    31 11.26   salad bar
    ## 1259                       Soup 12 oz    25 11.26   salad bar
    ## 1260                        8 oz Soup    24 11.26   salad bar
    ## 1261              Soda Fountain 16 oz    21 11.26        <NA>
    ## 1262              Soda Fountain 24 oz    17 11.26        <NA>
    ## 1263                  Coffee 16 oz SB    11 11.26        <NA>
    ## 1264                  Coffee 12 oz SB    12 11.26        <NA>
    ## 1265                 Side Potato Tots    12 11.26        <NA>
    ## 1266               Open Miscellaneous     2 11.26        <NA>
    ## 1267          Add Extra Protein $2.99     2 11.26        <NA>
    ## 1268             Water Aquafina 20 oz    12 11.26        <NA>
    ## 1269 Tea Jasmine Green Unsweet Ito En     6 11.26        <NA>
    ## 1270 Yerba Mate Enlighten Mint 15.5 o     5 11.26        <NA>
    ## 1271  Soda Ginger Ale Schweppes 20 Oz     8 11.26        <NA>
    ## 1272         Juice Protein Zone Naked     4 11.26        <NA>
    ## 1273 Juice Orange Premium Topicana 11     6 11.26        <NA>
    ## 1274            Soda Pepsi Diet 20 Oz     6 11.26        <NA>
    ## 1275     Frappuccino Mocha 13.7 Oz SB     3 11.26        <NA>
    ## 1276 Milk Chocolate LF BIG RED Refuel     6 11.26        <NA>
    ## 1277    Tea Black Milk Ito En 11.8 oz     3 11.26        <NA>
    ## 1278    Muscle Milk KO Chocolate 14oz     2 11.26        <NA>
    ## 1279 Tea Sweet W/ Le Pure Leaf Lipton     4 11.26        <NA>
    ## 1280         Juice Naked Mighty Mango     2 11.26        <NA>
    ## 1281 Juice Ocean Spray Cranbe Grape 1     4 11.26        <NA>
    ## 1282  Kombucha Pineapple Peach Kevita     2 11.26        <NA>
    ## 1283  Kombucha Rasp Lemon Kevita 15.2     2 11.26        <NA>
    ## 1284              Gatorade Blue 28 oz     3 11.26        <NA>
    ## 1285    Glacier Cherry Gatorade 28 oz     3 11.26        <NA>
    ## 1286       Juice AppleTropicana 11 oz     3 11.26        <NA>
    ## 1287 Tea Golden Oolong Unsweet Ito En     2 11.26        <NA>
    ## 1288                Water Aquafina 1L     3 11.26        <NA>
    ## 1289 Cold Brew Chocolate Cream Starbu     2 11.26        <NA>
    ## 1290 Energy Dragonberry Celsius 16 oz     2 11.26        <NA>
    ## 1291 Energy Mango Tango Celsius 16 oz     2 11.26        <NA>
    ## 1292     Soda Mountain Dew Zero 20 Oz     3 11.26        <NA>
    ## 1293                 Soda Pepsi 20 Oz     3 11.26        <NA>
    ## 1294  Soda Rootbeer Zero Sugar Mug 20     3 11.26        <NA>
    ## 1295  Tea Peach Pure Leaf Lipton 18.5     3 11.26        <NA>
    ## 1296 Energy Mango PassFruit Celsius 1     2 11.26        <NA>
    ## 1297  Tea Green Rasp Acai  Celsius 12     2 11.26        <NA>
    ## 1298     Water Life WTR Immune 700 ML     2 11.26        <NA>
    ## 1299        Gatorade Lemon Lime 28 oz     2 11.26        <NA>
    ## 1300        Gatorade Zero Grape 28 oz     2 11.26        <NA>
    ## 1301    Glacier Freeze Gatorade 28 oz     2 11.26        <NA>
    ## 1302  Juice Lively Lemonade Tropicana     2 11.26        <NA>
    ## 1303      Soda Mango Jarritos 12.5 oz     2 11.26        <NA>
    ## 1304 Muscle Milk P40 Strawberry Cream     1 11.26        <NA>
    ## 1305   Muscle Milk PP Chocolate 14 oz     1 11.26        <NA>
    ## 1306          Soda Orange Crush 20 Oz     2 11.26        <NA>
    ## 1307     Soda Pepsi Wild Cherry 20 Oz     2 11.26        <NA>
    ## 1308            Soda Pepsi  Zero 20oz     2 11.26        <NA>
    ## 1309 Tea Blackberry Pure Leaf 18.5 oz     2 11.26        <NA>
    ## 1310 Tea Unsweet Black W Lemon 18.5 o     2 11.26        <NA>
    ## 1311   Frappuccino Caramel SB 13.7 oz     1 11.26        <NA>
    ## 1312    Frappuccino Coffee 13.7 oz SB     1 11.26        <NA>
    ## 1313 Frappuccino Oatmilk Caramel Whit     1 11.26        <NA>
    ## 1314       Juice Naked PNCLDA 15.2 Oz     1 11.26        <NA>
    ## 1315          Naked Red Machine Juice     1 11.26        <NA>
    ## 1316    Naked Strawberry Banana Juice     1 11.26        <NA>
    ## 1317   Kombucha Ginger Kevita 15.2 oz     1 11.26        <NA>
    ## 1318 Gatorade Gatorlyte Cherry Lime 2     1 11.26        <NA>
    ## 1319 Gatorade Gatorlyte Glacier Freez     1 11.26        <NA>
    ## 1320 Gatorade Gatorlyte Zero Lemon Li     1 11.26        <NA>
    ## 1321         Yerba Mate Lemon 15.5 oz     1 11.26        <NA>
    ## 1322   Yerba Mate Revel Berry 15.5 oz     1 11.26        <NA>
    ## 1323 Cold Brew Vanilla Sweet Cream St     1 11.26        <NA>
    ## 1324 Energy 222 Blue Raspberry Odysse     1 11.26        <NA>
    ## 1325 Energy Orangesicle Celsius 16 oz     1 11.26        <NA>
    ## 1326 Energy Revive Prickly Pear Odyss     1 11.26        <NA>
    ## 1327 Juice Fuji Apple Red Jacket 12oz     1 11.26        <NA>
    ## 1328 Starbucks Espress & Salt Cara DS     1 11.26        <NA>
    ## 1329   Energy Artic Vibe Celsius 12oz     1 11.26        <NA>
    ## 1330 Energy Blue Raz Lemonade Celsius     1 11.26        <NA>
    ## 1331   Energy Fast Twitch Grape 12 oz     1 11.26        <NA>
    ## 1332 Energy Fast Twitch Watermelon St     1 11.26        <NA>
    ## 1333 Energy Fuji Apple Pear Celsius 1     1 11.26        <NA>
    ## 1334   Energy StrawLemon Celsius 12oz     1 11.26        <NA>
    ## 1335 Mountain Dew Kickstart Orange 16     1 11.26        <NA>
    ## 1336       Gatorade Fruit Punch 28 oz     1 11.26        <NA>
    ## 1337 Gatorade Galcier Freeze Zero G 2     1 11.26        <NA>
    ## 1338 Gatorade Propel Grape Water 1 lt     1 11.26        <NA>
    ## 1339 Gatorade Propel  Kiwi Straw Wate     1 11.26        <NA>
    ## 1340       Lipton Pure Leaf Sweet Tea     1 11.26        <NA>
    ## 1341          Soda Mountain Dew 20 Oz     1 11.26        <NA>
    ## 1342          Soda Rootbeer Mug 20 oz     1 11.26        <NA>
    ## 1343     Soda Starry Lemon Lime 20 oz     1 11.26        <NA>
    ## 1344   Tea Iced Rasberry Lipton 16 oz     1 11.26        <NA>
    ## 1345 Tea Pure Leaf Zero Sugar Sweet T     1 11.26        <NA>
    ## 1346 Tea Sweetened With Lemon Brisk 2     1 11.26        <NA>
    ## 1347 Tea Tea & Lemon Pure Leaf Lipton     1 11.26        <NA>
    ## 1348 Tea Unsweet Green Lipton 18.5 oz     1 11.26        <NA>
    ## 1349         Juice Lemonade Dole 20oz     1 11.26        <NA>
    ## 1350        Juice Orange Dole 15.2 oz     1 11.26        <NA>
    ## 1351   Ocean Spray Cranberry Cocktail     1 11.26        <NA>
    ## 1352   Soda Mandarin Jarritos 12.5 oz     1 11.26        <NA>
    ## 1353    Water Aquafina Alumitek 16 oz     1 11.26        <NA>
    ## 1354 Yogurt Mixed Berry Chobani Drink     3 11.26        <NA>
    ## 1355               Cornell Whole Milk     6 11.26        <NA>
    ## 1356  Yogurt Mango Chobani Drink 7 oz     2 11.26        <NA>
    ## 1357 Yogurt Strawberry Banana Greek C     3 11.26        <NA>
    ## 1358       Cornell 2% Milk (70000380)     2 11.26        <NA>
    ## 1359 Yogurt Flip Almond Coco Loco Cho     2 11.26        <NA>
    ## 1360 Yogurt Black Cherry Greek Choban     2 11.26        <NA>
    ## 1361 Yogurt Straw Banana Chobani Drin     1 11.26        <NA>
    ## 1362       Cornell 2% Milk (70000381)     2 11.26        <NA>
    ## 1363   Milk Chocolate LF Cornell 8 Oz     2 11.26        <NA>
    ## 1364 Yogurt 0% Fat Vanilla Greek Oiko     1 11.26        <NA>
    ## 1365       Yogurt Mango Greek Chobani     1 11.26        <NA>
    ## 1366       Yogurt Peach Greek Chobani     1 11.26        <NA>
    ## 1367   Yogurt Raspberry Greek Chobani     1 11.26        <NA>
    ## 1368  Yogurt Strawberry Greek Chobani     1 11.26        <NA>
    ## 1369      Jelly Konjac Peach Tastelli     6 11.26        <NA>
    ## 1370 Jelly Konjac Mango Pineapple Tas     3 11.26        <NA>
    ## 1371  Bar Peanut Butter Builders Clif     2 11.26        <NA>
    ## 1372 Jelly Konjac Apple Grape Tastell     2 11.26        <NA>
    ## 1373        Oat And Honey Granola Bar     3 11.26        <NA>
    ## 1374 Jelly Konjac Double Berry Tastel     1 11.26        <NA>
    ## 1375 Dark Choc Cherry Cashew Plus Kin     1 11.26        <NA>
    ## 1376 Bar That's It Apple+Strawberry 1     1 11.26        <NA>
    ## 1377                      Fruit Whole    23 11.26        <NA>
    ## 1378      Chewy Marshmallow GF 2.1 oz     4 11.26        <NA>
    ## 1379 GF Sweet Street Choloate Brownie     2 11.26        <NA>
    ## 1380                 Utz Regular Chip     2 11.26        <NA>
    ## 1381 Chip Potato Dirty BBQ Mesquite 2     1 11.26        <NA>
    ## 1382           Chips Dirty Sea Salted     1 11.26        <NA>
    ## 1383  Chip Sour Cream And Onion Dirty     1 11.26        <NA>
    ## 1384         Utz Barbecue Potato Chip     1 11.26        <NA>
    ## 1385       Utz GoodHealth Veggie Chip     1 11.26        <NA>
    ## 1386             Orbit Wintermint Gum     4 11.26        <NA>
    ## 1387 Candy Milk Choc Caff Bar Awake 1     1 11.26        <NA>
    ## 1388                           Cookie    18 11.26        <NA>
    ## 1389                     Muffin Jumbo    11 11.26        <NA>
    ## 1390                Croissant Blue CC     5 11.26        <NA>
    ## 1391                   Croissant Choc     4 11.26        <NA>
    ## 1392                  Plain Croissant     3 11.26        <NA>
    ## 1393               Croissant Straw CC     2 11.26        <NA>
    ## 1394      Combo 16 oz Coffee & Muffin     2 11.26        <NA>
    ## 1395                     Cinnamon Bun     2 11.26        <NA>
    ## 1396                      Coffee Cake     2 11.26        <NA>
    ## 1397                 BowlChickAlfrPen     1 11.26 grab 'n' go
    ## 1398                     PBJ on Wheat     1 11.26 grab 'n' go
    ## 1399 Sandwich Black Forrest Ham & Swi     2 11.26 grab 'n' go
    ## 1400 Sandwich Crispy Chicken Milanese     2 11.26 grab 'n' go
    ## 1401             Wrap Buffalo Chicken     2 11.26 grab 'n' go
    ## 1402       Quesadilla Deluxe Trillium   120 11.25     mexican
    ## 1403                Grilled Hamburger    67 11.25       grill
    ## 1404    Burrito Una Mano Trillium BYO    45 11.25     mexican
    ## 1405            Fried Chicken Tenders    45 11.25       grill
    ## 1406                     French Fries    77 11.25       grill
    ## 1407  Grilled Chicken Breast Sandwich    14 11.25       grill
    ## 1408                Quesadilla Cheese    12 11.25     mexican
    ## 1409             Seared Salmon Burger     9 11.25       grill
    ## 1410               Sweet Potato Fries    22 11.25       grill
    ## 1411 Trillium Grill Impossible Burger     6 11.25       grill
    ## 1412             ADD Beef Patty $2.99     9 11.25       grill
    ## 1413               ADD Chicken Breast     7 11.25       grill
    ## 1414                Black Bean Burger     2 11.25       grill
    ## 1415        ADD Burger Salmon Grilled     2 11.25       grill
    ## 1416              Add Sausage 2 Patty     2 11.25       grill
    ## 1417                       ADD Cheese     5 11.25       grill
    ## 1418                     Add Egg $.99     3 11.25       grill
    ## 1419      Add Impossible Burger Patty     0 11.25        <NA>
    ## 1420                1 Entree + 1 Side   104 11.25         wok
    ## 1421               Bowl Ramen Chicken    56 11.25       ramen
    ## 1422                1 Entree + 2 Side    47 11.25         wok
    ## 1423              2 Entrees + 2 Sides    10 11.25         wok
    ## 1424                  Bowl Ramen Tofu    10 11.25       ramen
    ## 1425          Side Vegetarian Lo Mein     8 11.25         wok
    ## 1426         Side White or Brown Rice     6 11.25         wok
    ## 1427                  Side Vegetables     3 11.25         wok
    ## 1428                     1 Wok Entree     1 11.25         wok
    ## 1429  Side Vegetarian Fried Rice with     1 11.25         wok
    ## 1430      Create Your Pasta Bowl MEAT    55 11.25     italian
    ## 1431       Create Your Pasta Bowl VEG    20 11.25     italian
    ## 1432              Pizza with Toppings    23 11.25     italian
    ## 1433                     Pizza Cheese    11 11.25     italian
    ## 1434                   Add Extra Meat    10 11.25     italian
    ## 1435                Burrito Breakfast    31 11.25        <NA>
    ## 1436              Small French Omelet    27 11.25        <NA>
    ## 1437 Egg Cheese Sausage Breakfast San    17 11.25        <NA>
    ## 1438             Grand Slam Breakfast     9 11.25        <NA>
    ## 1439 Egg Cheese Bacon Breakfast Sandw    10 11.25        <NA>
    ## 1440                        Add Bacon    17 11.25        <NA>
    ## 1441                         Two Eggs     4 11.25        <NA>
    ## 1442                   Pancake Single     3 11.25        <NA>
    ## 1443              Trillium Home Fries     1 11.25        <NA>
    ## 1444                   2 Slices Toast     3 11.25        <NA>
    ## 1445                 Burrito Bowl BYO    43 11.25     mexican
    ## 1446                      Single Taco     6 11.25     mexican
    ## 1447      Add Extra Toppings Una Mano     1 11.25     mexican
    ## 1448               Salad by the Pound    46 11.25   salad bar
    ## 1449                       Soup 12 oz    24 11.25   salad bar
    ## 1450                        8 oz Soup    14 11.25   salad bar
    ## 1451              Soda Fountain 24 oz    22 11.25        <NA>
    ## 1452              Soda Fountain 16 oz    24 11.25        <NA>
    ## 1453                  Coffee 12 oz SB    19 11.25        <NA>
    ## 1454                  Coffee 16 oz SB    11 11.25        <NA>
    ## 1455                    Hot Tea 20 oz     1 11.25        <NA>
    ## 1456                 Side Potato Tots     4 11.25        <NA>
    ## 1457            Soda Pepsi Diet 20 Oz    14 11.25        <NA>
    ## 1458             Water Aquafina 20 oz    11 11.25        <NA>
    ## 1459 Yerba Mate Enlighten Mint 15.5 o     6 11.25        <NA>
    ## 1460 Milk Chocolate LF BIG RED Refuel     8 11.25        <NA>
    ## 1461    Yerba Mate Bluephoria 15.5 oz     4 11.25        <NA>
    ## 1462    Tea Black Milk Ito En 11.8 oz     4 11.25        <NA>
    ## 1463 Tea Jasmine Green Unsweet Ito En     4 11.25        <NA>
    ## 1464                 Soda Pepsi 20 Oz     6 11.25        <NA>
    ## 1465   Energy Artic Vibe Celsius 12oz     4 11.25        <NA>
    ## 1466 Energy Mango PassFruit Celsius 1     4 11.25        <NA>
    ## 1467   Energy StrawLemon Celsius 12oz     4 11.25        <NA>
    ## 1468         Juice Naked Mighty Mango     3 11.25        <NA>
    ## 1469       Juice Naked PNCLDA 15.2 Oz     3 11.25        <NA>
    ## 1470       Lipton Pure Leaf Sweet Tea     5 11.25        <NA>
    ## 1471  Soda Ginger Ale Schweppes 20 Oz     5 11.25        <NA>
    ## 1472            Soda Pepsi  Zero 20oz     5 11.25        <NA>
    ## 1473 Tea Tea & Lemon Pure Leaf Lipton     5 11.25        <NA>
    ## 1474 Tea Unsweet Green Lipton 18.5 oz     5 11.25        <NA>
    ## 1475 Gatorade Gatorlyte Glacier Freez     3 11.25        <NA>
    ## 1476       Juice Grape Tropicana 11oz     4 11.25        <NA>
    ## 1477  Soda Rootbeer Zero Sugar Mug 20     4 11.25        <NA>
    ## 1478         Juice Apple Dole 15.2 oz     4 11.25        <NA>
    ## 1479   Frappuccino Caramel SB 13.7 oz     2 11.25        <NA>
    ## 1480        Juice Naked Green Machine     2 11.25        <NA>
    ## 1481 Juice Orange Homestyle Tropicana     3 11.25        <NA>
    ## 1482 Juice Orange Premium Topicana 11     3 11.25        <NA>
    ## 1483 Gatorade Gatorlyte Zero Lemon Li     2 11.25        <NA>
    ## 1484         Yerba Mate Lemon 15.5 oz     2 11.25        <NA>
    ## 1485   Yerba Mate Revel Berry 15.5 oz     2 11.25        <NA>
    ## 1486 Tea Green Matcha Milk Ito En 11.     2 11.25        <NA>
    ## 1487                Water Aquafina 1L     3 11.25        <NA>
    ## 1488 Red Jacket Strawberry Apple Juic     2 11.25        <NA>
    ## 1489     Soda Mountain Dew Zero 20 Oz     3 11.25        <NA>
    ## 1490     Soda Starry Lemon Lime 20 oz     3 11.25        <NA>
    ## 1491 Tea Blackberry Pure Leaf 18.5 oz     3 11.25        <NA>
    ## 1492 Tea Unsweet Black W Lemon 18.5 o     3 11.25        <NA>
    ## 1493 Energy Fuji Apple Pear Celsius 1     2 11.25        <NA>
    ## 1494 Tea Green Peach Mango Celsius 12     2 11.25        <NA>
    ## 1495  Tea Green Rasp Acai  Celsius 12     2 11.25        <NA>
    ## 1496              Gatorade Blue 28 oz     2 11.25        <NA>
    ## 1497 Gatorade Galcier Freeze Zero G 2     2 11.25        <NA>
    ## 1498    Glacier Freeze Gatorade 28 oz     2 11.25        <NA>
    ## 1499       Juice AppleTropicana 11 oz     2 11.25        <NA>
    ## 1500 Gatorade Propel Berry Water 1 lt     2 11.25        <NA>
    ## 1501   Soda Tamarind Jarritos 12.5 oz     2 11.25        <NA>
    ## 1502    Muscle Milk KO Chocolate 14oz     1 11.25        <NA>
    ## 1503 Muscle Milk PROSRS 40 Intense Va     1 11.25        <NA>
    ## 1504          Soda Mountain Dew 20 Oz     2 11.25        <NA>
    ## 1505     Soda Pepsi Wild Cherry 20 Oz     2 11.25        <NA>
    ## 1506   Tea Iced Rasberry Lipton 16 oz     2 11.25        <NA>
    ## 1507  Tea Peach Pure Leaf Lipton 18.5     2 11.25        <NA>
    ## 1508 Tea Unsweetened Pure Leaf Lipton     2 11.25        <NA>
    ## 1509     Frappuccino Mocha 13.7 Oz SB     1 11.25        <NA>
    ## 1510 Frappuccino Oatmilk Caramel Whit     1 11.25        <NA>
    ## 1511         Juice Naked Blue Machine     1 11.25        <NA>
    ## 1512         Juice Protein Zone Naked     1 11.25        <NA>
    ## 1513    Naked Strawberry Banana Juice     1 11.25        <NA>
    ## 1514  Kombucha Rasp Lemon Kevita 15.2     1 11.25        <NA>
    ## 1515 Gatorade Gatorlyte Strawberry Ki     1 11.25        <NA>
    ## 1516     Starbucks Double Shot 6.5 Oz     1 11.25        <NA>
    ## 1517 Tea Golden Oolong Unsweet Ito En     1 11.25        <NA>
    ## 1518    Water Aquafina Alumitek 16 oz     2 11.25        <NA>
    ## 1519 Cold Brew Chocolate Cream Starbu     1 11.25        <NA>
    ## 1520 Energy 222 Blue Raspberry Odysse     1 11.25        <NA>
    ## 1521 Energy Dragonberry Celsius 16 oz     1 11.25        <NA>
    ## 1522 Energy Fruit Burst Celsius 16 oz     1 11.25        <NA>
    ## 1523 Energy Passion Orange Guva Odyss     1 11.25        <NA>
    ## 1524 Juice Joes Lemonade  Red Jacket1     1 11.25        <NA>
    ## 1525    Rockstar Pure Zero Silver Ice     1 11.25        <NA>
    ## 1526  Energy Fast Twitch Cool Blue 12     1 11.25        <NA>
    ## 1527   Energy Fast Twitch Grape 12 oz     1 11.25        <NA>
    ## 1528     Water Life WTR Immune 700 ML     1 11.25        <NA>
    ## 1529       Gatorade Fruit Punch 28 oz     1 11.25        <NA>
    ## 1530        Gatorade Lemon Lime 28 oz     1 11.25        <NA>
    ## 1531             Gatorade Orange 28oz     1 11.25        <NA>
    ## 1532        Gatorade Zero Grape 28 oz     1 11.25        <NA>
    ## 1533    Glacier Cherry Gatorade 28 oz     1 11.25        <NA>
    ## 1534 Juice Raspberry Lemonade Tropica     1 11.25        <NA>
    ## 1535 Gatorade Propel Grape Water 1 lt     1 11.25        <NA>
    ## 1536  Sparkling Lemonade Mango Kevita     1 11.25        <NA>
    ## 1537 Sparkling lemonade Strawberry Ke     1 11.25        <NA>
    ## 1538             Water Gatorade 700ml     1 11.25        <NA>
    ## 1539     Tea Light Peach Lipton 20 oz     1 11.25        <NA>
    ## 1540 Tea Pure Leaf Zero Sugar Sweet T     1 11.25        <NA>
    ## 1541 Tea Sweetened With Lemon Brisk 2     1 11.25        <NA>
    ## 1542 Tea Sweet W/ Le Pure Leaf Lipton     1 11.25        <NA>
    ## 1543 Juice Ocean Spray Cranbe Grape 1     1 11.25        <NA>
    ## 1544   Cornell Low Fat Chocolate Milk     4 11.25        <NA>
    ## 1545  Yogurt Mango Chobani Drink 7 oz     3 11.25        <NA>
    ## 1546 Yogurt Straw Banana Chobani Drin     2 11.25        <NA>
    ## 1547                  Cornell 2% Milk     3 11.25        <NA>
    ## 1548               Cornell Whole Milk     3 11.25        <NA>
    ## 1549   Milk Chocolate LF Cornell 8 Oz     3 11.25        <NA>
    ## 1550   Yogurt Blueberry Greek Chobani     2 11.25        <NA>
    ## 1551          Silk Chocolate Soy Milk     2 11.25        <NA>
    ## 1552 Yogurt Mixed Berry Chobani Drink     1 11.25        <NA>
    ## 1553 Yogurt Flip Almond Coco Loco Cho     1 11.25        <NA>
    ## 1554 Yogurt 0% Fat Vanilla Greek Oiko     1 11.25        <NA>
    ## 1555       Yogurt Mango Greek Chobani     1 11.25        <NA>
    ## 1556   Yogurt Raspberry Greek Chobani     1 11.25        <NA>
    ## 1557 Yogurt Strawberry Banana Greek C     1 11.25        <NA>
    ## 1558  Yogurt Strawberry Greek Chobani     1 11.25        <NA>
    ## 1559            8 oz Vanilla Soy Silk     1 11.25        <NA>
    ## 1560 Bar Cookie & Cream Quest 2.12 oz     3 11.25        <NA>
    ## 1561      Jelly Konjac Peach Tastelli     3 11.25        <NA>
    ## 1562 Jelly Konjac Apple Grape Tastell     2 11.25        <NA>
    ## 1563 Jelly Konjac Double Berry Tastel     2 11.25        <NA>
    ## 1564 Cookie Peanut Butter Lenny & Lar     1 11.25        <NA>
    ## 1565 Bar CHOC Mint Builders Clif bar2     1 11.25        <NA>
    ## 1566   Fruit And Nut Delight Kind Bar     1 11.25        <NA>
    ## 1567   Kind Cranberry Almond Plus Bar     1 11.25        <NA>
    ## 1568 Bar That's It Apple+Strawberry 1     1 11.25        <NA>
    ## 1569       Jalapeno Heat Chips Kosher     2 11.25        <NA>
    ## 1570       Chip Maui Onion Dirty 2 oz     1 11.25        <NA>
    ## 1571 Chip Potato Dirty BBQ Mesquite 2     1 11.25        <NA>
    ## 1572  Chips Cracked Pepper & Sea Salt     1 11.25        <NA>
    ## 1573           Chips Dirty Sea Salted     1 11.25        <NA>
    ## 1574   Chip Voodoo Limited Zapps 2 oz     1 11.25        <NA>
    ## 1575       Utz GoodHealth Veggie Chip     1 11.25        <NA>
    ## 1576                      Fruit Whole    16 11.25        <NA>
    ## 1577  Ice Cream Mochi Sweet Mango 1.5     2 11.25        <NA>
    ## 1578                 MochiCookCrm1.5o     2 11.25        <NA>
    ## 1579 Ice Cream Mochi Double Chocolate     1 11.25        <NA>
    ## 1580      Chewy Marshmallow GF 2.1 oz     2 11.25        <NA>
    ## 1581 GF Sweet Street Choloate Brownie     1 11.25        <NA>
    ## 1582   Candy CHOC Cara Caff Bar Awake     1 11.25        <NA>
    ## 1583 Candy Dark Chocolate Awake Bar 1     1 11.25        <NA>
    ## 1584             Orbit Sweet Mint Gum     1 11.25        <NA>
    ## 1585                     Muffin Jumbo    15 11.25        <NA>
    ## 1586                           Cookie    12 11.25        <NA>
    ## 1587                   Croissant Choc     5 11.25        <NA>
    ## 1588                Croissant Blue CC     2 11.25        <NA>
    ## 1589                     Cinnamon Bun     2 11.25        <NA>
    ## 1590               Croissant Straw CC     1 11.25        <NA>
    ## 1591                  Plain Croissant     1 11.25        <NA>
    ## 1592                   BowlMedProtein     2 11.25 grab 'n' go
    ## 1593                 BowlMexicanChick     1 11.25 grab 'n' go
    ## 1594                 BowlSesAsianNood     1 11.25 grab 'n' go
    ## 1595                   BowlSouthChick     1 11.25 grab 'n' go
    ## 1596                 GF ChicCaesarSld     1 11.25 grab 'n' go
    ## 1597                   GF Turkey Sand     1 11.25 grab 'n' go
    ## 1598              Wrap Chicken Caesar     3 11.25 grab 'n' go
    ## 1599             Wrap Buffalo Chicken     1 11.25 grab 'n' go
    ## 1600       Quesadilla Deluxe Trillium   103 11.22     mexican
    ## 1601                Grilled Hamburger    51 11.22       grill
    ## 1602            Fried Chicken Tenders    48 11.22       grill
    ## 1603    Burrito Una Mano Trillium BYO    36 11.22     mexican
    ## 1604                     French Fries    81 11.22       grill
    ## 1605  Grilled Chicken Breast Sandwich    15 11.22       grill
    ## 1606 Trillium Grill Impossible Burger     8 11.22       grill
    ## 1607                Quesadilla Cheese     5 11.22     mexican
    ## 1608               Sweet Potato Fries    13 11.22       grill
    ## 1609             ADD Beef Patty $2.99     7 11.22       grill
    ## 1610              Add Sausage 2 Patty     5 11.22       grill
    ## 1611                Black Bean Burger     1 11.22       grill
    ## 1612             Seared Salmon Burger     1 11.22       grill
    ## 1613                       ADD Cheese     6 11.22       grill
    ## 1614                     Add Egg $.99     2 11.22       grill
    ## 1615                1 Entree + 1 Side    90 11.22         wok
    ## 1616                1 Entree + 2 Side    45 11.22         wok
    ## 1617               Bowl Ramen Chicken    49 11.22       ramen
    ## 1618              2 Entrees + 2 Sides    10 11.22         wok
    ## 1619                  Bowl Ramen Tofu     8 11.22       ramen
    ## 1620          Side Vegetarian Lo Mein    11 11.22         wok
    ## 1621         Side White or Brown Rice    10 11.22         wok
    ## 1622                     1 Wok Entree     2 11.22         wok
    ## 1623      Side Vegetable Spring Rolls     3 11.22         wok
    ## 1624                  Side Vegetables     2 11.22         wok
    ## 1625  Side Vegetarian Fried Rice with     1 11.22         wok
    ## 1626                Burrito Breakfast    66 11.22        <NA>
    ## 1627              Small French Omelet    27 11.22        <NA>
    ## 1628 Egg Cheese Bacon Breakfast Sandw    25 11.22        <NA>
    ## 1629             Grand Slam Breakfast    13 11.22        <NA>
    ## 1630 Egg Cheese Sausage Breakfast San    22 11.22        <NA>
    ## 1631                         Two Eggs    15 11.22        <NA>
    ## 1632                        Add Bacon    14 11.22        <NA>
    ## 1633                   Pancake Single     5 11.22        <NA>
    ## 1634                   2 Slices Toast     5 11.22        <NA>
    ## 1635              Trillium Home Fries     1 11.22        <NA>
    ## 1636                         PC Jelly     2 11.22        <NA>
    ## 1637                 PC Peanut Butter     1 11.22        <NA>
    ## 1638      Create Your Pasta Bowl MEAT    70 11.22     italian
    ## 1639              Pizza with Toppings    21 11.22     italian
    ## 1640                     Pizza Cheese    18 11.22     italian
    ## 1641       Create Your Pasta Bowl VEG     9 11.22     italian
    ## 1642                   Add Extra Meat    11 11.22     italian
    ## 1643                 Burrito Bowl BYO    51 11.22     mexican
    ## 1644                      Single Taco     6 11.22     mexican
    ## 1645                       Soup 12 oz    41 11.22   salad bar
    ## 1646                        8 oz Soup    32 11.22   salad bar
    ## 1647               Salad by the Pound    30 11.22   salad bar
    ## 1648              Soda Fountain 24 oz    21 11.22        <NA>
    ## 1649                  Coffee 12 oz SB    18 11.22        <NA>
    ## 1650                  Coffee 16 oz SB    15 11.22        <NA>
    ## 1651              Soda Fountain 16 oz    17 11.22        <NA>
    ## 1652                    Hot Tea 20 oz     4 11.22        <NA>
    ## 1653                        Hot Cocoa     3 11.22        <NA>
    ## 1654                 Side Potato Tots    19 11.22        <NA>
    ## 1655               Open Miscellaneous     2 11.22        <NA>
    ## 1656 Tea Jasmine Green Unsweet Ito En     8 11.22        <NA>
    ## 1657             Water Aquafina 20 oz    13 11.22        <NA>
    ## 1658            Soda Pepsi Diet 20 Oz    10 11.22        <NA>
    ## 1659 Milk Chocolate LF BIG RED Refuel     9 11.22        <NA>
    ## 1660       Juice Naked PNCLDA 15.2 Oz     4 11.22        <NA>
    ## 1661    Naked Strawberry Banana Juice     4 11.22        <NA>
    ## 1662       Juice AppleTropicana 11 oz     6 11.22        <NA>
    ## 1663      Yerba Mate Tropical 15.5 oz     4 11.22        <NA>
    ## 1664            Soda Pepsi  Zero 20oz     6 11.22        <NA>
    ## 1665   Energy Artic Vibe Celsius 12oz     4 11.22        <NA>
    ## 1666 Tea Green Peach Mango Celsius 12     4 11.22        <NA>
    ## 1667          Juice Naked Berry Blast     3 11.22        <NA>
    ## 1668 Tea Sweet W/ Le Pure Leaf Lipton     5 11.22        <NA>
    ## 1669 Tea Unsweetened Pure Leaf Lipton     5 11.22        <NA>
    ## 1670         Juice Apple Dole 15.2 oz     5 11.22        <NA>
    ## 1671             Water Gatorade 700ml     4 11.22        <NA>
    ## 1672 Energy Mango PassFruit Celsius 1     3 11.22        <NA>
    ## 1673                Water Aquafina 1L     4 11.22        <NA>
    ## 1674       Lipton Pure Leaf Sweet Tea     4 11.22        <NA>
    ## 1675  Soda Ginger Ale Schweppes 20 Oz     4 11.22        <NA>
    ## 1676     Soda Pepsi Wild Cherry 20 Oz     4 11.22        <NA>
    ## 1677  Tea Peach Pure Leaf Lipton 18.5     4 11.22        <NA>
    ## 1678 Tea Pure Leaf Zero Sugar Sweet T     4 11.22        <NA>
    ## 1679     Frappuccino Mocha 13.7 Oz SB     2 11.22        <NA>
    ## 1680         Juice Naked Blue Machine     2 11.22        <NA>
    ## 1681         Juice Protein Zone Naked     2 11.22        <NA>
    ## 1682  Kombucha Rasp Lemon Kevita 15.2     2 11.22        <NA>
    ## 1683              Gatorade Blue 28 oz     3 11.22        <NA>
    ## 1684 Gatorade Galcier Freeze Zero G 2     3 11.22        <NA>
    ## 1685 Juice Orange Homestyle Tropicana     3 11.22        <NA>
    ## 1686 Juice Orange Premium Topicana 11     3 11.22        <NA>
    ## 1687 Sparkling lemonade Strawberry Ke     3 11.22        <NA>
    ## 1688 Gatorade Gatorlyte Glacier Freez     2 11.22        <NA>
    ## 1689 Gatorade Gatorlyte Strawberry Ki     2 11.22        <NA>
    ## 1690 Gatorade Gatorlyte Zero Fruit Pu     2 11.22        <NA>
    ## 1691 Yerba Mate Enlighten Mint 15.5 o     2 11.22        <NA>
    ## 1692    Tea Black Milk Ito En 11.8 oz     2 11.22        <NA>
    ## 1693      Soda Mango Jarritos 12.5 oz     3 11.22        <NA>
    ## 1694  Energy Blue Crush Celsius 16 oz     2 11.22        <NA>
    ## 1695 Red Jacket Strawberry Apple Juic     2 11.22        <NA>
    ## 1696     Soda Mountain Dew Zero 20 Oz     3 11.22        <NA>
    ## 1697  Soda Rootbeer Zero Sugar Mug 20     3 11.22        <NA>
    ## 1698     Soda Starry Lemon Lime 20 oz     3 11.22        <NA>
    ## 1699   Tea Iced Rasberry Lipton 16 oz     3 11.22        <NA>
    ## 1700  Tea Green Rasp Acai  Celsius 12     2 11.22        <NA>
    ## 1701 Mountain Dew Kickstart Black Che     2 11.22        <NA>
    ## 1702        Gatorade Lemon Lime 28 oz     2 11.22        <NA>
    ## 1703    Glacier Freeze Gatorade 28 oz     2 11.22        <NA>
    ## 1704         Muscle Milk Choc PB 14oz     1 11.22        <NA>
    ## 1705    Muscle Milk KO Chocolate 14oz     1 11.22        <NA>
    ## 1706   Muscle Milk PP Chocolate 14 oz     1 11.22        <NA>
    ## 1707 Muscle Milk PROSRS 40 Intense Va     1 11.22        <NA>
    ## 1708          Soda Orange Crush 20 Oz     2 11.22        <NA>
    ## 1709          Soda Rootbeer Mug 20 oz     2 11.22        <NA>
    ## 1710 Tea Sweetened With Lemon Brisk 2     2 11.22        <NA>
    ## 1711 Starbucks Doubleshot Energy Moch     1 11.22        <NA>
    ## 1712  Starbucks Doubleshot Energy Van     1 11.22        <NA>
    ## 1713   Ocean Spray Cranberry Cocktail     2 11.22        <NA>
    ## 1714   Frappuccino Caramel SB 13.7 oz     1 11.22        <NA>
    ## 1715        Juice Naked Green Machine     1 11.22        <NA>
    ## 1716          Naked Red Machine Juice     1 11.22        <NA>
    ## 1717  Kombucha Pineapple Peach Kevita     1 11.22        <NA>
    ## 1718 Gatorade Gatorlyte MixBerry 20 o     1 11.22        <NA>
    ## 1719  Gatorade Gatorlyte Orange 20 oz     1 11.22        <NA>
    ## 1720         Yerba Mate Lemon 15.5 oz     1 11.22        <NA>
    ## 1721   Yerba Mate Revel Berry 15.5 oz     1 11.22        <NA>
    ## 1722 Cold Brew Chocolate Cream Starbu     1 11.22        <NA>
    ## 1723 Cold Brew Vanilla Sweet Cream St     1 11.22        <NA>
    ## 1724 Energy 222 Pineapple Mango Odyss     1 11.22        <NA>
    ## 1725 Energy Cherry Limeade Celsius 16     1 11.22        <NA>
    ## 1726 Energy Mango Tango Celsius 16 oz     1 11.22        <NA>
    ## 1727 Energy Passion Orange Guva Odyss     1 11.22        <NA>
    ## 1728 Energy Revive Prickly Pear Odyss     1 11.22        <NA>
    ## 1729 Juice Fuji Apple Red Jacket 12oz     1 11.22        <NA>
    ## 1730 Juice Joes Lemonade  Red Jacket1     1 11.22        <NA>
    ## 1731 Starbucks Espress & Salt Cara DS     1 11.22        <NA>
    ## 1732   Energy StrawLemon Celsius 12oz     1 11.22        <NA>
    ## 1733     Cider Apple Red Jacket 12 oz     1 11.22        <NA>
    ## 1734             Gatorade Orange 28oz     1 11.22        <NA>
    ## 1735    Glacier Cherry Gatorade 28 oz     1 11.22        <NA>
    ## 1736       Juice Grape Tropicana 11oz     1 11.22        <NA>
    ## 1737  Juice Lively Lemonade Tropicana     1 11.22        <NA>
    ## 1738 Juice Raspberry Lemonade Tropica     1 11.22        <NA>
    ## 1739 Gatorade Propel Grape Water 1 lt     1 11.22        <NA>
    ## 1740  Sparkling Lemonade Mango Kevita     1 11.22        <NA>
    ## 1741  Soda Pineapple Jarritos 12.5 oz     1 11.22        <NA>
    ## 1742   Soda Tamarind Jarritos 12.5 oz     1 11.22        <NA>
    ## 1743          Soda Mountain Dew 20 Oz     1 11.22        <NA>
    ## 1744 Tea Blackberry Pure Leaf 18.5 oz     1 11.22        <NA>
    ## 1745     Tea Light Peach Lipton 20 oz     1 11.22        <NA>
    ## 1746 Tea Unsweet Black W Lemon 18.5 o     1 11.22        <NA>
    ## 1747 Tea Unsweet Green Lipton 18.5 oz     1 11.22        <NA>
    ## 1748 Juice Ocean Spray Cranbe Grape 1     1 11.22        <NA>
    ## 1749   Soda Mandarin Jarritos 12.5 oz     1 11.22        <NA>
    ## 1750    Water Aquafina Alumitek 16 oz     1 11.22        <NA>
    ## 1751   Cornell Low Fat Chocolate Milk     5 11.22        <NA>
    ## 1752   Milk Chocolate LF Cornell 8 Oz     8 11.22        <NA>
    ## 1753 Yogurt Flip Almond Coco Loco Cho     3 11.22        <NA>
    ## 1754 Yogurt Straw Banana Chobani Drin     2 11.22        <NA>
    ## 1755       Cornell 2% Milk (70000381)     4 11.22        <NA>
    ## 1756  Yogurt Flip Peanut Butter Dream     2 11.22        <NA>
    ## 1757 Yogurt 0% Fat Vanilla Greek Oiko     2 11.22        <NA>
    ## 1758  Yogurt Strawberry Greek Chobani     2 11.22        <NA>
    ## 1759  Yogurt Mango Chobani Drink 7 oz     1 11.22        <NA>
    ## 1760       Cornell 2% Milk (70000380)     1 11.22        <NA>
    ## 1761   Yogurt Blueberry Greek Chobani     1 11.22        <NA>
    ## 1762       Yogurt Peach Greek Chobani     1 11.22        <NA>
    ## 1763                      Fruit Whole    34 11.22        <NA>
    ## 1764      Jelly Konjac Peach Tastelli     2 11.22        <NA>
    ## 1765 Bar That's It Apple+Mango 1.2 oz     2 11.22        <NA>
    ## 1766        Bar S'Mores Quest 2.12 oz     1 11.22        <NA>
    ## 1767  Cookie Choc Chip Lenny & Larrys     1 11.22        <NA>
    ## 1768 Bar CHOC Mint Builders Clif bar2     1 11.22        <NA>
    ## 1769  Bar Peanut Butter Builders Clif     1 11.22        <NA>
    ## 1770 Jelly Konjac Double Berry Tastel     1 11.22        <NA>
    ## 1771 Dark Choc Cherry Cashew Plus Kin     1 11.22        <NA>
    ## 1772 Chip Potato Dirty BBQ Mesquite 2     2 11.22        <NA>
    ## 1773 Chip Salt & Malt Vinegar Dirty 2     2 11.22        <NA>
    ## 1774  Chips Cracked Pepper & Sea Salt     2 11.22        <NA>
    ## 1775   Utz Honey Barbecue Chip 1.5 oz     2 11.22        <NA>
    ## 1776 Chip Potato Kettle Honey BBQ 2oz     1 11.22        <NA>
    ## 1777           Chips Dirty Sea Salted     1 11.22        <NA>
    ## 1778  Chip Sour Cream And Onion Dirty     1 11.22        <NA>
    ## 1779       Jalapeno Heat Chips Kosher     1 11.22        <NA>
    ## 1780          Utz Salt N Vinegar Chip     1 11.22        <NA>
    ## 1781      Chewy Marshmallow GF 2.1 oz     4 11.22        <NA>
    ## 1782 GF Sweet Street Choloate Brownie     1 11.22        <NA>
    ## 1783   Candy CHOC Cara Caff Bar Awake     2 11.22        <NA>
    ## 1784             Orbit Sweet Mint Gum     1 11.22        <NA>
    ## 1785             Orbit Wintermint Gum     1 11.22        <NA>
    ## 1786  Ice Cream Mochi Sweet Mango 1.5     1 11.22        <NA>
    ## 1787                 MochiCookCrm1.5o     1 11.22        <NA>
    ## 1788 Ice Cream Mochi Double Chocolate     1 11.22        <NA>
    ## 1789                           Cookie    23 11.22        <NA>
    ## 1790                     Muffin Jumbo    17 11.22        <NA>
    ## 1791               Croissant Straw CC     6 11.22        <NA>
    ## 1792                   Croissant Choc     4 11.22        <NA>
    ## 1793      Combo 16 oz Coffee & Muffin     2 11.22        <NA>
    ## 1794                     Cinnamon Bun     2 11.22        <NA>
    ## 1795                Croissant Blue CC     1 11.22        <NA>
    ## 1796                   BowlSouthChick     3 11.22 grab 'n' go
    ## 1797                 BowlMexicanChick     2 11.22 grab 'n' go
    ## 1798                     PBJ on Wheat     1 11.22 grab 'n' go
    ## 1799              Tempura Shrimp Roll     1 11.22 grab 'n' go
    ## 1800                  Spicy Tuna Roll     1 11.22 grab 'n' go
    ## 1801                     Avocado Roll     1 11.22 grab 'n' go
    ## 1802                  California Roll     1 11.22 grab 'n' go
    ## 1803 Sandwich Black Forrest Ham & Swi     1 11.22 grab 'n' go
    ## 1804 Sandwich Prosciutto & Mozzarella     1 11.22 grab 'n' go
    ## 1805       Quesadilla Deluxe Trillium   197 11.21     mexican
    ## 1806                Grilled Hamburger   109 11.21       grill
    ## 1807            Fried Chicken Tenders    93 11.21       grill
    ## 1808    Burrito Una Mano Trillium BYO    66 11.21     mexican
    ## 1809                     French Fries   113 11.21       grill
    ## 1810  Grilled Chicken Breast Sandwich    13 11.21       grill
    ## 1811               Sweet Potato Fries    32 11.21       grill
    ## 1812 Trillium Grill Impossible Burger     9 11.21       grill
    ## 1813                Quesadilla Cheese    11 11.21     mexican
    ## 1814             Seared Salmon Burger     9 11.21       grill
    ## 1815                Black Bean Burger     4 11.21       grill
    ## 1816             ADD Beef Patty $2.99     6 11.21       grill
    ## 1817               ADD Chicken Breast     5 11.21       grill
    ## 1818              Add Sausage 2 Patty     4 11.21       grill
    ## 1819        ADD Burger Salmon Grilled     1 11.21       grill
    ## 1820                       ADD Cheese     6 11.21       grill
    ## 1821                     Add Egg $.99     3 11.21       grill
    ## 1822                1 Entree + 1 Side   209 11.21         wok
    ## 1823                1 Entree + 2 Side    85 11.21         wok
    ## 1824               Bowl Ramen Chicken    77 11.21       ramen
    ## 1825              2 Entrees + 2 Sides    25 11.21         wok
    ## 1826                  Bowl Ramen Tofu    20 11.21       ramen
    ## 1827          Side Vegetarian Lo Mein    12 11.21         wok
    ## 1828                     1 Wok Entree     3 11.21         wok
    ## 1829         Side White or Brown Rice     5 11.21         wok
    ## 1830  Side Vegetarian Fried Rice with     2 11.21         wok
    ## 1831      Create Your Pasta Bowl MEAT   131 11.21     italian
    ## 1832              Pizza with Toppings    38 11.21     italian
    ## 1833       Create Your Pasta Bowl VEG    24 11.21     italian
    ## 1834                     Pizza Cheese    22 11.21     italian
    ## 1835                   Add Extra Meat    15 11.21     italian
    ## 1836                Burrito Breakfast    75 11.21        <NA>
    ## 1837              Small French Omelet    40 11.21        <NA>
    ## 1838             Grand Slam Breakfast    14 11.21        <NA>
    ## 1839 Egg Cheese Bacon Breakfast Sandw    23 11.21        <NA>
    ## 1840 Egg Cheese Sausage Breakfast San    21 11.21        <NA>
    ## 1841                        Add Bacon    33 11.21        <NA>
    ## 1842                         Two Eggs    23 11.21        <NA>
    ## 1843              Trillium Home Fries     4 11.21        <NA>
    ## 1844                   Pancake Single     2 11.21        <NA>
    ## 1845                   2 Slices Toast     5 11.21        <NA>
    ## 1846                            Toast     2 11.21        <NA>
    ## 1847                 Burrito Bowl BYO    92 11.21     mexican
    ## 1848                      Single Taco     4 11.21     mexican
    ## 1849                   Side Guacamole     2 11.21     mexican
    ## 1850      Add Extra Toppings Una Mano     1 11.21     mexican
    ## 1851                       Soup 12 oz    45 11.21   salad bar
    ## 1852                        8 oz Soup    36 11.21   salad bar
    ## 1853               Salad by the Pound    39 11.21   salad bar
    ## 1854              Soda Fountain 24 oz    39 11.21        <NA>
    ## 1855                  Coffee 16 oz SB    30 11.21        <NA>
    ## 1856                  Coffee 12 oz SB    29 11.21        <NA>
    ## 1857              Soda Fountain 16 oz    35 11.21        <NA>
    ## 1858                    Hot Tea 20 oz     4 11.21        <NA>
    ## 1859                        Hot Cocoa     3 11.21        <NA>
    ## 1860                 Side Potato Tots    35 11.21        <NA>
    ## 1861          Add Extra Protein $2.99     1 11.21        <NA>
    ## 1862               Open Miscellaneous     2 11.21        <NA>
    ## 1863             Water Aquafina 20 oz    24 11.21        <NA>
    ## 1864 Tea Golden Oolong Unsweet Ito En     9 11.21        <NA>
    ## 1865      Yerba Mate Tropical 15.5 oz     8 11.21        <NA>
    ## 1866            Soda Pepsi Diet 20 Oz    13 11.21        <NA>
    ## 1867   Energy Artic Vibe Celsius 12oz     8 11.21        <NA>
    ## 1868 Energy Mango PassFruit Celsius 1     8 11.21        <NA>
    ## 1869    Tea Black Milk Ito En 11.8 oz     7 11.21        <NA>
    ## 1870     Frappuccino Mocha 13.7 Oz SB     6 11.21        <NA>
    ## 1871                 Soda Pepsi 20 Oz    10 11.21        <NA>
    ## 1872  Kombucha Rasp Lemon Kevita 15.2     5 11.21        <NA>
    ## 1873    Muscle Milk KO Chocolate 14oz     4 11.21        <NA>
    ## 1874     Water Life WTR Immune 700 ML     6 11.21        <NA>
    ## 1875         Yerba Mate Lemon 15.5 oz     5 11.21        <NA>
    ## 1876 Yerba Mate Peach Revival 15.5 oz     5 11.21        <NA>
    ## 1877   Yerba Mate Revel Berry 15.5 oz     5 11.21        <NA>
    ## 1878 Milk Chocolate LF BIG RED Refuel     9 11.21        <NA>
    ## 1879       Juice Naked PNCLDA 15.2 Oz     4 11.21        <NA>
    ## 1880 Gatorade Galcier Freeze Zero G 2     6 11.21        <NA>
    ## 1881       Juice AppleTropicana 11 oz     6 11.21        <NA>
    ## 1882  Soda Ginger Ale Schweppes 20 Oz     7 11.21        <NA>
    ## 1883   Tea Iced Rasberry Lipton 16 oz     7 11.21        <NA>
    ## 1884                Water Aquafina 1L     6 11.21        <NA>
    ## 1885 Cold Brew Vanilla Sweet Cream St     4 11.21        <NA>
    ## 1886            Soda Pepsi  Zero 20oz     6 11.21        <NA>
    ## 1887          Soda Rootbeer Mug 20 oz     6 11.21        <NA>
    ## 1888              Gatorade Blue 28 oz     5 11.21        <NA>
    ## 1889  Juice Lively Lemonade Tropicana     5 11.21        <NA>
    ## 1890         Juice Apple Dole 15.2 oz     6 11.21        <NA>
    ## 1891         Juice Naked Blue Machine     3 11.21        <NA>
    ## 1892         Juice Naked Mighty Mango     3 11.21        <NA>
    ## 1893    Naked Strawberry Banana Juice     3 11.21        <NA>
    ## 1894          Soda Mountain Dew 20 Oz     5 11.21        <NA>
    ## 1895          Soda Orange Crush 20 Oz     5 11.21        <NA>
    ## 1896     Soda Pepsi Wild Cherry 20 Oz     5 11.21        <NA>
    ## 1897  Tea Peach Pure Leaf Lipton 18.5     5 11.21        <NA>
    ## 1898 Tea Pure Leaf Zero Sugar Sweet T     5 11.21        <NA>
    ## 1899 Tea Sweet W/ Le Pure Leaf Lipton     5 11.21        <NA>
    ## 1900    Yerba Mate Bluephoria 15.5 oz     3 11.21        <NA>
    ## 1901 Yerba Mate Enlighten Mint 15.5 o     3 11.21        <NA>
    ## 1902 Cold Brew Chocolate Cream Starbu     3 11.21        <NA>
    ## 1903 Juice Joes Lemonade  Red Jacket1     3 11.21        <NA>
    ## 1904 Energy Fuji Apple Pear Celsius 1     3 11.21        <NA>
    ## 1905 Muscle Milk P40 Strawberry Cream     2 11.21        <NA>
    ## 1906       Lipton Pure Leaf Sweet Tea     4 11.21        <NA>
    ## 1907  Soda Rootbeer Zero Sugar Mug 20     4 11.21        <NA>
    ## 1908 Tea Unsweetened Pure Leaf Lipton     4 11.21        <NA>
    ## 1909        Juice Orange Dole 15.2 oz     4 11.21        <NA>
    ## 1910          Juice Naked Berry Blast     2 11.21        <NA>
    ## 1911   Soda Mandarin Jarritos 12.5 oz     4 11.21        <NA>
    ## 1912   Kombucha Ginger Kevita 15.2 oz     2 11.21        <NA>
    ## 1913        Gatorade Lemon Lime 28 oz     3 11.21        <NA>
    ## 1914 Juice Orange Homestyle Tropicana     3 11.21        <NA>
    ## 1915 Gatorade Propel Grape Water 1 lt     3 11.21        <NA>
    ## 1916 Sparkling lemonade Strawberry Ke     3 11.21        <NA>
    ## 1917             Water Gatorade 700ml     3 11.21        <NA>
    ## 1918 Gatorade Gatorlyte Cherry Lime 2     2 11.21        <NA>
    ## 1919      Soda Mango Jarritos 12.5 oz     3 11.21        <NA>
    ## 1920  Energy Blue Crush Celsius 16 oz     2 11.21        <NA>
    ## 1921 Energy Orangesicle Celsius 16 oz     2 11.21        <NA>
    ## 1922 Red Jacket Strawberry Apple Juic     2 11.21        <NA>
    ## 1923 Tea Blackberry Pure Leaf 18.5 oz     3 11.21        <NA>
    ## 1924 Tea Tea & Lemon Pure Leaf Lipton     3 11.21        <NA>
    ## 1925 Tea Unsweet Black W Lemon 18.5 o     3 11.21        <NA>
    ## 1926 Energy Blue Raz Lemonade Celsius     2 11.21        <NA>
    ## 1927   Energy StrawLemon Celsius 12oz     2 11.21        <NA>
    ## 1928 Tea Green Peach Mango Celsius 12     2 11.21        <NA>
    ## 1929      Gatorade Berry Zero G 28 oz     2 11.21        <NA>
    ## 1930    Glacier Cherry Gatorade 28 oz     2 11.21        <NA>
    ## 1931    Glacier Freeze Gatorade 28 oz     2 11.21        <NA>
    ## 1932 Juice Orange Premium Topicana 11     2 11.21        <NA>
    ## 1933 Gatorade Propel  Kiwi Straw Wate     2 11.21        <NA>
    ## 1934  Sparkling Lemonade Mango Kevita     2 11.21        <NA>
    ## 1935         Muscle Milk Choc PB 14oz     1 11.21        <NA>
    ## 1936 Muscle Milk PROSRS 40 Intense Va     1 11.21        <NA>
    ## 1937 Tea Unsweet Green Lipton 18.5 oz     2 11.21        <NA>
    ## 1938 Starbucks Doubleshot Ener Coffee     1 11.21        <NA>
    ## 1939 Starbucks Doubleshot Energy Moch     1 11.21        <NA>
    ## 1940  Starbucks Doubleshot Energy Van     1 11.21        <NA>
    ## 1941   Frappuccino Vanilla SB 13.7 Oz     1 11.21        <NA>
    ## 1942        Juice Naked Green Machine     1 11.21        <NA>
    ## 1943 Gatorade Gatorlyte Glacier Freez     1 11.21        <NA>
    ## 1944  Gatorade Gatorlyte Orange 20 oz     1 11.21        <NA>
    ## 1945 Gatorade Gatorlyte Strawberry Ki     1 11.21        <NA>
    ## 1946 Gatorade Gatorlyte Zero Fruit Pu     1 11.21        <NA>
    ## 1947 Tea Green Matcha Milk Ito En 11.     1 11.21        <NA>
    ## 1948 Tea Jasmine Green Unsweet Ito En     1 11.21        <NA>
    ## 1949 Energy Fruit Burst Celsius 16 oz     1 11.21        <NA>
    ## 1950 Energy Mango Tango Celsius 16 oz     1 11.21        <NA>
    ## 1951 Energy Passion Orange Guva Odyss     1 11.21        <NA>
    ## 1952 Starbucks Espress & Salt Cara DS     1 11.21        <NA>
    ## 1953    Rockstar Pure Zero Silver Ice     1 11.21        <NA>
    ## 1954   Energy Fast Twitch Grape 12 oz     1 11.21        <NA>
    ## 1955 Energy Fast Twitch Watermelon St     1 11.21        <NA>
    ## 1956   Energy StrawGuava Celsius 12oz     1 11.21        <NA>
    ## 1957  Tea Green Rasp Acai  Celsius 12     1 11.21        <NA>
    ## 1958 Mountain Dew Kickstart Black Che     1 11.21        <NA>
    ## 1959 Mountain Dew Kickstart Orange 16     1 11.21        <NA>
    ## 1960     Cider Apple Red Jacket 12 oz     1 11.21        <NA>
    ## 1961       Gatorade Fruit Punch 28 oz     1 11.21        <NA>
    ## 1962             Gatorade Orange 28oz     1 11.21        <NA>
    ## 1963        Gatorade Zero Grape 28 oz     1 11.21        <NA>
    ## 1964 Juice Raspberry Lemonade Tropica     1 11.21        <NA>
    ## 1965 Juice Zero Summer Splash Punch 1     1 11.21        <NA>
    ## 1966 Kickstart Strawberry Start-up 16     1 11.21        <NA>
    ## 1967 Gatorade Propel Berry Water 1 lt     1 11.21        <NA>
    ## 1968     Soda Mountain Dew Zero 20 Oz     1 11.21        <NA>
    ## 1969     Soda Starry Lemon Lime 20 oz     1 11.21        <NA>
    ## 1970     Tea Light Peach Lipton 20 oz     1 11.21        <NA>
    ## 1971         Juice Lemonade Dole 20oz     1 11.21        <NA>
    ## 1972 Yogurt Flip Almond Coco Loco Cho     6 11.21        <NA>
    ## 1973 Yogurt 0% Fat Vanilla Greek Oiko     6 11.21        <NA>
    ## 1974 Yogurt Strawberry Banana Greek C     5 11.21        <NA>
    ## 1975 Yogurt Mixed Berry Chobani Drink     3 11.21        <NA>
    ## 1976   Cornell Low Fat Chocolate Milk     3 11.21        <NA>
    ## 1977  Yogurt Flip Peanut Butter Dream     3 11.21        <NA>
    ## 1978 Yogurt Straw Banana Chobani Drin     2 11.21        <NA>
    ## 1979               Cornell Whole Milk     4 11.21        <NA>
    ## 1980       Yogurt Peach Greek Chobani     3 11.21        <NA>
    ## 1981                  Cornell 2% Milk     3 11.21        <NA>
    ## 1982   Milk Chocolate LF Cornell 8 Oz     3 11.21        <NA>
    ## 1983   Yogurt Raspberry Greek Chobani     2 11.21        <NA>
    ## 1984 Yogurt Black Cherry Greek Choban     1 11.21        <NA>
    ## 1985   Yogurt Blueberry Greek Chobani     1 11.21        <NA>
    ## 1986       Yogurt Plain Greek Chobani     1 11.21        <NA>
    ## 1987  Yogurt Strawberry Greek Chobani     1 11.21        <NA>
    ## 1988       Cornell Dairy Peach Yogurt     1 11.21        <NA>
    ## 1989        Bar S'Mores Quest 2.12 oz     4 11.21        <NA>
    ## 1990  Bar Peanut Butter Builders Clif     4 11.21        <NA>
    ## 1991 Jelly Konjac Apple Grape Tastell     3 11.21        <NA>
    ## 1992  Bar Frosted Birthday Cake Quest     2 11.21        <NA>
    ## 1993 Bar CHOC Mint Builders Clif bar2     2 11.21        <NA>
    ## 1994 Jelly Konjac Double Berry Tastel     2 11.21        <NA>
    ## 1995 Jelly Konjac Mango Pineapple Tas     2 11.21        <NA>
    ## 1996      Jelly Konjac Peach Tastelli     2 11.21        <NA>
    ## 1997 Dark Choc Cherry Cashew Plus Kin     2 11.21        <NA>
    ## 1998   Bar Crunchy Peanut Butter Clif     1 11.21        <NA>
    ## 1999 Bar That's It Apple+Strawberry 1     1 11.21        <NA>
    ## 2000 Nature Valley Peanut Butter Gran     1 11.21        <NA>
    ## 2001        Oat And Honey Granola Bar     1 11.21        <NA>
    ## 2002  Oats And Dark Chocolate Granola     1 11.21        <NA>
    ## 2003                      Fruit Whole    51 11.21        <NA>
    ## 2004           Chips Dirty Sea Salted     4 11.21        <NA>
    ## 2005       Jalapeno Heat Chips Kosher     3 11.21        <NA>
    ## 2006 Chip Potato Dirty BBQ Mesquite 2     2 11.21        <NA>
    ## 2007  Chips Cracked Pepper & Sea Salt     2 11.21        <NA>
    ## 2008    Chip Kettle Salt Vinegar 2 oz     1 11.21        <NA>
    ## 2009 Chip Potato Kettle Honey BBQ 2oz     1 11.21        <NA>
    ## 2010 Chip Salt & Malt Vinegar Dirty 2     1 11.21        <NA>
    ## 2011  Chip Sour Cream And Onion Dirty     1 11.21        <NA>
    ## 2012   Utz Honey Barbecue Chip 1.5 oz     1 11.21        <NA>
    ## 2013          Utz Salt N Vinegar Chip     1 11.21        <NA>
    ## 2014             Orbit Sweet Mint Gum     4 11.21        <NA>
    ## 2015             Orbit Wintermint Gum     4 11.21        <NA>
    ## 2016   Candy CHOC Cara Caff Bar Awake     1 11.21        <NA>
    ## 2017      Chewy Marshmallow GF 2.1 oz     4 11.21        <NA>
    ## 2018 GF Sweet Street Choloate Brownie     1 11.21        <NA>
    ## 2019    Fresh Cut Pineapple Fruit Cup     1 11.21        <NA>
    ## 2020 Ice Cream Mochi Double Chocolate     1 11.21        <NA>
    ## 2021                     Muffin Jumbo    35 11.21        <NA>
    ## 2022                           Cookie    25 11.21        <NA>
    ## 2023               Croissant Straw CC     4 11.21        <NA>
    ## 2024                   Croissant Choc     3 11.21        <NA>
    ## 2025                     Cinnamon Bun     3 11.21        <NA>
    ## 2026                  Plain Croissant     3 11.21        <NA>
    ## 2027                Croissant Blue CC     2 11.21        <NA>
    ## 2028      Combo 16 oz Coffee & Muffin     1 11.21        <NA>
    ## 2029                     PBJ on Wheat    11 11.21 grab 'n' go
    ## 2030                 BowlSesAsianNood     1 11.21 grab 'n' go
    ## 2031                   BowlSouthChick     1 11.21 grab 'n' go
    ## 2032                  California Roll     2 11.21 grab 'n' go
    ## 2033            Hawaiian Volcano Roll     1 11.21 grab 'n' go
    ## 2034              Tempura Crunch Roll     1 11.21 grab 'n' go
    ## 2035              Tempura Shrimp Roll     1 11.21 grab 'n' go
    ## 2036                         TSA Roll     1 11.21 grab 'n' go
    ## 2037               Golden Dragon Roll     1 11.21 grab 'n' go
    ## 2038                  Hawaiian Sunset     1 11.21 grab 'n' go
    ## 2039                      Salmon Roll     1 11.21 grab 'n' go
    ## 2040                  Spicy Tuna Roll     1 11.21 grab 'n' go
    ## 2041                     Avocado Roll     1 11.21 grab 'n' go
    ## 2042              Wrap Chicken Caesar     5 11.21 grab 'n' go
    ## 2043     Sandwich Corned Beef & Swiss     2 11.21 grab 'n' go
    ## 2044 Sandwich Black Forrest Ham & Swi     1 11.21 grab 'n' go
    ## 2045 Sandwich Crispy Chicken Milanese     1 11.21 grab 'n' go
    ## 2046       Quesadilla Deluxe Trillium   162 11.20     mexican
    ## 2047                Grilled Hamburger    92 11.20       grill
    ## 2048            Fried Chicken Tenders    93 11.20       grill
    ## 2049    Burrito Una Mano Trillium BYO    72 11.20     mexican
    ## 2050                     French Fries   124 11.20       grill
    ## 2051             Seared Salmon Burger    14 11.20       grill
    ## 2052  Grilled Chicken Breast Sandwich    13 11.20       grill
    ## 2053                Quesadilla Cheese    12 11.20     mexican
    ## 2054 Trillium Grill Impossible Burger     7 11.20       grill
    ## 2055               Sweet Potato Fries    24 11.20       grill
    ## 2056               ADD Chicken Breast     9 11.20       grill
    ## 2057                Black Bean Burger     3 11.20       grill
    ## 2058             ADD Beef Patty $2.99     5 11.20       grill
    ## 2059                       ADD Cheese     6 11.20       grill
    ## 2060              Add Sausage 2 Patty     1 11.20       grill
    ## 2061                     Add Egg $.99     2 11.20       grill
    ## 2062                1 Entree + 1 Side   176 11.20         wok
    ## 2063                1 Entree + 2 Side    97 11.20         wok
    ## 2064               Bowl Ramen Chicken    80 11.20       ramen
    ## 2065              2 Entrees + 2 Sides    21 11.20         wok
    ## 2066                  Bowl Ramen Tofu    19 11.20       ramen
    ## 2067          Side Vegetarian Lo Mein    13 11.20         wok
    ## 2068         Side White or Brown Rice     7 11.20         wok
    ## 2069                     1 Wok Entree     2 11.20         wok
    ## 2070                  Side Vegetables     2 11.20         wok
    ## 2071      Side Vegetable Spring Rolls     1 11.20         wok
    ## 2072  Side Vegetarian Fried Rice with     1 11.20         wok
    ## 2073      Create Your Pasta Bowl MEAT   143 11.20     italian
    ## 2074       Create Your Pasta Bowl VEG    32 11.20     italian
    ## 2075                   Add Extra Meat    20 11.20     italian
    ## 2076                     Pizza Cheese    10 11.20     italian
    ## 2077              Pizza with Toppings     8 11.20     italian
    ## 2078         Side Bread Pasta Station     2 11.20     italian
    ## 2079                Burrito Breakfast    91 11.20        <NA>
    ## 2080              Small French Omelet    55 11.20        <NA>
    ## 2081             Grand Slam Breakfast    16 11.20        <NA>
    ## 2082 Egg Cheese Sausage Breakfast San    24 11.20        <NA>
    ## 2083 Egg Cheese Bacon Breakfast Sandw    20 11.20        <NA>
    ## 2084                        Add Bacon    22 11.20        <NA>
    ## 2085                         Two Eggs     7 11.20        <NA>
    ## 2086                   Pancake Single     4 11.20        <NA>
    ## 2087              Trillium Home Fries     1 11.20        <NA>
    ## 2088                   2 Slices Toast     1 11.20        <NA>
    ## 2089                            Toast     1 11.20        <NA>
    ## 2090                 PC Peanut Butter     1 11.20        <NA>
    ## 2091                 Burrito Bowl BYO    88 11.20     mexican
    ## 2092                      Single Taco     5 11.20     mexican
    ## 2093      Add Extra Toppings Una Mano     2 11.20     mexican
    ## 2094               Salad by the Pound    71 11.20   salad bar
    ## 2095                       Soup 12 oz    56 11.20   salad bar
    ## 2096                        8 oz Soup    35 11.20   salad bar
    ## 2097                  Coffee 16 oz SB    35 11.20        <NA>
    ## 2098              Soda Fountain 24 oz    39 11.20        <NA>
    ## 2099              Soda Fountain 16 oz    42 11.20        <NA>
    ## 2100                  Coffee 12 oz SB    21 11.20        <NA>
    ## 2101                    Hot Tea 20 oz     8 11.20        <NA>
    ## 2102                        Hot Cocoa     3 11.20        <NA>
    ## 2103               Open Miscellaneous     9 11.20        <NA>
    ## 2104                 Side Potato Tots    11 11.20        <NA>
    ## 2105             Water Aquafina 20 oz    17 11.20        <NA>
    ## 2106 Tea Golden Oolong Unsweet Ito En     9 11.20        <NA>
    ## 2107   Tea Iced Rasberry Lipton 16 oz    12 11.20        <NA>
    ## 2108    Yerba Mate Bluephoria 15.5 oz     7 11.20        <NA>
    ## 2109 Yerba Mate Enlighten Mint 15.5 o     7 11.20        <NA>
    ## 2110 Juice Orange Premium Topicana 11     9 11.20        <NA>
    ## 2111            Soda Pepsi Diet 20 Oz    10 11.20        <NA>
    ## 2112 Tea Green Peach Mango Celsius 12     7 11.20        <NA>
    ## 2113 Yerba Mate Peach Revival 15.5 oz     6 11.20        <NA>
    ## 2114 Milk Chocolate LF BIG RED Refuel    10 11.20        <NA>
    ## 2115 Energy Mango PassFruit Celsius 1     6 11.20        <NA>
    ## 2116  Tea Green Rasp Acai  Celsius 12     6 11.20        <NA>
    ## 2117 Gatorade Galcier Freeze Zero G 2     7 11.20        <NA>
    ## 2118   Yerba Mate Revel Berry 15.5 oz     5 11.20        <NA>
    ## 2119      Yerba Mate Tropical 15.5 oz     5 11.20        <NA>
    ## 2120    Tea Black Milk Ito En 11.8 oz     5 11.20        <NA>
    ## 2121 Gatorade Propel  Kiwi Straw Wate     7 11.20        <NA>
    ## 2122       Juice Naked PNCLDA 15.2 Oz     4 11.20        <NA>
    ## 2123                Water Aquafina 1L     7 11.20        <NA>
    ## 2124   Kombucha Ginger Kevita 15.2 oz     4 11.20        <NA>
    ## 2125       Lipton Pure Leaf Sweet Tea     7 11.20        <NA>
    ## 2126  Soda Rootbeer Zero Sugar Mug 20     7 11.20        <NA>
    ## 2127     Water Life WTR Immune 700 ML     5 11.20        <NA>
    ## 2128         Yerba Mate Lemon 15.5 oz     4 11.20        <NA>
    ## 2129 Cold Brew Chocolate Cream Starbu     4 11.20        <NA>
    ## 2130 Cold Brew Vanilla Sweet Cream St     4 11.20        <NA>
    ## 2131  Soda Ginger Ale Schweppes 20 Oz     6 11.20        <NA>
    ## 2132 Tea Sweet W/ Le Pure Leaf Lipton     6 11.20        <NA>
    ## 2133 Tea Tea & Lemon Pure Leaf Lipton     6 11.20        <NA>
    ## 2134 Tea Unsweetened Pure Leaf Lipton     6 11.20        <NA>
    ## 2135 Gatorade Propel Grape Water 1 lt     5 11.20        <NA>
    ## 2136   Energy Artic Vibe Celsius 12oz     4 11.20        <NA>
    ## 2137 Energy Fast Twitch Watermelon St     4 11.20        <NA>
    ## 2138   Energy StrawLemon Celsius 12oz     4 11.20        <NA>
    ## 2139   Frappuccino Vanilla SB 13.7 Oz     3 11.20        <NA>
    ## 2140        Juice Naked Green Machine     3 11.20        <NA>
    ## 2141         Juice Naked Mighty Mango     3 11.20        <NA>
    ## 2142    Naked Strawberry Banana Juice     3 11.20        <NA>
    ## 2143                 Soda Pepsi 20 Oz     5 11.20        <NA>
    ## 2144 Gatorade Gatorlyte Glacier Freez     3 11.20        <NA>
    ## 2145       Juice AppleTropicana 11 oz     4 11.20        <NA>
    ## 2146 Juice Orange Homestyle Tropicana     4 11.20        <NA>
    ## 2147 Juice Raspberry Lemonade Tropica     4 11.20        <NA>
    ## 2148  Energy Blue Crush Celsius 16 oz     3 11.20        <NA>
    ## 2149 Energy Fruit Burst Celsius 16 oz     3 11.20        <NA>
    ## 2150 Muscle Milk P40 Strawberry Cream     2 11.20        <NA>
    ## 2151 Muscle Milk PROSRS 40 Intense Va     2 11.20        <NA>
    ## 2152          Soda Mountain Dew 20 Oz     4 11.20        <NA>
    ## 2153          Soda Orange Crush 20 Oz     4 11.20        <NA>
    ## 2154            Soda Pepsi  Zero 20oz     4 11.20        <NA>
    ## 2155     Soda Starry Lemon Lime 20 oz     4 11.20        <NA>
    ## 2156  Tea Peach Pure Leaf Lipton 18.5     4 11.20        <NA>
    ## 2157 Frappuccino Oatmilk Caramel Whit     2 11.20        <NA>
    ## 2158          Juice Naked Berry Blast     2 11.20        <NA>
    ## 2159         Juice Protein Zone Naked     2 11.20        <NA>
    ## 2160 Juice Ocean Spray Cranbe Grape 1     4 11.20        <NA>
    ## 2161              Gatorade Blue 28 oz     3 11.20        <NA>
    ## 2162 Gatorade Gatorlyte Strawberry Ki     2 11.20        <NA>
    ## 2163     Starbucks Double Shot 6.5 Oz     2 11.20        <NA>
    ## 2164 Tea Green Matcha Milk Ito En 11.     2 11.20        <NA>
    ## 2165 Energy 222 Blue Raspberry Odysse     2 11.20        <NA>
    ## 2166 Energy 222 Pineapple Mango Odyss     2 11.20        <NA>
    ## 2167 Energy Cherry Limeade Celsius 16     2 11.20        <NA>
    ## 2168 Juice Fuji Apple Red Jacket 12oz     2 11.20        <NA>
    ## 2169 Juice Joes Lemonade  Red Jacket1     2 11.20        <NA>
    ## 2170 Red Jacket Strawberry Apple Juic     2 11.20        <NA>
    ## 2171 Starbucks Espress & Salt Cara DS     2 11.20        <NA>
    ## 2172     Soda Pepsi Wild Cherry 20 Oz     3 11.20        <NA>
    ## 2173          Soda Rootbeer Mug 20 oz     3 11.20        <NA>
    ## 2174 Tea Unsweet Green Lipton 18.5 oz     3 11.20        <NA>
    ## 2175         Juice Apple Dole 15.2 oz     3 11.20        <NA>
    ## 2176   Ocean Spray Cranberry Cocktail     3 11.20        <NA>
    ## 2177    Water Aquafina Alumitek 16 oz     3 11.20        <NA>
    ## 2178        Gatorade Lemon Lime 28 oz     2 11.20        <NA>
    ## 2179       Juice Grape Tropicana 11oz     2 11.20        <NA>
    ## 2180  Juice Lively Lemonade Tropicana     2 11.20        <NA>
    ## 2181 Gatorade Propel Berry Water 1 lt     2 11.20        <NA>
    ## 2182  Sparkling Lemonade Mango Kevita     2 11.20        <NA>
    ## 2183  Soda Pineapple Jarritos 12.5 oz     2 11.20        <NA>
    ## 2184         Muscle Milk Choc PB 14oz     1 11.20        <NA>
    ## 2185    Muscle Milk KO Chocolate 14oz     1 11.20        <NA>
    ## 2186 Tea Blackberry Pure Leaf 18.5 oz     2 11.20        <NA>
    ## 2187 Tea Pure Leaf Zero Sugar Sweet T     2 11.20        <NA>
    ## 2188 Tea Unsweet Black W Lemon 18.5 o     2 11.20        <NA>
    ## 2189 Starbucks Doubleshot Ener Coffee     1 11.20        <NA>
    ## 2190  Starbucks Doubleshot Energy Van     1 11.20        <NA>
    ## 2191   Frappuccino Caramel SB 13.7 oz     1 11.20        <NA>
    ## 2192     Frappuccino Mocha 13.7 Oz SB     1 11.20        <NA>
    ## 2193   Soda Mandarin Jarritos 12.5 oz     2 11.20        <NA>
    ## 2194  Kombucha Pineapple Peach Kevita     1 11.20        <NA>
    ## 2195  Kombucha Rasp Lemon Kevita 15.2     1 11.20        <NA>
    ## 2196 Gatorade Gatorlyte MixBerry 20 o     1 11.20        <NA>
    ## 2197  Gatorade Gatorlyte Orange 20 oz     1 11.20        <NA>
    ## 2198 Energy Dragonberry Celsius 16 oz     1 11.20        <NA>
    ## 2199 Energy Mango Tango Celsius 16 oz     1 11.20        <NA>
    ## 2200 Energy Orangesicle Celsius 16 oz     1 11.20        <NA>
    ## 2201 Energy Passion Orange Guva Odyss     1 11.20        <NA>
    ## 2202 Energy Revive Prickly Pear Odyss     1 11.20        <NA>
    ## 2203    Rockstar Pure Zero Silver Ice     1 11.20        <NA>
    ## 2204  Energy Fast Twitch Cool Blue 12     1 11.20        <NA>
    ## 2205   Energy Fast Twitch Grape 12 oz     1 11.20        <NA>
    ## 2206 Energy Fuji Apple Pear Celsius 1     1 11.20        <NA>
    ## 2207    Energy LemonLime Celsius 12oz     1 11.20        <NA>
    ## 2208   Energy StrawGuava Celsius 12oz     1 11.20        <NA>
    ## 2209 Mountain Dew Kickstart Orange 16     1 11.20        <NA>
    ## 2210           Water Gatorade 33.8 oz     1 11.20        <NA>
    ## 2211             Gatorade Orange 28oz     1 11.20        <NA>
    ## 2212        Gatorade Zero Grape 28 oz     1 11.20        <NA>
    ## 2213    Glacier Cherry Gatorade 28 oz     1 11.20        <NA>
    ## 2214    Glacier Freeze Gatorade 28 oz     1 11.20        <NA>
    ## 2215 Juice Zero Summer Splash Punch 1     1 11.20        <NA>
    ## 2216 Sparkling lemonade Strawberry Ke     1 11.20        <NA>
    ## 2217             Water Gatorade 700ml     1 11.20        <NA>
    ## 2218      Soda Guava Jarritos 12.5 oz     1 11.20        <NA>
    ## 2219      Soda Mango Jarritos 12.5 oz     1 11.20        <NA>
    ## 2220     Soda Mountain Dew Zero 20 Oz     1 11.20        <NA>
    ## 2221 Tea Sweetened With Lemon Brisk 2     1 11.20        <NA>
    ## 2222         Juice Lemonade Dole 20oz     1 11.20        <NA>
    ## 2223   Cornell Low Fat Chocolate Milk     5 11.20        <NA>
    ## 2224 Yogurt Mixed Berry Chobani Drink     4 11.20        <NA>
    ## 2225 Yogurt Flip Almond Coco Loco Cho     5 11.20        <NA>
    ## 2226 Yogurt 0% Fat Vanilla Greek Oiko     5 11.20        <NA>
    ## 2227       Yogurt Mango Greek Chobani     5 11.20        <NA>
    ## 2228  Yogurt Mango Chobani Drink 7 oz     3 11.20        <NA>
    ## 2229 Yogurt Black Cherry Greek Choban     4 11.20        <NA>
    ## 2230 Yogurt Straw Banana Chobani Drin     2 11.20        <NA>
    ## 2231   Yogurt Blueberry Greek Chobani     3 11.20        <NA>
    ## 2232       Cornell 2% Milk (70000380)     2 11.20        <NA>
    ## 2233  Yogurt Flip Peanut Butter Dream     2 11.20        <NA>
    ## 2234       Cornell 2% Milk (70000381)     3 11.20        <NA>
    ## 2235               Cornell Whole Milk     3 11.20        <NA>
    ## 2236   Milk Chocolate LF Cornell 8 Oz     3 11.20        <NA>
    ## 2237 Yogurt Strawberry Banana Greek C     2 11.20        <NA>
    ## 2238  Yogurt Strawberry Greek Chobani     2 11.20        <NA>
    ## 2239       Yogurt Peach Greek Chobani     1 11.20        <NA>
    ## 2240   Yogurt Raspberry Greek Chobani     1 11.20        <NA>
    ## 2241            8 oz Vanilla Soy Silk     1 11.20        <NA>
    ## 2242   Bar Crunchy Peanut Butter Clif     3 11.20        <NA>
    ## 2243        Bar S'Mores Quest 2.12 oz     2 11.20        <NA>
    ## 2244 Bar CHOC Mint Builders Clif bar2     2 11.20        <NA>
    ## 2245 Jelly Konjac Apple Grape Tastell     2 11.20        <NA>
    ## 2246 Jelly Konjac Double Berry Tastel     2 11.20        <NA>
    ## 2247 Bar Choc Chip Cookie Dough Quest     1 11.20        <NA>
    ## 2248 Bar Cookie & Cream Quest 2.12 oz     1 11.20        <NA>
    ## 2249 Cookie Birthday Cake Lenny & Lar     1 11.20        <NA>
    ## 2250  Bar Peanut Butter Builders Clif     1 11.20        <NA>
    ## 2251   Fruit And Nut Delight Kind Bar     1 11.20        <NA>
    ## 2252        Oat And Honey Granola Bar     2 11.20        <NA>
    ## 2253 Jelly Konjac Mango Pineapple Tas     1 11.20        <NA>
    ## 2254      Jelly Konjac Peach Tastelli     1 11.20        <NA>
    ## 2255       Bar Chocolate Chip Clifbar     1 11.20        <NA>
    ## 2256      Kind Almond And Coconut Bar     1 11.20        <NA>
    ## 2257 Bar That's It Apple+Strawberry 1     1 11.20        <NA>
    ## 2258                      Fruit Whole    58 11.20        <NA>
    ## 2259 Nuts Almonds Honey 1.5 oz Sahale     2 11.20        <NA>
    ## 2260 Chip Salt & Malt Vinegar Dirty 2     3 11.20        <NA>
    ## 2261   Utz Honey Barbecue Chip 1.5 oz     3 11.20        <NA>
    ## 2262           Chips Dirty Sea Salted     2 11.20        <NA>
    ## 2263  Chip Sour Cream And Onion Dirty     2 11.20        <NA>
    ## 2264     Chip Kettle Sweet Onion 2 oz     1 11.20        <NA>
    ## 2265 Chip Potato Dirty BBQ Mesquite 2     1 11.20        <NA>
    ## 2266  Chips Cracked Pepper & Sea Salt     1 11.20        <NA>
    ## 2267                 Utz Regular Chip     1 11.20        <NA>
    ## 2268          Utz Salt N Vinegar Chip     1 11.20        <NA>
    ## 2269         Pretzel Thin 2.12 oz Utz     1 11.20        <NA>
    ## 2270        Fresh Cut Melon Fruit Cup     4 11.20        <NA>
    ## 2271    Fresh Cut Pineapple Fruit Cup     4 11.20        <NA>
    ## 2272             Orbit Wintermint Gum     3 11.20        <NA>
    ## 2273 Candy Milk Choc Caff Bar Awake 1     2 11.20        <NA>
    ## 2274             Orbit Sweet Mint Gum     2 11.20        <NA>
    ## 2275 GF Sweet Street Choloate Brownie     3 11.20        <NA>
    ## 2276      Chewy Marshmallow GF 2.1 oz     2 11.20        <NA>
    ## 2277  Ice Cream Mochi Sweet Mango 1.5     2 11.20        <NA>
    ## 2278                 MochiCookCrm1.5o     1 11.20        <NA>
    ## 2279              Wrap Chicken Caesar     5 11.20 grab 'n' go
    ## 2280 Sandwich Prosciutto & Mozzarella     3 11.20 grab 'n' go
    ## 2281             Wrap Buffalo Chicken     3 11.20 grab 'n' go
    ## 2282 Sandwich Black Forrest Ham & Swi     1 11.20 grab 'n' go
    ## 2283 Sandwich Crispy Chicken Milanese     1 11.20 grab 'n' go
    ## 2284             Salad Chicken Caesar     1 11.20 grab 'n' go
    ## 2285                  California Roll     2 11.20 grab 'n' go
    ## 2286            Hawaiian Volcano Roll     1 11.20 grab 'n' go
    ## 2287                     Alaskan Roll     1 11.20 grab 'n' go
    ## 2288              Tempura Crunch Roll     1 11.20 grab 'n' go
    ## 2289              Tempura Shrimp Roll     1 11.20 grab 'n' go
    ## 2290                         TSA Roll     1 11.20 grab 'n' go
    ## 2291               Golden Dragon Roll     1 11.20 grab 'n' go
    ## 2292                  Hawaiian Sunset     1 11.20 grab 'n' go
    ## 2293                      Salmon Roll     1 11.20 grab 'n' go
    ## 2294                     Avocado Roll     1 11.20 grab 'n' go
    ## 2295                     Muffin Jumbo    25 11.20        <NA>
    ## 2296                           Cookie    23 11.20        <NA>
    ## 2297                  Plain Croissant     3 11.20        <NA>
    ## 2298                     Cinnamon Bun     2 11.20        <NA>
    ## 2299                            Scone     2 11.20        <NA>
    ## 2300                Croissant Blue CC     1 11.20        <NA>
    ## 2301                   Croissant Choc     1 11.20        <NA>
    ## 2302               Croissant Straw CC     1 11.20        <NA>
    ## 2303                          Brownie     1 11.20        <NA>
    ## 2304                   BowlMedProtein     3 11.20 grab 'n' go
    ## 2305                 BowlSesAsianNood     2 11.20 grab 'n' go
    ## 2306                 BowlChickAlfrPen     1 11.20 grab 'n' go
    ## 2307                     PBJ on Wheat     9 11.20 grab 'n' go
    ## 2308       Quesadilla Deluxe Trillium   189 11.19     mexican
    ## 2309                Grilled Hamburger   122 11.19       grill
    ## 2310            Fried Chicken Tenders    99 11.19       grill
    ## 2311    Burrito Una Mano Trillium BYO    69 11.19     mexican
    ## 2312                     French Fries   143 11.19       grill
    ## 2313  Grilled Chicken Breast Sandwich    26 11.19       grill
    ## 2314                Quesadilla Cheese    22 11.19     mexican
    ## 2315 Trillium Grill Impossible Burger    10 11.19       grill
    ## 2316               Sweet Potato Fries    31 11.19       grill
    ## 2317             Seared Salmon Burger     6 11.19       grill
    ## 2318                Black Bean Burger     3 11.19       grill
    ## 2319               ADD Chicken Breast     7 11.19       grill
    ## 2320             ADD Beef Patty $2.99     7 11.19       grill
    ## 2321                     Add Egg $.99     5 11.19       grill
    ## 2322        ADD Burger Salmon Grilled     1 11.19       grill
    ## 2323                       ADD Cheese     5 11.19       grill
    ## 2324                1 Entree + 1 Side   185 11.19         wok
    ## 2325                1 Entree + 2 Side    92 11.19         wok
    ## 2326               Bowl Ramen Chicken    70 11.19       ramen
    ## 2327              2 Entrees + 2 Sides    33 11.19         wok
    ## 2328                  Bowl Ramen Tofu    28 11.19       ramen
    ## 2329          Side Vegetarian Lo Mein    16 11.19         wok
    ## 2330                     1 Wok Entree     7 11.19         wok
    ## 2331         Side White or Brown Rice    12 11.19         wok
    ## 2332      Side Vegetable Spring Rolls     2 11.19         wok
    ## 2333  Side Vegetarian Fried Rice with     2 11.19         wok
    ## 2334      Create Your Pasta Bowl MEAT   115 11.19     italian
    ## 2335       Create Your Pasta Bowl VEG    28 11.19     italian
    ## 2336              Pizza with Toppings    38 11.19     italian
    ## 2337                     Pizza Cheese    22 11.19     italian
    ## 2338                   Add Extra Meat    18 11.19     italian
    ## 2339                Burrito Breakfast    95 11.19        <NA>
    ## 2340              Small French Omelet    47 11.19        <NA>
    ## 2341 Egg Cheese Sausage Breakfast San    30 11.19        <NA>
    ## 2342 Egg Cheese Bacon Breakfast Sandw    23 11.19        <NA>
    ## 2343             Grand Slam Breakfast    12 11.19        <NA>
    ## 2344                        Add Bacon    28 11.19        <NA>
    ## 2345                         Two Eggs    14 11.19        <NA>
    ## 2346                   Pancake Single     8 11.19        <NA>
    ## 2347              Trillium Home Fries     2 11.19        <NA>
    ## 2348                   2 Slices Toast     3 11.19        <NA>
    ## 2349                 Burrito Bowl BYO   110 11.19     mexican
    ## 2350                      Single Taco    11 11.19     mexican
    ## 2351                   Side Guacamole     3 11.19     mexican
    ## 2352      Add Extra Toppings Una Mano     3 11.19     mexican
    ## 2353               Salad by the Pound    75 11.19   salad bar
    ## 2354                       Soup 12 oz    52 11.19   salad bar
    ## 2355                        8 oz Soup    55 11.19   salad bar
    ## 2356              Soda Fountain 24 oz    48 11.19        <NA>
    ## 2357                  Coffee 16 oz SB    32 11.19        <NA>
    ## 2358              Soda Fountain 16 oz    41 11.19        <NA>
    ## 2359                  Coffee 12 oz SB    22 11.19        <NA>
    ## 2360                    Hot Tea 20 oz     2 11.19        <NA>
    ## 2361                        Hot Cocoa     3 11.19        <NA>
    ## 2362                 Side Potato Tots    21 11.19        <NA>
    ## 2363               Open Miscellaneous     8 11.19        <NA>
    ## 2364          Add Extra Protein $2.99     1 11.19        <NA>
    ## 2365            Soda Pepsi Diet 20 Oz    21 11.19        <NA>
    ## 2366             Water Aquafina 20 oz    19 11.19        <NA>
    ## 2367 Tea Jasmine Green Unsweet Ito En    10 11.19        <NA>
    ## 2368                Water Aquafina 1L    15 11.19        <NA>
    ## 2369 Yerba Mate Enlighten Mint 15.5 o     9 11.19        <NA>
    ## 2370 Milk Chocolate LF BIG RED Refuel    16 11.19        <NA>
    ## 2371 Yerba Mate Peach Revival 15.5 oz     8 11.19        <NA>
    ## 2372   Yerba Mate Revel Berry 15.5 oz     8 11.19        <NA>
    ## 2373   Frappuccino Caramel SB 13.7 oz     6 11.19        <NA>
    ## 2374   Energy Artic Vibe Celsius 12oz     7 11.19        <NA>
    ## 2375 Energy Mango PassFruit Celsius 1     7 11.19        <NA>
    ## 2376       Juice AppleTropicana 11 oz     8 11.19        <NA>
    ## 2377    Naked Strawberry Banana Juice     5 11.19        <NA>
    ## 2378         Juice Apple Dole 15.2 oz     9 11.19        <NA>
    ## 2379              Gatorade Blue 28 oz     7 11.19        <NA>
    ## 2380     Water Life WTR Immune 700 ML     6 11.19        <NA>
    ## 2381    Yerba Mate Bluephoria 15.5 oz     5 11.19        <NA>
    ## 2382   Tea Iced Rasberry Lipton 16 oz     8 11.19        <NA>
    ## 2383 Energy 222 Blue Raspberry Odysse     5 11.19        <NA>
    ## 2384   Frappuccino Vanilla SB 13.7 Oz     4 11.19        <NA>
    ## 2385   Kombucha Ginger Kevita 15.2 oz     4 11.19        <NA>
    ## 2386   Energy StrawGuava Celsius 12oz     5 11.19        <NA>
    ## 2387 Tea Green Peach Mango Celsius 12     5 11.19        <NA>
    ## 2388          Soda Mountain Dew 20 Oz     7 11.19        <NA>
    ## 2389 Muscle Milk P40 Strawberry Cream     3 11.19        <NA>
    ## 2390            Soda Pepsi  Zero 20oz     6 11.19        <NA>
    ## 2391          Soda Rootbeer Mug 20 oz     6 11.19        <NA>
    ## 2392  Starbucks Doubleshot Energy Van     3 11.19        <NA>
    ## 2393 Gatorade Propel  Kiwi Straw Wate     5 11.19        <NA>
    ## 2394     Frappuccino Mocha 13.7 Oz SB     3 11.19        <NA>
    ## 2395         Juice Naked Blue Machine     3 11.19        <NA>
    ## 2396         Juice Naked Mighty Mango     3 11.19        <NA>
    ## 2397       Juice Naked PNCLDA 15.2 Oz     3 11.19        <NA>
    ## 2398  Kombucha Pineapple Peach Kevita     3 11.19        <NA>
    ## 2399  Kombucha Rasp Lemon Kevita 15.2     3 11.19        <NA>
    ## 2400                 Soda Pepsi 20 Oz     5 11.19        <NA>
    ## 2401 Tea Tea & Lemon Pure Leaf Lipton     5 11.19        <NA>
    ## 2402 Tea Unsweet Black W Lemon 18.5 o     5 11.19        <NA>
    ## 2403 Tea Golden Oolong Unsweet Ito En     3 11.19        <NA>
    ## 2404 Tea Green Matcha Milk Ito En 11.     3 11.19        <NA>
    ## 2405       Gatorade Fruit Punch 28 oz     4 11.19        <NA>
    ## 2406  Juice Lively Lemonade Tropicana     4 11.19        <NA>
    ## 2407 Juice Orange Premium Topicana 11     4 11.19        <NA>
    ## 2408 Energy Mango Tango Celsius 16 oz     3 11.19        <NA>
    ## 2409 Starbucks Espress & Salt Cara DS     3 11.19        <NA>
    ## 2410      Soda Mango Jarritos 12.5 oz     4 11.19        <NA>
    ## 2411 Energy Blue Raz Lemonade Celsius     3 11.19        <NA>
    ## 2412 Energy Fast Twitch Watermelon St     3 11.19        <NA>
    ## 2413 Energy Fuji Apple Pear Celsius 1     3 11.19        <NA>
    ## 2414  Tea Green Rasp Acai  Celsius 12     3 11.19        <NA>
    ## 2415 Muscle Milk PROSRS 40 Intense Va     2 11.19        <NA>
    ## 2416     Soda Pepsi Wild Cherry 20 Oz     4 11.19        <NA>
    ## 2417  Tea Peach Pure Leaf Lipton 18.5     4 11.19        <NA>
    ## 2418 Tea Sweet W/ Le Pure Leaf Lipton     4 11.19        <NA>
    ## 2419 Tea Unsweet Green Lipton 18.5 oz     4 11.19        <NA>
    ## 2420        Juice Orange Dole 15.2 oz     4 11.19        <NA>
    ## 2421         Juice Protein Zone Naked     2 11.19        <NA>
    ## 2422 Juice Ocean Spray Cranbe Grape 1     4 11.19        <NA>
    ## 2423   Soda Mandarin Jarritos 12.5 oz     4 11.19        <NA>
    ## 2424 Gatorade Galcier Freeze Zero G 2     3 11.19        <NA>
    ## 2425        Gatorade Lemon Lime 28 oz     3 11.19        <NA>
    ## 2426    Glacier Freeze Gatorade 28 oz     3 11.19        <NA>
    ## 2427 Juice Orange Homestyle Tropicana     3 11.19        <NA>
    ## 2428 Juice Raspberry Lemonade Tropica     3 11.19        <NA>
    ## 2429 Sparkling lemonade Strawberry Ke     3 11.19        <NA>
    ## 2430 Gatorade Gatorlyte Strawberry Ki     2 11.19        <NA>
    ## 2431      Yerba Mate Tropical 15.5 oz     2 11.19        <NA>
    ## 2432    Tea Black Milk Ito En 11.8 oz     2 11.19        <NA>
    ## 2433  Energy Blue Crush Celsius 16 oz     2 11.19        <NA>
    ## 2434 Energy Cherry Limeade Celsius 16     2 11.19        <NA>
    ## 2435 Energy Dragonberry Celsius 16 oz     2 11.19        <NA>
    ## 2436 Juice Joes Lemonade  Red Jacket1     2 11.19        <NA>
    ## 2437 Red Jacket Strawberry Apple Juic     2 11.19        <NA>
    ## 2438  Soda Rootbeer Zero Sugar Mug 20     3 11.19        <NA>
    ## 2439 Tea Pure Leaf Zero Sugar Sweet T     3 11.19        <NA>
    ## 2440 Mountain Dew Kickstart Black Che     2 11.19        <NA>
    ## 2441    Water Aquafina Alumitek 16 oz     3 11.19        <NA>
    ## 2442             Gatorade Orange 28oz     2 11.19        <NA>
    ## 2443    Glacier Cherry Gatorade 28 oz     2 11.19        <NA>
    ## 2444 Gatorade Propel Berry Water 1 lt     2 11.19        <NA>
    ## 2445  Sparkling Lemonade Mango Kevita     2 11.19        <NA>
    ## 2446         Muscle Milk Choc PB 14oz     1 11.19        <NA>
    ## 2447    Muscle Milk KO Chocolate 14oz     1 11.19        <NA>
    ## 2448       Lipton Pure Leaf Sweet Tea     2 11.19        <NA>
    ## 2449  Soda Ginger Ale Schweppes 20 Oz     2 11.19        <NA>
    ## 2450 Tea Sweetened With Lemon Brisk 2     2 11.19        <NA>
    ## 2451 Tea Unsweetened Pure Leaf Lipton     2 11.19        <NA>
    ## 2452 Starbucks Doubleshot Ener Coffee     1 11.19        <NA>
    ## 2453 Starbucks Doubleshot Energy Moch     1 11.19        <NA>
    ## 2454         Juice Lemonade Dole 20oz     2 11.19        <NA>
    ## 2455          Juice Naked Berry Blast     1 11.19        <NA>
    ## 2456 Gatorade Gatorlyte Cherry Lime 2     1 11.19        <NA>
    ## 2457 Gatorade Gatorlyte Glacier Freez     1 11.19        <NA>
    ## 2458  Gatorade Gatorlyte Orange 20 oz     1 11.19        <NA>
    ## 2459         Yerba Mate Lemon 15.5 oz     1 11.19        <NA>
    ## 2460 Cold Brew Chocolate Cream Starbu     1 11.19        <NA>
    ## 2461 Cold Brew Vanilla Sweet Cream St     1 11.19        <NA>
    ## 2462 Energy 222 Pineapple Mango Odyss     1 11.19        <NA>
    ## 2463 Energy Orangesicle Celsius 16 oz     1 11.19        <NA>
    ## 2464 Energy Passion Orange Guva Odyss     1 11.19        <NA>
    ## 2465    Rockstar Pure Zero Silver Ice     1 11.19        <NA>
    ## 2466   Energy Fast Twitch Grape 12 oz     1 11.19        <NA>
    ## 2467   Energy StrawLemon Celsius 12oz     1 11.19        <NA>
    ## 2468 Mountain Dew Kickstart Orange 16     1 11.19        <NA>
    ## 2469           Water Gatorade 33.8 oz     1 11.19        <NA>
    ## 2470     Cider Apple Red Jacket 12 oz     1 11.19        <NA>
    ## 2471        Gatorade Zero Grape 28 oz     1 11.19        <NA>
    ## 2472 Juice Zero Summer Splash Punch 1     1 11.19        <NA>
    ## 2473 Kickstart Strawberry Start-up 16     1 11.19        <NA>
    ## 2474 Gatorade Propel Grape Water 1 lt     1 11.19        <NA>
    ## 2475             Water Gatorade 700ml     1 11.19        <NA>
    ## 2476      Soda Guava Jarritos 12.5 oz     1 11.19        <NA>
    ## 2477  Soda Pineapple Jarritos 12.5 oz     1 11.19        <NA>
    ## 2478     Soda Mountain Dew Zero 20 Oz     1 11.19        <NA>
    ## 2479          Soda Orange Crush 20 Oz     1 11.19        <NA>
    ## 2480     Soda Starry Lemon Lime 20 oz     1 11.19        <NA>
    ## 2481 Tea Blackberry Pure Leaf 18.5 oz     1 11.19        <NA>
    ## 2482     Tea Light Peach Lipton 20 oz     1 11.19        <NA>
    ## 2483   Cornell Low Fat Chocolate Milk    10 11.19        <NA>
    ## 2484 Yogurt 0% Fat Vanilla Greek Oiko    11 11.19        <NA>
    ## 2485   Milk Chocolate LF Cornell 8 Oz     8 11.19        <NA>
    ## 2486               Cornell Whole Milk     6 11.19        <NA>
    ## 2487 Yogurt Black Cherry Greek Choban     4 11.19        <NA>
    ## 2488   Yogurt Raspberry Greek Chobani     4 11.19        <NA>
    ## 2489 Yogurt Strawberry Banana Greek C     4 11.19        <NA>
    ## 2490 Yogurt Flip Almond Coco Loco Cho     3 11.19        <NA>
    ## 2491  Yogurt Flip Peanut Butter Dream     3 11.19        <NA>
    ## 2492  Yogurt Mango Chobani Drink 7 oz     2 11.19        <NA>
    ## 2493 Yogurt Straw Banana Chobani Drin     2 11.19        <NA>
    ## 2494       Cornell 2% Milk (70000380)     2 11.19        <NA>
    ## 2495       Cornell 2% Milk (70000381)     3 11.19        <NA>
    ## 2496       Yogurt Mango Greek Chobani     2 11.19        <NA>
    ## 2497       Yogurt Peach Greek Chobani     2 11.19        <NA>
    ## 2498  Yogurt Strawberry Greek Chobani     2 11.19        <NA>
    ## 2499 Yogurt Mixed Berry Chobani Drink     1 11.19        <NA>
    ## 2500   Yogurt Blueberry Greek Chobani     1 11.19        <NA>
    ## 2501       Yogurt Plain Greek Chobani     1 11.19        <NA>
    ## 2502 Bar CHOC Mint Builders Clif bar2     4 11.19        <NA>
    ## 2503 Jelly Konjac Double Berry Tastel     3 11.19        <NA>
    ## 2504 Jelly Konjac Mango Pineapple Tas     3 11.19        <NA>
    ## 2505      Jelly Konjac Peach Tastelli     3 11.19        <NA>
    ## 2506 Bar Cookie & Cream Quest 2.12 oz     2 11.19        <NA>
    ## 2507  Cookie Choc Chip Lenny & Larrys     2 11.19        <NA>
    ## 2508 Cookie Peanut Butter Lenny & Lar     2 11.19        <NA>
    ## 2509 Bar White Chocolate Macadamia Cl     2 11.19        <NA>
    ## 2510   Kind Cranberry Almond Plus Bar     2 11.19        <NA>
    ## 2511 Bar That's It Apple+Strawberry 1     2 11.19        <NA>
    ## 2512        Bar S'Mores Quest 2.12 oz     1 11.19        <NA>
    ## 2513 Cookie Double Choc Lenny & Larry     1 11.19        <NA>
    ## 2514  Bar Peanut Butter Builders Clif     1 11.19        <NA>
    ## 2515 Dark Choc Cherry Cashew Plus Kin     1 11.19        <NA>
    ## 2516      Kind Almond And Coconut Bar     1 11.19        <NA>
    ## 2517        Oat And Honey Granola Bar     1 11.19        <NA>
    ## 2518  Oats And Dark Chocolate Granola     1 11.19        <NA>
    ## 2519                      Fruit Whole    65 11.19        <NA>
    ## 2520        Fresh Cut Melon Fruit Cup     6 11.19        <NA>
    ## 2521   Fresh Cut Watermelon Fruit Cup     3 11.19        <NA>
    ## 2522    Fresh Cut Pineapple Fruit Cup     2 11.19        <NA>
    ## 2523  Chips Cracked Pepper & Sea Salt     3 11.19        <NA>
    ## 2524                 Utz Regular Chip     3 11.19        <NA>
    ## 2525       Chip Maui Onion Dirty 2 oz     2 11.19        <NA>
    ## 2526 Chip Salt & Malt Vinegar Dirty 2     2 11.19        <NA>
    ## 2527  Chip Sour Cream And Onion Dirty     2 11.19        <NA>
    ## 2528   Chip Voodoo Limited Zapps 2 oz     2 11.19        <NA>
    ## 2529 Chip Potato Kettle Honey BBQ 2oz     1 11.19        <NA>
    ## 2530 Chip Potato Dirty BBQ Mesquite 2     1 11.19        <NA>
    ## 2531           Chips Dirty Sea Salted     1 11.19        <NA>
    ## 2532       Jalapeno Heat Chips Kosher     1 11.19        <NA>
    ## 2533   Utz Honey Barbecue Chip 1.5 oz     1 11.19        <NA>
    ## 2534         Pretzel Thin 2.12 oz Utz     1 11.19        <NA>
    ## 2535      Chewy Marshmallow GF 2.1 oz     5 11.19        <NA>
    ## 2536 GF Sweet Street Choloate Brownie     4 11.19        <NA>
    ## 2537             Orbit Sweet Mint Gum     4 11.19        <NA>
    ## 2538             Orbit Wintermint Gum     3 11.19        <NA>
    ## 2539 Candy Dark Chocolate Awake Bar 1     1 11.19        <NA>
    ## 2540  Ice Cream Mochi Sweet Mango 1.5     2 11.19        <NA>
    ## 2541               Golden Dragon Roll     3 11.19 grab 'n' go
    ## 2542              Tempura Crunch Roll     2 11.19 grab 'n' go
    ## 2543              Tempura Shrimp Roll     2 11.19 grab 'n' go
    ## 2544            Hawaiian Volcano Roll     1 11.19 grab 'n' go
    ## 2545                     Alaskan Roll     1 11.19 grab 'n' go
    ## 2546                         TSA Roll     1 11.19 grab 'n' go
    ## 2547                  Hawaiian Sunset     1 11.19 grab 'n' go
    ## 2548                      Salmon Roll     1 11.19 grab 'n' go
    ## 2549                  Spicy Tuna Roll     1 11.19 grab 'n' go
    ## 2550                     Avocado Roll     1 11.19 grab 'n' go
    ## 2551                  California Roll     1 11.19 grab 'n' go
    ## 2552             Wrap Buffalo Chicken     7 11.19 grab 'n' go
    ## 2553 Sandwich Black Forrest Ham & Swi     3 11.19 grab 'n' go
    ## 2554 Sandwich Crispy Chicken Milanese     2 11.19 grab 'n' go
    ## 2555              Wrap Chicken Caesar     2 11.19 grab 'n' go
    ## 2556             Salad Chicken Caesar     1 11.19 grab 'n' go
    ## 2557                     Muffin Jumbo    37 11.19        <NA>
    ## 2558                           Cookie    21 11.19        <NA>
    ## 2559                   Croissant Choc     4 11.19        <NA>
    ## 2560               Croissant Straw CC     4 11.19        <NA>
    ## 2561                     Cinnamon Bun     3 11.19        <NA>
    ## 2562                Croissant Blue CC     2 11.19        <NA>
    ## 2563                  Plain Croissant     2 11.19        <NA>
    ## 2564                      Coffee Cake     2 11.19        <NA>
    ## 2565      Combo 16 oz Coffee & Muffin     1 11.19        <NA>
    ## 2566                   BowlMedProtein     3 11.19 grab 'n' go
    ## 2567                 BowlMexicanChick     1 11.19 grab 'n' go
    ## 2568                 BowlSesAsianNood     1 11.19 grab 'n' go
    ## 2569                   BowlSouthChick     1 11.19 grab 'n' go
    ## 2570                 GF ChicCaesarSld     2 11.19 grab 'n' go
    ## 2571                   GF Turkey Sand     2 11.19 grab 'n' go
    ## 2572                 GFSunButterJelly     2 11.19 grab 'n' go
    ## 2573                     PBJ on Wheat     5 11.19 grab 'n' go
    ## 2574       Quesadilla Deluxe Trillium   155 11.18     mexican
    ## 2575                Grilled Hamburger    82 11.18       grill
    ## 2576            Fried Chicken Tenders    84 11.18       grill
    ## 2577    Burrito Una Mano Trillium BYO    54 11.18     mexican
    ## 2578                     French Fries   135 11.18       grill
    ## 2579  Grilled Chicken Breast Sandwich    15 11.18       grill
    ## 2580 Trillium Grill Impossible Burger    12 11.18       grill
    ## 2581             Seared Salmon Burger    14 11.18       grill
    ## 2582                Black Bean Burger     7 11.18       grill
    ## 2583             ADD Beef Patty $2.99    16 11.18       grill
    ## 2584                Quesadilla Cheese     5 11.18     mexican
    ## 2585               ADD Chicken Breast     6 11.18       grill
    ## 2586              Add Sausage 2 Patty     4 11.18       grill
    ## 2587                       ADD Cheese    11 11.18       grill
    ## 2588                     Add Egg $.99     2 11.18       grill
    ## 2589                1 Entree + 1 Side   168 11.18         wok
    ## 2590                1 Entree + 2 Side    76 11.18         wok
    ## 2591               Bowl Ramen Chicken    66 11.18       ramen
    ## 2592              2 Entrees + 2 Sides    22 11.18         wok
    ## 2593                  Bowl Ramen Tofu    11 11.18       ramen
    ## 2594                     1 Wok Entree     5 11.18         wok
    ## 2595          Side Vegetarian Lo Mein     6 11.18         wok
    ## 2596         Side White or Brown Rice     4 11.18         wok
    ## 2597      Side Vegetable Spring Rolls     2 11.18         wok
    ## 2598  Side Vegetarian Fried Rice with     2 11.18         wok
    ## 2599      Create Your Pasta Bowl MEAT   116 11.18     italian
    ## 2600       Create Your Pasta Bowl VEG    30 11.18     italian
    ## 2601              Pizza with Toppings    35 11.18     italian
    ## 2602                     Pizza Cheese    21 11.18     italian
    ## 2603                   Add Extra Meat    18 11.18     italian
    ## 2604                Burrito Breakfast    99 11.18        <NA>
    ## 2605              Small French Omelet    62 11.18        <NA>
    ## 2606 Egg Cheese Sausage Breakfast San    25 11.18        <NA>
    ## 2607 Egg Cheese Bacon Breakfast Sandw    19 11.18        <NA>
    ## 2608             Grand Slam Breakfast    10 11.18        <NA>
    ## 2609                        Add Bacon    28 11.18        <NA>
    ## 2610                         Two Eggs    14 11.18        <NA>
    ## 2611              Trillium Home Fries     3 11.18        <NA>
    ## 2612                   Pancake Single     3 11.18        <NA>
    ## 2613                   2 Slices Toast     1 11.18        <NA>
    ## 2614                            Toast     1 11.18        <NA>
    ## 2615                 PC Peanut Butter     1 11.18        <NA>
    ## 2616                 Burrito Bowl BYO   109 11.18     mexican
    ## 2617                      Single Taco     6 11.18     mexican
    ## 2618                   Side Guacamole     1 11.18     mexican
    ## 2619                  Side Sour Cream     1 11.18        <NA>
    ## 2620               Salad by the Pound    55 11.18   salad bar
    ## 2621                       Soup 12 oz    49 11.18   salad bar
    ## 2622                        8 oz Soup    39 11.18   salad bar
    ## 2623                  Coffee 12 oz SB    33 11.18        <NA>
    ## 2624              Soda Fountain 16 oz    36 11.18        <NA>
    ## 2625              Soda Fountain 24 oz    30 11.18        <NA>
    ## 2626                  Coffee 16 oz SB    18 11.18        <NA>
    ## 2627                    Hot Tea 20 oz     5 11.18        <NA>
    ## 2628               Open Miscellaneous    12 11.18        <NA>
    ## 2629                 Side Potato Tots    16 11.18        <NA>
    ## 2630 Tea Jasmine Green Unsweet Ito En    12 11.18        <NA>
    ## 2631            Soda Pepsi Diet 20 Oz    18 11.18        <NA>
    ## 2632 Yerba Mate Enlighten Mint 15.5 o     8 11.18        <NA>
    ## 2633 Milk Chocolate LF BIG RED Refuel    14 11.18        <NA>
    ## 2634 Gatorade Propel  Kiwi Straw Wate    10 11.18        <NA>
    ## 2635         Juice Naked Mighty Mango     6 11.18        <NA>
    ## 2636                Water Aquafina 1L    10 11.18        <NA>
    ## 2637 Energy Mango PassFruit Celsius 1     7 11.18        <NA>
    ## 2638   Energy StrawGuava Celsius 12oz     7 11.18        <NA>
    ## 2639         Juice Naked Blue Machine     5 11.18        <NA>
    ## 2640            Soda Pepsi  Zero 20oz     9 11.18        <NA>
    ## 2641   Energy Artic Vibe Celsius 12oz     6 11.18        <NA>
    ## 2642 Energy Blue Raz Lemonade Celsius     6 11.18        <NA>
    ## 2643   Energy StrawLemon Celsius 12oz     6 11.18        <NA>
    ## 2644        Gatorade Lemon Lime 28 oz     7 11.18        <NA>
    ## 2645 Yerba Mate Peach Revival 15.5 oz     5 11.18        <NA>
    ## 2646   Yerba Mate Revel Berry 15.5 oz     5 11.18        <NA>
    ## 2647 Energy Cherry Limeade Celsius 16     5 11.18        <NA>
    ## 2648    Naked Strawberry Banana Juice     4 11.18        <NA>
    ## 2649             Water Aquafina 20 oz     8 11.18        <NA>
    ## 2650 Energy Fuji Apple Pear Celsius 1     5 11.18        <NA>
    ## 2651 Tea Green Peach Mango Celsius 12     5 11.18        <NA>
    ## 2652 Tea Golden Oolong Unsweet Ito En     4 11.18        <NA>
    ## 2653    Water Aquafina Alumitek 16 oz     8 11.18        <NA>
    ## 2654 Muscle Milk P40 Strawberry Cream     3 11.18        <NA>
    ## 2655 Tea Sweet W/ Le Pure Leaf Lipton     6 11.18        <NA>
    ## 2656 Tea Tea & Lemon Pure Leaf Lipton     6 11.18        <NA>
    ## 2657 Tea Unsweet Green Lipton 18.5 oz     6 11.18        <NA>
    ## 2658  Juice Lively Lemonade Tropicana     5 11.18        <NA>
    ## 2659 Juice Orange Homestyle Tropicana     5 11.18        <NA>
    ## 2660 Juice Orange Premium Topicana 11     5 11.18        <NA>
    ## 2661        Juice Naked Green Machine     3 11.18        <NA>
    ## 2662         Juice Protein Zone Naked     3 11.18        <NA>
    ## 2663     Water Life WTR Immune 700 ML     4 11.18        <NA>
    ## 2664       Lipton Pure Leaf Sweet Tea     5 11.18        <NA>
    ## 2665    Yerba Mate Bluephoria 15.5 oz     3 11.18        <NA>
    ## 2666      Yerba Mate Tropical 15.5 oz     3 11.18        <NA>
    ## 2667 Gatorade Galcier Freeze Zero G 2     4 11.18        <NA>
    ## 2668       Juice Grape Tropicana 11oz     4 11.18        <NA>
    ## 2669 Cold Brew Vanilla Sweet Cream St     3 11.18        <NA>
    ## 2670 Energy 222 Blue Raspberry Odysse     3 11.18        <NA>
    ## 2671 Energy Fast Twitch Watermelon St     3 11.18        <NA>
    ## 2672  Tea Green Rasp Acai  Celsius 12     3 11.18        <NA>
    ## 2673 Muscle Milk PROSRS 40 Intense Va     2 11.18        <NA>
    ## 2674 Mountain Dew Kickstart Orange 16     3 11.18        <NA>
    ## 2675                 Soda Pepsi 20 Oz     4 11.18        <NA>
    ## 2676   Tea Iced Rasberry Lipton 16 oz     4 11.18        <NA>
    ## 2677  Tea Peach Pure Leaf Lipton 18.5     4 11.18        <NA>
    ## 2678   Ocean Spray Cranberry Cocktail     4 11.18        <NA>
    ## 2679   Kombucha Ginger Kevita 15.2 oz     2 11.18        <NA>
    ## 2680    Glacier Freeze Gatorade 28 oz     3 11.18        <NA>
    ## 2681 Gatorade Propel Grape Water 1 lt     3 11.18        <NA>
    ## 2682 Gatorade Gatorlyte Cherry Lime 2     2 11.18        <NA>
    ## 2683 Gatorade Gatorlyte Strawberry Ki     2 11.18        <NA>
    ## 2684 Gatorade Gatorlyte Zero Fruit Pu     2 11.18        <NA>
    ## 2685         Yerba Mate Lemon 15.5 oz     2 11.18        <NA>
    ## 2686    Tea Black Milk Ito En 11.8 oz     2 11.18        <NA>
    ## 2687 Energy Mango Tango Celsius 16 oz     2 11.18        <NA>
    ## 2688 Juice Fuji Apple Red Jacket 12oz     2 11.18        <NA>
    ## 2689 Starbucks Espress & Salt Cara DS     2 11.18        <NA>
    ## 2690     Soda Mountain Dew Zero 20 Oz     3 11.18        <NA>
    ## 2691          Soda Orange Crush 20 Oz     3 11.18        <NA>
    ## 2692  Soda Rootbeer Zero Sugar Mug 20     3 11.18        <NA>
    ## 2693    Rockstar Pure Zero Silver Ice     2 11.18        <NA>
    ## 2694   Energy Fast Twitch Grape 12 oz     2 11.18        <NA>
    ## 2695      Gatorade Berry Zero G 28 oz     2 11.18        <NA>
    ## 2696              Gatorade Blue 28 oz     2 11.18        <NA>
    ## 2697    Glacier Cherry Gatorade 28 oz     2 11.18        <NA>
    ## 2698       Juice AppleTropicana 11 oz     2 11.18        <NA>
    ## 2699  Sparkling Lemonade Mango Kevita     2 11.18        <NA>
    ## 2700             Water Gatorade 700ml     2 11.18        <NA>
    ## 2701    Muscle Milk KO Chocolate 14oz     1 11.18        <NA>
    ## 2702   Muscle Milk PP Chocolate 14 oz     1 11.18        <NA>
    ## 2703  Soda Ginger Ale Schweppes 20 Oz     2 11.18        <NA>
    ## 2704     Soda Starry Lemon Lime 20 oz     2 11.18        <NA>
    ## 2705 Tea Pure Leaf Zero Sugar Sweet T     2 11.18        <NA>
    ## 2706 Tea Unsweet Black W Lemon 18.5 o     2 11.18        <NA>
    ## 2707 Starbucks Doubleshot Ener Coffee     1 11.18        <NA>
    ## 2708         Juice Apple Dole 15.2 oz     2 11.18        <NA>
    ## 2709        Juice Orange Dole 15.2 oz     2 11.18        <NA>
    ## 2710   Frappuccino Caramel SB 13.7 oz     1 11.18        <NA>
    ## 2711          Juice Naked Berry Blast     1 11.18        <NA>
    ## 2712          Naked Red Machine Juice     1 11.18        <NA>
    ## 2713  Kombucha Pineapple Peach Kevita     1 11.18        <NA>
    ## 2714  Kombucha Rasp Lemon Kevita 15.2     1 11.18        <NA>
    ## 2715 Gatorade Gatorlyte Glacier Freez     1 11.18        <NA>
    ## 2716 Gatorade Gatorlyte MixBerry 20 o     1 11.18        <NA>
    ## 2717  Gatorade Gatorlyte Orange 20 oz     1 11.18        <NA>
    ## 2718 Tea Green Matcha Milk Ito En 11.     1 11.18        <NA>
    ## 2719 Cold Brew Chocolate Cream Starbu     1 11.18        <NA>
    ## 2720 Energy 222 Pineapple Mango Odyss     1 11.18        <NA>
    ## 2721  Energy Blue Crush Celsius 16 oz     1 11.18        <NA>
    ## 2722 Energy Orangesicle Celsius 16 oz     1 11.18        <NA>
    ## 2723 Energy Passion Orange Guva Odyss     1 11.18        <NA>
    ## 2724 Energy Revive Prickly Pear Odyss     1 11.18        <NA>
    ## 2725 Red Jacket Strawberry Apple Juic     1 11.18        <NA>
    ## 2726           Water Gatorade 33.8 oz     1 11.18        <NA>
    ## 2727       Gatorade Fruit Punch 28 oz     1 11.18        <NA>
    ## 2728             Gatorade Orange 28oz     1 11.18        <NA>
    ## 2729        Gatorade Zero Grape 28 oz     1 11.18        <NA>
    ## 2730 Sparkling lemonade Strawberry Ke     1 11.18        <NA>
    ## 2731      Soda Guava Jarritos 12.5 oz     1 11.18        <NA>
    ## 2732  Soda Pineapple Jarritos 12.5 oz     1 11.18        <NA>
    ## 2733   Soda Tamarind Jarritos 12.5 oz     1 11.18        <NA>
    ## 2734          Soda Mountain Dew 20 Oz     1 11.18        <NA>
    ## 2735          Soda Rootbeer Mug 20 oz     1 11.18        <NA>
    ## 2736 Tea Blackberry Pure Leaf 18.5 oz     1 11.18        <NA>
    ## 2737 Tea Sweetened With Lemon Brisk 2     1 11.18        <NA>
    ## 2738 Tea Unsweetened Pure Leaf Lipton     1 11.18        <NA>
    ## 2739 Juice Ocean Spray Cranbe Grape 1     1 11.18        <NA>
    ## 2740   Soda Mandarin Jarritos 12.5 oz     1 11.18        <NA>
    ## 2741 Yogurt 0% Fat Vanilla Greek Oiko     6 11.18        <NA>
    ## 2742 Yogurt Mixed Berry Chobani Drink     4 11.18        <NA>
    ## 2743   Cornell Low Fat Chocolate Milk     4 11.18        <NA>
    ## 2744                  Cornell 2% Milk     6 11.18        <NA>
    ## 2745   Milk Chocolate LF Cornell 8 Oz     6 11.18        <NA>
    ## 2746 Yogurt Flip Almond Coco Loco Cho     3 11.18        <NA>
    ## 2747  Yogurt Mango Chobani Drink 7 oz     2 11.18        <NA>
    ## 2748 Yogurt Straw Banana Chobani Drin     2 11.18        <NA>
    ## 2749 Yogurt Strawberry Banana Greek C     3 11.18        <NA>
    ## 2750            8 oz Vanilla Soy Silk     3 11.18        <NA>
    ## 2751               Cornell Whole Milk     3 11.18        <NA>
    ## 2752 Yogurt Black Cherry Greek Choban     2 11.18        <NA>
    ## 2753       Yogurt Plain Greek Chobani     2 11.18        <NA>
    ## 2754  Cornell Dairy Strawberry Yogurt     2 11.18        <NA>
    ## 2755   Yogurt Blueberry Greek Chobani     1 11.18        <NA>
    ## 2756       Yogurt Mango Greek Chobani     1 11.18        <NA>
    ## 2757   Yogurt Raspberry Greek Chobani     1 11.18        <NA>
    ## 2758  Yogurt Strawberry Greek Chobani     1 11.18        <NA>
    ## 2759 Bar Cookie & Cream Quest 2.12 oz     4 11.18        <NA>
    ## 2760 Jelly Konjac Mango Pineapple Tas     4 11.18        <NA>
    ## 2761 Bar CHOC Mint Builders Clif bar2     2 11.18        <NA>
    ## 2762 Jelly Konjac Apple Grape Tastell     2 11.18        <NA>
    ## 2763 Jelly Konjac Double Berry Tastel     2 11.18        <NA>
    ## 2764      Jelly Konjac Peach Tastelli     2 11.18        <NA>
    ## 2765   Kind Cranberry Almond Plus Bar     2 11.18        <NA>
    ## 2766 Bar Choc Chip Cookie Dough Quest     1 11.18        <NA>
    ## 2767  Bar Frosted Birthday Cake Quest     1 11.18        <NA>
    ## 2768 Cookie Peanut Butter Lenny & Lar     1 11.18        <NA>
    ## 2769  Bar Peanut Butter Builders Clif     1 11.18        <NA>
    ## 2770   Fruit And Nut Delight Kind Bar     1 11.18        <NA>
    ## 2771       Bar Chocolate Chip Clifbar     1 11.18        <NA>
    ## 2772   Bar Crunchy Peanut Butter Clif     1 11.18        <NA>
    ## 2773      Kind Almond And Coconut Bar     1 11.18        <NA>
    ## 2774        Oat And Honey Granola Bar     1 11.18        <NA>
    ## 2775   Fresh Cut Watermelon Fruit Cup     8 11.18        <NA>
    ## 2776        Fresh Cut Melon Fruit Cup     3 11.18        <NA>
    ## 2777    Fresh Cut Pineapple Fruit Cup     3 11.18        <NA>
    ## 2778                      Fruit Whole    57 11.18        <NA>
    ## 2779       Chip Baked Jax  Utz 1.5 oz     3 11.18        <NA>
    ## 2780 Chip Potato Dirty BBQ Mesquite 2     2 11.18        <NA>
    ## 2781  Chips Cracked Pepper & Sea Salt     2 11.18        <NA>
    ## 2782           Chips Dirty Sea Salted     2 11.18        <NA>
    ## 2783       Jalapeno Heat Chips Kosher     2 11.18        <NA>
    ## 2784 Nuts Almonds Honey 1.5 oz Sahale     1 11.18        <NA>
    ## 2785           Crunchsters BBQ 1.3 oz     1 11.18        <NA>
    ## 2786     Chip Kettle Sweet Onion 2 oz     1 11.18        <NA>
    ## 2787   Chip Voodoo Limited Zapps 2 oz     1 11.18        <NA>
    ## 2788       Utz GoodHealth Veggie Chip     1 11.18        <NA>
    ## 2789          Utz Salt N Vinegar Chip     1 11.18        <NA>
    ## 2790                 MochiCookCrm1.5o     4 11.18        <NA>
    ## 2791 Ice Cream Mochi Double Chocolate     3 11.18        <NA>
    ## 2792      Chewy Marshmallow GF 2.1 oz     3 11.18        <NA>
    ## 2793 GF Sweet Street Choloate Brownie     1 11.18        <NA>
    ## 2794             Orbit Sweet Mint Gum     2 11.18        <NA>
    ## 2795 Candy Milk Choc Caff Bar Awake 1     1 11.18        <NA>
    ## 2796             Orbit Wintermint Gum     1 11.18        <NA>
    ## 2797                      Salmon Roll     3 11.18 grab 'n' go
    ## 2798               Golden Dragon Roll     2 11.18 grab 'n' go
    ## 2799                  Spicy Tuna Roll     2 11.18 grab 'n' go
    ## 2800                  California Roll     2 11.18 grab 'n' go
    ## 2801            Hawaiian Volcano Roll     1 11.18 grab 'n' go
    ## 2802                     Alaskan Roll     1 11.18 grab 'n' go
    ## 2803              Tempura Shrimp Roll     1 11.18 grab 'n' go
    ## 2804                         TSA Roll     1 11.18 grab 'n' go
    ## 2805                  Hawaiian Sunset     1 11.18 grab 'n' go
    ## 2806              Wrap Chicken Caesar     4 11.18 grab 'n' go
    ## 2807             Wrap Buffalo Chicken     3 11.18 grab 'n' go
    ## 2808 Sandwich Crispy Chicken Milanese     2 11.18 grab 'n' go
    ## 2809                     Muffin Jumbo    25 11.18        <NA>
    ## 2810                           Cookie    28 11.18        <NA>
    ## 2811                   Croissant Choc     5 11.18        <NA>
    ## 2812                Croissant Blue CC     3 11.18        <NA>
    ## 2813               Croissant Straw CC     3 11.18        <NA>
    ## 2814                     Cinnamon Bun     3 11.18        <NA>
    ## 2815                  Plain Croissant     2 11.18        <NA>
    ## 2816      Combo 16 oz Coffee & Muffin     1 11.18        <NA>
    ## 2817                 BowlSesAsianNood     2 11.18 grab 'n' go
    ## 2818                   BowlMedProtein     1 11.18 grab 'n' go
    ## 2819                 BowlMexicanChick     1 11.18 grab 'n' go
    ## 2820                     PBJ on Wheat     3 11.18 grab 'n' go
    ## 2821       Quesadilla Deluxe Trillium   125 11.15     mexican
    ## 2822                Grilled Hamburger    72 11.15       grill
    ## 2823    Burrito Una Mano Trillium BYO    43 11.15     mexican
    ## 2824            Fried Chicken Tenders    47 11.15       grill
    ## 2825                     French Fries    92 11.15       grill
    ## 2826                Quesadilla Cheese    11 11.15     mexican
    ## 2827 Trillium Grill Impossible Burger     7 11.15       grill
    ## 2828  Grilled Chicken Breast Sandwich     7 11.15       grill
    ## 2829                Black Bean Burger     4 11.15       grill
    ## 2830             Seared Salmon Burger     4 11.15       grill
    ## 2831             ADD Beef Patty $2.99     8 11.15       grill
    ## 2832               ADD Chicken Breast     4 11.15       grill
    ## 2833        ADD Burger Salmon Grilled     3 11.15       grill
    ## 2834                       ADD Cheese     6 11.15       grill
    ## 2835                     Add Egg $.99     2 11.15       grill
    ## 2836                1 Entree + 1 Side    94 11.15         wok
    ## 2837                1 Entree + 2 Side    64 11.15         wok
    ## 2838               Bowl Ramen Chicken    59 11.15       ramen
    ## 2839              2 Entrees + 2 Sides    18 11.15         wok
    ## 2840                  Bowl Ramen Tofu    17 11.15       ramen
    ## 2841          Side Vegetarian Lo Mein     6 11.15         wok
    ## 2842                     1 Wok Entree     3 11.15         wok
    ## 2843  Side Vegetarian Fried Rice with     4 11.15         wok
    ## 2844         Side White or Brown Rice     6 11.15         wok
    ## 2845                  Side Vegetables     3 11.15         wok
    ## 2846      Side Vegetable Spring Rolls     2 11.15         wok
    ## 2847                Burrito Breakfast    81 11.15        <NA>
    ## 2848              Small French Omelet    39 11.15        <NA>
    ## 2849             Grand Slam Breakfast    20 11.15        <NA>
    ## 2850 Egg Cheese Sausage Breakfast San    28 11.15        <NA>
    ## 2851 Egg Cheese Bacon Breakfast Sandw    19 11.15        <NA>
    ## 2852                        Add Bacon    17 11.15        <NA>
    ## 2853                         Two Eggs     7 11.15        <NA>
    ## 2854              Trillium Home Fries     4 11.15        <NA>
    ## 2855                            Toast     1 11.15        <NA>
    ## 2856                 PC Peanut Butter     1 11.15        <NA>
    ## 2857      Create Your Pasta Bowl MEAT    79 11.15     italian
    ## 2858              Pizza with Toppings    23 11.15     italian
    ## 2859       Create Your Pasta Bowl VEG    15 11.15     italian
    ## 2860                     Pizza Cheese    12 11.15     italian
    ## 2861                   Add Extra Meat    19 11.15     italian
    ## 2862         Side Bread Pasta Station     1 11.15     italian
    ## 2863                 Burrito Bowl BYO    56 11.15     mexican
    ## 2864                      Single Taco     7 11.15     mexican
    ## 2865                  Side Sour Cream     1 11.15        <NA>
    ## 2866               Salad by the Pound    39 11.15   salad bar
    ## 2867                       Soup 12 oz    27 11.15   salad bar
    ## 2868                        8 oz Soup    28 11.15   salad bar
    ## 2869              Soda Fountain 16 oz    37 11.15        <NA>
    ## 2870              Soda Fountain 24 oz    25 11.15        <NA>
    ## 2871                  Coffee 12 oz SB    19 11.15        <NA>
    ## 2872                  Coffee 16 oz SB    16 11.15        <NA>
    ## 2873                    Hot Tea 20 oz     3 11.15        <NA>
    ## 2874                 Side Potato Tots    23 11.15        <NA>
    ## 2875               Open Miscellaneous     4 11.15        <NA>
    ## 2876            Soda Pepsi Diet 20 Oz    12 11.15        <NA>
    ## 2877         Juice Naked Mighty Mango     6 11.15        <NA>
    ## 2878         Juice Naked Blue Machine     5 11.15        <NA>
    ## 2879    Naked Strawberry Banana Juice     5 11.15        <NA>
    ## 2880 Tea Jasmine Green Unsweet Ito En     5 11.15        <NA>
    ## 2881     Soda Pepsi Wild Cherry 20 Oz     8 11.15        <NA>
    ## 2882 Red Jacket Strawberry Apple Juic     5 11.15        <NA>
    ## 2883             Water Aquafina 20 oz     8 11.15        <NA>
    ## 2884 Yerba Mate Peach Revival 15.5 oz     4 11.15        <NA>
    ## 2885 Muscle Milk PROSRS 40 Intense Va     3 11.15        <NA>
    ## 2886            Soda Pepsi  Zero 20oz     6 11.15        <NA>
    ## 2887 Tea Tea & Lemon Pure Leaf Lipton     6 11.15        <NA>
    ## 2888 Gatorade Galcier Freeze Zero G 2     5 11.15        <NA>
    ## 2889       Juice AppleTropicana 11 oz     5 11.15        <NA>
    ## 2890     Water Life WTR Immune 700 ML     4 11.15        <NA>
    ## 2891  Kombucha Rasp Lemon Kevita 15.2     3 11.15        <NA>
    ## 2892 Milk Chocolate LF BIG RED Refuel     6 11.15        <NA>
    ## 2893                Water Aquafina 1L     5 11.15        <NA>
    ## 2894    Yerba Mate Bluephoria 15.5 oz     3 11.15        <NA>
    ## 2895              Gatorade Blue 28 oz     4 11.15        <NA>
    ## 2896        Gatorade Zero Grape 28 oz     4 11.15        <NA>
    ## 2897 Juice Orange Premium Topicana 11     4 11.15        <NA>
    ## 2898 Cold Brew Chocolate Cream Starbu     3 11.15        <NA>
    ## 2899   Energy Artic Vibe Celsius 12oz     3 11.15        <NA>
    ## 2900 Muscle Milk P40 Strawberry Cream     2 11.15        <NA>
    ## 2901  Tea Peach Pure Leaf Lipton 18.5     4 11.15        <NA>
    ## 2902 Tea Sweet W/ Le Pure Leaf Lipton     4 11.15        <NA>
    ## 2903  Starbucks Doubleshot Energy Van     2 11.15        <NA>
    ## 2904     Frappuccino Mocha 13.7 Oz SB     2 11.15        <NA>
    ## 2905        Juice Naked Green Machine     2 11.15        <NA>
    ## 2906       Juice Naked PNCLDA 15.2 Oz     2 11.15        <NA>
    ## 2907 Juice Orange Homestyle Tropicana     3 11.15        <NA>
    ## 2908 Gatorade Propel Berry Water 1 lt     3 11.15        <NA>
    ## 2909             Water Gatorade 700ml     3 11.15        <NA>
    ## 2910 Gatorade Gatorlyte Glacier Freez     2 11.15        <NA>
    ## 2911  Gatorade Gatorlyte Orange 20 oz     2 11.15        <NA>
    ## 2912 Gatorade Gatorlyte Zero Fruit Pu     2 11.15        <NA>
    ## 2913 Gatorade Gatorlyte Zero Lemon Li     2 11.15        <NA>
    ## 2914 Yerba Mate Enlighten Mint 15.5 o     2 11.15        <NA>
    ## 2915         Yerba Mate Lemon 15.5 oz     2 11.15        <NA>
    ## 2916   Yerba Mate Revel Berry 15.5 oz     2 11.15        <NA>
    ## 2917 Energy 222 Pineapple Mango Odyss     2 11.15        <NA>
    ## 2918          Soda Orange Crush 20 Oz     3 11.15        <NA>
    ## 2919                 Soda Pepsi 20 Oz     3 11.15        <NA>
    ## 2920 Tea Blackberry Pure Leaf 18.5 oz     3 11.15        <NA>
    ## 2921   Tea Iced Rasberry Lipton 16 oz     3 11.15        <NA>
    ## 2922 Tea Unsweetened Pure Leaf Lipton     3 11.15        <NA>
    ## 2923 Energy Fuji Apple Pear Celsius 1     2 11.15        <NA>
    ## 2924 Energy Mango PassFruit Celsius 1     2 11.15        <NA>
    ## 2925   Energy StrawGuava Celsius 12oz     2 11.15        <NA>
    ## 2926 Tea Green Peach Mango Celsius 12     2 11.15        <NA>
    ## 2927      Gatorade Berry Zero G 28 oz     2 11.15        <NA>
    ## 2928    Glacier Freeze Gatorade 28 oz     2 11.15        <NA>
    ## 2929       Juice Grape Tropicana 11oz     2 11.15        <NA>
    ## 2930 Juice Raspberry Lemonade Tropica     2 11.15        <NA>
    ## 2931  Sparkling Lemonade Mango Kevita     2 11.15        <NA>
    ## 2932 Sparkling lemonade Strawberry Ke     2 11.15        <NA>
    ## 2933         Muscle Milk Choc PB 14oz     1 11.15        <NA>
    ## 2934       Lipton Pure Leaf Sweet Tea     2 11.15        <NA>
    ## 2935  Soda Ginger Ale Schweppes 20 Oz     2 11.15        <NA>
    ## 2936          Soda Mountain Dew 20 Oz     2 11.15        <NA>
    ## 2937     Soda Mountain Dew Zero 20 Oz     2 11.15        <NA>
    ## 2938     Soda Starry Lemon Lime 20 oz     2 11.15        <NA>
    ## 2939 Tea Pure Leaf Zero Sugar Sweet T     2 11.15        <NA>
    ## 2940    Frappuccino Coffee 13.7 oz SB     1 11.15        <NA>
    ## 2941 Juice Ocean Spray Cranbe Grape 1     2 11.15        <NA>
    ## 2942   Kombucha Ginger Kevita 15.2 oz     1 11.15        <NA>
    ## 2943 Gatorade Gatorlyte Strawberry Ki     1 11.15        <NA>
    ## 2944      Yerba Mate Tropical 15.5 oz     1 11.15        <NA>
    ## 2945    Tea Black Milk Ito En 11.8 oz     1 11.15        <NA>
    ## 2946 Tea Green Matcha Milk Ito En 11.     1 11.15        <NA>
    ## 2947 Cold Brew Vanilla Sweet Cream St     1 11.15        <NA>
    ## 2948 Energy 222 Blue Raspberry Odysse     1 11.15        <NA>
    ## 2949  Energy Blue Crush Celsius 16 oz     1 11.15        <NA>
    ## 2950 Energy Cherry Limeade Celsius 16     1 11.15        <NA>
    ## 2951 Energy Passion Orange Guva Odyss     1 11.15        <NA>
    ## 2952 Energy Revive Prickly Pear Odyss     1 11.15        <NA>
    ## 2953 Juice Fuji Apple Red Jacket 12oz     1 11.15        <NA>
    ## 2954    Rockstar Pure Zero Silver Ice     1 11.15        <NA>
    ## 2955 Mountain Dew Kickstart Orange 16     1 11.15        <NA>
    ## 2956           Water Gatorade 33.8 oz     1 11.15        <NA>
    ## 2957     Cider Apple Red Jacket 12 oz     1 11.15        <NA>
    ## 2958    Glacier Cherry Gatorade 28 oz     1 11.15        <NA>
    ## 2959 Kickstart Strawberry Start-up 16     1 11.15        <NA>
    ## 2960 Gatorade Propel Grape Water 1 lt     1 11.15        <NA>
    ## 2961      Soda Guava Jarritos 12.5 oz     1 11.15        <NA>
    ## 2962  Soda Pineapple Jarritos 12.5 oz     1 11.15        <NA>
    ## 2963          Soda Rootbeer Mug 20 oz     1 11.15        <NA>
    ## 2964  Soda Rootbeer Zero Sugar Mug 20     1 11.15        <NA>
    ## 2965     Tea Light Peach Lipton 20 oz     1 11.15        <NA>
    ## 2966 Tea Sweetened With Lemon Brisk 2     1 11.15        <NA>
    ## 2967 Tea Unsweet Black W Lemon 18.5 o     1 11.15        <NA>
    ## 2968         Juice Lemonade Dole 20oz     1 11.15        <NA>
    ## 2969        Juice Orange Dole 15.2 oz     1 11.15        <NA>
    ## 2970   Ocean Spray Cranberry Cocktail     1 11.15        <NA>
    ## 2971    Water Aquafina Alumitek 16 oz     1 11.15        <NA>
    ## 2972   Cornell Low Fat Chocolate Milk    11 11.15        <NA>
    ## 2973       Cornell 2% Milk (70000380)     4 11.15        <NA>
    ## 2974               Cornell Whole Milk     5 11.15        <NA>
    ## 2975 Yogurt Flip Almond Coco Loco Cho     2 11.15        <NA>
    ## 2976  Yogurt Flip Peanut Butter Dream     2 11.15        <NA>
    ## 2977       Yogurt Peach Greek Chobani     2 11.15        <NA>
    ## 2978  Yogurt Mango Chobani Drink 7 oz     1 11.15        <NA>
    ## 2979 Yogurt Mixed Berry Chobani Drink     1 11.15        <NA>
    ## 2980       Cornell 2% Milk (70000381)     2 11.15        <NA>
    ## 2981   Milk Chocolate LF Cornell 8 Oz     2 11.15        <NA>
    ## 2982 Yogurt 0% Fat Vanilla Greek Oiko     1 11.15        <NA>
    ## 2983       Yogurt Mango Greek Chobani     1 11.15        <NA>
    ## 2984  Yogurt Strawberry Greek Chobani     1 11.15        <NA>
    ## 2985          Silk Chocolate Soy Milk     1 11.15        <NA>
    ## 2986      Jelly Konjac Peach Tastelli     2 11.15        <NA>
    ## 2987 Bar Choc Chip Cookie Dough Quest     1 11.15        <NA>
    ## 2988 Bar Cookie & Cream Quest 2.12 oz     1 11.15        <NA>
    ## 2989  Bar Frosted Birthday Cake Quest     1 11.15        <NA>
    ## 2990 Cookie Birthday Cake Lenny & Lar     1 11.15        <NA>
    ## 2991  Bar Peanut Butter Builders Clif     1 11.15        <NA>
    ## 2992 Jelly Konjac Apple Grape Tastell     1 11.15        <NA>
    ## 2993 Jelly Konjac Double Berry Tastel     1 11.15        <NA>
    ## 2994       Bar Chocolate Chip Clifbar     1 11.15        <NA>
    ## 2995 Dark Choc Cherry Cashew Plus Kin     1 11.15        <NA>
    ## 2996        Oat And Honey Granola Bar     1 11.15        <NA>
    ## 2997                      Fruit Whole    36 11.15        <NA>
    ## 2998           Chips Dirty Sea Salted     2 11.15        <NA>
    ## 2999        Chip Kettle Sea Salt 2 oz     1 11.15        <NA>
    ## 3000     Chip Funky Fusion Dirty 2 oz     1 11.15        <NA>
    ## 3001  Chips Cracked Pepper & Sea Salt     1 11.15        <NA>
    ## 3002                 Utz Regular Chip     1 11.15        <NA>
    ## 3003         Pretzel Thin 2.12 oz Utz     1 11.15        <NA>
    ## 3004   Fresh Cut Watermelon Fruit Cup     3 11.15        <NA>
    ## 3005             Orbit Wintermint Gum     2 11.15        <NA>
    ## 3006 Candy Dark Chocolate Awake Bar 1     1 11.15        <NA>
    ## 3007             Orbit Sweet Mint Gum     1 11.15        <NA>
    ## 3008      Chewy Marshmallow GF 2.1 oz     2 11.15        <NA>
    ## 3009 GF Sweet Street Choloate Brownie     1 11.15        <NA>
    ## 3010                 MochiCookCrm1.5o     2 11.15        <NA>
    ## 3011  Ice Cream Mochi Sweet Mango 1.5     1 11.15        <NA>
    ## 3012               Golden Dragon Roll     2 11.15 grab 'n' go
    ## 3013                     Alaskan Roll     1 11.15 grab 'n' go
    ## 3014              Tempura Crunch Roll     1 11.15 grab 'n' go
    ## 3015              Tempura Shrimp Roll     1 11.15 grab 'n' go
    ## 3016                         TSA Roll     1 11.15 grab 'n' go
    ## 3017                  Spicy Tuna Roll     1 11.15 grab 'n' go
    ## 3018                  California Roll     1 11.15 grab 'n' go
    ## 3019              Wrap Chicken Caesar     3 11.15 grab 'n' go
    ## 3020 Sandwich Prosciutto & Mozzarella     2 11.15 grab 'n' go
    ## 3021     Sandwich Corned Beef & Swiss     1 11.15 grab 'n' go
    ## 3022                     Muffin Jumbo    14 11.15        <NA>
    ## 3023                           Cookie    15 11.15        <NA>
    ## 3024               Croissant Straw CC     3 11.15        <NA>
    ## 3025                   Croissant Choc     2 11.15        <NA>
    ## 3026      Combo 16 oz Coffee & Muffin     2 11.15        <NA>
    ## 3027                     Cinnamon Bun     2 11.15        <NA>
    ## 3028                      Coffee Cake     3 11.15        <NA>
    ## 3029                  Plain Croissant     1 11.15        <NA>
    ## 3030                 BowlMexicanChick     1 11.15 grab 'n' go
    ## 3031                   BowlSouthChick     1 11.15 grab 'n' go
    ## 3032                 GF ChicCaesarSld     1 11.15 grab 'n' go
    ## 3033                     PBJ on Wheat     1 11.15 grab 'n' go
    ## 3034       Quesadilla Deluxe Trillium   178 11.14     mexican
    ## 3035                Grilled Hamburger    99 11.14       grill
    ## 3036            Fried Chicken Tenders   108 11.14       grill
    ## 3037    Burrito Una Mano Trillium BYO    59 11.14     mexican
    ## 3038                     French Fries   175 11.14       grill
    ## 3039  Grilled Chicken Breast Sandwich    20 11.14       grill
    ## 3040 Trillium Grill Impossible Burger    15 11.14       grill
    ## 3041                Quesadilla Cheese    12 11.14     mexican
    ## 3042             Seared Salmon Burger     8 11.14       grill
    ## 3043             ADD Beef Patty $2.99    10 11.14       grill
    ## 3044                Black Bean Burger     3 11.14       grill
    ## 3045               ADD Chicken Breast     5 11.14       grill
    ## 3046                       ADD Cheese    12 11.14       grill
    ## 3047               Sweet Potato Fries     1 11.14       grill
    ## 3048                     Add Egg $.99     1 11.14       grill
    ## 3049                1 Entree + 1 Side   175 11.14         wok
    ## 3050                1 Entree + 2 Side    84 11.14         wok
    ## 3051               Bowl Ramen Chicken    78 11.14       ramen
    ## 3052              2 Entrees + 2 Sides    33 11.14         wok
    ## 3053                  Bowl Ramen Tofu    21 11.14       ramen
    ## 3054          Side Vegetarian Lo Mein    11 11.14         wok
    ## 3055                     1 Wok Entree     4 11.14         wok
    ## 3056         Side White or Brown Rice     8 11.14         wok
    ## 3057           Side Fried Spring Roll     2 11.14         wok
    ## 3058  Side Vegetarian Fried Rice with     1 11.14         wok
    ## 3059                Burrito Breakfast    82 11.14        <NA>
    ## 3060              Small French Omelet    56 11.14        <NA>
    ## 3061             Grand Slam Breakfast    18 11.14        <NA>
    ## 3062 Egg Cheese Sausage Breakfast San    26 11.14        <NA>
    ## 3063 Egg Cheese Bacon Breakfast Sandw    19 11.14        <NA>
    ## 3064                        Add Bacon    33 11.14        <NA>
    ## 3065                         Two Eggs    18 11.14        <NA>
    ## 3066                   Pancake Single     5 11.14        <NA>
    ## 3067              Trillium Home Fries     4 11.14        <NA>
    ## 3068                   2 Slices Toast     3 11.14        <NA>
    ## 3069      Create Your Pasta Bowl MEAT   103 11.14     italian
    ## 3070       Create Your Pasta Bowl VEG    31 11.14     italian
    ## 3071              Pizza with Toppings    38 11.14     italian
    ## 3072                     Pizza Cheese    22 11.14     italian
    ## 3073                   Add Extra Meat    16 11.14     italian
    ## 3074                 Burrito Bowl BYO    86 11.14     mexican
    ## 3075                      Single Taco     5 11.14     mexican
    ## 3076                   Side Guacamole     3 11.14     mexican
    ## 3077                       Side Salsa     1 11.14     mexican
    ## 3078               Salad by the Pound    63 11.14   salad bar
    ## 3079                        8 oz Soup    43 11.14   salad bar
    ## 3080                       Soup 12 oz    37 11.14   salad bar
    ## 3081              Soda Fountain 24 oz    33 11.14        <NA>
    ## 3082              Soda Fountain 16 oz    30 11.14        <NA>
    ## 3083                  Coffee 12 oz SB    24 11.14        <NA>
    ## 3084                  Coffee 16 oz SB    19 11.14        <NA>
    ## 3085                    Hot Tea 20 oz     4 11.14        <NA>
    ## 3086                        Hot Cocoa     1 11.14        <NA>
    ## 3087                 Side Potato Tots    23 11.14        <NA>
    ## 3088               Open Miscellaneous     6 11.14        <NA>
    ## 3089          Add Extra Protein $2.99     4 11.14        <NA>
    ## 3090             Water Aquafina 20 oz    19 11.14        <NA>
    ## 3091            Soda Pepsi Diet 20 Oz    15 11.14        <NA>
    ## 3092 Yerba Mate Enlighten Mint 15.5 o     8 11.14        <NA>
    ## 3093 Tea Jasmine Green Unsweet Ito En     8 11.14        <NA>
    ## 3094         Juice Naked Mighty Mango     6 11.14        <NA>
    ## 3095            Soda Pepsi  Zero 20oz    11 11.14        <NA>
    ## 3096                 Soda Pepsi 20 Oz    10 11.14        <NA>
    ## 3097   Energy Artic Vibe Celsius 12oz     7 11.14        <NA>
    ## 3098 Yerba Mate Peach Revival 15.5 oz     6 11.14        <NA>
    ## 3099     Water Life WTR Immune 700 ML     7 11.14        <NA>
    ## 3100                Water Aquafina 1L     9 11.14        <NA>
    ## 3101 Gatorade Propel Berry Water 1 lt     8 11.14        <NA>
    ## 3102 Energy Mango PassFruit Celsius 1     6 11.14        <NA>
    ## 3103   Energy StrawGuava Celsius 12oz     6 11.14        <NA>
    ## 3104 Juice Orange Premium Topicana 11     7 11.14        <NA>
    ## 3105    Yerba Mate Bluephoria 15.5 oz     5 11.14        <NA>
    ## 3106         Yerba Mate Lemon 15.5 oz     5 11.14        <NA>
    ## 3107 Cold Brew Vanilla Sweet Cream St     5 11.14        <NA>
    ## 3108         Juice Naked Blue Machine     4 11.14        <NA>
    ## 3109  Kombucha Rasp Lemon Kevita 15.2     4 11.14        <NA>
    ## 3110 Tea Green Peach Mango Celsius 12     5 11.14        <NA>
    ## 3111   Tea Iced Rasberry Lipton 16 oz     7 11.14        <NA>
    ## 3112    Tea Black Milk Ito En 11.8 oz     4 11.14        <NA>
    ## 3113 Tea Green Matcha Milk Ito En 11.     4 11.14        <NA>
    ## 3114 Muscle Milk P40 Strawberry Cream     3 11.14        <NA>
    ## 3115 Muscle Milk PROSRS 40 Intense Va     3 11.14        <NA>
    ## 3116 Milk Chocolate LF BIG RED Refuel     7 11.14        <NA>
    ## 3117 Energy Fruit Burst Celsius 16 oz     4 11.14        <NA>
    ## 3118   Energy StrawLemon Celsius 12oz     4 11.14        <NA>
    ## 3119   Frappuccino Caramel SB 13.7 oz     3 11.14        <NA>
    ## 3120    Naked Strawberry Banana Juice     3 11.14        <NA>
    ## 3121           Water Gatorade 33.8 oz     4 11.14        <NA>
    ## 3122          Soda Rootbeer Mug 20 oz     5 11.14        <NA>
    ## 3123 Tea Pure Leaf Zero Sugar Sweet T     5 11.14        <NA>
    ## 3124 Tea Golden Oolong Unsweet Ito En     3 11.14        <NA>
    ## 3125        Gatorade Lemon Lime 28 oz     4 11.14        <NA>
    ## 3126             Gatorade Orange 28oz     4 11.14        <NA>
    ## 3127       Juice Grape Tropicana 11oz     4 11.14        <NA>
    ## 3128 Juice Raspberry Lemonade Tropica     4 11.14        <NA>
    ## 3129 Cold Brew Chocolate Cream Starbu     3 11.14        <NA>
    ## 3130 Red Jacket Strawberry Apple Juic     3 11.14        <NA>
    ## 3131  Energy Fast Twitch Cool Blue 12     3 11.14        <NA>
    ## 3132  Tea Green Rasp Acai  Celsius 12     3 11.14        <NA>
    ## 3133    Muscle Milk KO Chocolate 14oz     2 11.14        <NA>
    ## 3134       Lipton Pure Leaf Sweet Tea     4 11.14        <NA>
    ## 3135          Soda Orange Crush 20 Oz     4 11.14        <NA>
    ## 3136 Tea Tea & Lemon Pure Leaf Lipton     4 11.14        <NA>
    ## 3137 Tea Unsweet Green Lipton 18.5 oz     4 11.14        <NA>
    ## 3138  Starbucks Doubleshot Energy Van     2 11.14        <NA>
    ## 3139     Frappuccino Mocha 13.7 Oz SB     2 11.14        <NA>
    ## 3140   Frappuccino Vanilla SB 13.7 Oz     2 11.14        <NA>
    ## 3141  Kombucha Pineapple Peach Kevita     2 11.14        <NA>
    ## 3142              Gatorade Blue 28 oz     3 11.14        <NA>
    ## 3143 Gatorade Galcier Freeze Zero G 2     3 11.14        <NA>
    ## 3144       Juice AppleTropicana 11 oz     3 11.14        <NA>
    ## 3145 Gatorade Propel Grape Water 1 lt     3 11.14        <NA>
    ## 3146             Water Gatorade 700ml     3 11.14        <NA>
    ## 3147      Yerba Mate Tropical 15.5 oz     2 11.14        <NA>
    ## 3148      Soda Mango Jarritos 12.5 oz     3 11.14        <NA>
    ## 3149 Energy Orangesicle Celsius 16 oz     2 11.14        <NA>
    ## 3150 Energy Passion Orange Guva Odyss     2 11.14        <NA>
    ## 3151  Soda Ginger Ale Schweppes 20 Oz     3 11.14        <NA>
    ## 3152          Soda Mountain Dew 20 Oz     3 11.14        <NA>
    ## 3153     Soda Mountain Dew Zero 20 Oz     3 11.14        <NA>
    ## 3154     Soda Starry Lemon Lime 20 oz     3 11.14        <NA>
    ## 3155  Tea Peach Pure Leaf Lipton 18.5     3 11.14        <NA>
    ## 3156 Tea Sweetened With Lemon Brisk 2     3 11.14        <NA>
    ## 3157 Tea Sweet W/ Le Pure Leaf Lipton     3 11.14        <NA>
    ## 3158    Rockstar Pure Zero Silver Ice     2 11.14        <NA>
    ## 3159   Ocean Spray Cranberry Cocktail     3 11.14        <NA>
    ## 3160 Energy Fast Twitch Watermelon St     2 11.14        <NA>
    ## 3161 Energy Fuji Apple Pear Celsius 1     2 11.14        <NA>
    ## 3162 Mountain Dew Kickstart Black Che     2 11.14        <NA>
    ## 3163    Water Aquafina Alumitek 16 oz     3 11.14        <NA>
    ## 3164    Glacier Cherry Gatorade 28 oz     2 11.14        <NA>
    ## 3165  Sparkling Lemonade Mango Kevita     2 11.14        <NA>
    ## 3166                 Cornell Lemonade     3 11.14        <NA>
    ## 3167      Soda Guava Jarritos 12.5 oz     2 11.14        <NA>
    ## 3168     Soda Pepsi Wild Cherry 20 Oz     2 11.14        <NA>
    ## 3169 Tea Blackberry Pure Leaf 18.5 oz     2 11.14        <NA>
    ## 3170 Starbucks Doubleshot Ener Coffee     1 11.14        <NA>
    ## 3171         Juice Lemonade Dole 20oz     2 11.14        <NA>
    ## 3172        Juice Orange Dole 15.2 oz     2 11.14        <NA>
    ## 3173    Frappuccino Coffee 13.7 oz SB     1 11.14        <NA>
    ## 3174          Juice Naked Berry Blast     1 11.14        <NA>
    ## 3175        Juice Naked Green Machine     1 11.14        <NA>
    ## 3176       Juice Naked PNCLDA 15.2 Oz     1 11.14        <NA>
    ## 3177          Naked Red Machine Juice     1 11.14        <NA>
    ## 3178 Juice Ocean Spray Cranbe Grape 1     2 11.14        <NA>
    ## 3179   Soda Mandarin Jarritos 12.5 oz     2 11.14        <NA>
    ## 3180   Kombucha Ginger Kevita 15.2 oz     1 11.14        <NA>
    ## 3181 Gatorade Gatorlyte Glacier Freez     1 11.14        <NA>
    ## 3182 Gatorade Gatorlyte MixBerry 20 o     1 11.14        <NA>
    ## 3183  Gatorade Gatorlyte Orange 20 oz     1 11.14        <NA>
    ## 3184 Gatorade Gatorlyte Strawberry Ki     1 11.14        <NA>
    ## 3185 Gatorade Gatorlyte Zero Fruit Pu     1 11.14        <NA>
    ## 3186 Gatorade Gatorlyte Zero Lemon Li     1 11.14        <NA>
    ## 3187   Yerba Mate Revel Berry 15.5 oz     1 11.14        <NA>
    ## 3188     Starbucks Double Shot 6.5 Oz     1 11.14        <NA>
    ## 3189 Energy 222 Pineapple Mango Odyss     1 11.14        <NA>
    ## 3190 Energy Cherry Limeade Celsius 16     1 11.14        <NA>
    ## 3191 Energy Dragonberry Celsius 16 oz     1 11.14        <NA>
    ## 3192 Juice Joes Lemonade  Red Jacket1     1 11.14        <NA>
    ## 3193 Energy Blue Raz Lemonade Celsius     1 11.14        <NA>
    ## 3194   Energy Fast Twitch Grape 12 oz     1 11.14        <NA>
    ## 3195 Mountain Dew Kickstart Orange 16     1 11.14        <NA>
    ## 3196     Cider Apple Red Jacket 12 oz     1 11.14        <NA>
    ## 3197      Gatorade Berry Zero G 28 oz     1 11.14        <NA>
    ## 3198        Gatorade Zero Grape 28 oz     1 11.14        <NA>
    ## 3199    Glacier Freeze Gatorade 28 oz     1 11.14        <NA>
    ## 3200  Juice Lively Lemonade Tropicana     1 11.14        <NA>
    ## 3201 Juice Zero Summer Splash Punch 1     1 11.14        <NA>
    ## 3202 Sparkling lemonade Strawberry Ke     1 11.14        <NA>
    ## 3203  Soda Pineapple Jarritos 12.5 oz     1 11.14        <NA>
    ## 3204   Soda Tamarind Jarritos 12.5 oz     1 11.14        <NA>
    ## 3205  Soda Rootbeer Zero Sugar Mug 20     1 11.14        <NA>
    ## 3206     Tea Light Peach Lipton 20 oz     1 11.14        <NA>
    ## 3207 Tea Unsweet Black W Lemon 18.5 o     1 11.14        <NA>
    ## 3208 Tea Unsweetened Pure Leaf Lipton     1 11.14        <NA>
    ## 3209         Juice Apple Dole 15.2 oz     1 11.14        <NA>
    ## 3210   Cornell Low Fat Chocolate Milk     7 11.14        <NA>
    ## 3211 Yogurt Straw Banana Chobani Drin     4 11.14        <NA>
    ## 3212               Cornell Whole Milk     5 11.14        <NA>
    ## 3213 Yogurt 0% Fat Vanilla Greek Oiko     3 11.14        <NA>
    ## 3214 Yogurt Mixed Berry Chobani Drink     2 11.14        <NA>
    ## 3215       Yogurt Mango Greek Chobani     3 11.14        <NA>
    ## 3216  Yogurt Flip Peanut Butter Dream     2 11.14        <NA>
    ## 3217                  Cornell 2% Milk     3 11.14        <NA>
    ## 3218   Yogurt Blueberry Greek Chobani     2 11.14        <NA>
    ## 3219   Yogurt Raspberry Greek Chobani     2 11.14        <NA>
    ## 3220  Yogurt Strawberry Greek Chobani     2 11.14        <NA>
    ## 3221  Cornell Dairy Strawberry Yogurt     2 11.14        <NA>
    ## 3222 Yogurt Flip Almond Coco Loco Cho     1 11.14        <NA>
    ## 3223       Yogurt Peach Greek Chobani     1 11.14        <NA>
    ## 3224       Yogurt Plain Greek Chobani     1 11.14        <NA>
    ## 3225   Milk Chocolate LF Cornell 8 Oz     1 11.14        <NA>
    ## 3226   Fresh Cut Watermelon Fruit Cup    11 11.14        <NA>
    ## 3227        Fresh Cut Melon Fruit Cup     5 11.14        <NA>
    ## 3228 Bar Cookie & Cream Quest 2.12 oz     3 11.14        <NA>
    ## 3229      Jelly Konjac Peach Tastelli     3 11.14        <NA>
    ## 3230 Bar CHOC Mint Builders Clif bar2     2 11.14        <NA>
    ## 3231  Bar Peanut Butter Builders Clif     2 11.14        <NA>
    ## 3232 Jelly Konjac Apple Grape Tastell     2 11.14        <NA>
    ## 3233        Bar S'Mores Quest 2.12 oz     1 11.14        <NA>
    ## 3234  Cookie Choc Chip Lenny & Larrys     1 11.14        <NA>
    ## 3235 Jelly Konjac Mango Pineapple Tas     1 11.14        <NA>
    ## 3236 Dark Choc Cherry Cashew Plus Kin     1 11.14        <NA>
    ## 3237      Kind Almond And Coconut Bar     1 11.14        <NA>
    ## 3238 Bar That's It Apple+Blueberry 1.     1 11.14        <NA>
    ## 3239 Nature Valley Peanut Butter Gran     1 11.14        <NA>
    ## 3240        Oat And Honey Granola Bar     1 11.14        <NA>
    ## 3241                      Fruit Whole    47 11.14        <NA>
    ## 3242 Chip Salt & Malt Vinegar Dirty 2     3 11.14        <NA>
    ## 3243          Utz Salt N Vinegar Chip     3 11.14        <NA>
    ## 3244 Chip Potato Dirty BBQ Mesquite 2     2 11.14        <NA>
    ## 3245  Chips Cracked Pepper & Sea Salt     2 11.14        <NA>
    ## 3246       Jalapeno Heat Chips Kosher     2 11.14        <NA>
    ## 3247     Chip Funky Fusion Dirty 2 oz     1 11.14        <NA>
    ## 3248       Chip Maui Onion Dirty 2 oz     1 11.14        <NA>
    ## 3249           Chips Dirty Sea Salted     1 11.14        <NA>
    ## 3250   Utz Honey Barbecue Chip 1.5 oz     1 11.14        <NA>
    ## 3251                 Utz Regular Chip     1 11.14        <NA>
    ## 3252         Pretzel Thin 2.12 oz Utz     1 11.14        <NA>
    ## 3253             Orbit Sweet Mint Gum     7 11.14        <NA>
    ## 3254             Orbit Wintermint Gum     3 11.14        <NA>
    ## 3255   Candy CHOC Cara Caff Bar Awake     1 11.14        <NA>
    ## 3256      Chewy Marshmallow GF 2.1 oz     3 11.14        <NA>
    ## 3257  Ice Cream Mochi Sweet Mango 1.5     3 11.14        <NA>
    ## 3258                     Muffin Jumbo    32 11.14        <NA>
    ## 3259                           Cookie    34 11.14        <NA>
    ## 3260                   Croissant Choc     5 11.14        <NA>
    ## 3261                Croissant Blue CC     4 11.14        <NA>
    ## 3262                  Plain Croissant     4 11.14        <NA>
    ## 3263               Croissant Straw CC     3 11.14        <NA>
    ## 3264                     Cinnamon Bun     3 11.14        <NA>
    ## 3265                      Coffee Cake     2 11.14        <NA>
    ## 3266                   BowlMedProtein     3 11.14 grab 'n' go
    ## 3267                 BowlMexicanChick     2 11.14 grab 'n' go
    ## 3268                   BowlSouthChick     2 11.14 grab 'n' go
    ## 3269                     PBJ on Wheat     8 11.14 grab 'n' go
    ## 3270                 GF ChicCaesarSld     1 11.14 grab 'n' go
    ## 3271                 GFSunButterJelly     2 11.14 grab 'n' go
    ## 3272              Tempura Crunch Roll     2 11.14 grab 'n' go
    ## 3273                         TSA Roll     2 11.14 grab 'n' go
    ## 3274            Hawaiian Volcano Roll     1 11.14 grab 'n' go
    ## 3275                     Alaskan Roll     1 11.14 grab 'n' go
    ## 3276               Golden Dragon Roll     1 11.14 grab 'n' go
    ## 3277                  Hawaiian Sunset     1 11.14 grab 'n' go
    ## 3278                  Spicy Tuna Roll     1 11.14 grab 'n' go
    ## 3279                     Avocado Roll     1 11.14 grab 'n' go
    ## 3280              Wrap Chicken Caesar     7 11.14 grab 'n' go
    ## 3281 Sandwich Black Forrest Ham & Swi     1 11.14 grab 'n' go
    ## 3282 Sandwich Prosciutto & Mozzarella     1 11.14 grab 'n' go
    ## 3283             Wrap Buffalo Chicken     1 11.14 grab 'n' go
    ## 3284             Salad Chicken Caesar     1 11.14 grab 'n' go
    ## 3285       Quesadilla Deluxe Trillium   155 11.13     mexican
    ## 3286                Grilled Hamburger   107 11.13       grill
    ## 3287            Fried Chicken Tenders   101 11.13       grill
    ## 3288    Burrito Una Mano Trillium BYO    66 11.13     mexican
    ## 3289                     French Fries   169 11.13       grill
    ## 3290  Grilled Chicken Breast Sandwich    15 11.13       grill
    ## 3291                Quesadilla Cheese    13 11.13     mexican
    ## 3292             Seared Salmon Burger     9 11.13       grill
    ## 3293 Trillium Grill Impossible Burger     6 11.13       grill
    ## 3294             ADD Beef Patty $2.99    13 11.13       grill
    ## 3295               ADD Chicken Breast     7 11.13       grill
    ## 3296                Black Bean Burger     2 11.13       grill
    ## 3297                       ADD Cheese     4 11.13       grill
    ## 3298              Add Sausage 2 Patty     1 11.13       grill
    ## 3299                     Add Egg $.99     2 11.13       grill
    ## 3300                1 Entree + 1 Side   201 11.13         wok
    ## 3301                1 Entree + 2 Side    89 11.13         wok
    ## 3302               Bowl Ramen Chicken    74 11.13       ramen
    ## 3303              2 Entrees + 2 Sides    18 11.13         wok
    ## 3304                  Bowl Ramen Tofu    17 11.13       ramen
    ## 3305                     1 Wok Entree     5 11.13         wok
    ## 3306          Side Vegetarian Lo Mein     7 11.13         wok
    ## 3307                  Side Vegetables     2 11.13         wok
    ## 3308      Side Vegetable Spring Rolls     2 11.13         wok
    ## 3309         Side White or Brown Rice     3 11.13         wok
    ## 3310           Side Fried Spring Roll     1 11.13         wok
    ## 3311      Create Your Pasta Bowl MEAT   113 11.13     italian
    ## 3312       Create Your Pasta Bowl VEG    26 11.13     italian
    ## 3313              Pizza with Toppings    38 11.13     italian
    ## 3314                     Pizza Cheese    19 11.13     italian
    ## 3315                   Add Extra Meat    22 11.13     italian
    ## 3316         Side Bread Pasta Station     2 11.13     italian
    ## 3317                Burrito Breakfast    79 11.13        <NA>
    ## 3318              Small French Omelet    54 11.13        <NA>
    ## 3319             Grand Slam Breakfast    23 11.13        <NA>
    ## 3320 Egg Cheese Sausage Breakfast San    31 11.13        <NA>
    ## 3321 Egg Cheese Bacon Breakfast Sandw    23 11.13        <NA>
    ## 3322                        Add Bacon    26 11.13        <NA>
    ## 3323                         Two Eggs     9 11.13        <NA>
    ## 3324              Trillium Home Fries     1 11.13        <NA>
    ## 3325                   2 Slices Toast     2 11.13        <NA>
    ## 3326                            Toast     2 11.13        <NA>
    ## 3327                 Burrito Bowl BYO   106 11.13     mexican
    ## 3328                      Single Taco    13 11.13     mexican
    ## 3329               Salad by the Pound    69 11.13   salad bar
    ## 3330                       Soup 12 oz    45 11.13   salad bar
    ## 3331                        8 oz Soup    43 11.13   salad bar
    ## 3332              Soda Fountain 24 oz    32 11.13        <NA>
    ## 3333                  Coffee 16 oz SB    26 11.13        <NA>
    ## 3334              Soda Fountain 16 oz    32 11.13        <NA>
    ## 3335                  Coffee 12 oz SB    17 11.13        <NA>
    ## 3336                    Hot Tea 20 oz     7 11.13        <NA>
    ## 3337               Open Miscellaneous     9 11.13        <NA>
    ## 3338                 Side Potato Tots    17 11.13        <NA>
    ## 3339          Add Extra Protein $2.99     1 11.13        <NA>
    ## 3340         Juice Naked Mighty Mango     9 11.13        <NA>
    ## 3341   Yerba Mate Revel Berry 15.5 oz    10 11.13        <NA>
    ## 3342            Soda Pepsi Diet 20 Oz    16 11.13        <NA>
    ## 3343             Water Aquafina 20 oz    17 11.13        <NA>
    ## 3344                 Soda Pepsi 20 Oz    13 11.13        <NA>
    ## 3345 Yerba Mate Peach Revival 15.5 oz     7 11.13        <NA>
    ## 3346 Energy Mango PassFruit Celsius 1     7 11.13        <NA>
    ## 3347 Tea Golden Oolong Unsweet Ito En     6 11.13        <NA>
    ## 3348     Water Life WTR Immune 700 ML     7 11.13        <NA>
    ## 3349        Gatorade Lemon Lime 28 oz     8 11.13        <NA>
    ## 3350 Juice Orange Premium Topicana 11     8 11.13        <NA>
    ## 3351   Frappuccino Caramel SB 13.7 oz     5 11.13        <NA>
    ## 3352  Energy Blue Crush Celsius 16 oz     6 11.13        <NA>
    ## 3353            Soda Pepsi  Zero 20oz     9 11.13        <NA>
    ## 3354          Soda Rootbeer Mug 20 oz     9 11.13        <NA>
    ## 3355   Energy StrawGuava Celsius 12oz     6 11.13        <NA>
    ## 3356    Muscle Milk KO Chocolate 14oz     4 11.13        <NA>
    ## 3357 Muscle Milk PROSRS 40 Intense Va     4 11.13        <NA>
    ## 3358    Tea Black Milk Ito En 11.8 oz     5 11.13        <NA>
    ## 3359 Tea Jasmine Green Unsweet Ito En     5 11.13        <NA>
    ## 3360         Juice Protein Zone Naked     4 11.13        <NA>
    ## 3361                Water Aquafina 1L     7 11.13        <NA>
    ## 3362  Soda Rootbeer Zero Sugar Mug 20     7 11.13        <NA>
    ## 3363   Tea Iced Rasberry Lipton 16 oz     7 11.13        <NA>
    ## 3364 Gatorade Galcier Freeze Zero G 2     5 11.13        <NA>
    ## 3365       Juice AppleTropicana 11 oz     5 11.13        <NA>
    ## 3366 Juice Raspberry Lemonade Tropica     5 11.13        <NA>
    ## 3367  Starbucks Doubleshot Energy Van     3 11.13        <NA>
    ## 3368 Tea Green Peach Mango Celsius 12     4 11.13        <NA>
    ## 3369    Naked Strawberry Banana Juice     3 11.13        <NA>
    ## 3370   Kombucha Ginger Kevita 15.2 oz     3 11.13        <NA>
    ## 3371  Kombucha Pineapple Peach Kevita     3 11.13        <NA>
    ## 3372  Kombucha Rasp Lemon Kevita 15.2     3 11.13        <NA>
    ## 3373       Lipton Pure Leaf Sweet Tea     5 11.13        <NA>
    ## 3374 Tea Sweet W/ Le Pure Leaf Lipton     5 11.13        <NA>
    ## 3375 Tea Unsweet Black W Lemon 18.5 o     5 11.13        <NA>
    ## 3376 Tea Unsweet Green Lipton 18.5 oz     5 11.13        <NA>
    ## 3377 Gatorade Gatorlyte Glacier Freez     3 11.13        <NA>
    ## 3378 Tea Green Matcha Milk Ito En 11.     3 11.13        <NA>
    ## 3379 Energy Cherry Limeade Celsius 16     3 11.13        <NA>
    ## 3380   Energy Artic Vibe Celsius 12oz     3 11.13        <NA>
    ## 3381  Energy Fast Twitch Cool Blue 12     3 11.13        <NA>
    ## 3382 Energy Fuji Apple Pear Celsius 1     3 11.13        <NA>
    ## 3383 Muscle Milk P40 Strawberry Cream     2 11.13        <NA>
    ## 3384           Water Gatorade 33.8 oz     3 11.13        <NA>
    ## 3385  Tea Peach Pure Leaf Lipton 18.5     4 11.13        <NA>
    ## 3386 Tea Sweetened With Lemon Brisk 2     4 11.13        <NA>
    ## 3387 Tea Unsweetened Pure Leaf Lipton     4 11.13        <NA>
    ## 3388   Frappuccino Vanilla SB 13.7 Oz     2 11.13        <NA>
    ## 3389       Juice Naked PNCLDA 15.2 Oz     2 11.13        <NA>
    ## 3390 Gatorade Propel Berry Water 1 lt     3 11.13        <NA>
    ## 3391 Gatorade Propel Grape Water 1 lt     3 11.13        <NA>
    ## 3392 Gatorade Propel  Kiwi Straw Wate     3 11.13        <NA>
    ## 3393 Gatorade Gatorlyte MixBerry 20 o     2 11.13        <NA>
    ## 3394    Yerba Mate Bluephoria 15.5 oz     2 11.13        <NA>
    ## 3395 Yerba Mate Enlighten Mint 15.5 o     2 11.13        <NA>
    ## 3396      Yerba Mate Tropical 15.5 oz     2 11.13        <NA>
    ## 3397     Starbucks Double Shot 6.5 Oz     2 11.13        <NA>
    ## 3398   Soda Tamarind Jarritos 12.5 oz     3 11.13        <NA>
    ## 3399 Cold Brew Chocolate Cream Starbu     2 11.13        <NA>
    ## 3400 Energy 222 Blue Raspberry Odysse     2 11.13        <NA>
    ## 3401 Energy 222 Pineapple Mango Odyss     2 11.13        <NA>
    ## 3402 Energy Dragonberry Celsius 16 oz     2 11.13        <NA>
    ## 3403 Juice Fuji Apple Red Jacket 12oz     2 11.13        <NA>
    ## 3404          Soda Mountain Dew 20 Oz     3 11.13        <NA>
    ## 3405     Soda Starry Lemon Lime 20 oz     3 11.13        <NA>
    ## 3406 Tea Blackberry Pure Leaf 18.5 oz     3 11.13        <NA>
    ## 3407    Rockstar Pure Zero Silver Ice     2 11.13        <NA>
    ## 3408         Juice Lemonade Dole 20oz     3 11.13        <NA>
    ## 3409 Energy Fast Twitch Watermelon St     2 11.13        <NA>
    ## 3410   Energy StrawLemon Celsius 12oz     2 11.13        <NA>
    ## 3411  Tea Green Rasp Acai  Celsius 12     2 11.13        <NA>
    ## 3412 Juice Ocean Spray Cranbe Grape 1     3 11.13        <NA>
    ## 3413   Soda Mandarin Jarritos 12.5 oz     3 11.13        <NA>
    ## 3414 Milk Chocolate LF BIG RED Refuel     3 11.13        <NA>
    ## 3415      Gatorade Berry Zero G 28 oz     2 11.13        <NA>
    ## 3416              Gatorade Blue 28 oz     2 11.13        <NA>
    ## 3417        Gatorade Zero Grape 28 oz     2 11.13        <NA>
    ## 3418  Juice Lively Lemonade Tropicana     2 11.13        <NA>
    ## 3419  Sparkling Lemonade Mango Kevita     2 11.13        <NA>
    ## 3420         Muscle Milk Choc PB 14oz     1 11.13        <NA>
    ## 3421          Soda Orange Crush 20 Oz     2 11.13        <NA>
    ## 3422     Soda Pepsi Wild Cherry 20 Oz     2 11.13        <NA>
    ## 3423 Tea Pure Leaf Zero Sugar Sweet T     2 11.13        <NA>
    ## 3424 Tea Tea & Lemon Pure Leaf Lipton     2 11.13        <NA>
    ## 3425 Starbucks Doubleshot Energy Moch     1 11.13        <NA>
    ## 3426         Juice Apple Dole 15.2 oz     2 11.13        <NA>
    ## 3427   Ocean Spray Cranberry Cocktail     2 11.13        <NA>
    ## 3428    Frappuccino Coffee 13.7 oz SB     1 11.13        <NA>
    ## 3429     Frappuccino Mocha 13.7 Oz SB     1 11.13        <NA>
    ## 3430         Juice Naked Blue Machine     1 11.13        <NA>
    ## 3431        Juice Naked Green Machine     1 11.13        <NA>
    ## 3432         Yerba Mate Lemon 15.5 oz     1 11.13        <NA>
    ## 3433 Cold Brew Vanilla Sweet Cream St     1 11.13        <NA>
    ## 3434 Energy Fruit Burst Celsius 16 oz     1 11.13        <NA>
    ## 3435 Energy Mango Tango Celsius 16 oz     1 11.13        <NA>
    ## 3436 Energy Orangesicle Celsius 16 oz     1 11.13        <NA>
    ## 3437 Energy Passion Orange Guva Odyss     1 11.13        <NA>
    ## 3438 Energy Revive Prickly Pear Odyss     1 11.13        <NA>
    ## 3439 Juice Joes Lemonade  Red Jacket1     1 11.13        <NA>
    ## 3440 Red Jacket Strawberry Apple Juic     1 11.13        <NA>
    ## 3441                 Cornell Lemonade     2 11.13        <NA>
    ## 3442   Energy Fast Twitch Grape 12 oz     1 11.13        <NA>
    ## 3443  Energy Tropical Vibe Celsius 12     1 11.13        <NA>
    ## 3444 Mountain Dew Kickstart Black Che     1 11.13        <NA>
    ## 3445 Mountain Dew Kickstart Orange 16     1 11.13        <NA>
    ## 3446     Cider Apple Red Jacket 12 oz     1 11.13        <NA>
    ## 3447       Gatorade Fruit Punch 28 oz     1 11.13        <NA>
    ## 3448             Gatorade Orange 28oz     1 11.13        <NA>
    ## 3449       Juice Grape Tropicana 11oz     1 11.13        <NA>
    ## 3450 Juice Orange Homestyle Tropicana     1 11.13        <NA>
    ## 3451 Juice Zero Summer Splash Punch 1     1 11.13        <NA>
    ## 3452 Kickstart Strawberry Start-up 16     1 11.13        <NA>
    ## 3453 Sparkling lemonade Strawberry Ke     1 11.13        <NA>
    ## 3454             Water Gatorade 700ml     1 11.13        <NA>
    ## 3455      Soda Guava Jarritos 12.5 oz     1 11.13        <NA>
    ## 3456  Soda Pineapple Jarritos 12.5 oz     1 11.13        <NA>
    ## 3457  Soda Ginger Ale Schweppes 20 Oz     1 11.13        <NA>
    ## 3458     Soda Mountain Dew Zero 20 Oz     1 11.13        <NA>
    ## 3459     Tea Light Peach Lipton 20 oz     1 11.13        <NA>
    ## 3460    Water Aquafina Alumitek 16 oz     1 11.13        <NA>
    ## 3461 Yogurt 0% Fat Vanilla Greek Oiko     9 11.13        <NA>
    ## 3462 Yogurt Flip Almond Coco Loco Cho     7 11.13        <NA>
    ## 3463   Milk Chocolate LF Cornell 8 Oz    10 11.13        <NA>
    ## 3464   Cornell Low Fat Chocolate Milk     4 11.13        <NA>
    ## 3465               Cornell Whole Milk     5 11.13        <NA>
    ## 3466  Yogurt Flip Peanut Butter Dream     3 11.13        <NA>
    ## 3467  Yogurt Mango Chobani Drink 7 oz     2 11.13        <NA>
    ## 3468 Yogurt Mixed Berry Chobani Drink     2 11.13        <NA>
    ## 3469 Yogurt Straw Banana Chobani Drin     2 11.13        <NA>
    ## 3470       Yogurt Peach Greek Chobani     3 11.13        <NA>
    ## 3471       Yogurt Plain Greek Chobani     3 11.13        <NA>
    ## 3472  Yogurt Strawberry Greek Chobani     3 11.13        <NA>
    ## 3473       Cornell 2% Milk (70000380)     2 11.13        <NA>
    ## 3474   Yogurt Raspberry Greek Chobani     2 11.13        <NA>
    ## 3475            8 oz Vanilla Soy Silk     2 11.13        <NA>
    ## 3476       Cornell 2% Milk (70000381)     2 11.13        <NA>
    ## 3477 Yogurt Black Cherry Greek Choban     1 11.13        <NA>
    ## 3478          Silk Chocolate Soy Milk     1 11.13        <NA>
    ## 3479  Cornell Dairy Strawberry Yogurt     1 11.13        <NA>
    ## 3480 Jelly Konjac Apple Grape Tastell     4 11.13        <NA>
    ## 3481      Jelly Konjac Peach Tastelli     4 11.13        <NA>
    ## 3482 Bar Cookie & Cream Quest 2.12 oz     2 11.13        <NA>
    ## 3483 Jelly Konjac Mango Pineapple Tas     2 11.13        <NA>
    ## 3484       Bar Chocolate Chip Clifbar     2 11.13        <NA>
    ## 3485  Bar Frosted Birthday Cake Quest     1 11.13        <NA>
    ## 3486 Cookie Birthday Cake Lenny & Lar     1 11.13        <NA>
    ## 3487   Bar Crunchy Peanut Butter Clif     1 11.13        <NA>
    ## 3488 Dark Choc Cherry Cashew Plus Kin     1 11.13        <NA>
    ## 3489 Bar That's It Apple+Strawberry 1     1 11.13        <NA>
    ## 3490  Oats And Dark Chocolate Granola     1 11.13        <NA>
    ## 3491                      Fruit Whole    43 11.13        <NA>
    ## 3492           Chips Dirty Sea Salted     2 11.13        <NA>
    ## 3493  Chip Sour Cream And Onion Dirty     2 11.13        <NA>
    ## 3494   Chip Voodoo Limited Zapps 2 oz     2 11.13        <NA>
    ## 3495   Utz Honey Barbecue Chip 1.5 oz     2 11.13        <NA>
    ## 3496         Pretzel Thin 2.12 oz Utz     2 11.13        <NA>
    ## 3497        Chip Kettle Sea Salt 2 oz     1 11.13        <NA>
    ## 3498     Chip Funky Fusion Dirty 2 oz     1 11.13        <NA>
    ## 3499 Chip Potato Dirty BBQ Mesquite 2     1 11.13        <NA>
    ## 3500 Chip Salt & Malt Vinegar Dirty 2     1 11.13        <NA>
    ## 3501       Utz GoodHealth Veggie Chip     1 11.13        <NA>
    ## 3502                 Utz Regular Chip     1 11.13        <NA>
    ## 3503      Chewy Marshmallow GF 2.1 oz     4 11.13        <NA>
    ## 3504 GF Sweet Street Choloate Brownie     3 11.13        <NA>
    ## 3505    Fresh Cut Pineapple Fruit Cup     3 11.13        <NA>
    ## 3506 Candy Milk Choc Caff Bar Awake 1     2 11.13        <NA>
    ## 3507             Orbit Wintermint Gum     2 11.13        <NA>
    ## 3508             Orbit Sweet Mint Gum     1 11.13        <NA>
    ## 3509 Ice Cream Mochi Double Chocolate     3 11.13        <NA>
    ## 3510  Ice Cream Mochi Sweet Mango 1.5     1 11.13        <NA>
    ## 3511                     Muffin Jumbo    35 11.13        <NA>
    ## 3512                           Cookie    26 11.13        <NA>
    ## 3513                   Croissant Choc     5 11.13        <NA>
    ## 3514               Croissant Straw CC     5 11.13        <NA>
    ## 3515                     Cinnamon Bun     4 11.13        <NA>
    ## 3516                  Plain Croissant     4 11.13        <NA>
    ## 3517                Croissant Blue CC     3 11.13        <NA>
    ## 3518      Combo 16 oz Coffee & Muffin     2 11.13        <NA>
    ## 3519                      Coffee Cake     3 11.13        <NA>
    ## 3520                 BowlMexicanChick     3 11.13 grab 'n' go
    ## 3521                   BowlMedProtein     2 11.13 grab 'n' go
    ## 3522                   BowlSouthChick     2 11.13 grab 'n' go
    ## 3523                 BowlChickAlfrPen     1 11.13 grab 'n' go
    ## 3524                 GF ChicCaesarSld     2 11.13 grab 'n' go
    ## 3525                   GF Turkey Sand     2 11.13 grab 'n' go
    ## 3526                 GFSunButterJelly     1 11.13 grab 'n' go
    ## 3527                     PBJ on Wheat     6 11.13 grab 'n' go
    ## 3528               Golden Dragon Roll     3 11.13 grab 'n' go
    ## 3529                  California Roll     3 11.13 grab 'n' go
    ## 3530              Tempura Crunch Roll     2 11.13 grab 'n' go
    ## 3531              Tempura Shrimp Roll     2 11.13 grab 'n' go
    ## 3532                  Spicy Tuna Roll     2 11.13 grab 'n' go
    ## 3533                     Alaskan Roll     1 11.13 grab 'n' go
    ## 3534                  Hawaiian Sunset     1 11.13 grab 'n' go
    ## 3535                      Salmon Roll     1 11.13 grab 'n' go
    ## 3536                     Avocado Roll     1 11.13 grab 'n' go
    ## 3537             Wrap Buffalo Chicken     4 11.13 grab 'n' go
    ## 3538              Wrap Chicken Caesar     4 11.13 grab 'n' go
    ## 3539 Sandwich Black Forrest Ham & Swi     1 11.13 grab 'n' go
    ## 3540 Sandwich Crispy Chicken Milanese     1 11.13 grab 'n' go
    ## 3541 Sandwich Prosciutto & Mozzarella     1 11.13 grab 'n' go
    ## 3542       Quesadilla Deluxe Trillium   166 11.12     mexican
    ## 3543                Grilled Hamburger    89 11.12       grill
    ## 3544            Fried Chicken Tenders    96 11.12       grill
    ## 3545    Burrito Una Mano Trillium BYO    61 11.12     mexican
    ## 3546                     French Fries   162 11.12       grill
    ## 3547  Grilled Chicken Breast Sandwich    27 11.12       grill
    ## 3548                Quesadilla Cheese    11 11.12     mexican
    ## 3549 Trillium Grill Impossible Burger     8 11.12       grill
    ## 3550             Seared Salmon Burger     7 11.12       grill
    ## 3551             ADD Beef Patty $2.99    17 11.12       grill
    ## 3552                Black Bean Burger     3 11.12       grill
    ## 3553               ADD Chicken Breast     5 11.12       grill
    ## 3554                       ADD Cheese    12 11.12       grill
    ## 3555              Add Sausage 2 Patty     3 11.12       grill
    ## 3556                     Add Egg $.99     4 11.12       grill
    ## 3557                1 Entree + 1 Side   186 11.12         wok
    ## 3558                1 Entree + 2 Side    85 11.12         wok
    ## 3559               Bowl Ramen Chicken    75 11.12       ramen
    ## 3560              2 Entrees + 2 Sides    32 11.12         wok
    ## 3561                  Bowl Ramen Tofu    19 11.12       ramen
    ## 3562                     1 Wok Entree    10 11.12         wok
    ## 3563          Side Vegetarian Lo Mein    15 11.12         wok
    ## 3564         Side White or Brown Rice    14 11.12         wok
    ## 3565                  Bowl Ramen Pork     1 11.12        <NA>
    ## 3566                  Side Vegetables     2 11.12         wok
    ## 3567      Side Vegetable Spring Rolls     2 11.12         wok
    ## 3568                Burrito Breakfast    84 11.12        <NA>
    ## 3569              Small French Omelet    55 11.12        <NA>
    ## 3570             Grand Slam Breakfast    23 11.12        <NA>
    ## 3571 Egg Cheese Sausage Breakfast San    36 11.12        <NA>
    ## 3572 Egg Cheese Bacon Breakfast Sandw    27 11.12        <NA>
    ## 3573                        Add Bacon    26 11.12        <NA>
    ## 3574                         Two Eggs    26 11.12        <NA>
    ## 3575              Trillium Home Fries     7 11.12        <NA>
    ## 3576                   Pancake Single     4 11.12        <NA>
    ## 3577                            Toast     3 11.12        <NA>
    ## 3578      Create Your Pasta Bowl MEAT   112 11.12     italian
    ## 3579              Pizza with Toppings    33 11.12     italian
    ## 3580       Create Your Pasta Bowl VEG    20 11.12     italian
    ## 3581                     Pizza Cheese    19 11.12     italian
    ## 3582                   Add Extra Meat    14 11.12     italian
    ## 3583                 Burrito Bowl BYO    93 11.12     mexican
    ## 3584                      Single Taco     6 11.12     mexican
    ## 3585                   Side Guacamole     1 11.12     mexican
    ## 3586      Add Extra Toppings Una Mano     1 11.12     mexican
    ## 3587               Salad by the Pound    56 11.12   salad bar
    ## 3588                        8 oz Soup    59 11.12   salad bar
    ## 3589                       Soup 12 oz    40 11.12   salad bar
    ## 3590                  Coffee 16 oz SB    29 11.12        <NA>
    ## 3591                  Coffee 12 oz SB    30 11.12        <NA>
    ## 3592              Soda Fountain 24 oz    28 11.12        <NA>
    ## 3593              Soda Fountain 16 oz    25 11.12        <NA>
    ## 3594                    Hot Tea 20 oz     7 11.12        <NA>
    ## 3595                 Side Potato Tots    23 11.12        <NA>
    ## 3596               Open Miscellaneous    11 11.12        <NA>
    ## 3597          Add Extra Protein $2.99     3 11.12        <NA>
    ## 3598 Tea Jasmine Green Unsweet Ito En    12 11.12        <NA>
    ## 3599 Yerba Mate Peach Revival 15.5 oz    11 11.12        <NA>
    ## 3600   Yerba Mate Revel Berry 15.5 oz    10 11.12        <NA>
    ## 3601    Yerba Mate Bluephoria 15.5 oz     9 11.12        <NA>
    ## 3602             Water Aquafina 20 oz    15 11.12        <NA>
    ## 3603 Milk Chocolate LF BIG RED Refuel    15 11.12        <NA>
    ## 3604 Yerba Mate Enlighten Mint 15.5 o     7 11.12        <NA>
    ## 3605 Energy Mango PassFruit Celsius 1     8 11.12        <NA>
    ## 3606            Soda Pepsi Diet 20 Oz    11 11.12        <NA>
    ## 3607   Energy StrawGuava Celsius 12oz     7 11.12        <NA>
    ## 3608   Frappuccino Caramel SB 13.7 oz     5 11.12        <NA>
    ## 3609 Cold Brew Vanilla Sweet Cream St     6 11.12        <NA>
    ## 3610   Energy Artic Vibe Celsius 12oz     6 11.12        <NA>
    ## 3611     Water Life WTR Immune 700 ML     6 11.12        <NA>
    ## 3612 Juice Fuji Apple Red Jacket 12oz     5 11.12        <NA>
    ## 3613   Frappuccino Vanilla SB 13.7 Oz     4 11.12        <NA>
    ## 3614        Juice Naked Green Machine     4 11.12        <NA>
    ## 3615   Energy StrawLemon Celsius 12oz     5 11.12        <NA>
    ## 3616 Tea Green Peach Mango Celsius 12     5 11.12        <NA>
    ## 3617 Gatorade Propel Grape Water 1 lt     6 11.12        <NA>
    ## 3618  Energy Triple Shot Dark Caramel     4 11.12        <NA>
    ## 3619    Tea Black Milk Ito En 11.8 oz     4 11.12        <NA>
    ## 3620 Tea Golden Oolong Unsweet Ito En     4 11.12        <NA>
    ## 3621 Muscle Milk P40 Strawberry Cream     3 11.12        <NA>
    ## 3622 Muscle Milk PROSRS 40 Intense Va     3 11.12        <NA>
    ## 3623 Cold Brew Chocolate Cream Starbu     4 11.12        <NA>
    ## 3624            Soda Pepsi  Zero 20oz     6 11.12        <NA>
    ## 3625     Tea Light Peach Lipton 20 oz     6 11.12        <NA>
    ## 3626  Tea Peach Pure Leaf Lipton 18.5     6 11.12        <NA>
    ## 3627 Tea Sweet W/ Le Pure Leaf Lipton     6 11.12        <NA>
    ## 3628        Gatorade Lemon Lime 28 oz     5 11.12        <NA>
    ## 3629 Gatorade Propel Berry Water 1 lt     5 11.12        <NA>
    ## 3630 Energy Fast Twitch Watermelon St     4 11.12        <NA>
    ## 3631    Frappuccino Coffee 13.7 oz SB     3 11.12        <NA>
    ## 3632     Frappuccino Mocha 13.7 Oz SB     3 11.12        <NA>
    ## 3633         Juice Naked Mighty Mango     3 11.12        <NA>
    ## 3634       Juice Naked PNCLDA 15.2 Oz     3 11.12        <NA>
    ## 3635    Naked Strawberry Banana Juice     3 11.12        <NA>
    ## 3636                Water Aquafina 1L     5 11.12        <NA>
    ## 3637          Soda Mountain Dew 20 Oz     5 11.12        <NA>
    ## 3638     Soda Mountain Dew Zero 20 Oz     5 11.12        <NA>
    ## 3639          Soda Rootbeer Mug 20 oz     5 11.12        <NA>
    ## 3640 Tea Tea & Lemon Pure Leaf Lipton     5 11.12        <NA>
    ## 3641 Tea Unsweet Green Lipton 18.5 oz     5 11.12        <NA>
    ## 3642 Gatorade Gatorlyte Zero Fruit Pu     3 11.12        <NA>
    ## 3643      Yerba Mate Tropical 15.5 oz     3 11.12        <NA>
    ## 3644     Starbucks Double Shot 6.5 Oz     3 11.12        <NA>
    ## 3645 Juice Orange Premium Topicana 11     4 11.12        <NA>
    ## 3646 Juice Raspberry Lemonade Tropica     4 11.12        <NA>
    ## 3647 Energy Cherry Limeade Celsius 16     3 11.12        <NA>
    ## 3648 Energy Mango Tango Celsius 16 oz     3 11.12        <NA>
    ## 3649 Sparkling lemonade Strawberry Ke     4 11.12        <NA>
    ## 3650   Energy Fast Twitch Grape 12 oz     3 11.12        <NA>
    ## 3651  Tea Green Rasp Acai  Celsius 12     3 11.12        <NA>
    ## 3652    Muscle Milk KO Chocolate 14oz     2 11.12        <NA>
    ## 3653    Water Aquafina Alumitek 16 oz     5 11.12        <NA>
    ## 3654       Lipton Pure Leaf Sweet Tea     4 11.12        <NA>
    ## 3655                 Soda Pepsi 20 Oz     4 11.12        <NA>
    ## 3656   Tea Iced Rasberry Lipton 16 oz     4 11.12        <NA>
    ## 3657         Juice Apple Dole 15.2 oz     4 11.12        <NA>
    ## 3658         Juice Naked Blue Machine     2 11.12        <NA>
    ## 3659          Naked Red Machine Juice     2 11.12        <NA>
    ## 3660   Soda Mandarin Jarritos 12.5 oz     4 11.12        <NA>
    ## 3661  Kombucha Pineapple Peach Kevita     2 11.12        <NA>
    ## 3662 Gatorade Galcier Freeze Zero G 2     3 11.12        <NA>
    ## 3663    Glacier Freeze Gatorade 28 oz     3 11.12        <NA>
    ## 3664       Juice AppleTropicana 11 oz     3 11.12        <NA>
    ## 3665 Gatorade Propel  Kiwi Straw Wate     3 11.12        <NA>
    ## 3666         Yerba Mate Lemon 15.5 oz     2 11.12        <NA>
    ## 3667 Tea Green Matcha Milk Ito En 11.     2 11.12        <NA>
    ## 3668      Soda Guava Jarritos 12.5 oz     3 11.12        <NA>
    ## 3669 Energy 222 Pineapple Mango Odyss     2 11.12        <NA>
    ## 3670  Energy Blue Crush Celsius 16 oz     2 11.12        <NA>
    ## 3671 Energy Dragonberry Celsius 16 oz     2 11.12        <NA>
    ## 3672 Energy Revive Prickly Pear Odyss     2 11.12        <NA>
    ## 3673          Soda Orange Crush 20 Oz     3 11.12        <NA>
    ## 3674  Soda Rootbeer Zero Sugar Mug 20     3 11.12        <NA>
    ## 3675 Energy Fuji Apple Pear Celsius 1     2 11.12        <NA>
    ## 3676 Mountain Dew Kickstart Black Che     2 11.12        <NA>
    ## 3677              Gatorade Blue 28 oz     2 11.12        <NA>
    ## 3678  Juice Lively Lemonade Tropicana     2 11.12        <NA>
    ## 3679 Juice Orange Homestyle Tropicana     2 11.12        <NA>
    ## 3680 Juice Zero Summer Splash Punch 1     2 11.12        <NA>
    ## 3681      Soda Mango Jarritos 12.5 oz     2 11.12        <NA>
    ## 3682  Soda Pineapple Jarritos 12.5 oz     2 11.12        <NA>
    ## 3683         Muscle Milk Choc PB 14oz     1 11.12        <NA>
    ## 3684 Tea Blackberry Pure Leaf 18.5 oz     2 11.12        <NA>
    ## 3685 Tea Pure Leaf Zero Sugar Sweet T     2 11.12        <NA>
    ## 3686 Tea Sweetened With Lemon Brisk 2     2 11.12        <NA>
    ## 3687 Tea Unsweet Black W Lemon 18.5 o     2 11.12        <NA>
    ## 3688 Starbucks Doubleshot Energy Moch     1 11.12        <NA>
    ## 3689  Starbucks Doubleshot Energy Van     1 11.12        <NA>
    ## 3690   Ocean Spray Cranberry Cocktail     2 11.12        <NA>
    ## 3691         Juice Protein Zone Naked     1 11.12        <NA>
    ## 3692 Juice Ocean Spray Cranbe Grape 1     2 11.12        <NA>
    ## 3693   Kombucha Ginger Kevita 15.2 oz     1 11.12        <NA>
    ## 3694  Kombucha Rasp Lemon Kevita 15.2     1 11.12        <NA>
    ## 3695 Gatorade Gatorlyte Glacier Freez     1 11.12        <NA>
    ## 3696  Gatorade Gatorlyte Orange 20 oz     1 11.12        <NA>
    ## 3697 Gatorade Gatorlyte Strawberry Ki     1 11.12        <NA>
    ## 3698 Gatorade Gatorlyte Zero Lemon Li     1 11.12        <NA>
    ## 3699 Energy 222 Blue Raspberry Odysse     1 11.12        <NA>
    ## 3700 Energy Fruit Burst Celsius 16 oz     1 11.12        <NA>
    ## 3701 Energy Orangesicle Celsius 16 oz     1 11.12        <NA>
    ## 3702    Rockstar Pure Zero Silver Ice     1 11.12        <NA>
    ## 3703 Energy Blue Raz Lemonade Celsius     1 11.12        <NA>
    ## 3704  Energy Fast Twitch Cool Blue 12     1 11.12        <NA>
    ## 3705             Gatorade Orange 28oz     1 11.12        <NA>
    ## 3706        Gatorade Zero Grape 28 oz     1 11.12        <NA>
    ## 3707    Glacier Cherry Gatorade 28 oz     1 11.12        <NA>
    ## 3708 Kickstart Strawberry Start-up 16     1 11.12        <NA>
    ## 3709  Sparkling Lemonade Mango Kevita     1 11.12        <NA>
    ## 3710   Soda Tamarind Jarritos 12.5 oz     1 11.12        <NA>
    ## 3711  Soda Ginger Ale Schweppes 20 Oz     1 11.12        <NA>
    ## 3712     Soda Pepsi Wild Cherry 20 Oz     1 11.12        <NA>
    ## 3713     Soda Starry Lemon Lime 20 oz     1 11.12        <NA>
    ## 3714 Tea Unsweetened Pure Leaf Lipton     1 11.12        <NA>
    ## 3715         Juice Lemonade Dole 20oz     1 11.12        <NA>
    ## 3716   Cornell Low Fat Chocolate Milk     7 11.12        <NA>
    ## 3717 Yogurt Mixed Berry Chobani Drink     4 11.12        <NA>
    ## 3718 Yogurt Straw Banana Chobani Drin     4 11.12        <NA>
    ## 3719       Yogurt Peach Greek Chobani     5 11.12        <NA>
    ## 3720 Yogurt 0% Fat Vanilla Greek Oiko     4 11.12        <NA>
    ## 3721               Cornell Whole Milk     4 11.12        <NA>
    ## 3722   Milk Chocolate LF Cornell 8 Oz     4 11.12        <NA>
    ## 3723   Yogurt Blueberry Greek Chobani     3 11.12        <NA>
    ## 3724       Yogurt Mango Greek Chobani     3 11.12        <NA>
    ## 3725 Yogurt Black Cherry Greek Choban     2 11.12        <NA>
    ## 3726       Yogurt Plain Greek Chobani     2 11.12        <NA>
    ## 3727  Yogurt Strawberry Greek Chobani     2 11.12        <NA>
    ## 3728  Yogurt Mango Chobani Drink 7 oz     1 11.12        <NA>
    ## 3729       Cornell 2% Milk (70000381)     2 11.12        <NA>
    ## 3730       Cornell 2% Milk (70000380)     1 11.12        <NA>
    ## 3731  Yogurt Flip Peanut Butter Dream     1 11.12        <NA>
    ## 3732   Yogurt Raspberry Greek Chobani     1 11.12        <NA>
    ## 3733 Yogurt Strawberry Banana Greek C     1 11.12        <NA>
    ## 3734          Silk Chocolate Soy Milk     1 11.12        <NA>
    ## 3735       Cornell Dairy Mango Yogurt     1 11.12        <NA>
    ## 3736       Cornell Dairy Peach Yogurt     1 11.12        <NA>
    ## 3737  Cornell Dairy Strawberry Yogurt     1 11.12        <NA>
    ## 3738 Jelly Konjac Mango Pineapple Tas     3 11.12        <NA>
    ## 3739        Bar S'Mores Quest 2.12 oz     2 11.12        <NA>
    ## 3740 Bar That's It Apple+Strawberry 1     3 11.12        <NA>
    ## 3741 Bar CHOC Mint Builders Clif bar2     2 11.12        <NA>
    ## 3742      Jelly Konjac Peach Tastelli     2 11.12        <NA>
    ## 3743  Oats And Dark Chocolate Granola     3 11.12        <NA>
    ## 3744 Bar Cookie & Cream Quest 2.12 oz     1 11.12        <NA>
    ## 3745  Bar Frosted Birthday Cake Quest     1 11.12        <NA>
    ## 3746  Cookie Choc Chip Lenny & Larrys     1 11.12        <NA>
    ## 3747  Bar Peanut Butter Builders Clif     1 11.12        <NA>
    ## 3748 Jelly Konjac Apple Grape Tastell     1 11.12        <NA>
    ## 3749 Jelly Konjac Double Berry Tastel     1 11.12        <NA>
    ## 3750   Bar Crunchy Peanut Butter Clif     1 11.12        <NA>
    ## 3751 Nature Valley Peanut Butter Gran     1 11.12        <NA>
    ## 3752                      Fruit Whole    44 11.12        <NA>
    ## 3753           Chips Dirty Sea Salted     3 11.12        <NA>
    ## 3754  Chip Sour Cream And Onion Dirty     2 11.12        <NA>
    ## 3755       Jalapeno Heat Chips Kosher     2 11.12        <NA>
    ## 3756   Utz Honey Barbecue Chip 1.5 oz     2 11.12        <NA>
    ## 3757 Nuts Almonds Honey 1.5 oz Sahale     1 11.12        <NA>
    ## 3758 Chip Potato Dirty BBQ Mesquite 2     1 11.12        <NA>
    ## 3759  Chips Cracked Pepper & Sea Salt     1 11.12        <NA>
    ## 3760   Chip Voodoo Limited Zapps 2 oz     1 11.12        <NA>
    ## 3761       Utz GoodHealth Veggie Chip     1 11.12        <NA>
    ## 3762                 Utz Regular Chip     1 11.12        <NA>
    ## 3763         Pretzel Thin 2.12 oz Utz     1 11.12        <NA>
    ## 3764             Orbit Wintermint Gum     7 11.12        <NA>
    ## 3765             Orbit Sweet Mint Gum     4 11.12        <NA>
    ## 3766   Candy CHOC Cara Caff Bar Awake     1 11.12        <NA>
    ## 3767 Candy Dark Chocolate Awake Bar 1     1 11.12        <NA>
    ## 3768      Chewy Marshmallow GF 2.1 oz     3 11.12        <NA>
    ## 3769 GF Sweet Street Choloate Brownie     2 11.12        <NA>
    ## 3770    Fresh Cut Pineapple Fruit Cup     3 11.12        <NA>
    ## 3771                 MochiCookCrm1.5o     2 11.12        <NA>
    ## 3772             Wrap Buffalo Chicken     5 11.12 grab 'n' go
    ## 3773              Wrap Chicken Caesar     4 11.12 grab 'n' go
    ## 3774 Sandwich Black Forrest Ham & Swi     2 11.12 grab 'n' go
    ## 3775 Sandwich Crispy Chicken Milanese     2 11.12 grab 'n' go
    ## 3776     Sandwich Corned Beef & Swiss     1 11.12 grab 'n' go
    ## 3777 Sandwich Prosciutto & Mozzarella     1 11.12 grab 'n' go
    ## 3778                  Spicy Tuna Roll     3 11.12 grab 'n' go
    ## 3779               Golden Dragon Roll     2 11.12 grab 'n' go
    ## 3780            Hawaiian Volcano Roll     1 11.12 grab 'n' go
    ## 3781                     Alaskan Roll     1 11.12 grab 'n' go
    ## 3782              Tempura Shrimp Roll     1 11.12 grab 'n' go
    ## 3783                  Hawaiian Sunset     1 11.12 grab 'n' go
    ## 3784                      Salmon Roll     1 11.12 grab 'n' go
    ## 3785                  California Roll     1 11.12 grab 'n' go
    ## 3786                     Muffin Jumbo    28 11.12        <NA>
    ## 3787                           Cookie    26 11.12        <NA>
    ## 3788                   Croissant Choc     4 11.12        <NA>
    ## 3789                     Cinnamon Bun     4 11.12        <NA>
    ## 3790               Croissant Straw CC     3 11.12        <NA>
    ## 3791      Combo 16 oz Coffee & Muffin     3 11.12        <NA>
    ## 3792                  Plain Croissant     3 11.12        <NA>
    ## 3793                   BowlSouthChick     2 11.12 grab 'n' go
    ## 3794                   BowlMedProtein     1 11.12 grab 'n' go
    ## 3795                 BowlMexicanChick     1 11.12 grab 'n' go
    ## 3796                 BowlSesAsianNood     1 11.12 grab 'n' go
    ## 3797                     PBJ on Wheat     5 11.12 grab 'n' go
    ## 3798                 GFSunButterJelly     2 11.12 grab 'n' go
    ## 3799       Quesadilla Deluxe Trillium   162 11.11     mexican
    ## 3800                Grilled Hamburger    81 11.11       grill
    ## 3801            Fried Chicken Tenders    86 11.11       grill
    ## 3802    Burrito Una Mano Trillium BYO    51 11.11     mexican
    ## 3803                     French Fries   104 11.11       grill
    ## 3804  Grilled Chicken Breast Sandwich    16 11.11       grill
    ## 3805                Quesadilla Cheese    12 11.11     mexican
    ## 3806               Sweet Potato Fries    32 11.11       grill
    ## 3807 Trillium Grill Impossible Burger     8 11.11       grill
    ## 3808             Seared Salmon Burger     4 11.11       grill
    ## 3809               ADD Chicken Breast     8 11.11       grill
    ## 3810                Black Bean Burger     3 11.11       grill
    ## 3811             ADD Beef Patty $2.99     4 11.11       grill
    ## 3812              Add Sausage 2 Patty     3 11.11       grill
    ## 3813                     Add Egg $.99     2 11.11       grill
    ## 3814                       ADD Cheese     3 11.11       grill
    ## 3815                1 Entree + 1 Side   171 11.11         wok
    ## 3816                1 Entree + 2 Side    79 11.11         wok
    ## 3817               Bowl Ramen Chicken    69 11.11       ramen
    ## 3818              2 Entrees + 2 Sides    24 11.11         wok
    ## 3819                  Bowl Ramen Tofu    12 11.11       ramen
    ## 3820          Side Vegetarian Lo Mein    13 11.11         wok
    ## 3821                     1 Wok Entree     2 11.11         wok
    ## 3822         Side White or Brown Rice     2 11.11         wok
    ## 3823                  Side Vegetables     1 11.11         wok
    ## 3824      Side Vegetable Spring Rolls     1 11.11         wok
    ## 3825  Side Vegetarian Fried Rice with     1 11.11         wok
    ## 3826      Create Your Pasta Bowl MEAT   119 11.11     italian
    ## 3827       Create Your Pasta Bowl VEG    25 11.11     italian
    ## 3828              Pizza with Toppings    34 11.11     italian
    ## 3829                     Pizza Cheese    16 11.11     italian
    ## 3830                   Add Extra Meat    26 11.11     italian
    ## 3831         Side Bread Pasta Station     1 11.11     italian
    ## 3832                Burrito Breakfast    91 11.11        <NA>
    ## 3833              Small French Omelet    54 11.11        <NA>
    ## 3834             Grand Slam Breakfast    15 11.11        <NA>
    ## 3835 Egg Cheese Sausage Breakfast San    20 11.11        <NA>
    ## 3836 Egg Cheese Bacon Breakfast Sandw    19 11.11        <NA>
    ## 3837                        Add Bacon    28 11.11        <NA>
    ## 3838                         Two Eggs    18 11.11        <NA>
    ## 3839              Trillium Home Fries     7 11.11        <NA>
    ## 3840                   Pancake Single     3 11.11        <NA>
    ## 3841                   2 Slices Toast     3 11.11        <NA>
    ## 3842                 PC Peanut Butter     1 11.11        <NA>
    ## 3843                 Burrito Bowl BYO   107 11.11     mexican
    ## 3844                      Single Taco     3 11.11     mexican
    ## 3845               Salad by the Pound    60 11.11   salad bar
    ## 3846                       Soup 12 oz    49 11.11   salad bar
    ## 3847                        8 oz Soup    48 11.11   salad bar
    ## 3848                  Coffee 16 oz SB    32 11.11        <NA>
    ## 3849              Soda Fountain 16 oz    42 11.11        <NA>
    ## 3850              Soda Fountain 24 oz    36 11.11        <NA>
    ## 3851                  Coffee 12 oz SB    27 11.11        <NA>
    ## 3852                    Hot Tea 20 oz     5 11.11        <NA>
    ## 3853                 Side Potato Tots    22 11.11        <NA>
    ## 3854               Open Miscellaneous     9 11.11        <NA>
    ## 3855             Water Aquafina 20 oz    17 11.11        <NA>
    ## 3856 Milk Chocolate LF BIG RED Refuel    16 11.11        <NA>
    ## 3857                 Soda Pepsi 20 Oz    14 11.11        <NA>
    ## 3858            Soda Pepsi Diet 20 Oz    13 11.11        <NA>
    ## 3859   Frappuccino Vanilla SB 13.7 Oz     6 11.11        <NA>
    ## 3860         Juice Naked Mighty Mango     6 11.11        <NA>
    ## 3861     Water Life WTR Immune 700 ML     8 11.11        <NA>
    ## 3862 Juice Raspberry Lemonade Tropica     9 11.11        <NA>
    ## 3863 Energy Mango PassFruit Celsius 1     7 11.11        <NA>
    ## 3864   Energy StrawGuava Celsius 12oz     7 11.11        <NA>
    ## 3865 Tea Green Peach Mango Celsius 12     7 11.11        <NA>
    ## 3866    Yerba Mate Bluephoria 15.5 oz     6 11.11        <NA>
    ## 3867 Energy Fuji Apple Pear Celsius 1     6 11.11        <NA>
    ## 3868 Tea Green Matcha Milk Ito En 11.     5 11.11        <NA>
    ## 3869 Gatorade Propel  Kiwi Straw Wate     7 11.11        <NA>
    ## 3870         Juice Naked Blue Machine     4 11.11        <NA>
    ## 3871                Water Aquafina 1L     7 11.11        <NA>
    ## 3872 Tea Pure Leaf Zero Sugar Sweet T     7 11.11        <NA>
    ## 3873 Gatorade Gatorlyte Glacier Freez     4 11.11        <NA>
    ## 3874 Yerba Mate Peach Revival 15.5 oz     4 11.11        <NA>
    ## 3875 Tea Golden Oolong Unsweet Ito En     4 11.11        <NA>
    ## 3876 Tea Jasmine Green Unsweet Ito En     4 11.11        <NA>
    ## 3877    Muscle Milk KO Chocolate 14oz     3 11.11        <NA>
    ## 3878            Soda Pepsi  Zero 20oz     6 11.11        <NA>
    ## 3879   Tea Iced Rasberry Lipton 16 oz     6 11.11        <NA>
    ## 3880 Tea Sweet W/ Le Pure Leaf Lipton     6 11.11        <NA>
    ## 3881 Juice Orange Homestyle Tropicana     5 11.11        <NA>
    ## 3882  Starbucks Doubleshot Energy Van     3 11.11        <NA>
    ## 3883   Energy Artic Vibe Celsius 12oz     4 11.11        <NA>
    ## 3884    Water Aquafina Alumitek 16 oz     7 11.11        <NA>
    ## 3885   Frappuccino Caramel SB 13.7 oz     3 11.11        <NA>
    ## 3886    Naked Strawberry Banana Juice     3 11.11        <NA>
    ## 3887   Kombucha Ginger Kevita 15.2 oz     3 11.11        <NA>
    ## 3888  Kombucha Pineapple Peach Kevita     3 11.11        <NA>
    ## 3889     Soda Pepsi Wild Cherry 20 Oz     5 11.11        <NA>
    ## 3890 Tea Unsweetened Pure Leaf Lipton     5 11.11        <NA>
    ## 3891 Gatorade Gatorlyte Zero Fruit Pu     3 11.11        <NA>
    ## 3892         Yerba Mate Lemon 15.5 oz     3 11.11        <NA>
    ## 3893      Yerba Mate Tropical 15.5 oz     3 11.11        <NA>
    ## 3894    Tea Black Milk Ito En 11.8 oz     3 11.11        <NA>
    ## 3895       Juice AppleTropicana 11 oz     4 11.11        <NA>
    ## 3896 Juice Orange Premium Topicana 11     4 11.11        <NA>
    ## 3897 Energy Fruit Burst Celsius 16 oz     3 11.11        <NA>
    ## 3898 Energy Mango Tango Celsius 16 oz     3 11.11        <NA>
    ## 3899 Gatorade Propel Grape Water 1 lt     4 11.11        <NA>
    ## 3900   Soda Tamarind Jarritos 12.5 oz     4 11.11        <NA>
    ## 3901   Energy StrawLemon Celsius 12oz     3 11.11        <NA>
    ## 3902  Tea Green Rasp Acai  Celsius 12     3 11.11        <NA>
    ## 3903 Mountain Dew Kickstart Orange 16     3 11.11        <NA>
    ## 3904  Soda Ginger Ale Schweppes 20 Oz     4 11.11        <NA>
    ## 3905  Soda Rootbeer Zero Sugar Mug 20     4 11.11        <NA>
    ## 3906   Ocean Spray Cranberry Cocktail     4 11.11        <NA>
    ## 3907       Juice Naked PNCLDA 15.2 Oz     2 11.11        <NA>
    ## 3908 Gatorade Galcier Freeze Zero G 2     3 11.11        <NA>
    ## 3909        Gatorade Lemon Lime 28 oz     3 11.11        <NA>
    ## 3910 Kickstart Strawberry Start-up 16     3 11.11        <NA>
    ## 3911             Water Gatorade 700ml     3 11.11        <NA>
    ## 3912 Yerba Mate Enlighten Mint 15.5 o     2 11.11        <NA>
    ## 3913 Energy 222 Blue Raspberry Odysse     2 11.11        <NA>
    ## 3914 Energy Cherry Limeade Celsius 16     2 11.11        <NA>
    ## 3915 Energy Passion Orange Guva Odyss     2 11.11        <NA>
    ## 3916 Energy Revive Prickly Pear Odyss     2 11.11        <NA>
    ## 3917 Red Jacket Strawberry Apple Juic     2 11.11        <NA>
    ## 3918       Lipton Pure Leaf Sweet Tea     3 11.11        <NA>
    ## 3919     Soda Mountain Dew Zero 20 Oz     3 11.11        <NA>
    ## 3920     Soda Starry Lemon Lime 20 oz     3 11.11        <NA>
    ## 3921 Tea Blackberry Pure Leaf 18.5 oz     3 11.11        <NA>
    ## 3922 Tea Unsweet Green Lipton 18.5 oz     3 11.11        <NA>
    ## 3923 Energy Blue Raz Lemonade Celsius     2 11.11        <NA>
    ## 3924 Energy Fast Twitch Watermelon St     2 11.11        <NA>
    ## 3925 Juice Ocean Spray Cranbe Grape 1     3 11.11        <NA>
    ## 3926      Gatorade Berry Zero G 28 oz     2 11.11        <NA>
    ## 3927              Gatorade Blue 28 oz     2 11.11        <NA>
    ## 3928             Gatorade Orange 28oz     2 11.11        <NA>
    ## 3929       Juice Grape Tropicana 11oz     2 11.11        <NA>
    ## 3930  Juice Lively Lemonade Tropicana     2 11.11        <NA>
    ## 3931 Gatorade Propel Berry Water 1 lt     2 11.11        <NA>
    ## 3932  Sparkling Lemonade Mango Kevita     2 11.11        <NA>
    ## 3933                 Cornell Lemonade     3 11.11        <NA>
    ## 3934         Muscle Milk Choc PB 14oz     1 11.11        <NA>
    ## 3935 Muscle Milk PROSRS 40 Intense Va     1 11.11        <NA>
    ## 3936          Soda Mountain Dew 20 Oz     2 11.11        <NA>
    ## 3937          Soda Rootbeer Mug 20 oz     2 11.11        <NA>
    ## 3938 Tea Sweetened With Lemon Brisk 2     2 11.11        <NA>
    ## 3939 Tea Unsweet Black W Lemon 18.5 o     2 11.11        <NA>
    ## 3940 Starbucks Doubleshot Ener Coffee     1 11.11        <NA>
    ## 3941         Juice Apple Dole 15.2 oz     2 11.11        <NA>
    ## 3942         Juice Lemonade Dole 20oz     2 11.11        <NA>
    ## 3943    Frappuccino Coffee 13.7 oz SB     1 11.11        <NA>
    ## 3944     Frappuccino Mocha 13.7 Oz SB     1 11.11        <NA>
    ## 3945          Juice Naked Berry Blast     1 11.11        <NA>
    ## 3946         Juice Protein Zone Naked     1 11.11        <NA>
    ## 3947  Kombucha Rasp Lemon Kevita 15.2     1 11.11        <NA>
    ## 3948  Energy Triple Shot Dark Caramel     1 11.11        <NA>
    ## 3949 Gatorade Gatorlyte Cherry Lime 2     1 11.11        <NA>
    ## 3950  Gatorade Gatorlyte Orange 20 oz     1 11.11        <NA>
    ## 3951 Gatorade Gatorlyte Strawberry Ki     1 11.11        <NA>
    ## 3952 Gatorade Gatorlyte Zero Lemon Li     1 11.11        <NA>
    ## 3953   Yerba Mate Revel Berry 15.5 oz     1 11.11        <NA>
    ## 3954 Cold Brew Vanilla Sweet Cream St     1 11.11        <NA>
    ## 3955 Energy 222 Pineapple Mango Odyss     1 11.11        <NA>
    ## 3956 Energy Orangesicle Celsius 16 oz     1 11.11        <NA>
    ## 3957    Rockstar Pure Zero Silver Ice     1 11.11        <NA>
    ## 3958  Energy Fast Twitch Cool Blue 12     1 11.11        <NA>
    ## 3959 Mountain Dew Kickstart Black Che     1 11.11        <NA>
    ## 3960           Water Gatorade 33.8 oz     1 11.11        <NA>
    ## 3961     Cider Apple Red Jacket 12 oz     1 11.11        <NA>
    ## 3962       Gatorade Fruit Punch 28 oz     1 11.11        <NA>
    ## 3963    Glacier Cherry Gatorade 28 oz     1 11.11        <NA>
    ## 3964    Glacier Freeze Gatorade 28 oz     1 11.11        <NA>
    ## 3965 Juice Zero Summer Splash Punch 1     1 11.11        <NA>
    ## 3966 Sparkling lemonade Strawberry Ke     1 11.11        <NA>
    ## 3967      Soda Mango Jarritos 12.5 oz     1 11.11        <NA>
    ## 3968  Soda Pineapple Jarritos 12.5 oz     1 11.11        <NA>
    ## 3969          Soda Orange Crush 20 Oz     1 11.11        <NA>
    ## 3970     Tea Light Peach Lipton 20 oz     1 11.11        <NA>
    ## 3971  Tea Peach Pure Leaf Lipton 18.5     1 11.11        <NA>
    ## 3972 Tea Tea & Lemon Pure Leaf Lipton     1 11.11        <NA>
    ## 3973   Soda Mandarin Jarritos 12.5 oz     1 11.11        <NA>
    ## 3974   Cornell Low Fat Chocolate Milk     5 11.11        <NA>
    ## 3975 Yogurt Flip Almond Coco Loco Cho     5 11.11        <NA>
    ## 3976 Yogurt 0% Fat Vanilla Greek Oiko     4 11.11        <NA>
    ## 3977 Yogurt Strawberry Banana Greek C     3 11.11        <NA>
    ## 3978            8 oz Vanilla Soy Silk     3 11.11        <NA>
    ## 3979   Milk Chocolate LF Cornell 8 Oz     3 11.11        <NA>
    ## 3980 Yogurt Black Cherry Greek Choban     2 11.11        <NA>
    ## 3981       Yogurt Peach Greek Chobani     2 11.11        <NA>
    ## 3982  Yogurt Strawberry Greek Chobani     2 11.11        <NA>
    ## 3983  Yogurt Mango Chobani Drink 7 oz     1 11.11        <NA>
    ## 3984 Yogurt Mixed Berry Chobani Drink     1 11.11        <NA>
    ## 3985 Yogurt Straw Banana Chobani Drin     1 11.11        <NA>
    ## 3986  Cornell Dairy Strawberry Yogurt     2 11.11        <NA>
    ## 3987               Cornell Whole Milk     2 11.11        <NA>
    ## 3988  Yogurt Flip Peanut Butter Dream     1 11.11        <NA>
    ## 3989       Yogurt Plain Greek Chobani     1 11.11        <NA>
    ## 3990   Yogurt Raspberry Greek Chobani     1 11.11        <NA>
    ## 3991   Fresh Cut Watermelon Fruit Cup    13 11.11        <NA>
    ## 3992        Fresh Cut Melon Fruit Cup     7 11.11        <NA>
    ## 3993    Fresh Cut Pineapple Fruit Cup     1 11.11        <NA>
    ## 3994      Jelly Konjac Peach Tastelli     7 11.11        <NA>
    ## 3995 Bar Cookie & Cream Quest 2.12 oz     2 11.11        <NA>
    ## 3996 Bar That's It Apple+Strawberry 1     3 11.11        <NA>
    ## 3997 Jelly Konjac Mango Pineapple Tas     2 11.11        <NA>
    ## 3998   Bar Crunchy Peanut Butter Clif     2 11.11        <NA>
    ## 3999      Kind Almond And Coconut Bar     2 11.11        <NA>
    ## 4000        Bar S'Mores Quest 2.12 oz     1 11.11        <NA>
    ## 4001  Cookie Choc Chip Lenny & Larrys     1 11.11        <NA>
    ## 4002 Bar CHOC Mint Builders Clif bar2     1 11.11        <NA>
    ## 4003  Bar Peanut Butter Builders Clif     1 11.11        <NA>
    ## 4004       Jalapeno Heat Chips Kosher     3 11.11        <NA>
    ## 4005                 Utz Regular Chip     3 11.11        <NA>
    ## 4006 Chip Salt & Malt Vinegar Dirty 2     2 11.11        <NA>
    ## 4007        Chip Kettle Sea Salt 2 oz     1 11.11        <NA>
    ## 4008 Chip Potato Kettle Honey BBQ 2oz     1 11.11        <NA>
    ## 4009 Chip Potato Dirty BBQ Mesquite 2     1 11.11        <NA>
    ## 4010           Chips Dirty Sea Salted     1 11.11        <NA>
    ## 4011  Chip Sour Cream And Onion Dirty     1 11.11        <NA>
    ## 4012   Chip Voodoo Limited Zapps 2 oz     1 11.11        <NA>
    ## 4013       Chip Baked Jax  Utz 1.5 oz     1 11.11        <NA>
    ## 4014                      Fruit Whole    31 11.11        <NA>
    ## 4015      Chewy Marshmallow GF 2.1 oz     5 11.11        <NA>
    ## 4016 GF Sweet Street Choloate Brownie     2 11.11        <NA>
    ## 4017             Orbit Wintermint Gum     3 11.11        <NA>
    ## 4018   Candy CHOC Cara Caff Bar Awake     1 11.11        <NA>
    ## 4019 Candy Dark Chocolate Awake Bar 1     1 11.11        <NA>
    ## 4020 Candy Milk Choc Caff Bar Awake 1     1 11.11        <NA>
    ## 4021             Orbit Sweet Mint Gum     1 11.11        <NA>
    ## 4022                 MochiCookCrm1.5o     3 11.11        <NA>
    ## 4023  Ice Cream Mochi Sweet Mango 1.5     1 11.11        <NA>
    ## 4024 Ice Cream Mochi Double Chocolate     1 11.11        <NA>
    ## 4025                     Muffin Jumbo    30 11.11        <NA>
    ## 4026                           Cookie    24 11.11        <NA>
    ## 4027                   Croissant Choc     4 11.11        <NA>
    ## 4028                Croissant Blue CC     3 11.11        <NA>
    ## 4029               Croissant Straw CC     3 11.11        <NA>
    ## 4030      Combo 16 oz Coffee & Muffin     3 11.11        <NA>
    ## 4031                     Cinnamon Bun     3 11.11        <NA>
    ## 4032                  Plain Croissant     3 11.11        <NA>
    ## 4033                      Coffee Cake     3 11.11        <NA>
    ## 4034                 BowlMexicanChick     4 11.11 grab 'n' go
    ## 4035                   BowlMedProtein     2 11.11 grab 'n' go
    ## 4036                 BowlSesAsianNood     2 11.11 grab 'n' go
    ## 4037                   BowlSouthChick     2 11.11 grab 'n' go
    ## 4038                     PBJ on Wheat     7 11.11 grab 'n' go
    ## 4039                 GFSunButterJelly     1 11.11 grab 'n' go
    ## 4040                  California Roll     3 11.11 grab 'n' go
    ## 4041               Golden Dragon Roll     2 11.11 grab 'n' go
    ## 4042                     Avocado Roll     2 11.11 grab 'n' go
    ## 4043            Hawaiian Volcano Roll     1 11.11 grab 'n' go
    ## 4044                     Alaskan Roll     1 11.11 grab 'n' go
    ## 4045                         TSA Roll     1 11.11 grab 'n' go
    ## 4046                  Hawaiian Sunset     1 11.11 grab 'n' go
    ## 4047                      Salmon Roll     1 11.11 grab 'n' go
    ## 4048                  Spicy Tuna Roll     1 11.11 grab 'n' go
    ## 4049             Wrap Buffalo Chicken     6 11.11 grab 'n' go
    ## 4050 Sandwich Prosciutto & Mozzarella     2 11.11 grab 'n' go
    ## 4051              Wrap Chicken Caesar     2 11.11 grab 'n' go
    ## 4052             Salad Chicken Caesar     2 11.11 grab 'n' go
    ## 4053 Sandwich Black Forrest Ham & Swi     1 11.11 grab 'n' go
    ## 4054     Sandwich Corned Beef & Swiss     1 11.11 grab 'n' go
    ## 4055       Quesadilla Deluxe Trillium   116 11.08     mexican
    ## 4056                Grilled Hamburger    80 11.08       grill
    ## 4057    Burrito Una Mano Trillium BYO    45 11.08     mexican
    ## 4058            Fried Chicken Tenders    55 11.08       grill
    ## 4059                     French Fries    97 11.08       grill
    ## 4060  Grilled Chicken Breast Sandwich    12 11.08       grill
    ## 4061                Quesadilla Cheese    12 11.08     mexican
    ## 4062               Sweet Potato Fries    32 11.08       grill
    ## 4063 Trillium Grill Impossible Burger     7 11.08       grill
    ## 4064             Seared Salmon Burger     7 11.08       grill
    ## 4065             ADD Beef Patty $2.99     5 11.08       grill
    ## 4066              Add Sausage 2 Patty     4 11.08       grill
    ## 4067                Black Bean Burger     1 11.08       grill
    ## 4068               ADD Chicken Breast     2 11.08       grill
    ## 4069                       ADD Cheese     6 11.08       grill
    ## 4070                     Add Egg $.99     2 11.08       grill
    ## 4071                1 Entree + 1 Side   126 11.08         wok
    ## 4072               Bowl Ramen Chicken    59 11.08       ramen
    ## 4073                1 Entree + 2 Side    53 11.08         wok
    ## 4074              2 Entrees + 2 Sides    20 11.08         wok
    ## 4075                  Bowl Ramen Tofu    14 11.08       ramen
    ## 4076          Side Vegetarian Lo Mein     8 11.08         wok
    ## 4077                     1 Wok Entree     2 11.08         wok
    ## 4078  Side Vegetarian Fried Rice with     3 11.08         wok
    ## 4079                Burrito Breakfast    97 11.08        <NA>
    ## 4080              Small French Omelet    57 11.08        <NA>
    ## 4081             Grand Slam Breakfast    18 11.08        <NA>
    ## 4082 Egg Cheese Bacon Breakfast Sandw    25 11.08        <NA>
    ## 4083 Egg Cheese Sausage Breakfast San    23 11.08        <NA>
    ## 4084                         Two Eggs    24 11.08        <NA>
    ## 4085                        Add Bacon    23 11.08        <NA>
    ## 4086                   Pancake Single     5 11.08        <NA>
    ## 4087              Trillium Home Fries     4 11.08        <NA>
    ## 4088                   2 Slices Toast     1 11.08        <NA>
    ## 4089                 PC Peanut Butter     1 11.08        <NA>
    ## 4090      Create Your Pasta Bowl MEAT    92 11.08     italian
    ## 4091       Create Your Pasta Bowl VEG    16 11.08     italian
    ## 4092              Pizza with Toppings    22 11.08     italian
    ## 4093                     Pizza Cheese    12 11.08     italian
    ## 4094                   Add Extra Meat    12 11.08     italian
    ## 4095         Side Bread Pasta Station     1 11.08     italian
    ## 4096                 Burrito Bowl BYO    71 11.08     mexican
    ## 4097                   Side Guacamole     3 11.08     mexican
    ## 4098                      Single Taco     1 11.08     mexican
    ## 4099               Salad by the Pound    41 11.08   salad bar
    ## 4100                       Soup 12 oz    33 11.08   salad bar
    ## 4101                        8 oz Soup    29 11.08   salad bar
    ## 4102              Soda Fountain 16 oz    33 11.08        <NA>
    ## 4103              Soda Fountain 24 oz    26 11.08        <NA>
    ## 4104                  Coffee 16 oz SB    17 11.08        <NA>
    ## 4105                  Coffee 12 oz SB    15 11.08        <NA>
    ## 4106                 Side Potato Tots    20 11.08        <NA>
    ## 4107               Open Miscellaneous     3 11.08        <NA>
    ## 4108          Add Extra Protein $2.99     1 11.08        <NA>
    ## 4109             Water Aquafina 20 oz    17 11.08        <NA>
    ## 4110 Milk Chocolate LF BIG RED Refuel    15 11.08        <NA>
    ## 4111   Yerba Mate Revel Berry 15.5 oz     7 11.08        <NA>
    ## 4112 Tea Jasmine Green Unsweet Ito En     7 11.08        <NA>
    ## 4113         Juice Naked Mighty Mango     6 11.08        <NA>
    ## 4114         Yerba Mate Lemon 15.5 oz     6 11.08        <NA>
    ## 4115         Juice Naked Blue Machine     5 11.08        <NA>
    ## 4116                 Soda Pepsi 20 Oz     9 11.08        <NA>
    ## 4117 Energy Mango PassFruit Celsius 1     6 11.08        <NA>
    ## 4118        Juice Naked Green Machine     4 11.08        <NA>
    ## 4119    Naked Strawberry Banana Juice     4 11.08        <NA>
    ## 4120                Water Aquafina 1L     7 11.08        <NA>
    ## 4121   Tea Iced Rasberry Lipton 16 oz     7 11.08        <NA>
    ## 4122            Soda Pepsi Diet 20 Oz     6 11.08        <NA>
    ## 4123     Soda Pepsi Wild Cherry 20 Oz     6 11.08        <NA>
    ## 4124 Tea Unsweet Green Lipton 18.5 oz     6 11.08        <NA>
    ## 4125        Gatorade Lemon Lime 28 oz     5 11.08        <NA>
    ## 4126 Juice Orange Homestyle Tropicana     5 11.08        <NA>
    ## 4127 Tea Green Peach Mango Celsius 12     4 11.08        <NA>
    ## 4128     Frappuccino Mocha 13.7 Oz SB     3 11.08        <NA>
    ## 4129         Juice Protein Zone Naked     3 11.08        <NA>
    ## 4130     Water Life WTR Immune 700 ML     4 11.08        <NA>
    ## 4131 Gatorade Gatorlyte Cherry Lime 2     3 11.08        <NA>
    ## 4132              Gatorade Blue 28 oz     4 11.08        <NA>
    ## 4133 Energy 222 Blue Raspberry Odysse     3 11.08        <NA>
    ## 4134 Gatorade Propel  Kiwi Straw Wate     4 11.08        <NA>
    ## 4135 Energy Fuji Apple Pear Celsius 1     3 11.08        <NA>
    ## 4136         Muscle Milk Choc PB 14oz     2 11.08        <NA>
    ## 4137 Muscle Milk PROSRS 40 Intense Va     2 11.08        <NA>
    ## 4138  Soda Rootbeer Zero Sugar Mug 20     4 11.08        <NA>
    ## 4139   Frappuccino Caramel SB 13.7 oz     2 11.08        <NA>
    ## 4140   Frappuccino Vanilla SB 13.7 Oz     2 11.08        <NA>
    ## 4141   Soda Mandarin Jarritos 12.5 oz     4 11.08        <NA>
    ## 4142   Kombucha Ginger Kevita 15.2 oz     2 11.08        <NA>
    ## 4143  Kombucha Pineapple Peach Kevita     2 11.08        <NA>
    ## 4144  Kombucha Rasp Lemon Kevita 15.2     2 11.08        <NA>
    ## 4145       Juice AppleTropicana 11 oz     3 11.08        <NA>
    ## 4146  Juice Lively Lemonade Tropicana     3 11.08        <NA>
    ## 4147 Juice Orange Premium Topicana 11     3 11.08        <NA>
    ## 4148 Juice Raspberry Lemonade Tropica     3 11.08        <NA>
    ## 4149 Gatorade Propel Berry Water 1 lt     3 11.08        <NA>
    ## 4150 Gatorade Propel Grape Water 1 lt     3 11.08        <NA>
    ## 4151 Gatorade Gatorlyte Strawberry Ki     2 11.08        <NA>
    ## 4152 Tea Golden Oolong Unsweet Ito En     2 11.08        <NA>
    ## 4153   Soda Tamarind Jarritos 12.5 oz     3 11.08        <NA>
    ## 4154 Cold Brew Vanilla Sweet Cream St     2 11.08        <NA>
    ## 4155  Energy Blue Crush Celsius 16 oz     2 11.08        <NA>
    ## 4156 Energy Orangesicle Celsius 16 oz     2 11.08        <NA>
    ## 4157 Juice Joes Lemonade  Red Jacket1     2 11.08        <NA>
    ## 4158 Red Jacket Strawberry Apple Juic     2 11.08        <NA>
    ## 4159       Lipton Pure Leaf Sweet Tea     3 11.08        <NA>
    ## 4160  Soda Ginger Ale Schweppes 20 Oz     3 11.08        <NA>
    ## 4161          Soda Mountain Dew 20 Oz     3 11.08        <NA>
    ## 4162          Soda Rootbeer Mug 20 oz     3 11.08        <NA>
    ## 4163 Tea Blackberry Pure Leaf 18.5 oz     3 11.08        <NA>
    ## 4164     Tea Light Peach Lipton 20 oz     3 11.08        <NA>
    ## 4165 Tea Pure Leaf Zero Sugar Sweet T     3 11.08        <NA>
    ## 4166 Tea Unsweetened Pure Leaf Lipton     3 11.08        <NA>
    ## 4167    Rockstar Pure Zero Silver Ice     2 11.08        <NA>
    ## 4168         Juice Apple Dole 15.2 oz     3 11.08        <NA>
    ## 4169 Energy Blue Raz Lemonade Celsius     2 11.08        <NA>
    ## 4170   Energy StrawGuava Celsius 12oz     2 11.08        <NA>
    ## 4171     Cider Apple Red Jacket 12 oz     2 11.08        <NA>
    ## 4172      Gatorade Berry Zero G 28 oz     2 11.08        <NA>
    ## 4173 Gatorade Galcier Freeze Zero G 2     2 11.08        <NA>
    ## 4174             Gatorade Orange 28oz     2 11.08        <NA>
    ## 4175       Juice Grape Tropicana 11oz     2 11.08        <NA>
    ## 4176 Kickstart Strawberry Start-up 16     2 11.08        <NA>
    ## 4177      Soda Mango Jarritos 12.5 oz     2 11.08        <NA>
    ## 4178 Muscle Milk P40 Strawberry Cream     1 11.08        <NA>
    ## 4179     Soda Mountain Dew Zero 20 Oz     2 11.08        <NA>
    ## 4180          Soda Orange Crush 20 Oz     2 11.08        <NA>
    ## 4181            Soda Pepsi  Zero 20oz     2 11.08        <NA>
    ## 4182 Tea Sweet W/ Le Pure Leaf Lipton     2 11.08        <NA>
    ## 4183 Tea Tea & Lemon Pure Leaf Lipton     2 11.08        <NA>
    ## 4184 Tea Unsweet Black W Lemon 18.5 o     2 11.08        <NA>
    ## 4185 Starbucks Doubleshot Energy Moch     1 11.08        <NA>
    ## 4186  Starbucks Doubleshot Energy Van     1 11.08        <NA>
    ## 4187   Ocean Spray Cranberry Cocktail     2 11.08        <NA>
    ## 4188    Frappuccino Coffee 13.7 oz SB     1 11.08        <NA>
    ## 4189          Juice Naked Berry Blast     1 11.08        <NA>
    ## 4190       Juice Naked PNCLDA 15.2 Oz     1 11.08        <NA>
    ## 4191 Gatorade Gatorlyte Zero Fruit Pu     1 11.08        <NA>
    ## 4192 Gatorade Gatorlyte Zero Lemon Li     1 11.08        <NA>
    ## 4193     Starbucks Double Shot 6.5 Oz     1 11.08        <NA>
    ## 4194    Water Aquafina Alumitek 16 oz     2 11.08        <NA>
    ## 4195 Cold Brew Chocolate Cream Starbu     1 11.08        <NA>
    ## 4196 Energy Dragonberry Celsius 16 oz     1 11.08        <NA>
    ## 4197 Energy Mango Tango Celsius 16 oz     1 11.08        <NA>
    ## 4198 Energy Revive Prickly Pear Odyss     1 11.08        <NA>
    ## 4199 Juice Fuji Apple Red Jacket 12oz     1 11.08        <NA>
    ## 4200                 Cornell Lemonade     2 11.08        <NA>
    ## 4201   Energy Artic Vibe Celsius 12oz     1 11.08        <NA>
    ## 4202 Energy Fast Twitch Watermelon St     1 11.08        <NA>
    ## 4203   Energy StrawLemon Celsius 12oz     1 11.08        <NA>
    ## 4204 Mountain Dew Kickstart Black Che     1 11.08        <NA>
    ## 4205 Mountain Dew Kickstart Orange 16     1 11.08        <NA>
    ## 4206       Gatorade Fruit Punch 28 oz     1 11.08        <NA>
    ## 4207    Glacier Freeze Gatorade 28 oz     1 11.08        <NA>
    ## 4208  Sparkling Lemonade Mango Kevita     1 11.08        <NA>
    ## 4209 Sparkling lemonade Strawberry Ke     1 11.08        <NA>
    ## 4210             Water Gatorade 700ml     1 11.08        <NA>
    ## 4211     Soda Starry Lemon Lime 20 oz     1 11.08        <NA>
    ## 4212 Tea Sweetened With Lemon Brisk 2     1 11.08        <NA>
    ## 4213         Juice Lemonade Dole 20oz     1 11.08        <NA>
    ## 4214 Juice Ocean Spray Cranbe Grape 1     1 11.08        <NA>
    ## 4215 Yogurt 0% Fat Vanilla Greek Oiko     6 11.08        <NA>
    ## 4216   Cornell Low Fat Chocolate Milk     5 11.08        <NA>
    ## 4217 Yogurt Mixed Berry Chobani Drink     4 11.08        <NA>
    ## 4218 Yogurt Flip Almond Coco Loco Cho     5 11.08        <NA>
    ## 4219 Yogurt Straw Banana Chobani Drin     2 11.08        <NA>
    ## 4220 Yogurt Strawberry Banana Greek C     3 11.08        <NA>
    ## 4221                  Cornell 2% Milk     2 11.08        <NA>
    ## 4222               Cornell Whole Milk     3 11.08        <NA>
    ## 4223   Milk Chocolate LF Cornell 8 Oz     3 11.08        <NA>
    ## 4224       Yogurt Peach Greek Chobani     2 11.08        <NA>
    ## 4225   Yogurt Raspberry Greek Chobani     2 11.08        <NA>
    ## 4226          Silk Chocolate Soy Milk     2 11.08        <NA>
    ## 4227  Yogurt Flip Peanut Butter Dream     1 11.08        <NA>
    ## 4228 Yogurt Black Cherry Greek Choban     1 11.08        <NA>
    ## 4229       Yogurt Mango Greek Chobani     1 11.08        <NA>
    ## 4230       Yogurt Plain Greek Chobani     1 11.08        <NA>
    ## 4231  Yogurt Strawberry Greek Chobani     1 11.08        <NA>
    ## 4232  Cornell Dairy Strawberry Yogurt     1 11.08        <NA>
    ## 4233 Bar Cookie & Cream Quest 2.12 oz     3 11.08        <NA>
    ## 4234 Bar Choc Chip Cookie Dough Quest     2 11.08        <NA>
    ## 4235 Jelly Konjac Apple Grape Tastell     2 11.08        <NA>
    ## 4236 Jelly Konjac Mango Pineapple Tas     2 11.08        <NA>
    ## 4237 Bar White Chocolate Macadamia Cl     2 11.08        <NA>
    ## 4238 Bar That's It Apple+Blueberry 1.     2 11.08        <NA>
    ## 4239  Bar Frosted Birthday Cake Quest     1 11.08        <NA>
    ## 4240        Bar S'Mores Quest 2.12 oz     1 11.08        <NA>
    ## 4241 Cookie Wht/Choc Mac Lenny & Larr     1 11.08        <NA>
    ## 4242      Jelly Konjac Peach Tastelli     1 11.08        <NA>
    ## 4243       Bar Chocolate Chip Clifbar     1 11.08        <NA>
    ## 4244   Bar Crunchy Peanut Butter Clif     1 11.08        <NA>
    ## 4245   Fresh Cut Watermelon Fruit Cup     5 11.08        <NA>
    ## 4246        Fresh Cut Melon Fruit Cup     3 11.08        <NA>
    ## 4247    Fresh Cut Pineapple Fruit Cup     2 11.08        <NA>
    ## 4248                      Fruit Whole    30 11.08        <NA>
    ## 4249 Chip Salt & Malt Vinegar Dirty 2     3 11.08        <NA>
    ## 4250                 Utz Regular Chip     2 11.08        <NA>
    ## 4251       Chip Baked Jax  Utz 1.5 oz     2 11.08        <NA>
    ## 4252  Crunchsters Smokey Balsamic 1.3     1 11.08        <NA>
    ## 4253     Chip Funky Fusion Dirty 2 oz     1 11.08        <NA>
    ## 4254           Chips Dirty Sea Salted     1 11.08        <NA>
    ## 4255   Chip Voodoo Limited Zapps 2 oz     1 11.08        <NA>
    ## 4256   Utz Honey Barbecue Chip 1.5 oz     1 11.08        <NA>
    ## 4257          Utz Salt N Vinegar Chip     1 11.08        <NA>
    ## 4258 GF Sweet Street Choloate Brownie     4 11.08        <NA>
    ## 4259      Chewy Marshmallow GF 2.1 oz     1 11.08        <NA>
    ## 4260   Candy CHOC Cara Caff Bar Awake     1 11.08        <NA>
    ## 4261             Orbit Sweet Mint Gum     1 11.08        <NA>
    ## 4262             Orbit Wintermint Gum     1 11.08        <NA>
    ## 4263  Ice Cream Mochi Sweet Mango 1.5     1 11.08        <NA>
    ## 4264                     Muffin Jumbo    28 11.08        <NA>
    ## 4265                           Cookie    13 11.08        <NA>
    ## 4266               Croissant Straw CC     4 11.08        <NA>
    ## 4267      Combo 16 oz Coffee & Muffin     4 11.08        <NA>
    ## 4268                     Cinnamon Bun     4 11.08        <NA>
    ## 4269                Croissant Blue CC     2 11.08        <NA>
    ## 4270                      Coffee Cake     2 11.08        <NA>
    ## 4271                   Croissant Choc     1 11.08        <NA>
    ## 4272                 BowlMexicanChick     2 11.08 grab 'n' go
    ## 4273                 BowlSesAsianNood     2 11.08 grab 'n' go
    ## 4274                   BowlSouthChick     2 11.08 grab 'n' go
    ## 4275                 GF ChicCaesarSld     1 11.08 grab 'n' go
    ## 4276                  California Roll     3 11.08 grab 'n' go
    ## 4277                  Hawaiian Sunset     2 11.08 grab 'n' go
    ## 4278                     Alaskan Roll     1 11.08 grab 'n' go
    ## 4279              Tempura Shrimp Roll     1 11.08 grab 'n' go
    ## 4280                         TSA Roll     1 11.08 grab 'n' go
    ## 4281               Golden Dragon Roll     1 11.08 grab 'n' go
    ## 4282                      Salmon Roll     1 11.08 grab 'n' go
    ## 4283                  Spicy Tuna Roll     1 11.08 grab 'n' go
    ## 4284                     Avocado Roll     1 11.08 grab 'n' go
    ## 4285 Sandwich Black Forrest Ham & Swi     4 11.08 grab 'n' go
    ## 4286              Wrap Chicken Caesar     2 11.08 grab 'n' go
    ## 4287             Wrap Buffalo Chicken     1 11.08 grab 'n' go
    ## 4288       Quesadilla Deluxe Trillium   160 11.07     mexican
    ## 4289                Grilled Hamburger    98 11.07       grill
    ## 4290            Fried Chicken Tenders   105 11.07       grill
    ## 4291    Burrito Una Mano Trillium BYO    64 11.07     mexican
    ## 4292                     French Fries   141 11.07       grill
    ## 4293  Grilled Chicken Breast Sandwich    19 11.07       grill
    ## 4294             Seared Salmon Burger    14 11.07       grill
    ## 4295               Sweet Potato Fries    33 11.07       grill
    ## 4296 Trillium Grill Impossible Burger     7 11.07       grill
    ## 4297                Quesadilla Cheese     9 11.07     mexican
    ## 4298             ADD Beef Patty $2.99    12 11.07       grill
    ## 4299                Black Bean Burger     4 11.07       grill
    ## 4300               ADD Chicken Breast     9 11.07       grill
    ## 4301              Add Sausage 2 Patty     2 11.07       grill
    ## 4302                     Add Egg $.99     3 11.07       grill
    ## 4303                       ADD Cheese     3 11.07       grill
    ## 4304                1 Entree + 1 Side   193 11.07         wok
    ## 4305                1 Entree + 2 Side    94 11.07         wok
    ## 4306               Bowl Ramen Chicken    82 11.07       ramen
    ## 4307              2 Entrees + 2 Sides    30 11.07         wok
    ## 4308                  Bowl Ramen Tofu    14 11.07       ramen
    ## 4309          Side Vegetarian Lo Mein    12 11.07         wok
    ## 4310                     1 Wok Entree     5 11.07         wok
    ## 4311         Side White or Brown Rice     5 11.07         wok
    ## 4312                  Bowl Ramen Pork     1 11.07        <NA>
    ## 4313      Side Vegetable Spring Rolls     2 11.07         wok
    ## 4314      Create Your Pasta Bowl MEAT   132 11.07     italian
    ## 4315       Create Your Pasta Bowl VEG    28 11.07     italian
    ## 4316              Pizza with Toppings    38 11.07     italian
    ## 4317                     Pizza Cheese    21 11.07     italian
    ## 4318                   Add Extra Meat    18 11.07     italian
    ## 4319         Side Bread Pasta Station     1 11.07     italian
    ## 4320                Burrito Breakfast    90 11.07        <NA>
    ## 4321              Small French Omelet    60 11.07        <NA>
    ## 4322             Grand Slam Breakfast    17 11.07        <NA>
    ## 4323 Egg Cheese Sausage Breakfast San    24 11.07        <NA>
    ## 4324 Egg Cheese Bacon Breakfast Sandw    22 11.07        <NA>
    ## 4325                        Add Bacon    24 11.07        <NA>
    ## 4326                         Two Eggs    19 11.07        <NA>
    ## 4327              Trillium Home Fries     3 11.07        <NA>
    ## 4328                   Pancake Single     2 11.07        <NA>
    ## 4329                 PC Peanut Butter     2 11.07        <NA>
    ## 4330                            Toast     1 11.07        <NA>
    ## 4331                 Burrito Bowl BYO    98 11.07     mexican
    ## 4332                      Single Taco     6 11.07     mexican
    ## 4333                   Side Guacamole     5 11.07     mexican
    ## 4334      Add Extra Toppings Una Mano     2 11.07     mexican
    ## 4335                       Side Salsa     1 11.07     mexican
    ## 4336               Salad by the Pound    69 11.07   salad bar
    ## 4337                        8 oz Soup    41 11.07   salad bar
    ## 4338                       Soup 12 oz    34 11.07   salad bar
    ## 4339              Soda Fountain 24 oz    34 11.07        <NA>
    ## 4340                  Coffee 12 oz SB    22 11.07        <NA>
    ## 4341                  Coffee 16 oz SB    18 11.07        <NA>
    ## 4342              Soda Fountain 16 oz    20 11.07        <NA>
    ## 4343                    Hot Tea 20 oz     4 11.07        <NA>
    ## 4344                 Side Potato Tots    25 11.07        <NA>
    ## 4345               Open Miscellaneous    10 11.07        <NA>
    ## 4346             Water Aquafina 20 oz    22 11.07        <NA>
    ## 4347   Yerba Mate Revel Berry 15.5 oz    10 11.07        <NA>
    ## 4348 Yerba Mate Enlighten Mint 15.5 o     8 11.07        <NA>
    ## 4349            Soda Pepsi Diet 20 Oz    12 11.07        <NA>
    ## 4350         Yerba Mate Lemon 15.5 oz     7 11.07        <NA>
    ## 4351 Tea Jasmine Green Unsweet Ito En     7 11.07        <NA>
    ## 4352         Juice Naked Mighty Mango     6 11.07        <NA>
    ## 4353 Muscle Milk PROSRS 40 Intense Va     5 11.07        <NA>
    ## 4354            Soda Pepsi  Zero 20oz    10 11.07        <NA>
    ## 4355 Energy Mango PassFruit Celsius 1     7 11.07        <NA>
    ## 4356   Energy StrawGuava Celsius 12oz     7 11.07        <NA>
    ## 4357 Tea Golden Oolong Unsweet Ito En     6 11.07        <NA>
    ## 4358 Juice Orange Homestyle Tropicana     8 11.07        <NA>
    ## 4359   Energy Artic Vibe Celsius 12oz     6 11.07        <NA>
    ## 4360  Tea Green Rasp Acai  Celsius 12     6 11.07        <NA>
    ## 4361     Water Life WTR Immune 700 ML     6 11.07        <NA>
    ## 4362      Yerba Mate Tropical 15.5 oz     5 11.07        <NA>
    ## 4363   Tea Iced Rasberry Lipton 16 oz     8 11.07        <NA>
    ## 4364          Juice Naked Berry Blast     4 11.07        <NA>
    ## 4365        Juice Naked Green Machine     4 11.07        <NA>
    ## 4366 Gatorade Galcier Freeze Zero G 2     6 11.07        <NA>
    ## 4367 Juice Orange Premium Topicana 11     6 11.07        <NA>
    ## 4368     Soda Mountain Dew Zero 20 Oz     7 11.07        <NA>
    ## 4369                 Soda Pepsi 20 Oz     7 11.07        <NA>
    ## 4370    Tea Black Milk Ito En 11.8 oz     4 11.07        <NA>
    ## 4371    Muscle Milk KO Chocolate 14oz     3 11.07        <NA>
    ## 4372 Cold Brew Vanilla Sweet Cream St     4 11.07        <NA>
    ## 4373        Gatorade Lemon Lime 28 oz     5 11.07        <NA>
    ## 4374   Energy StrawLemon Celsius 12oz     4 11.07        <NA>
    ## 4375    Frappuccino Coffee 13.7 oz SB     3 11.07        <NA>
    ## 4376         Juice Naked Blue Machine     3 11.07        <NA>
    ## 4377    Naked Strawberry Banana Juice     3 11.07        <NA>
    ## 4378   Kombucha Ginger Kevita 15.2 oz     3 11.07        <NA>
    ## 4379     Cider Apple Red Jacket 12 oz     4 11.07        <NA>
    ## 4380  Tea Peach Pure Leaf Lipton 18.5     5 11.07        <NA>
    ## 4381 Tea Sweetened With Lemon Brisk 2     5 11.07        <NA>
    ## 4382 Tea Sweet W/ Le Pure Leaf Lipton     5 11.07        <NA>
    ## 4383 Tea Tea & Lemon Pure Leaf Lipton     5 11.07        <NA>
    ## 4384 Tea Unsweet Green Lipton 18.5 oz     5 11.07        <NA>
    ## 4385    Yerba Mate Bluephoria 15.5 oz     3 11.07        <NA>
    ## 4386              Gatorade Blue 28 oz     4 11.07        <NA>
    ## 4387  Energy Blue Crush Celsius 16 oz     3 11.07        <NA>
    ## 4388 Gatorade Propel Grape Water 1 lt     4 11.07        <NA>
    ## 4389 Gatorade Propel  Kiwi Straw Wate     4 11.07        <NA>
    ## 4390   Energy Fast Twitch Grape 12 oz     3 11.07        <NA>
    ## 4391 Energy Fuji Apple Pear Celsius 1     3 11.07        <NA>
    ## 4392                Water Aquafina 1L     4 11.07        <NA>
    ## 4393           Water Gatorade 33.8 oz     3 11.07        <NA>
    ## 4394          Soda Orange Crush 20 Oz     4 11.07        <NA>
    ## 4395 Tea Unsweet Black W Lemon 18.5 o     4 11.07        <NA>
    ## 4396  Starbucks Doubleshot Energy Van     2 11.07        <NA>
    ## 4397   Frappuccino Caramel SB 13.7 oz     2 11.07        <NA>
    ## 4398     Frappuccino Mocha 13.7 Oz SB     2 11.07        <NA>
    ## 4399       Juice Naked PNCLDA 15.2 Oz     2 11.07        <NA>
    ## 4400 Juice Ocean Spray Cranbe Grape 1     4 11.07        <NA>
    ## 4401             Gatorade Orange 28oz     3 11.07        <NA>
    ## 4402    Glacier Freeze Gatorade 28 oz     3 11.07        <NA>
    ## 4403       Juice Grape Tropicana 11oz     3 11.07        <NA>
    ## 4404  Juice Lively Lemonade Tropicana     3 11.07        <NA>
    ## 4405 Juice Raspberry Lemonade Tropica     3 11.07        <NA>
    ## 4406 Milk Chocolate LF BIG RED Refuel     4 11.07        <NA>
    ## 4407 Sparkling lemonade Strawberry Ke     3 11.07        <NA>
    ## 4408  Energy Triple Shot Dark Caramel     2 11.07        <NA>
    ## 4409     Starbucks Double Shot 6.5 Oz     2 11.07        <NA>
    ## 4410 Cold Brew Chocolate Cream Starbu     2 11.07        <NA>
    ## 4411 Energy 222 Blue Raspberry Odysse     2 11.07        <NA>
    ## 4412 Energy Mango Tango Celsius 16 oz     2 11.07        <NA>
    ## 4413 Juice Fuji Apple Red Jacket 12oz     2 11.07        <NA>
    ## 4414 Red Jacket Strawberry Apple Juic     2 11.07        <NA>
    ## 4415       Lipton Pure Leaf Sweet Tea     3 11.07        <NA>
    ## 4416  Soda Ginger Ale Schweppes 20 Oz     3 11.07        <NA>
    ## 4417          Soda Mountain Dew 20 Oz     3 11.07        <NA>
    ## 4418     Soda Starry Lemon Lime 20 oz     3 11.07        <NA>
    ## 4419   Rockstar Strawberry Punch 16oz     2 11.07        <NA>
    ## 4420 Energy Blue Raz Lemonade Celsius     2 11.07        <NA>
    ## 4421   Soda Mandarin Jarritos 12.5 oz     3 11.07        <NA>
    ## 4422 Mountain Dew Kickstart Orange 16     2 11.07        <NA>
    ## 4423      Gatorade Berry Zero G 28 oz     2 11.07        <NA>
    ## 4424       Juice AppleTropicana 11 oz     2 11.07        <NA>
    ## 4425 Gatorade Propel Berry Water 1 lt     2 11.07        <NA>
    ## 4426  Sparkling Lemonade Mango Kevita     2 11.07        <NA>
    ## 4427 Muscle Milk P40 Strawberry Cream     1 11.07        <NA>
    ## 4428     Soda Pepsi Wild Cherry 20 Oz     2 11.07        <NA>
    ## 4429  Soda Rootbeer Zero Sugar Mug 20     2 11.07        <NA>
    ## 4430 Tea Blackberry Pure Leaf 18.5 oz     2 11.07        <NA>
    ## 4431     Tea Light Peach Lipton 20 oz     2 11.07        <NA>
    ## 4432 Tea Pure Leaf Zero Sugar Sweet T     2 11.07        <NA>
    ## 4433 Starbucks Doubleshot Energy Moch     1 11.07        <NA>
    ## 4434         Juice Apple Dole 15.2 oz     2 11.07        <NA>
    ## 4435   Frappuccino Vanilla SB 13.7 Oz     1 11.07        <NA>
    ## 4436         Juice Protein Zone Naked     1 11.07        <NA>
    ## 4437          Naked Red Machine Juice     1 11.07        <NA>
    ## 4438  Kombucha Pineapple Peach Kevita     1 11.07        <NA>
    ## 4439  Kombucha Rasp Lemon Kevita 15.2     1 11.07        <NA>
    ## 4440 Gatorade Gatorlyte Cherry Lime 2     1 11.07        <NA>
    ## 4441 Gatorade Gatorlyte MixBerry 20 o     1 11.07        <NA>
    ## 4442  Gatorade Gatorlyte Orange 20 oz     1 11.07        <NA>
    ## 4443 Gatorade Gatorlyte Zero Lemon Li     1 11.07        <NA>
    ## 4444 Tea Green Matcha Milk Ito En 11.     1 11.07        <NA>
    ## 4445 Energy Cherry Limeade Celsius 16     1 11.07        <NA>
    ## 4446 Energy Dragonberry Celsius 16 oz     1 11.07        <NA>
    ## 4447 Energy Orangesicle Celsius 16 oz     1 11.07        <NA>
    ## 4448 Energy Revive Prickly Pear Odyss     1 11.07        <NA>
    ## 4449    Rockstar Pure Zero Silver Ice     1 11.07        <NA>
    ## 4450  Energy Fast Twitch Cool Blue 12     1 11.07        <NA>
    ## 4451 Energy Fast Twitch Watermelon St     1 11.07        <NA>
    ## 4452 Tea Green Peach Mango Celsius 12     1 11.07        <NA>
    ## 4453       Gatorade Fruit Punch 28 oz     1 11.07        <NA>
    ## 4454    Glacier Cherry Gatorade 28 oz     1 11.07        <NA>
    ## 4455 Kickstart Strawberry Start-up 16     1 11.07        <NA>
    ## 4456             Water Gatorade 700ml     1 11.07        <NA>
    ## 4457      Soda Guava Jarritos 12.5 oz     1 11.07        <NA>
    ## 4458      Soda Mango Jarritos 12.5 oz     1 11.07        <NA>
    ## 4459  Soda Pineapple Jarritos 12.5 oz     1 11.07        <NA>
    ## 4460   Soda Tamarind Jarritos 12.5 oz     1 11.07        <NA>
    ## 4461 Tea Unsweetened Pure Leaf Lipton     1 11.07        <NA>
    ## 4462         Juice Lemonade Dole 20oz     1 11.07        <NA>
    ## 4463        Juice Orange Dole 15.2 oz     1 11.07        <NA>
    ## 4464   Ocean Spray Cranberry Cocktail     1 11.07        <NA>
    ## 4465    Water Aquafina Alumitek 16 oz     1 11.07        <NA>
    ## 4466 Yogurt 0% Fat Vanilla Greek Oiko     8 11.07        <NA>
    ## 4467   Milk Chocolate LF Cornell 8 Oz    11 11.07        <NA>
    ## 4468  Yogurt Mango Chobani Drink 7 oz     5 11.07        <NA>
    ## 4469               Cornell Whole Milk     7 11.07        <NA>
    ## 4470       Yogurt Mango Greek Chobani     5 11.07        <NA>
    ## 4471                  Cornell 2% Milk     5 11.07        <NA>
    ## 4472 Yogurt Flip Almond Coco Loco Cho     3 11.07        <NA>
    ## 4473  Yogurt Strawberry Greek Chobani     3 11.07        <NA>
    ## 4474   Cornell Low Fat Chocolate Milk     2 11.07        <NA>
    ## 4475  Yogurt Flip Peanut Butter Dream     2 11.07        <NA>
    ## 4476  Cornell Dairy Strawberry Yogurt     3 11.07        <NA>
    ## 4477 Yogurt Black Cherry Greek Choban     2 11.07        <NA>
    ## 4478       Yogurt Peach Greek Chobani     2 11.07        <NA>
    ## 4479 Yogurt Mixed Berry Chobani Drink     1 11.07        <NA>
    ## 4480       Cornell Dairy Peach Yogurt     2 11.07        <NA>
    ## 4481   Yogurt Blueberry Greek Chobani     1 11.07        <NA>
    ## 4482       Yogurt Plain Greek Chobani     1 11.07        <NA>
    ## 4483   Yogurt Raspberry Greek Chobani     1 11.07        <NA>
    ## 4484 Jelly Konjac Apple Grape Tastell     4 11.07        <NA>
    ## 4485 Jelly Konjac Mango Pineapple Tas     3 11.07        <NA>
    ## 4486 Bar Choc Chip Cookie Dough Quest     2 11.07        <NA>
    ## 4487 Bar Cookie & Cream Quest 2.12 oz     2 11.07        <NA>
    ## 4488        Bar S'Mores Quest 2.12 oz     2 11.07        <NA>
    ## 4489 Bar CHOC Mint Builders Clif bar2     2 11.07        <NA>
    ## 4490 Jelly Konjac Double Berry Tastel     2 11.07        <NA>
    ## 4491      Jelly Konjac Peach Tastelli     2 11.07        <NA>
    ## 4492 Cookie Birthday Cake Lenny & Lar     1 11.07        <NA>
    ## 4493  Cookie Choc Chip Lenny & Larrys     1 11.07        <NA>
    ## 4494   Fruit And Nut Delight Kind Bar     1 11.07        <NA>
    ## 4495       Bar Chocolate Chip Clifbar     1 11.07        <NA>
    ## 4496 Dark Choc Cherry Cashew Plus Kin     1 11.07        <NA>
    ## 4497 Bar That's It Apple+Blueberry 1.     1 11.07        <NA>
    ## 4498 Bar That's It Apple+Strawberry 1     1 11.07        <NA>
    ## 4499  Oats And Dark Chocolate Granola     1 11.07        <NA>
    ## 4500  Chip Sour Cream And Onion Dirty     5 11.07        <NA>
    ## 4501       Jalapeno Heat Chips Kosher     4 11.07        <NA>
    ## 4502 Nuts Almonds Honey 1.5 oz Sahale     2 11.07        <NA>
    ## 4503       Chip Baked Jax  Utz 1.5 oz     4 11.07        <NA>
    ## 4504    Chip Kettle Salt Vinegar 2 oz     2 11.07        <NA>
    ## 4505     Chip Funky Fusion Dirty 2 oz     2 11.07        <NA>
    ## 4506 Chip Salt & Malt Vinegar Dirty 2     2 11.07        <NA>
    ## 4507   Chip Voodoo Limited Zapps 2 oz     2 11.07        <NA>
    ## 4508     Chip Kettle Sweet Onion 2 oz     1 11.07        <NA>
    ## 4509 Chip Potato Kettle Honey BBQ 2oz     1 11.07        <NA>
    ## 4510       Chip Maui Onion Dirty 2 oz     1 11.07        <NA>
    ## 4511           Chips Dirty Sea Salted     1 11.07        <NA>
    ## 4512   Utz Honey Barbecue Chip 1.5 oz     1 11.07        <NA>
    ## 4513                 Utz Regular Chip     1 11.07        <NA>
    ## 4514          Utz Salt N Vinegar Chip     1 11.07        <NA>
    ## 4515    Utz Sour Cream and Onion Chip     1 11.07        <NA>
    ## 4516         Pretzel Thin 2.12 oz Utz     1 11.07        <NA>
    ## 4517                      Fruit Whole    46 11.07        <NA>
    ## 4518             Orbit Sweet Mint Gum     4 11.07        <NA>
    ## 4519 Candy Milk Choc Caff Bar Awake 1     2 11.07        <NA>
    ## 4520             Orbit Wintermint Gum     2 11.07        <NA>
    ## 4521 Candy Dark Chocolate Awake Bar 1     1 11.07        <NA>
    ## 4522    Fresh Cut Pineapple Fruit Cup     2 11.07        <NA>
    ## 4523        Fresh Cut Melon Fruit Cup     1 11.07        <NA>
    ## 4524  Ice Cream Mochi Sweet Mango 1.5     4 11.07        <NA>
    ## 4525      Chewy Marshmallow GF 2.1 oz     2 11.07        <NA>
    ## 4526                  Spicy Tuna Roll     3 11.07 grab 'n' go
    ## 4527                  California Roll     3 11.07 grab 'n' go
    ## 4528              Tempura Shrimp Roll     2 11.07 grab 'n' go
    ## 4529               Golden Dragon Roll     2 11.07 grab 'n' go
    ## 4530                      Salmon Roll     2 11.07 grab 'n' go
    ## 4531                     Alaskan Roll     1 11.07 grab 'n' go
    ## 4532              Tempura Crunch Roll     1 11.07 grab 'n' go
    ## 4533                         TSA Roll     1 11.07 grab 'n' go
    ## 4534                  Hawaiian Sunset     1 11.07 grab 'n' go
    ## 4535                     Avocado Roll     1 11.07 grab 'n' go
    ## 4536              Wrap Chicken Caesar     6 11.07 grab 'n' go
    ## 4537     Sandwich Corned Beef & Swiss     3 11.07 grab 'n' go
    ## 4538 Sandwich Black Forrest Ham & Swi     2 11.07 grab 'n' go
    ## 4539 Sandwich Crispy Chicken Milanese     2 11.07 grab 'n' go
    ## 4540             Wrap Buffalo Chicken     2 11.07 grab 'n' go
    ## 4541 Sandwich Prosciutto & Mozzarella     1 11.07 grab 'n' go
    ## 4542             Salad Chicken Caesar     1 11.07 grab 'n' go
    ## 4543                     Muffin Jumbo    29 11.07        <NA>
    ## 4544                           Cookie    19 11.07        <NA>
    ## 4545               Croissant Straw CC     6 11.07        <NA>
    ## 4546                   Croissant Choc     5 11.07        <NA>
    ## 4547      Combo 16 oz Coffee & Muffin     5 11.07        <NA>
    ## 4548                     Cinnamon Bun     4 11.07        <NA>
    ## 4549                            Scone     4 11.07        <NA>
    ## 4550                      Coffee Cake     3 11.07        <NA>
    ## 4551                Croissant Blue CC     1 11.07        <NA>
    ## 4552                 BowlMexicanChick     2 11.07 grab 'n' go
    ## 4553                   BowlSouthChick     2 11.07 grab 'n' go
    ## 4554                 BowlSesAsianNood     1 11.07 grab 'n' go
    ## 4555                     PBJ on Wheat     7 11.07 grab 'n' go
    ## 4556                   GF Turkey Sand     2 11.07 grab 'n' go
    ## 4557                 GFSunButterJelly     2 11.07 grab 'n' go
    ## 4558       Quesadilla Deluxe Trillium   130 11.06     mexican
    ## 4559                Grilled Hamburger   104 11.06       grill
    ## 4560            Fried Chicken Tenders    94 11.06       grill
    ## 4561    Burrito Una Mano Trillium BYO    59 11.06     mexican
    ## 4562                     French Fries   148 11.06       grill
    ## 4563  Grilled Chicken Breast Sandwich    16 11.06       grill
    ## 4564               Sweet Potato Fries    37 11.06       grill
    ## 4565 Trillium Grill Impossible Burger     9 11.06       grill
    ## 4566                Quesadilla Cheese    11 11.06     mexican
    ## 4567             Seared Salmon Burger     6 11.06       grill
    ## 4568               ADD Chicken Breast     8 11.06       grill
    ## 4569             ADD Beef Patty $2.99     7 11.06       grill
    ## 4570                Black Bean Burger     2 11.06       grill
    ## 4571              Add Sausage 2 Patty     6 11.06       grill
    ## 4572                       ADD Cheese     7 11.06       grill
    ## 4573                     Add Egg $.99     3 11.06       grill
    ## 4574                1 Entree + 1 Side   209 11.06         wok
    ## 4575               Bowl Ramen Chicken    90 11.06       ramen
    ## 4576                1 Entree + 2 Side    54 11.06         wok
    ## 4577              2 Entrees + 2 Sides    27 11.06         wok
    ## 4578                  Bowl Ramen Tofu    17 11.06       ramen
    ## 4579                     1 Wok Entree    14 11.06         wok
    ## 4580          Side Vegetarian Lo Mein     9 11.06         wok
    ## 4581  Side Vegetarian Fried Rice with     7 11.06         wok
    ## 4582         Side White or Brown Rice    13 11.06         wok
    ## 4583                  Side Vegetables     2 11.06         wok
    ## 4584      Create Your Pasta Bowl MEAT   136 11.06     italian
    ## 4585       Create Your Pasta Bowl VEG    30 11.06     italian
    ## 4586              Pizza with Toppings    38 11.06     italian
    ## 4587                     Pizza Cheese    20 11.06     italian
    ## 4588                   Add Extra Meat    27 11.06     italian
    ## 4589                Burrito Breakfast    94 11.06        <NA>
    ## 4590              Small French Omelet    64 11.06        <NA>
    ## 4591             Grand Slam Breakfast    17 11.06        <NA>
    ## 4592 Egg Cheese Bacon Breakfast Sandw    24 11.06        <NA>
    ## 4593 Egg Cheese Sausage Breakfast San    23 11.06        <NA>
    ## 4594                        Add Bacon    27 11.06        <NA>
    ## 4595                         Two Eggs    19 11.06        <NA>
    ## 4596              Trillium Home Fries     7 11.06        <NA>
    ## 4597                   Pancake Single     6 11.06        <NA>
    ## 4598                            Toast     3 11.06        <NA>
    ## 4599                 PC Peanut Butter     2 11.06        <NA>
    ## 4600                   2 Slices Toast     1 11.06        <NA>
    ## 4601                 Burrito Bowl BYO   109 11.06     mexican
    ## 4602                      Single Taco     5 11.06     mexican
    ## 4603                  Side Sour Cream     1 11.06        <NA>
    ## 4604               Salad by the Pound    59 11.06   salad bar
    ## 4605                        8 oz Soup    54 11.06   salad bar
    ## 4606                       Soup 12 oz    40 11.06   salad bar
    ## 4607              Soda Fountain 16 oz    42 11.06        <NA>
    ## 4608              Soda Fountain 24 oz    32 11.06        <NA>
    ## 4609                  Coffee 16 oz SB    20 11.06        <NA>
    ## 4610                  Coffee 12 oz SB    18 11.06        <NA>
    ## 4611                    Hot Tea 20 oz     5 11.06        <NA>
    ## 4612               Open Miscellaneous    15 11.06        <NA>
    ## 4613                 Side Potato Tots    19 11.06        <NA>
    ## 4614          Add Extra Protein $2.99     1 11.06        <NA>
    ## 4615            Soda Pepsi Diet 20 Oz    23 11.06        <NA>
    ## 4616 Milk Chocolate LF BIG RED Refuel    19 11.06        <NA>
    ## 4617 Tea Jasmine Green Unsweet Ito En    10 11.06        <NA>
    ## 4618                Water Aquafina 1L    14 11.06        <NA>
    ## 4619             Water Aquafina 20 oz    15 11.06        <NA>
    ## 4620    Yerba Mate Bluephoria 15.5 oz     7 11.06        <NA>
    ## 4621   Energy Artic Vibe Celsius 12oz     8 11.06        <NA>
    ## 4622 Tea Golden Oolong Unsweet Ito En     7 11.06        <NA>
    ## 4623                 Soda Pepsi 20 Oz    11 11.06        <NA>
    ## 4624            Soda Pepsi  Zero 20oz    10 11.06        <NA>
    ## 4625 Energy Fuji Apple Pear Celsius 1     7 11.06        <NA>
    ## 4626 Yerba Mate Enlighten Mint 15.5 o     6 11.06        <NA>
    ## 4627     Water Life WTR Immune 700 ML     7 11.06        <NA>
    ## 4628         Juice Protein Zone Naked     5 11.06        <NA>
    ## 4629  Kombucha Rasp Lemon Kevita 15.2     5 11.06        <NA>
    ## 4630 Tea Sweet W/ Le Pure Leaf Lipton     9 11.06        <NA>
    ## 4631   Energy StrawLemon Celsius 12oz     6 11.06        <NA>
    ## 4632 Juice Orange Homestyle Tropicana     7 11.06        <NA>
    ## 4633         Yerba Mate Lemon 15.5 oz     5 11.06        <NA>
    ## 4634      Yerba Mate Tropical 15.5 oz     5 11.06        <NA>
    ## 4635   Frappuccino Caramel SB 13.7 oz     4 11.06        <NA>
    ## 4636   Frappuccino Vanilla SB 13.7 Oz     4 11.06        <NA>
    ## 4637        Juice Naked Green Machine     4 11.06        <NA>
    ## 4638 Energy Mango PassFruit Celsius 1     5 11.06        <NA>
    ## 4639 Gatorade Propel  Kiwi Straw Wate     6 11.06        <NA>
    ## 4640     Starbucks Double Shot 6.5 Oz     4 11.06        <NA>
    ## 4641 Cold Brew Vanilla Sweet Cream St     4 11.06        <NA>
    ## 4642       Lipton Pure Leaf Sweet Tea     6 11.06        <NA>
    ## 4643 Juice Orange Premium Topicana 11     5 11.06        <NA>
    ## 4644 Juice Raspberry Lemonade Tropica     5 11.06        <NA>
    ## 4645 Tea Green Peach Mango Celsius 12     4 11.06        <NA>
    ## 4646    Naked Strawberry Banana Juice     3 11.06        <NA>
    ## 4647  Kombucha Pineapple Peach Kevita     3 11.06        <NA>
    ## 4648 Mountain Dew Kickstart Orange 16     4 11.06        <NA>
    ## 4649 Tea Blackberry Pure Leaf 18.5 oz     5 11.06        <NA>
    ## 4650 Tea Unsweet Green Lipton 18.5 oz     5 11.06        <NA>
    ## 4651 Gatorade Gatorlyte Glacier Freez     3 11.06        <NA>
    ## 4652 Gatorade Gatorlyte Strawberry Ki     3 11.06        <NA>
    ## 4653 Gatorade Gatorlyte Zero Fruit Pu     3 11.06        <NA>
    ## 4654   Yerba Mate Revel Berry 15.5 oz     3 11.06        <NA>
    ## 4655 Tea Green Matcha Milk Ito En 11.     3 11.06        <NA>
    ## 4656    Water Aquafina Alumitek 16 oz     6 11.06        <NA>
    ## 4657       Juice AppleTropicana 11 oz     4 11.06        <NA>
    ## 4658  Energy Blue Crush Celsius 16 oz     3 11.06        <NA>
    ## 4659 Juice Joes Lemonade  Red Jacket1     3 11.06        <NA>
    ## 4660 Gatorade Propel Berry Water 1 lt     4 11.06        <NA>
    ## 4661 Gatorade Propel Grape Water 1 lt     4 11.06        <NA>
    ## 4662 Energy Blue Raz Lemonade Celsius     3 11.06        <NA>
    ## 4663  Energy Fast Twitch Cool Blue 12     3 11.06        <NA>
    ## 4664 Energy Fast Twitch Watermelon St     3 11.06        <NA>
    ## 4665   Energy StrawGuava Celsius 12oz     3 11.06        <NA>
    ## 4666 Muscle Milk PROSRS 40 Intense Va     2 11.06        <NA>
    ## 4667  Soda Ginger Ale Schweppes 20 Oz     4 11.06        <NA>
    ## 4668          Soda Rootbeer Mug 20 oz     4 11.06        <NA>
    ## 4669 Tea Pure Leaf Zero Sugar Sweet T     4 11.06        <NA>
    ## 4670 Tea Tea & Lemon Pure Leaf Lipton     4 11.06        <NA>
    ## 4671 Tea Unsweet Black W Lemon 18.5 o     4 11.06        <NA>
    ## 4672 Starbucks Doubleshot Energy Moch     2 11.06        <NA>
    ## 4673         Juice Apple Dole 15.2 oz     4 11.06        <NA>
    ## 4674 Juice Ocean Spray Cranbe Grape 1     4 11.06        <NA>
    ## 4675   Kombucha Ginger Kevita 15.2 oz     2 11.06        <NA>
    ## 4676              Gatorade Blue 28 oz     3 11.06        <NA>
    ## 4677        Gatorade Lemon Lime 28 oz     3 11.06        <NA>
    ## 4678    Glacier Cherry Gatorade 28 oz     3 11.06        <NA>
    ## 4679 Sparkling lemonade Strawberry Ke     3 11.06        <NA>
    ## 4680             Water Gatorade 700ml     3 11.06        <NA>
    ## 4681      Soda Mango Jarritos 12.5 oz     3 11.06        <NA>
    ## 4682 Energy Cherry Limeade Celsius 16     2 11.06        <NA>
    ## 4683 Energy Passion Orange Guva Odyss     2 11.06        <NA>
    ## 4684          Soda Mountain Dew 20 Oz     3 11.06        <NA>
    ## 4685          Soda Orange Crush 20 Oz     3 11.06        <NA>
    ## 4686     Soda Pepsi Wild Cherry 20 Oz     3 11.06        <NA>
    ## 4687  Soda Rootbeer Zero Sugar Mug 20     3 11.06        <NA>
    ## 4688     Soda Starry Lemon Lime 20 oz     3 11.06        <NA>
    ## 4689   Tea Iced Rasberry Lipton 16 oz     3 11.06        <NA>
    ## 4690    Rockstar Pure Zero Silver Ice     2 11.06        <NA>
    ## 4691   Ocean Spray Cranberry Cocktail     3 11.06        <NA>
    ## 4692       Gatorade Fruit Punch 28 oz     2 11.06        <NA>
    ## 4693             Gatorade Orange 28oz     2 11.06        <NA>
    ## 4694    Glacier Freeze Gatorade 28 oz     2 11.06        <NA>
    ## 4695 Juice Zero Summer Splash Punch 1     2 11.06        <NA>
    ## 4696  Sparkling Lemonade Mango Kevita     2 11.06        <NA>
    ## 4697      Soda Guava Jarritos 12.5 oz     2 11.06        <NA>
    ## 4698  Soda Pineapple Jarritos 12.5 oz     2 11.06        <NA>
    ## 4699    Muscle Milk KO Chocolate 14oz     1 11.06        <NA>
    ## 4700 Muscle Milk P40 Strawberry Cream     1 11.06        <NA>
    ## 4701     Tea Light Peach Lipton 20 oz     2 11.06        <NA>
    ## 4702  Tea Peach Pure Leaf Lipton 18.5     2 11.06        <NA>
    ## 4703 Tea Unsweetened Pure Leaf Lipton     2 11.06        <NA>
    ## 4704     Frappuccino Mocha 13.7 Oz SB     1 11.06        <NA>
    ## 4705         Juice Naked Blue Machine     1 11.06        <NA>
    ## 4706         Juice Naked Mighty Mango     1 11.06        <NA>
    ## 4707       Juice Naked PNCLDA 15.2 Oz     1 11.06        <NA>
    ## 4708   Soda Mandarin Jarritos 12.5 oz     2 11.06        <NA>
    ## 4709 Gatorade Gatorlyte Cherry Lime 2     1 11.06        <NA>
    ## 4710 Gatorade Gatorlyte MixBerry 20 o     1 11.06        <NA>
    ## 4711  Gatorade Gatorlyte Orange 20 oz     1 11.06        <NA>
    ## 4712    Tea Black Milk Ito En 11.8 oz     1 11.06        <NA>
    ## 4713 Energy 222 Pineapple Mango Odyss     1 11.06        <NA>
    ## 4714 Energy Dragonberry Celsius 16 oz     1 11.06        <NA>
    ## 4715 Energy Fruit Burst Celsius 16 oz     1 11.06        <NA>
    ## 4716 Energy Mango Tango Celsius 16 oz     1 11.06        <NA>
    ## 4717 Energy Orangesicle Celsius 16 oz     1 11.06        <NA>
    ## 4718 Energy Revive Prickly Pear Odyss     1 11.06        <NA>
    ## 4719                 Cornell Lemonade     2 11.06        <NA>
    ## 4720   Rockstar Strawberry Punch 16oz     1 11.06        <NA>
    ## 4721  Tea Green Rasp Acai  Celsius 12     1 11.06        <NA>
    ## 4722           Water Gatorade 33.8 oz     1 11.06        <NA>
    ## 4723     Cider Apple Red Jacket 12 oz     1 11.06        <NA>
    ## 4724 Gatorade Galcier Freeze Zero G 2     1 11.06        <NA>
    ## 4725       Juice Grape Tropicana 11oz     1 11.06        <NA>
    ## 4726  Juice Lively Lemonade Tropicana     1 11.06        <NA>
    ## 4727 Kickstart Strawberry Start-up 16     1 11.06        <NA>
    ## 4728     Soda Mountain Dew Zero 20 Oz     1 11.06        <NA>
    ## 4729 Tea Sweetened With Lemon Brisk 2     1 11.06        <NA>
    ## 4730         Juice Lemonade Dole 20oz     1 11.06        <NA>
    ## 4731        Juice Orange Dole 15.2 oz     1 11.06        <NA>
    ## 4732   Cornell Low Fat Chocolate Milk     7 11.06        <NA>
    ## 4733  Yogurt Strawberry Greek Chobani     8 11.06        <NA>
    ## 4734 Yogurt Mixed Berry Chobani Drink     4 11.06        <NA>
    ## 4735 Yogurt Straw Banana Chobani Drin     4 11.06        <NA>
    ## 4736 Yogurt 0% Fat Vanilla Greek Oiko     5 11.06        <NA>
    ## 4737       Cornell 2% Milk (70000380)     4 11.06        <NA>
    ## 4738   Milk Chocolate LF Cornell 8 Oz     6 11.06        <NA>
    ## 4739       Cornell 2% Milk (70000381)     5 11.06        <NA>
    ## 4740               Cornell Whole Milk     4 11.06        <NA>
    ## 4741 Yogurt Black Cherry Greek Choban     3 11.06        <NA>
    ## 4742       Yogurt Plain Greek Chobani     3 11.06        <NA>
    ## 4743 Yogurt Flip Almond Coco Loco Cho     2 11.06        <NA>
    ## 4744  Yogurt Mango Chobani Drink 7 oz     1 11.06        <NA>
    ## 4745  Yogurt Flip Peanut Butter Dream     1 11.06        <NA>
    ## 4746   Yogurt Blueberry Greek Chobani     1 11.06        <NA>
    ## 4747       Yogurt Peach Greek Chobani     1 11.06        <NA>
    ## 4748            8 oz Vanilla Soy Silk     1 11.06        <NA>
    ## 4749          Silk Chocolate Soy Milk     1 11.06        <NA>
    ## 4750 Bar Choc Chip Cookie Dough Quest     2 11.06        <NA>
    ## 4751 Jelly Konjac Apple Grape Tastell     2 11.06        <NA>
    ## 4752 Jelly Konjac Mango Pineapple Tas     2 11.06        <NA>
    ## 4753  Bar Frosted Birthday Cake Quest     1 11.06        <NA>
    ## 4754        Bar S'Mores Quest 2.12 oz     1 11.06        <NA>
    ## 4755  Cookie Choc Chip Lenny & Larrys     1 11.06        <NA>
    ## 4756 Cookie Double Choc Lenny & Larry     1 11.06        <NA>
    ## 4757  Bar Peanut Butter Builders Clif     1 11.06        <NA>
    ## 4758   Fruit And Nut Delight Kind Bar     1 11.06        <NA>
    ## 4759 Nature Valley Peanut Butter Gran     2 11.06        <NA>
    ## 4760 Jelly Konjac Double Berry Tastel     1 11.06        <NA>
    ## 4761      Jelly Konjac Peach Tastelli     1 11.06        <NA>
    ## 4762      Kind Almond And Coconut Bar     1 11.06        <NA>
    ## 4763 Bar That's It Apple+Strawberry 1     1 11.06        <NA>
    ## 4764           Chips Dirty Sea Salted     4 11.06        <NA>
    ## 4765                 Utz Regular Chip     3 11.06        <NA>
    ## 4766 Chip Potato Dirty BBQ Mesquite 2     2 11.06        <NA>
    ## 4767 Chip Salt & Malt Vinegar Dirty 2     2 11.06        <NA>
    ## 4768  Chips Cracked Pepper & Sea Salt     2 11.06        <NA>
    ## 4769       Jalapeno Heat Chips Kosher     2 11.06        <NA>
    ## 4770   Utz Honey Barbecue Chip 1.5 oz     2 11.06        <NA>
    ## 4771 Chip Potato Kettle Honey BBQ 2oz     1 11.06        <NA>
    ## 4772       Chip Maui Onion Dirty 2 oz     1 11.06        <NA>
    ## 4773  Chip Sour Cream And Onion Dirty     1 11.06        <NA>
    ## 4774       Utz GoodHealth Veggie Chip     1 11.06        <NA>
    ## 4775          Utz Salt N Vinegar Chip     1 11.06        <NA>
    ## 4776                      Fruit Whole    42 11.06        <NA>
    ## 4777   Fresh Cut Watermelon Fruit Cup     7 11.06        <NA>
    ## 4778        Fresh Cut Melon Fruit Cup     1 11.06        <NA>
    ## 4779    Fresh Cut Pineapple Fruit Cup     1 11.06        <NA>
    ## 4780      Chewy Marshmallow GF 2.1 oz     5 11.06        <NA>
    ## 4781 GF Sweet Street Choloate Brownie     1 11.06        <NA>
    ## 4782             Orbit Sweet Mint Gum     3 11.06        <NA>
    ## 4783   Candy CHOC Cara Caff Bar Awake     1 11.06        <NA>
    ## 4784 Candy Dark Chocolate Awake Bar 1     1 11.06        <NA>
    ## 4785             Orbit Wintermint Gum     1 11.06        <NA>
    ## 4786  Ice Cream Mochi Sweet Mango 1.5     2 11.06        <NA>
    ## 4787                 MochiCookCrm1.5o     2 11.06        <NA>
    ## 4788 Ice Cream Mochi Double Chocolate     1 11.06        <NA>
    ## 4789                  California Roll     4 11.06 grab 'n' go
    ## 4790               Golden Dragon Roll     3 11.06 grab 'n' go
    ## 4791                     Alaskan Roll     2 11.06 grab 'n' go
    ## 4792              Tempura Shrimp Roll     2 11.06 grab 'n' go
    ## 4793                         TSA Roll     2 11.06 grab 'n' go
    ## 4794                      Salmon Roll     2 11.06 grab 'n' go
    ## 4795                  Spicy Tuna Roll     2 11.06 grab 'n' go
    ## 4796            Hawaiian Volcano Roll     1 11.06 grab 'n' go
    ## 4797              Tempura Crunch Roll     1 11.06 grab 'n' go
    ## 4798                  Hawaiian Sunset     1 11.06 grab 'n' go
    ## 4799                     Avocado Roll     1 11.06 grab 'n' go
    ## 4800              Wrap Chicken Caesar     3 11.06 grab 'n' go
    ## 4801             Wrap Buffalo Chicken     2 11.06 grab 'n' go
    ## 4802     Sandwich Corned Beef & Swiss     1 11.06 grab 'n' go
    ## 4803 Sandwich Crispy Chicken Milanese     1 11.06 grab 'n' go
    ## 4804 Sandwich Prosciutto & Mozzarella     1 11.06 grab 'n' go
    ## 4805             Salad Chicken Caesar     1 11.06 grab 'n' go
    ## 4806                     Muffin Jumbo    27 11.06        <NA>
    ## 4807                           Cookie    28 11.06        <NA>
    ## 4808                     Cinnamon Bun     5 11.06        <NA>
    ## 4809                   Croissant Choc     4 11.06        <NA>
    ## 4810               Croissant Straw CC     4 11.06        <NA>
    ## 4811                            Scone     3 11.06        <NA>
    ## 4812                Croissant Blue CC     2 11.06        <NA>
    ## 4813                      Coffee Cake     1 11.06        <NA>
    ## 4814                   BowlMedProtein     4 11.06 grab 'n' go
    ## 4815                 BowlMexicanChick     2 11.06 grab 'n' go
    ## 4816                 BowlSesAsianNood     2 11.06 grab 'n' go
    ## 4817                 BowlChickAlfrPen     1 11.06 grab 'n' go
    ## 4818                   BowlSouthChick     1 11.06 grab 'n' go
    ## 4819                     PBJ on Wheat     9 11.06 grab 'n' go
    ## 4820                   GF Turkey Sand     2 11.06 grab 'n' go
    ## 4821                 GF ChicCaesarSld     1 11.06 grab 'n' go
    ## 4822                 GFSunButterJelly     1 11.06 grab 'n' go
    ## 4823       Quesadilla Deluxe Trillium   165 11.05     mexican
    ## 4824                Grilled Hamburger   107 11.05       grill
    ## 4825            Fried Chicken Tenders   105 11.05       grill
    ## 4826    Burrito Una Mano Trillium BYO    63 11.05     mexican
    ## 4827                     French Fries   136 11.05       grill
    ## 4828 Trillium Grill Impossible Burger    14 11.05       grill
    ## 4829  Grilled Chicken Breast Sandwich    11 11.05       grill
    ## 4830                Quesadilla Cheese     8 11.05     mexican
    ## 4831             Seared Salmon Burger     4 11.05       grill
    ## 4832               Sweet Potato Fries    11 11.05       grill
    ## 4833             ADD Beef Patty $2.99     9 11.05       grill
    ## 4834                Black Bean Burger     2 11.05       grill
    ## 4835               ADD Chicken Breast     4 11.05       grill
    ## 4836              Add Sausage 2 Patty     3 11.05       grill
    ## 4837                       ADD Cheese     2 11.05       grill
    ## 4838                     Add Egg $.99     1 11.05       grill
    ## 4839                1 Entree + 1 Side   212 11.05         wok
    ## 4840                1 Entree + 2 Side    79 11.05         wok
    ## 4841               Bowl Ramen Chicken    73 11.05       ramen
    ## 4842              2 Entrees + 2 Sides    27 11.05         wok
    ## 4843                  Bowl Ramen Tofu    11 11.05       ramen
    ## 4844          Side Vegetarian Lo Mein    10 11.05         wok
    ## 4845                     1 Wok Entree     5 11.05         wok
    ## 4846                  Side Vegetables     1 11.05         wok
    ## 4847                Burrito Breakfast    83 11.05        <NA>
    ## 4848              Small French Omelet    52 11.05        <NA>
    ## 4849 Egg Cheese Bacon Breakfast Sandw    37 11.05        <NA>
    ## 4850 Egg Cheese Sausage Breakfast San    33 11.05        <NA>
    ## 4851             Grand Slam Breakfast    15 11.05        <NA>
    ## 4852                        Add Bacon    29 11.05        <NA>
    ## 4853                         Two Eggs    18 11.05        <NA>
    ## 4854                   Pancake Single     5 11.05        <NA>
    ## 4855                   2 Slices Toast     3 11.05        <NA>
    ## 4856                            Toast     1 11.05        <NA>
    ## 4857      Create Your Pasta Bowl MEAT   103 11.05     italian
    ## 4858              Pizza with Toppings    38 11.05     italian
    ## 4859       Create Your Pasta Bowl VEG    20 11.05     italian
    ## 4860                     Pizza Cheese    20 11.05     italian
    ## 4861                   Add Extra Meat     6 11.05     italian
    ## 4862         Side Bread Pasta Station     1 11.05     italian
    ## 4863                 Burrito Bowl BYO   109 11.05     mexican
    ## 4864                      Single Taco     4 11.05     mexican
    ## 4865      Add Extra Toppings Una Mano     6 11.05     mexican
    ## 4866                   Side Guacamole     1 11.05     mexican
    ## 4867                       Side Salsa     1 11.05     mexican
    ## 4868               Salad by the Pound    55 11.05   salad bar
    ## 4869                       Soup 12 oz    37 11.05   salad bar
    ## 4870                        8 oz Soup    30 11.05   salad bar
    ## 4871              Soda Fountain 24 oz    31 11.05        <NA>
    ## 4872              Soda Fountain 16 oz    33 11.05        <NA>
    ## 4873                  Coffee 16 oz SB    24 11.05        <NA>
    ## 4874                  Coffee 12 oz SB    14 11.05        <NA>
    ## 4875                    Hot Tea 20 oz     3 11.05        <NA>
    ## 4876                 Side Potato Tots    22 11.05        <NA>
    ## 4877               Open Miscellaneous     9 11.05        <NA>
    ## 4878          Add Extra Protein $2.99     2 11.05        <NA>
    ## 4879            Soda Pepsi Diet 20 Oz    23 11.05        <NA>
    ## 4880             Water Aquafina 20 oz    23 11.05        <NA>
    ## 4881 Tea Jasmine Green Unsweet Ito En    12 11.05        <NA>
    ## 4882                Water Aquafina 1L    17 11.05        <NA>
    ## 4883    Yerba Mate Bluephoria 15.5 oz    10 11.05        <NA>
    ## 4884         Yerba Mate Lemon 15.5 oz     8 11.05        <NA>
    ## 4885    Naked Strawberry Banana Juice     7 11.05        <NA>
    ## 4886 Yerba Mate Enlighten Mint 15.5 o     7 11.05        <NA>
    ## 4887   Energy StrawGuava Celsius 12oz     8 11.05        <NA>
    ## 4888     Water Life WTR Immune 700 ML     8 11.05        <NA>
    ## 4889      Yerba Mate Tropical 15.5 oz     6 11.05        <NA>
    ## 4890       Juice AppleTropicana 11 oz     7 11.05        <NA>
    ## 4891    Tea Black Milk Ito En 11.8 oz     5 11.05        <NA>
    ## 4892  Starbucks Doubleshot Energy Van     4 11.05        <NA>
    ## 4893          Juice Naked Berry Blast     4 11.05        <NA>
    ## 4894 Gatorade Galcier Freeze Zero G 2     6 11.05        <NA>
    ## 4895   Energy Artic Vibe Celsius 12oz     5 11.05        <NA>
    ## 4896     Soda Mountain Dew Zero 20 Oz     7 11.05        <NA>
    ## 4897 Cold Brew Vanilla Sweet Cream St     4 11.05        <NA>
    ## 4898     Soda Pepsi Wild Cherry 20 Oz     6 11.05        <NA>
    ## 4899            Soda Pepsi  Zero 20oz     6 11.05        <NA>
    ## 4900   Tea Iced Rasberry Lipton 16 oz     6 11.05        <NA>
    ## 4901  Tea Peach Pure Leaf Lipton 18.5     6 11.05        <NA>
    ## 4902 Juice Orange Homestyle Tropicana     5 11.05        <NA>
    ## 4903 Gatorade Propel  Kiwi Straw Wate     5 11.05        <NA>
    ## 4904 Tea Green Peach Mango Celsius 12     4 11.05        <NA>
    ## 4905        Juice Naked Green Machine     3 11.05        <NA>
    ## 4906         Juice Naked Mighty Mango     3 11.05        <NA>
    ## 4907         Juice Protein Zone Naked     3 11.05        <NA>
    ## 4908       Lipton Pure Leaf Sweet Tea     5 11.05        <NA>
    ## 4909          Soda Orange Crush 20 Oz     5 11.05        <NA>
    ## 4910          Soda Rootbeer Mug 20 oz     5 11.05        <NA>
    ## 4911  Soda Rootbeer Zero Sugar Mug 20     5 11.05        <NA>
    ## 4912 Tea Sweet W/ Le Pure Leaf Lipton     5 11.05        <NA>
    ## 4913 Tea Tea & Lemon Pure Leaf Lipton     5 11.05        <NA>
    ## 4914 Gatorade Gatorlyte Glacier Freez     3 11.05        <NA>
    ## 4915     Starbucks Double Shot 6.5 Oz     3 11.05        <NA>
    ## 4916    Water Aquafina Alumitek 16 oz     6 11.05        <NA>
    ## 4917  Energy Blue Crush Celsius 16 oz     3 11.05        <NA>
    ## 4918 Energy Passion Orange Guva Odyss     3 11.05        <NA>
    ## 4919 Energy Fast Twitch Watermelon St     3 11.05        <NA>
    ## 4920 Energy Mango PassFruit Celsius 1     3 11.05        <NA>
    ## 4921                 Soda Pepsi 20 Oz     4 11.05        <NA>
    ## 4922 Tea Sweetened With Lemon Brisk 2     4 11.05        <NA>
    ## 4923 Tea Unsweet Black W Lemon 18.5 o     4 11.05        <NA>
    ## 4924   Frappuccino Caramel SB 13.7 oz     2 11.05        <NA>
    ## 4925   Frappuccino Vanilla SB 13.7 Oz     2 11.05        <NA>
    ## 4926       Juice Naked PNCLDA 15.2 Oz     2 11.05        <NA>
    ## 4927   Kombucha Ginger Kevita 15.2 oz     2 11.05        <NA>
    ## 4928  Kombucha Pineapple Peach Kevita     2 11.05        <NA>
    ## 4929             Gatorade Orange 28oz     3 11.05        <NA>
    ## 4930 Juice Raspberry Lemonade Tropica     3 11.05        <NA>
    ## 4931 Juice Zero Summer Splash Punch 1     3 11.05        <NA>
    ## 4932 Gatorade Propel Grape Water 1 lt     3 11.05        <NA>
    ## 4933 Gatorade Gatorlyte Strawberry Ki     2 11.05        <NA>
    ## 4934 Gatorade Gatorlyte Zero Lemon Li     2 11.05        <NA>
    ## 4935   Yerba Mate Revel Berry 15.5 oz     2 11.05        <NA>
    ## 4936 Tea Golden Oolong Unsweet Ito En     2 11.05        <NA>
    ## 4937      Soda Guava Jarritos 12.5 oz     3 11.05        <NA>
    ## 4938      Soda Mango Jarritos 12.5 oz     3 11.05        <NA>
    ## 4939 Juice Fuji Apple Red Jacket 12oz     2 11.05        <NA>
    ## 4940  Soda Ginger Ale Schweppes 20 Oz     3 11.05        <NA>
    ## 4941          Soda Mountain Dew 20 Oz     3 11.05        <NA>
    ## 4942 Tea Unsweet Green Lipton 18.5 oz     3 11.05        <NA>
    ## 4943         Juice Lemonade Dole 20oz     3 11.05        <NA>
    ## 4944   Ocean Spray Cranberry Cocktail     3 11.05        <NA>
    ## 4945   Energy StrawLemon Celsius 12oz     2 11.05        <NA>
    ## 4946   Soda Mandarin Jarritos 12.5 oz     3 11.05        <NA>
    ## 4947     Cider Apple Red Jacket 12 oz     2 11.05        <NA>
    ## 4948      Gatorade Berry Zero G 28 oz     2 11.05        <NA>
    ## 4949              Gatorade Blue 28 oz     2 11.05        <NA>
    ## 4950       Gatorade Fruit Punch 28 oz     2 11.05        <NA>
    ## 4951        Gatorade Lemon Lime 28 oz     2 11.05        <NA>
    ## 4952        Gatorade Zero Grape 28 oz     2 11.05        <NA>
    ## 4953    Glacier Freeze Gatorade 28 oz     2 11.05        <NA>
    ## 4954       Juice Grape Tropicana 11oz     2 11.05        <NA>
    ## 4955  Juice Lively Lemonade Tropicana     2 11.05        <NA>
    ## 4956 Juice Orange Premium Topicana 11     2 11.05        <NA>
    ## 4957 Kickstart Strawberry Start-up 16     2 11.05        <NA>
    ## 4958 Gatorade Propel Berry Water 1 lt     2 11.05        <NA>
    ## 4959 Sparkling lemonade Strawberry Ke     2 11.05        <NA>
    ## 4960             Water Gatorade 700ml     2 11.05        <NA>
    ## 4961   Soda Tamarind Jarritos 12.5 oz     2 11.05        <NA>
    ## 4962    Muscle Milk KO Chocolate 14oz     1 11.05        <NA>
    ## 4963 Muscle Milk PROSRS 40 Intense Va     1 11.05        <NA>
    ## 4964     Soda Starry Lemon Lime 20 oz     2 11.05        <NA>
    ## 4965 Tea Blackberry Pure Leaf 18.5 oz     2 11.05        <NA>
    ## 4966     Tea Light Peach Lipton 20 oz     2 11.05        <NA>
    ## 4967 Tea Unsweetened Pure Leaf Lipton     2 11.05        <NA>
    ## 4968 Starbucks Doubleshot Ener Coffee     1 11.05        <NA>
    ## 4969 Starbucks Doubleshot Energy Moch     1 11.05        <NA>
    ## 4970    Frappuccino Coffee 13.7 oz SB     1 11.05        <NA>
    ## 4971     Frappuccino Mocha 13.7 Oz SB     1 11.05        <NA>
    ## 4972  Kombucha Rasp Lemon Kevita 15.2     1 11.05        <NA>
    ## 4973  Energy Triple Shot Dark Caramel     1 11.05        <NA>
    ## 4974 Gatorade Gatorlyte Cherry Lime 2     1 11.05        <NA>
    ## 4975 Gatorade Gatorlyte Zero Fruit Pu     1 11.05        <NA>
    ## 4976 Tea Green Matcha Milk Ito En 11.     1 11.05        <NA>
    ## 4977 Cold Brew Chocolate Cream Starbu     1 11.05        <NA>
    ## 4978 Energy 222 Pineapple Mango Odyss     1 11.05        <NA>
    ## 4979 Energy Dragonberry Celsius 16 oz     1 11.05        <NA>
    ## 4980 Energy Mango Tango Celsius 16 oz     1 11.05        <NA>
    ## 4981 Energy Orangesicle Celsius 16 oz     1 11.05        <NA>
    ## 4982 Energy Revive Prickly Pear Odyss     1 11.05        <NA>
    ## 4983   Rockstar Strawberry Punch 16oz     1 11.05        <NA>
    ## 4984 Energy Blue Raz Lemonade Celsius     1 11.05        <NA>
    ## 4985   Energy Fast Twitch Grape 12 oz     1 11.05        <NA>
    ## 4986 Energy Fuji Apple Pear Celsius 1     1 11.05        <NA>
    ## 4987  Tea Green Rasp Acai  Celsius 12     1 11.05        <NA>
    ## 4988 Mountain Dew Kickstart Black Che     1 11.05        <NA>
    ## 4989 Mountain Dew Kickstart Orange 16     1 11.05        <NA>
    ## 4990           Water Gatorade 33.8 oz     1 11.05        <NA>
    ## 4991    Glacier Cherry Gatorade 28 oz     1 11.05        <NA>
    ## 4992  Sparkling Lemonade Mango Kevita     1 11.05        <NA>
    ## 4993  Water Life WTR PH Balance 20 oz     1 11.05        <NA>
    ## 4994 Tea Pure Leaf Zero Sugar Sweet T     1 11.05        <NA>
    ## 4995         Juice Apple Dole 15.2 oz     1 11.05        <NA>
    ## 4996 Juice Ocean Spray Cranbe Grape 1     1 11.05        <NA>
    ## 4997                 Cornell Lemonade     1 11.05        <NA>
    ## 4998 Yogurt Straw Banana Chobani Drin     5 11.05        <NA>
    ## 4999 Yogurt 0% Fat Vanilla Greek Oiko     6 11.05        <NA>
    ## 5000   Cornell Low Fat Chocolate Milk     5 11.05        <NA>
    ## 5001  Yogurt Strawberry Greek Chobani     6 11.05        <NA>
    ## 5002   Milk Chocolate LF Cornell 8 Oz     7 11.05        <NA>
    ## 5003 Yogurt Mixed Berry Chobani Drink     3 11.05        <NA>
    ## 5004               Cornell Whole Milk     6 11.05        <NA>
    ## 5005       Cornell 2% Milk (70000380)     3 11.05        <NA>
    ## 5006       Yogurt Mango Greek Chobani     4 11.05        <NA>
    ## 5007 Yogurt Flip Almond Coco Loco Cho     3 11.05        <NA>
    ## 5008  Yogurt Flip Peanut Butter Dream     3 11.05        <NA>
    ## 5009       Cornell 2% Milk (70000381)     3 11.05        <NA>
    ## 5010       Yogurt Peach Greek Chobani     2 11.05        <NA>
    ## 5011  Yogurt Mango Chobani Drink 7 oz     1 11.05        <NA>
    ## 5012 Yogurt Black Cherry Greek Choban     1 11.05        <NA>
    ## 5013       Yogurt Plain Greek Chobani     1 11.05        <NA>
    ## 5014  Cornell Dairy Strawberry Yogurt     1 11.05        <NA>
    ## 5015   Fresh Cut Watermelon Fruit Cup    14 11.05        <NA>
    ## 5016        Fresh Cut Melon Fruit Cup     6 11.05        <NA>
    ## 5017    Fresh Cut Pineapple Fruit Cup     2 11.05        <NA>
    ## 5018 Bar Choc Chip Cookie Dough Quest     4 11.05        <NA>
    ## 5019 Jelly Konjac Mango Pineapple Tas     3 11.05        <NA>
    ## 5020 Bar CHOC Mint Builders Clif bar2     2 11.05        <NA>
    ## 5021 Jelly Konjac Apple Grape Tastell     2 11.05        <NA>
    ## 5022 Jelly Konjac Double Berry Tastel     2 11.05        <NA>
    ## 5023 Bar Cookie & Cream Quest 2.12 oz     1 11.05        <NA>
    ## 5024  Bar Frosted Birthday Cake Quest     1 11.05        <NA>
    ## 5025        Bar S'Mores Quest 2.12 oz     1 11.05        <NA>
    ## 5026  Bar Peanut Butter Builders Clif     1 11.05        <NA>
    ## 5027      Jelly Konjac Peach Tastelli     1 11.05        <NA>
    ## 5028       Bar Chocolate Chip Clifbar     1 11.05        <NA>
    ## 5029 Bar That's It Apple+Strawberry 1     1 11.05        <NA>
    ## 5030  Oats And Dark Chocolate Granola     1 11.05        <NA>
    ## 5031 Chip Salt & Malt Vinegar Dirty 2     5 11.05        <NA>
    ## 5032 Chip Potato Dirty BBQ Mesquite 2     3 11.05        <NA>
    ## 5033           Chips Dirty Sea Salted     2 11.05        <NA>
    ## 5034       Jalapeno Heat Chips Kosher     2 11.05        <NA>
    ## 5035                 Utz Regular Chip     2 11.05        <NA>
    ## 5036          Utz Salt N Vinegar Chip     2 11.05        <NA>
    ## 5037 Nuts Berry Almond Macaroon 1.5 o     1 11.05        <NA>
    ## 5038 Chip Potato Kettle Honey BBQ 2oz     1 11.05        <NA>
    ## 5039     Chip Funky Fusion Dirty 2 oz     1 11.05        <NA>
    ## 5040  Chip Sour Cream And Onion Dirty     1 11.05        <NA>
    ## 5041                      Fruit Whole    36 11.05        <NA>
    ## 5042             Orbit Sweet Mint Gum     5 11.05        <NA>
    ## 5043 Candy Milk Choc Caff Bar Awake 1     3 11.05        <NA>
    ## 5044   Candy CHOC Cara Caff Bar Awake     1 11.05        <NA>
    ## 5045             Orbit Wintermint Gum     1 11.05        <NA>
    ## 5046  Ice Cream Mochi Sweet Mango 1.5     5 11.05        <NA>
    ## 5047 Ice Cream Mochi Double Chocolate     4 11.05        <NA>
    ## 5048                 MochiCookCrm1.5o     1 11.05        <NA>
    ## 5049      Chewy Marshmallow GF 2.1 oz     4 11.05        <NA>
    ## 5050 GF Sweet Street Choloate Brownie     2 11.05        <NA>
    ## 5051                  Spicy Tuna Roll     3 11.05 grab 'n' go
    ## 5052                  California Roll     3 11.05 grab 'n' go
    ## 5053                     Alaskan Roll     2 11.05 grab 'n' go
    ## 5054               Golden Dragon Roll     2 11.05 grab 'n' go
    ## 5055                      Salmon Roll     2 11.05 grab 'n' go
    ## 5056            Hawaiian Volcano Roll     1 11.05 grab 'n' go
    ## 5057              Tempura Crunch Roll     1 11.05 grab 'n' go
    ## 5058              Tempura Shrimp Roll     1 11.05 grab 'n' go
    ## 5059                  Hawaiian Sunset     1 11.05 grab 'n' go
    ## 5060                     Avocado Roll     1 11.05 grab 'n' go
    ## 5061              Wrap Chicken Caesar     3 11.05 grab 'n' go
    ## 5062     Sandwich Corned Beef & Swiss     2 11.05 grab 'n' go
    ## 5063 Sandwich Prosciutto & Mozzarella     2 11.05 grab 'n' go
    ## 5064             Wrap Buffalo Chicken     2 11.05 grab 'n' go
    ## 5065 Sandwich Black Forrest Ham & Swi     1 11.05 grab 'n' go
    ## 5066 Sandwich Crispy Chicken Milanese     1 11.05 grab 'n' go
    ## 5067             Salad Chicken Caesar     1 11.05 grab 'n' go
    ## 5068                           Cookie    25 11.05        <NA>
    ## 5069                     Muffin Jumbo    13 11.05        <NA>
    ## 5070                   Croissant Choc     4 11.05        <NA>
    ## 5071               Croissant Straw CC     4 11.05        <NA>
    ## 5072                Croissant Blue CC     3 11.05        <NA>
    ## 5073                     Cinnamon Bun     3 11.05        <NA>
    ## 5074      Combo 16 oz Coffee & Muffin     2 11.05        <NA>
    ## 5075                            Scone     1 11.05        <NA>
    ## 5076                      Coffee Cake     1 11.05        <NA>
    ## 5077                 BowlChickAlfrPen     2 11.05 grab 'n' go
    ## 5078                 BowlMexicanChick     2 11.05 grab 'n' go
    ## 5079                 BowlSesAsianNood     1 11.05 grab 'n' go
    ## 5080                   BowlSouthChick     1 11.05 grab 'n' go
    ## 5081                 GF ChicCaesarSld     3 11.05 grab 'n' go
    ## 5082                   GF Turkey Sand     1 11.05 grab 'n' go
    ## 5083                 GFSunButterJelly     1 11.05 grab 'n' go
    ## 5084                     PBJ on Wheat     3 11.05 grab 'n' go
    ## 5085       Quesadilla Deluxe Trillium   170 11.04     mexican
    ## 5086                Grilled Hamburger    80 11.04       grill
    ## 5087            Fried Chicken Tenders    95 11.04       grill
    ## 5088    Burrito Una Mano Trillium BYO    71 11.04     mexican
    ## 5089                     French Fries   176 11.04       grill
    ## 5090                Quesadilla Cheese    15 11.04     mexican
    ## 5091  Grilled Chicken Breast Sandwich    12 11.04       grill
    ## 5092             Seared Salmon Burger    11 11.04       grill
    ## 5093 Trillium Grill Impossible Burger     7 11.04       grill
    ## 5094             ADD Beef Patty $2.99    13 11.04       grill
    ## 5095                Black Bean Burger     4 11.04       grill
    ## 5096               ADD Chicken Breast     3 11.04       grill
    ## 5097                       ADD Cheese     5 11.04       grill
    ## 5098              Add Sausage 2 Patty     1 11.04       grill
    ## 5099                     Add Egg $.99     1 11.04       grill
    ## 5100                1 Entree + 1 Side   171 11.04         wok
    ## 5101                1 Entree + 2 Side    84 11.04         wok
    ## 5102               Bowl Ramen Chicken    65 11.04       ramen
    ## 5103              2 Entrees + 2 Sides    26 11.04         wok
    ## 5104                  Bowl Ramen Tofu    20 11.04       ramen
    ## 5105                     1 Wok Entree     7 11.04         wok
    ## 5106          Side Vegetarian Lo Mein    11 11.04         wok
    ## 5107  Side Vegetarian Fried Rice with     5 11.04         wok
    ## 5108                  Side Vegetables     3 11.04         wok
    ## 5109         Side White or Brown Rice     1 11.04         wok
    ## 5110                Burrito Breakfast    97 11.04        <NA>
    ## 5111              Small French Omelet    51 11.04        <NA>
    ## 5112 Egg Cheese Sausage Breakfast San    45 11.04        <NA>
    ## 5113 Egg Cheese Bacon Breakfast Sandw    42 11.04        <NA>
    ## 5114             Grand Slam Breakfast    17 11.04        <NA>
    ## 5115                        Add Bacon    28 11.04        <NA>
    ## 5116                         Two Eggs    10 11.04        <NA>
    ## 5117              Trillium Home Fries     3 11.04        <NA>
    ## 5118                   Pancake Single     3 11.04        <NA>
    ## 5119                   2 Slices Toast     2 11.04        <NA>
    ## 5120                            Toast     1 11.04        <NA>
    ## 5121      Create Your Pasta Bowl MEAT   118 11.04     italian
    ## 5122       Create Your Pasta Bowl VEG    34 11.04     italian
    ## 5123              Pizza with Toppings    27 11.04     italian
    ## 5124                     Pizza Cheese    17 11.04     italian
    ## 5125                   Add Extra Meat    27 11.04     italian
    ## 5126         Side Bread Pasta Station     1 11.04     italian
    ## 5127                 Burrito Bowl BYO    91 11.04     mexican
    ## 5128                      Single Taco    11 11.04     mexican
    ## 5129                   Side Guacamole     2 11.04     mexican
    ## 5130      Add Extra Toppings Una Mano     4 11.04     mexican
    ## 5131               Salad by the Pound    46 11.04   salad bar
    ## 5132                       Soup 12 oz    44 11.04   salad bar
    ## 5133                        8 oz Soup    32 11.04   salad bar
    ## 5134                  Coffee 16 oz SB    31 11.04        <NA>
    ## 5135              Soda Fountain 16 oz    40 11.04        <NA>
    ## 5136              Soda Fountain 24 oz    31 11.04        <NA>
    ## 5137                  Coffee 12 oz SB    23 11.04        <NA>
    ## 5138                    Hot Tea 20 oz     4 11.04        <NA>
    ## 5139               Open Miscellaneous    11 11.04        <NA>
    ## 5140                 Side Potato Tots     9 11.04        <NA>
    ## 5141             Water Aquafina 20 oz    17 11.04        <NA>
    ## 5142 Yerba Mate Peach Revival 15.5 oz     9 11.04        <NA>
    ## 5143 Yerba Mate Enlighten Mint 15.5 o     8 11.04        <NA>
    ## 5144                 Soda Pepsi 20 Oz    12 11.04        <NA>
    ## 5145            Soda Pepsi Diet 20 Oz    12 11.04        <NA>
    ## 5146 Juice Orange Premium Topicana 11    10 11.04        <NA>
    ## 5147   Energy Artic Vibe Celsius 12oz     7 11.04        <NA>
    ## 5148 Tea Jasmine Green Unsweet Ito En     6 11.04        <NA>
    ## 5149                Water Aquafina 1L     9 11.04        <NA>
    ## 5150   Frappuccino Caramel SB 13.7 oz     5 11.04        <NA>
    ## 5151            Soda Pepsi  Zero 20oz     9 11.04        <NA>
    ## 5152   Tea Iced Rasberry Lipton 16 oz     9 11.04        <NA>
    ## 5153 Energy Mango PassFruit Celsius 1     6 11.04        <NA>
    ## 5154     Water Life WTR Immune 700 ML     6 11.04        <NA>
    ## 5155    Yerba Mate Bluephoria 15.5 oz     5 11.04        <NA>
    ## 5156      Yerba Mate Tropical 15.5 oz     5 11.04        <NA>
    ## 5157 Tea Golden Oolong Unsweet Ito En     5 11.04        <NA>
    ## 5158 Energy Cherry Limeade Celsius 16     5 11.04        <NA>
    ## 5159    Naked Strawberry Banana Juice     4 11.04        <NA>
    ## 5160              Gatorade Blue 28 oz     6 11.04        <NA>
    ## 5161 Tea Sweet W/ Le Pure Leaf Lipton     7 11.04        <NA>
    ## 5162         Yerba Mate Lemon 15.5 oz     4 11.04        <NA>
    ## 5163 Muscle Milk PROSRS 40 Intense Va     3 11.04        <NA>
    ## 5164 Cold Brew Chocolate Cream Starbu     4 11.04        <NA>
    ## 5165 Juice Orange Homestyle Tropicana     5 11.04        <NA>
    ## 5166 Energy Blue Raz Lemonade Celsius     4 11.04        <NA>
    ## 5167 Energy Fuji Apple Pear Celsius 1     4 11.04        <NA>
    ## 5168 Tea Green Peach Mango Celsius 12     4 11.04        <NA>
    ## 5169         Juice Naked Mighty Mango     3 11.04        <NA>
    ## 5170   Kombucha Ginger Kevita 15.2 oz     3 11.04        <NA>
    ## 5171  Kombucha Rasp Lemon Kevita 15.2     3 11.04        <NA>
    ## 5172 Tea Pure Leaf Zero Sugar Sweet T     5 11.04        <NA>
    ## 5173 Tea Tea & Lemon Pure Leaf Lipton     5 11.04        <NA>
    ## 5174 Tea Unsweetened Pure Leaf Lipton     5 11.04        <NA>
    ## 5175 Tea Unsweet Green Lipton 18.5 oz     5 11.04        <NA>
    ## 5176 Gatorade Gatorlyte Glacier Freez     3 11.04        <NA>
    ## 5177 Gatorade Gatorlyte Zero Lemon Li     3 11.04        <NA>
    ## 5178        Gatorade Lemon Lime 28 oz     4 11.04        <NA>
    ## 5179 Juice Raspberry Lemonade Tropica     4 11.04        <NA>
    ## 5180 Gatorade Propel Berry Water 1 lt     4 11.04        <NA>
    ## 5181 Gatorade Propel Grape Water 1 lt     4 11.04        <NA>
    ## 5182 Gatorade Propel  Kiwi Straw Wate     4 11.04        <NA>
    ## 5183   Energy StrawGuava Celsius 12oz     3 11.04        <NA>
    ## 5184   Energy StrawLemon Celsius 12oz     3 11.04        <NA>
    ## 5185         Muscle Milk Choc PB 14oz     2 11.04        <NA>
    ## 5186    Muscle Milk KO Chocolate 14oz     2 11.04        <NA>
    ## 5187       Lipton Pure Leaf Sweet Tea     4 11.04        <NA>
    ## 5188  Soda Ginger Ale Schweppes 20 Oz     4 11.04        <NA>
    ## 5189          Soda Mountain Dew 20 Oz     4 11.04        <NA>
    ## 5190     Soda Pepsi Wild Cherry 20 Oz     4 11.04        <NA>
    ## 5191  Tea Peach Pure Leaf Lipton 18.5     4 11.04        <NA>
    ## 5192     Frappuccino Mocha 13.7 Oz SB     2 11.04        <NA>
    ## 5193   Frappuccino Vanilla SB 13.7 Oz     2 11.04        <NA>
    ## 5194          Juice Naked Berry Blast     2 11.04        <NA>
    ## 5195        Juice Naked Green Machine     2 11.04        <NA>
    ## 5196       Juice Naked PNCLDA 15.2 Oz     2 11.04        <NA>
    ## 5197         Juice Protein Zone Naked     2 11.04        <NA>
    ## 5198  Kombucha Pineapple Peach Kevita     2 11.04        <NA>
    ## 5199    Glacier Freeze Gatorade 28 oz     3 11.04        <NA>
    ## 5200       Juice Grape Tropicana 11oz     3 11.04        <NA>
    ## 5201  Juice Lively Lemonade Tropicana     3 11.04        <NA>
    ## 5202 Gatorade Gatorlyte MixBerry 20 o     2 11.04        <NA>
    ## 5203  Gatorade Gatorlyte Orange 20 oz     2 11.04        <NA>
    ## 5204 Gatorade Gatorlyte Zero Fruit Pu     2 11.04        <NA>
    ## 5205   Yerba Mate Revel Berry 15.5 oz     2 11.04        <NA>
    ## 5206 Energy 222 Blue Raspberry Odysse     2 11.04        <NA>
    ## 5207  Energy Blue Crush Celsius 16 oz     2 11.04        <NA>
    ## 5208 Energy Fruit Burst Celsius 16 oz     2 11.04        <NA>
    ## 5209 Energy Mango Tango Celsius 16 oz     2 11.04        <NA>
    ## 5210     Soda Mountain Dew Zero 20 Oz     3 11.04        <NA>
    ## 5211  Soda Rootbeer Zero Sugar Mug 20     3 11.04        <NA>
    ## 5212 Tea Blackberry Pure Leaf 18.5 oz     3 11.04        <NA>
    ## 5213    Rockstar Pure Zero Silver Ice     2 11.04        <NA>
    ## 5214  Tea Green Rasp Acai  Celsius 12     2 11.04        <NA>
    ## 5215     Cider Apple Red Jacket 12 oz     2 11.04        <NA>
    ## 5216      Gatorade Berry Zero G 28 oz     2 11.04        <NA>
    ## 5217 Gatorade Galcier Freeze Zero G 2     2 11.04        <NA>
    ## 5218             Gatorade Orange 28oz     2 11.04        <NA>
    ## 5219  Sparkling Lemonade Mango Kevita     2 11.04        <NA>
    ## 5220             Water Gatorade 700ml     2 11.04        <NA>
    ## 5221 Muscle Milk P40 Strawberry Cream     1 11.04        <NA>
    ## 5222          Soda Orange Crush 20 Oz     2 11.04        <NA>
    ## 5223          Soda Rootbeer Mug 20 oz     2 11.04        <NA>
    ## 5224 Tea Unsweet Black W Lemon 18.5 o     2 11.04        <NA>
    ## 5225 Starbucks Doubleshot Ener Coffee     1 11.04        <NA>
    ## 5226  Starbucks Doubleshot Energy Van     1 11.04        <NA>
    ## 5227         Juice Apple Dole 15.2 oz     2 11.04        <NA>
    ## 5228        Juice Orange Dole 15.2 oz     2 11.04        <NA>
    ## 5229   Ocean Spray Cranberry Cocktail     2 11.04        <NA>
    ## 5230         Juice Naked Blue Machine     1 11.04        <NA>
    ## 5231 Juice Ocean Spray Cranbe Grape 1     2 11.04        <NA>
    ## 5232  Energy Triple Shot Dark Caramel     1 11.04        <NA>
    ## 5233 Gatorade Gatorlyte Strawberry Ki     1 11.04        <NA>
    ## 5234     Starbucks Double Shot 6.5 Oz     1 11.04        <NA>
    ## 5235 Tea Green Matcha Milk Ito En 11.     1 11.04        <NA>
    ## 5236    Water Aquafina Alumitek 16 oz     2 11.04        <NA>
    ## 5237 Cold Brew Vanilla Sweet Cream St     1 11.04        <NA>
    ## 5238 Energy Orangesicle Celsius 16 oz     1 11.04        <NA>
    ## 5239 Energy Passion Orange Guva Odyss     1 11.04        <NA>
    ## 5240 Juice Fuji Apple Red Jacket 12oz     1 11.04        <NA>
    ## 5241  Energy Fast Twitch Cool Blue 12     1 11.04        <NA>
    ## 5242   Energy Fast Twitch Grape 12 oz     1 11.04        <NA>
    ## 5243 Mountain Dew Kickstart Black Che     1 11.04        <NA>
    ## 5244 Mountain Dew Kickstart Orange 16     1 11.04        <NA>
    ## 5245           Water Gatorade 33.8 oz     1 11.04        <NA>
    ## 5246       Gatorade Fruit Punch 28 oz     1 11.04        <NA>
    ## 5247        Gatorade Zero Grape 28 oz     1 11.04        <NA>
    ## 5248    Glacier Cherry Gatorade 28 oz     1 11.04        <NA>
    ## 5249       Juice AppleTropicana 11 oz     1 11.04        <NA>
    ## 5250 Kickstart Strawberry Start-up 16     1 11.04        <NA>
    ## 5251 Sparkling lemonade Strawberry Ke     1 11.04        <NA>
    ## 5252      Soda Guava Jarritos 12.5 oz     1 11.04        <NA>
    ## 5253  Soda Pineapple Jarritos 12.5 oz     1 11.04        <NA>
    ## 5254   Soda Tamarind Jarritos 12.5 oz     1 11.04        <NA>
    ## 5255     Tea Light Peach Lipton 20 oz     1 11.04        <NA>
    ## 5256 Tea Sweetened With Lemon Brisk 2     1 11.04        <NA>
    ## 5257         Juice Lemonade Dole 20oz     1 11.04        <NA>
    ## 5258   Soda Mandarin Jarritos 12.5 oz     1 11.04        <NA>
    ## 5259 Yogurt Mixed Berry Chobani Drink     7 11.04        <NA>
    ## 5260 Yogurt 0% Fat Vanilla Greek Oiko     9 11.04        <NA>
    ## 5261   Cornell Low Fat Chocolate Milk     7 11.04        <NA>
    ## 5262       Yogurt Mango Greek Chobani     5 11.04        <NA>
    ## 5263       Yogurt Plain Greek Chobani     5 11.04        <NA>
    ## 5264 Yogurt Straw Banana Chobani Drin     3 11.04        <NA>
    ## 5265       Yogurt Peach Greek Chobani     4 11.04        <NA>
    ## 5266 Yogurt Flip Almond Coco Loco Cho     3 11.04        <NA>
    ## 5267   Milk Chocolate LF Cornell 8 Oz     4 11.04        <NA>
    ## 5268       Cornell 2% Milk (70000380)     2 11.04        <NA>
    ## 5269  Yogurt Strawberry Greek Chobani     2 11.04        <NA>
    ## 5270            8 oz Vanilla Soy Silk     2 11.04        <NA>
    ## 5271  Yogurt Mango Chobani Drink 7 oz     1 11.04        <NA>
    ## 5272       Cornell 2% Milk (70000381)     2 11.04        <NA>
    ## 5273               Cornell Whole Milk     2 11.04        <NA>
    ## 5274  Yogurt Flip Peanut Butter Dream     1 11.04        <NA>
    ## 5275 Yogurt Black Cherry Greek Choban     1 11.04        <NA>
    ## 5276   Yogurt Blueberry Greek Chobani     1 11.04        <NA>
    ## 5277  Cornell Dairy Strawberry Yogurt     1 11.04        <NA>
    ## 5278   Fresh Cut Watermelon Fruit Cup    12 11.04        <NA>
    ## 5279    Fresh Cut Pineapple Fruit Cup     4 11.04        <NA>
    ## 5280        Fresh Cut Melon Fruit Cup     2 11.04        <NA>
    ## 5281        Bar S'Mores Quest 2.12 oz     3 11.04        <NA>
    ## 5282 Jelly Konjac Mango Pineapple Tas     3 11.04        <NA>
    ## 5283      Jelly Konjac Peach Tastelli     2 11.04        <NA>
    ## 5284      Kind Almond And Coconut Bar     2 11.04        <NA>
    ## 5285 Bar Cookie & Cream Quest 2.12 oz     1 11.04        <NA>
    ## 5286 Cookie Peanut Butter Lenny & Lar     1 11.04        <NA>
    ## 5287  Bar Peanut Butter Builders Clif     1 11.04        <NA>
    ## 5288 Jelly Konjac Apple Grape Tastell     1 11.04        <NA>
    ## 5289 Jelly Konjac Double Berry Tastel     1 11.04        <NA>
    ## 5290   Bar Crunchy Peanut Butter Clif     1 11.04        <NA>
    ## 5291 Bar White Chocolate Macadamia Cl     1 11.04        <NA>
    ## 5292 Bar That's It Apple+Blueberry 1.     1 11.04        <NA>
    ## 5293             Orbit Wintermint Gum     6 11.04        <NA>
    ## 5294 Candy Dark Chocolate Awake Bar 1     4 11.04        <NA>
    ## 5295   Candy CHOC Cara Caff Bar Awake     3 11.04        <NA>
    ## 5296             Orbit Sweet Mint Gum     2 11.04        <NA>
    ## 5297 Candy Milk Choc Caff Bar Awake 1     1 11.04        <NA>
    ## 5298                      Fruit Whole    36 11.04        <NA>
    ## 5299    Chip Kettle Salt Vinegar 2 oz     3 11.04        <NA>
    ## 5300 Chip Potato Dirty BBQ Mesquite 2     2 11.04        <NA>
    ## 5301       Chip Baked Jax  Utz 1.5 oz     2 11.04        <NA>
    ## 5302     Chip Funky Fusion Dirty 2 oz     1 11.04        <NA>
    ## 5303  Chips Cracked Pepper & Sea Salt     1 11.04        <NA>
    ## 5304           Chips Dirty Sea Salted     1 11.04        <NA>
    ## 5305   Utz Honey Barbecue Chip 1.5 oz     1 11.04        <NA>
    ## 5306                 Utz Regular Chip     1 11.04        <NA>
    ## 5307      Chewy Marshmallow GF 2.1 oz     4 11.04        <NA>
    ## 5308 GF Sweet Street Choloate Brownie     3 11.04        <NA>
    ## 5309                 MochiCookCrm1.5o     4 11.04        <NA>
    ## 5310 Ice Cream Mochi Double Chocolate     2 11.04        <NA>
    ## 5311  Ice Cream Mochi Sweet Mango 1.5     1 11.04        <NA>
    ## 5312                     Muffin Jumbo    28 11.04        <NA>
    ## 5313                           Cookie    17 11.04        <NA>
    ## 5314                   Croissant Choc     5 11.04        <NA>
    ## 5315               Croissant Straw CC     3 11.04        <NA>
    ## 5316                     Cinnamon Bun     3 11.04        <NA>
    ## 5317                            Scone     3 11.04        <NA>
    ## 5318      Combo 16 oz Coffee & Muffin     2 11.04        <NA>
    ## 5319                Croissant Blue CC     1 11.04        <NA>
    ## 5320                   BowlMedProtein     4 11.04 grab 'n' go
    ## 5321                   BowlSouthChick     3 11.04 grab 'n' go
    ## 5322                 BowlMexicanChick     1 11.04 grab 'n' go
    ## 5323                 BowlSesAsianNood     1 11.04 grab 'n' go
    ## 5324                     PBJ on Wheat    12 11.04 grab 'n' go
    ## 5325                   GF Turkey Sand     2 11.04 grab 'n' go
    ## 5326                 GFSunButterJelly     2 11.04 grab 'n' go
    ## 5327              Wrap Chicken Caesar     4 11.04 grab 'n' go
    ## 5328 Sandwich Crispy Chicken Milanese     3 11.04 grab 'n' go
    ## 5329             Wrap Buffalo Chicken     3 11.04 grab 'n' go
    ## 5330             Salad Chicken Caesar     2 11.04 grab 'n' go
    ## 5331 Sandwich Black Forrest Ham & Swi     1 11.04 grab 'n' go
    ## 5332 Sandwich Prosciutto & Mozzarella     1 11.04 grab 'n' go
    ## 5333               Golden Dragon Roll     3 11.04 grab 'n' go
    ## 5334                  California Roll     2 11.04 grab 'n' go
    ## 5335                         TSA Roll     1 11.04 grab 'n' go
    ## 5336                      Salmon Roll     1 11.04 grab 'n' go
    ## 5337                  Spicy Tuna Roll     1 11.04 grab 'n' go
    ## 5338                     Avocado Roll     1 11.04 grab 'n' go
    ## 5339       Quesadilla Deluxe Trillium   118 11.01     mexican
    ## 5340                Grilled Hamburger    66 11.01       grill
    ## 5341            Fried Chicken Tenders    71 11.01       grill
    ## 5342    Burrito Una Mano Trillium BYO    48 11.01     mexican
    ## 5343                     French Fries    95 11.01       grill
    ## 5344                Quesadilla Cheese    15 11.01     mexican
    ## 5345  Grilled Chicken Breast Sandwich    12 11.01       grill
    ## 5346 Trillium Grill Impossible Burger     9 11.01       grill
    ## 5347             Seared Salmon Burger     4 11.01       grill
    ## 5348                Black Bean Burger     3 11.01       grill
    ## 5349             ADD Beef Patty $2.99     8 11.01       grill
    ## 5350               Sweet Potato Fries     4 11.01       grill
    ## 5351               ADD Chicken Breast     1 11.01       grill
    ## 5352              Add Sausage 2 Patty     1 11.01       grill
    ## 5353                     Add Egg $.99     1 11.01       grill
    ## 5354                1 Entree + 1 Side   117 11.01         wok
    ## 5355                1 Entree + 2 Side    51 11.01         wok
    ## 5356               Bowl Ramen Chicken    34 11.01       ramen
    ## 5357              2 Entrees + 2 Sides    12 11.01         wok
    ## 5358                  Bowl Ramen Tofu    13 11.01       ramen
    ## 5359          Side Vegetarian Lo Mein     8 11.01         wok
    ## 5360                     1 Wok Entree     4 11.01         wok
    ## 5361                  Side Vegetables     3 11.01         wok
    ## 5362         Side White or Brown Rice     5 11.01         wok
    ## 5363      Side Vegetable Spring Rolls     2 11.01         wok
    ## 5364  Side Vegetarian Fried Rice with     1 11.01         wok
    ## 5365                Burrito Breakfast    57 11.01        <NA>
    ## 5366             Grand Slam Breakfast    28 11.01        <NA>
    ## 5367              Small French Omelet    25 11.01        <NA>
    ## 5368 Egg Cheese Bacon Breakfast Sandw    24 11.01        <NA>
    ## 5369 Egg Cheese Sausage Breakfast San    21 11.01        <NA>
    ## 5370                        Add Bacon    18 11.01        <NA>
    ## 5371                         Two Eggs    10 11.01        <NA>
    ## 5372              Trillium Home Fries     4 11.01        <NA>
    ## 5373                   Pancake Single     4 11.01        <NA>
    ## 5374      Create Your Pasta Bowl MEAT    87 11.01     italian
    ## 5375              Pizza with Toppings    17 11.01     italian
    ## 5376       Create Your Pasta Bowl VEG     8 11.01     italian
    ## 5377                     Pizza Cheese    13 11.01     italian
    ## 5378                   Add Extra Meat     7 11.01     italian
    ## 5379                 Burrito Bowl BYO    59 11.01     mexican
    ## 5380                      Single Taco     6 11.01     mexican
    ## 5381                   Side Guacamole     2 11.01     mexican
    ## 5382               Salad by the Pound    45 11.01   salad bar
    ## 5383                        8 oz Soup    40 11.01   salad bar
    ## 5384                       Soup 12 oz    21 11.01   salad bar
    ## 5385              Soda Fountain 24 oz    22 11.01        <NA>
    ## 5386                  Coffee 12 oz SB    17 11.01        <NA>
    ## 5387              Soda Fountain 16 oz    20 11.01        <NA>
    ## 5388                  Coffee 16 oz SB    12 11.01        <NA>
    ## 5389                    Hot Tea 20 oz     2 11.01        <NA>
    ## 5390                 Side Potato Tots    12 11.01        <NA>
    ## 5391               Open Miscellaneous     2 11.01        <NA>
    ## 5392          Add Extra Protein $2.99     2 11.01        <NA>
    ## 5393 Yerba Mate Peach Revival 15.5 oz     9 11.01        <NA>
    ## 5394                 Soda Pepsi 20 Oz    10 11.01        <NA>
    ## 5395             Water Aquafina 20 oz    10 11.01        <NA>
    ## 5396 Muscle Milk P40 Strawberry Cream     4 11.01        <NA>
    ## 5397 Juice Orange Premium Topicana 11     7 11.01        <NA>
    ## 5398    Yerba Mate Bluephoria 15.5 oz     5 11.01        <NA>
    ## 5399       Lipton Pure Leaf Sweet Tea     7 11.01        <NA>
    ## 5400            Soda Pepsi  Zero 20oz     7 11.01        <NA>
    ## 5401     Soda Starry Lemon Lime 20 oz     7 11.01        <NA>
    ## 5402      Yerba Mate Tropical 15.5 oz     4 11.01        <NA>
    ## 5403  Energy Blue Crush Celsius 16 oz     4 11.01        <NA>
    ## 5404 Energy Dragonberry Celsius 16 oz     4 11.01        <NA>
    ## 5405   Energy StrawLemon Celsius 12oz     4 11.01        <NA>
    ## 5406          Soda Mountain Dew 20 Oz     5 11.01        <NA>
    ## 5407            Soda Pepsi Diet 20 Oz     5 11.01        <NA>
    ## 5408 Tea Pure Leaf Zero Sugar Sweet T     5 11.01        <NA>
    ## 5409    Tea Black Milk Ito En 11.8 oz     3 11.01        <NA>
    ## 5410       Juice AppleTropicana 11 oz     4 11.01        <NA>
    ## 5411 Milk Chocolate LF BIG RED Refuel     5 11.01        <NA>
    ## 5412  Tea Green Rasp Acai  Celsius 12     3 11.01        <NA>
    ## 5413    Muscle Milk KO Chocolate 14oz     2 11.01        <NA>
    ## 5414     Water Life WTR Immune 700 ML     3 11.01        <NA>
    ## 5415 Tea Unsweet Black W Lemon 18.5 o     4 11.01        <NA>
    ## 5416   Frappuccino Vanilla SB 13.7 Oz     2 11.01        <NA>
    ## 5417          Juice Naked Berry Blast     2 11.01        <NA>
    ## 5418       Juice Naked PNCLDA 15.2 Oz     2 11.01        <NA>
    ## 5419    Naked Strawberry Banana Juice     2 11.01        <NA>
    ## 5420  Kombucha Pineapple Peach Kevita     2 11.01        <NA>
    ## 5421  Kombucha Rasp Lemon Kevita 15.2     2 11.01        <NA>
    ## 5422              Gatorade Blue 28 oz     3 11.01        <NA>
    ## 5423    Glacier Freeze Gatorade 28 oz     3 11.01        <NA>
    ## 5424 Gatorade Propel Berry Water 1 lt     3 11.01        <NA>
    ## 5425 Gatorade Propel Grape Water 1 lt     3 11.01        <NA>
    ## 5426  Gatorade Gatorlyte Orange 20 oz     2 11.01        <NA>
    ## 5427         Yerba Mate Lemon 15.5 oz     2 11.01        <NA>
    ## 5428 Tea Golden Oolong Unsweet Ito En     2 11.01        <NA>
    ## 5429    Water Aquafina Alumitek 16 oz     4 11.01        <NA>
    ## 5430 Cold Brew Vanilla Sweet Cream St     2 11.01        <NA>
    ## 5431 Energy Passion Orange Guva Odyss     2 11.01        <NA>
    ## 5432 Juice Fuji Apple Red Jacket 12oz     2 11.01        <NA>
    ## 5433  Soda Ginger Ale Schweppes 20 Oz     3 11.01        <NA>
    ## 5434     Soda Mountain Dew Zero 20 Oz     3 11.01        <NA>
    ## 5435     Soda Pepsi Wild Cherry 20 Oz     3 11.01        <NA>
    ## 5436  Soda Rootbeer Zero Sugar Mug 20     3 11.01        <NA>
    ## 5437   Tea Iced Rasberry Lipton 16 oz     3 11.01        <NA>
    ## 5438 Tea Sweet W/ Le Pure Leaf Lipton     3 11.01        <NA>
    ## 5439 Tea Tea & Lemon Pure Leaf Lipton     3 11.01        <NA>
    ## 5440   Rockstar Strawberry Punch 16oz     2 11.01        <NA>
    ## 5441 Energy Fuji Apple Pear Celsius 1     2 11.01        <NA>
    ## 5442 Tea Green Peach Mango Celsius 12     2 11.01        <NA>
    ## 5443           Water Gatorade 33.8 oz     2 11.01        <NA>
    ## 5444     Cider Apple Red Jacket 12 oz     2 11.01        <NA>
    ## 5445      Gatorade Berry Zero G 28 oz     2 11.01        <NA>
    ## 5446       Gatorade Fruit Punch 28 oz     2 11.01        <NA>
    ## 5447 Gatorade Galcier Freeze Zero G 2     2 11.01        <NA>
    ## 5448        Gatorade Lemon Lime 28 oz     2 11.01        <NA>
    ## 5449        Gatorade Zero Grape 28 oz     2 11.01        <NA>
    ## 5450 Juice Orange Homestyle Tropicana     2 11.01        <NA>
    ## 5451 Juice Raspberry Lemonade Tropica     2 11.01        <NA>
    ## 5452 Sparkling lemonade Strawberry Ke     2 11.01        <NA>
    ## 5453             Water Gatorade 700ml     2 11.01        <NA>
    ## 5454                 Cornell Lemonade     3 11.01        <NA>
    ## 5455      Soda Mango Jarritos 12.5 oz     2 11.01        <NA>
    ## 5456   Soda Tamarind Jarritos 12.5 oz     2 11.01        <NA>
    ## 5457   Muscle Milk PP Chocolate 14 oz     1 11.01        <NA>
    ## 5458                Water Aquafina 1L     2 11.01        <NA>
    ## 5459          Soda Rootbeer Mug 20 oz     2 11.01        <NA>
    ## 5460 Tea Blackberry Pure Leaf 18.5 oz     2 11.01        <NA>
    ## 5461 Tea Sweetened With Lemon Brisk 2     2 11.01        <NA>
    ## 5462 Tea Unsweet Green Lipton 18.5 oz     2 11.01        <NA>
    ## 5463 Starbucks Doubleshot Ener Coffee     1 11.01        <NA>
    ## 5464  Starbucks Doubleshot Energy Van     1 11.01        <NA>
    ## 5465   Frappuccino Caramel SB 13.7 oz     1 11.01        <NA>
    ## 5466     Frappuccino Mocha 13.7 Oz SB     1 11.01        <NA>
    ## 5467         Juice Naked Blue Machine     1 11.01        <NA>
    ## 5468        Juice Naked Green Machine     1 11.01        <NA>
    ## 5469         Juice Naked Mighty Mango     1 11.01        <NA>
    ## 5470         Juice Protein Zone Naked     1 11.01        <NA>
    ## 5471 Juice Ocean Spray Cranbe Grape 1     2 11.01        <NA>
    ## 5472   Kombucha Ginger Kevita 15.2 oz     1 11.01        <NA>
    ## 5473 Gatorade Gatorlyte MixBerry 20 o     1 11.01        <NA>
    ## 5474 Gatorade Gatorlyte Zero Fruit Pu     1 11.01        <NA>
    ## 5475 Yerba Mate Enlighten Mint 15.5 o     1 11.01        <NA>
    ## 5476     Starbucks Double Shot 6.5 Oz     1 11.01        <NA>
    ## 5477 Tea Jasmine Green Unsweet Ito En     1 11.01        <NA>
    ## 5478 Cold Brew Chocolate Cream Starbu     1 11.01        <NA>
    ## 5479 Energy 222 Pineapple Mango Odyss     1 11.01        <NA>
    ## 5480 Energy Cherry Limeade Celsius 16     1 11.01        <NA>
    ## 5481    Rockstar Pure Zero Silver Ice     1 11.01        <NA>
    ## 5482   Energy Artic Vibe Celsius 12oz     1 11.01        <NA>
    ## 5483 Energy Mango PassFruit Celsius 1     1 11.01        <NA>
    ## 5484 Mountain Dew Kickstart Orange 16     1 11.01        <NA>
    ## 5485    Glacier Cherry Gatorade 28 oz     1 11.01        <NA>
    ## 5486       Juice Grape Tropicana 11oz     1 11.01        <NA>
    ## 5487  Juice Lively Lemonade Tropicana     1 11.01        <NA>
    ## 5488 Juice Zero Summer Splash Punch 1     1 11.01        <NA>
    ## 5489 Kickstart Strawberry Start-up 16     1 11.01        <NA>
    ## 5490  Sparkling Lemonade Mango Kevita     1 11.01        <NA>
    ## 5491  Water Life WTR PH Balance 20 oz     1 11.01        <NA>
    ## 5492      Soda Guava Jarritos 12.5 oz     1 11.01        <NA>
    ## 5493  Soda Pineapple Jarritos 12.5 oz     1 11.01        <NA>
    ## 5494          Soda Orange Crush 20 Oz     1 11.01        <NA>
    ## 5495  Tea Peach Pure Leaf Lipton 18.5     1 11.01        <NA>
    ## 5496 Tea Unsweetened Pure Leaf Lipton     1 11.01        <NA>
    ## 5497         Juice Apple Dole 15.2 oz     1 11.01        <NA>
    ## 5498        Juice Orange Dole 15.2 oz     1 11.01        <NA>
    ## 5499   Ocean Spray Cranberry Cocktail     1 11.01        <NA>
    ## 5500   Soda Mandarin Jarritos 12.5 oz     1 11.01        <NA>
    ## 5501 Yogurt Mixed Berry Chobani Drink     6 11.01        <NA>
    ## 5502  Yogurt Mango Chobani Drink 7 oz     4 11.01        <NA>
    ## 5503 Yogurt Straw Banana Chobani Drin     2 11.01        <NA>
    ## 5504   Cornell Low Fat Chocolate Milk     2 11.01        <NA>
    ## 5505  Yogurt Flip Peanut Butter Dream     2 11.01        <NA>
    ## 5506                  Cornell 2% Milk     3 11.01        <NA>
    ## 5507 Yogurt 0% Fat Vanilla Greek Oiko     2 11.01        <NA>
    ## 5508 Yogurt Black Cherry Greek Choban     2 11.01        <NA>
    ## 5509       Yogurt Mango Greek Chobani     2 11.01        <NA>
    ## 5510            8 oz Vanilla Soy Silk     2 11.01        <NA>
    ## 5511               Cornell Whole Milk     2 11.01        <NA>
    ## 5512   Milk Chocolate LF Cornell 8 Oz     2 11.01        <NA>
    ## 5513 Yogurt Flip Almond Coco Loco Cho     1 11.01        <NA>
    ## 5514   Yogurt Blueberry Greek Chobani     1 11.01        <NA>
    ## 5515          Silk Chocolate Soy Milk     1 11.01        <NA>
    ## 5516  Cornell Dairy Strawberry Yogurt     1 11.01        <NA>
    ## 5517 Bar Cookie & Cream Quest 2.12 oz     2 11.01        <NA>
    ## 5518  Bar Frosted Birthday Cake Quest     2 11.01        <NA>
    ## 5519 Cookie Peanut Butter Lenny & Lar     2 11.01        <NA>
    ## 5520      Jelly Konjac Peach Tastelli     2 11.01        <NA>
    ## 5521       Bar Chocolate Chip Clifbar     2 11.01        <NA>
    ## 5522 Dark Choc Cherry Cashew Plus Kin     2 11.01        <NA>
    ## 5523 Bar Choc Chip Cookie Dough Quest     1 11.01        <NA>
    ## 5524 Bar CHOC Mint Builders Clif bar2     1 11.01        <NA>
    ## 5525  Bar Peanut Butter Builders Clif     1 11.01        <NA>
    ## 5526   Bar Crunchy Peanut Butter Clif     1 11.01        <NA>
    ## 5527 Bar White Chocolate Macadamia Cl     1 11.01        <NA>
    ## 5528      Kind Almond And Coconut Bar     1 11.01        <NA>
    ## 5529        Oat And Honey Granola Bar     1 11.01        <NA>
    ## 5530 GF Sweet Street Choloate Brownie     6 11.01        <NA>
    ## 5531      Chewy Marshmallow GF 2.1 oz     3 11.01        <NA>
    ## 5532  Chip Sour Cream And Onion Dirty     2 11.01        <NA>
    ## 5533       Chip Maui Onion Dirty 2 oz     1 11.01        <NA>
    ## 5534  Chips Cracked Pepper & Sea Salt     1 11.01        <NA>
    ## 5535           Chips Dirty Sea Salted     1 11.01        <NA>
    ## 5536   Chip Voodoo Limited Zapps 2 oz     1 11.01        <NA>
    ## 5537       Jalapeno Heat Chips Kosher     1 11.01        <NA>
    ## 5538       Utz GoodHealth Veggie Chip     1 11.01        <NA>
    ## 5539                 Utz Regular Chip     1 11.01        <NA>
    ## 5540    Utz Sour Cream and Onion Chip     1 11.01        <NA>
    ## 5541                      Fruit Whole    20 11.01        <NA>
    ## 5542                 MochiCookCrm1.5o     3 11.01        <NA>
    ## 5543  Ice Cream Mochi Sweet Mango 1.5     1 11.01        <NA>
    ## 5544 Ice Cream Mochi Double Chocolate     1 11.01        <NA>
    ## 5545             Orbit Sweet Mint Gum     2 11.01        <NA>
    ## 5546 Candy Milk Choc Caff Bar Awake 1     1 11.01        <NA>
    ## 5547             Orbit Wintermint Gum     1 11.01        <NA>
    ## 5548              Tempura Shrimp Roll     2 11.01 grab 'n' go
    ## 5549              Tempura Crunch Roll     1 11.01 grab 'n' go
    ## 5550               Golden Dragon Roll     1 11.01 grab 'n' go
    ## 5551                  Hawaiian Sunset     1 11.01 grab 'n' go
    ## 5552                      Salmon Roll     1 11.01 grab 'n' go
    ## 5553                  Spicy Tuna Roll     1 11.01 grab 'n' go
    ## 5554                     Avocado Roll     1 11.01 grab 'n' go
    ## 5555                  California Roll     1 11.01 grab 'n' go
    ## 5556 Sandwich Crispy Chicken Milanese     4 11.01 grab 'n' go
    ## 5557             Wrap Buffalo Chicken     2 11.01 grab 'n' go
    ## 5558              Wrap Chicken Caesar     2 11.01 grab 'n' go
    ## 5559 Sandwich Prosciutto & Mozzarella     1 11.01 grab 'n' go
    ## 5560                     Muffin Jumbo    14 11.01        <NA>
    ## 5561                           Cookie    14 11.01        <NA>
    ## 5562                   Croissant Choc     3 11.01        <NA>
    ## 5563                     Cinnamon Bun     3 11.01        <NA>
    ## 5564                            Scone     3 11.01        <NA>
    ## 5565               Croissant Straw CC     2 11.01        <NA>
    ## 5566                Croissant Blue CC     1 11.01        <NA>
    ## 5567                      Coffee Cake     1 11.01        <NA>
    ## 5568                 GF ChicCaesarSld     3 11.01 grab 'n' go
    ## 5569                   GF Turkey Sand     1 11.01 grab 'n' go
    ## 5570                 BowlMexicanChick     1 11.01 grab 'n' go
    ## 5571                 BowlSesAsianNood     1 11.01 grab 'n' go
    ## 5572                   BowlSouthChick     1 11.01 grab 'n' go
    ## 5573                     PBJ on Wheat     6 11.01 grab 'n' go
    ## 5574       Quesadilla Deluxe Trillium   164 10.31     mexican
    ## 5575                Grilled Hamburger   107 10.31       grill
    ## 5576            Fried Chicken Tenders   100 10.31       grill
    ## 5577    Burrito Una Mano Trillium BYO    70 10.31     mexican
    ## 5578                     French Fries   135 10.31       grill
    ## 5579  Grilled Chicken Breast Sandwich    21 10.31       grill
    ## 5580 Trillium Grill Impossible Burger    13 10.31       grill
    ## 5581                Quesadilla Cheese    14 10.31     mexican
    ## 5582               Sweet Potato Fries    27 10.31       grill
    ## 5583             Seared Salmon Burger     7 10.31       grill
    ## 5584             ADD Beef Patty $2.99    14 10.31       grill
    ## 5585               ADD Chicken Breast     3 10.31       grill
    ## 5586                Black Bean Burger     1 10.31       grill
    ## 5587                       ADD Cheese    12 10.31       grill
    ## 5588              Add Sausage 2 Patty     3 10.31       grill
    ## 5589                     Add Egg $.99     2 10.31       grill
    ## 5590                1 Entree + 1 Side   226 10.31         wok
    ## 5591                1 Entree + 2 Side    77 10.31         wok
    ## 5592               Bowl Ramen Chicken    79 10.31       ramen
    ## 5593              2 Entrees + 2 Sides    29 10.31         wok
    ## 5594                  Bowl Ramen Tofu    10 10.31       ramen
    ## 5595          Side Vegetarian Lo Mein    10 10.31         wok
    ## 5596                     1 Wok Entree     6 10.31         wok
    ## 5597  Side Vegetarian Fried Rice with     4 10.31         wok
    ## 5598         Side White or Brown Rice     6 10.31         wok
    ## 5599      Side Vegetable Spring Rolls     2 10.31         wok
    ## 5600                  Side Vegetables     1 10.31         wok
    ## 5601                Burrito Breakfast    92 10.31        <NA>
    ## 5602              Small French Omelet    55 10.31        <NA>
    ## 5603 Egg Cheese Sausage Breakfast San    34 10.31        <NA>
    ## 5604 Egg Cheese Bacon Breakfast Sandw    30 10.31        <NA>
    ## 5605             Grand Slam Breakfast    16 10.31        <NA>
    ## 5606                        Add Bacon    23 10.31        <NA>
    ## 5607                         Two Eggs    19 10.31        <NA>
    ## 5608              Trillium Home Fries     3 10.31        <NA>
    ## 5609                   Pancake Single     1 10.31        <NA>
    ## 5610                            Toast     2 10.31        <NA>
    ## 5611                   2 Slices Toast     1 10.31        <NA>
    ## 5612      Create Your Pasta Bowl MEAT   120 10.31     italian
    ## 5613       Create Your Pasta Bowl VEG    27 10.31     italian
    ## 5614              Pizza with Toppings    23 10.31     italian
    ## 5615                     Pizza Cheese    25 10.31     italian
    ## 5616                   Add Extra Meat    16 10.31     italian
    ## 5617         Side Bread Pasta Station     2 10.31     italian
    ## 5618                 Burrito Bowl BYO    93 10.31     mexican
    ## 5619                      Single Taco     2 10.31     mexican
    ## 5620                   Side Guacamole     2 10.31     mexican
    ## 5621                       Side Salsa     1 10.31     mexican
    ## 5622               Salad by the Pound    62 10.31   salad bar
    ## 5623                        8 oz Soup    27 10.31   salad bar
    ## 5624                       Soup 12 oz    21 10.31   salad bar
    ## 5625              Soda Fountain 16 oz    40 10.31        <NA>
    ## 5626              Soda Fountain 24 oz    29 10.31        <NA>
    ## 5627                  Coffee 16 oz SB    23 10.31        <NA>
    ## 5628                  Coffee 12 oz SB    22 10.31        <NA>
    ## 5629                    Hot Tea 20 oz     3 10.31        <NA>
    ## 5630                 Side Potato Tots    22 10.31        <NA>
    ## 5631               Open Miscellaneous     6 10.31        <NA>
    ## 5632          Add Extra Protein $2.99     2 10.31        <NA>
    ## 5633            Soda Pepsi Diet 20 Oz    21 10.31        <NA>
    ## 5634 Tea Jasmine Green Unsweet Ito En    12 10.31        <NA>
    ## 5635             Water Aquafina 20 oz    20 10.31        <NA>
    ## 5636                Water Aquafina 1L    17 10.31        <NA>
    ## 5637 Milk Chocolate LF BIG RED Refuel    17 10.31        <NA>
    ## 5638 Yerba Mate Peach Revival 15.5 oz     8 10.31        <NA>
    ## 5639         Juice Naked Mighty Mango     7 10.31        <NA>
    ## 5640 Yerba Mate Enlighten Mint 15.5 o     7 10.31        <NA>
    ## 5641 Energy Fuji Apple Pear Celsius 1     8 10.31        <NA>
    ## 5642  Kombucha Pineapple Peach Kevita     6 10.31        <NA>
    ## 5643 Tea Unsweetened Pure Leaf Lipton    10 10.31        <NA>
    ## 5644   Energy StrawGuava Celsius 12oz     7 10.31        <NA>
    ## 5645 Tea Golden Oolong Unsweet Ito En     6 10.31        <NA>
    ## 5646     Water Life WTR Immune 700 ML     7 10.31        <NA>
    ## 5647 Cold Brew Vanilla Sweet Cream St     6 10.31        <NA>
    ## 5648 Energy Mango PassFruit Celsius 1     6 10.31        <NA>
    ## 5649    Muscle Milk KO Chocolate 14oz     4 10.31        <NA>
    ## 5650 Muscle Milk PROSRS 40 Intense Va     4 10.31        <NA>
    ## 5651   Yerba Mate Revel Berry 15.5 oz     5 10.31        <NA>
    ## 5652         Juice Protein Zone Naked     4 10.31        <NA>
    ## 5653    Naked Strawberry Banana Juice     4 10.31        <NA>
    ## 5654   Kombucha Ginger Kevita 15.2 oz     4 10.31        <NA>
    ## 5655 Energy Blue Raz Lemonade Celsius     5 10.31        <NA>
    ## 5656 Tea Green Peach Mango Celsius 12     5 10.31        <NA>
    ## 5657                 Soda Pepsi 20 Oz     7 10.31        <NA>
    ## 5658 Sparkling lemonade Strawberry Ke     6 10.31        <NA>
    ## 5659           Water Gatorade 33.8 oz     5 10.31        <NA>
    ## 5660    Yerba Mate Bluephoria 15.5 oz     4 10.31        <NA>
    ## 5661     Cider Apple Red Jacket 12 oz     5 10.31        <NA>
    ## 5662    Water Aquafina Alumitek 16 oz     8 10.31        <NA>
    ## 5663 Juice Fuji Apple Red Jacket 12oz     4 10.31        <NA>
    ## 5664  Soda Ginger Ale Schweppes 20 Oz     6 10.31        <NA>
    ## 5665            Soda Pepsi  Zero 20oz     6 10.31        <NA>
    ## 5666 Tea Pure Leaf Zero Sugar Sweet T     6 10.31        <NA>
    ## 5667 Tea Tea & Lemon Pure Leaf Lipton     6 10.31        <NA>
    ## 5668 Gatorade Propel Grape Water 1 lt     5 10.31        <NA>
    ## 5669 Gatorade Propel  Kiwi Straw Wate     5 10.31        <NA>
    ## 5670         Juice Lemonade Dole 20oz     6 10.31        <NA>
    ## 5671   Energy Artic Vibe Celsius 12oz     4 10.31        <NA>
    ## 5672         Juice Naked Blue Machine     3 10.31        <NA>
    ## 5673  Kombucha Rasp Lemon Kevita 15.2     3 10.31        <NA>
    ## 5674      Soda Mango Jarritos 12.5 oz     5 10.31        <NA>
    ## 5675     Soda Pepsi Wild Cherry 20 Oz     5 10.31        <NA>
    ## 5676  Soda Rootbeer Zero Sugar Mug 20     5 10.31        <NA>
    ## 5677     Soda Starry Lemon Lime 20 oz     5 10.31        <NA>
    ## 5678   Tea Iced Rasberry Lipton 16 oz     5 10.31        <NA>
    ## 5679 Gatorade Gatorlyte Glacier Freez     3 10.31        <NA>
    ## 5680 Gatorade Gatorlyte MixBerry 20 o     3 10.31        <NA>
    ## 5681 Gatorade Gatorlyte Strawberry Ki     3 10.31        <NA>
    ## 5682         Yerba Mate Lemon 15.5 oz     3 10.31        <NA>
    ## 5683     Starbucks Double Shot 6.5 Oz     3 10.31        <NA>
    ## 5684    Tea Black Milk Ito En 11.8 oz     3 10.31        <NA>
    ## 5685 Gatorade Galcier Freeze Zero G 2     4 10.31        <NA>
    ## 5686             Gatorade Orange 28oz     4 10.31        <NA>
    ## 5687    Glacier Cherry Gatorade 28 oz     4 10.31        <NA>
    ## 5688  Juice Lively Lemonade Tropicana     4 10.31        <NA>
    ## 5689 Energy Dragonberry Celsius 16 oz     3 10.31        <NA>
    ## 5690 Gatorade Propel Berry Water 1 lt     4 10.31        <NA>
    ## 5691          Soda Mountain Dew 20 Oz     4 10.31        <NA>
    ## 5692 Tea Blackberry Pure Leaf 18.5 oz     4 10.31        <NA>
    ## 5693 Tea Sweetened With Lemon Brisk 2     4 10.31        <NA>
    ## 5694 Tea Sweet W/ Le Pure Leaf Lipton     4 10.31        <NA>
    ## 5695  Starbucks Doubleshot Energy Van     2 10.31        <NA>
    ## 5696     Frappuccino Mocha 13.7 Oz SB     2 10.31        <NA>
    ## 5697          Juice Naked Berry Blast     2 10.31        <NA>
    ## 5698        Juice Naked Green Machine     2 10.31        <NA>
    ## 5699              Gatorade Blue 28 oz     3 10.31        <NA>
    ## 5700        Gatorade Lemon Lime 28 oz     3 10.31        <NA>
    ## 5701       Juice AppleTropicana 11 oz     3 10.31        <NA>
    ## 5702       Juice Grape Tropicana 11oz     3 10.31        <NA>
    ## 5703 Juice Orange Premium Topicana 11     3 10.31        <NA>
    ## 5704 Juice Raspberry Lemonade Tropica     3 10.31        <NA>
    ## 5705  Sparkling Lemonade Mango Kevita     3 10.31        <NA>
    ## 5706 Gatorade Gatorlyte Cherry Lime 2     2 10.31        <NA>
    ## 5707  Gatorade Gatorlyte Orange 20 oz     2 10.31        <NA>
    ## 5708      Yerba Mate Tropical 15.5 oz     2 10.31        <NA>
    ## 5709 Tea Green Matcha Milk Ito En 11.     2 10.31        <NA>
    ## 5710 Cold Brew Chocolate Cream Starbu     2 10.31        <NA>
    ## 5711 Energy 222 Blue Raspberry Odysse     2 10.31        <NA>
    ## 5712 Energy Fruit Burst Celsius 16 oz     2 10.31        <NA>
    ## 5713 Energy Passion Orange Guva Odyss     2 10.31        <NA>
    ## 5714          Soda Orange Crush 20 Oz     3 10.31        <NA>
    ## 5715          Soda Rootbeer Mug 20 oz     3 10.31        <NA>
    ## 5716  Tea Peach Pure Leaf Lipton 18.5     3 10.31        <NA>
    ## 5717 Tea Unsweet Black W Lemon 18.5 o     3 10.31        <NA>
    ## 5718 Tea Unsweet Green Lipton 18.5 oz     3 10.31        <NA>
    ## 5719    Rockstar Pure Zero Silver Ice     2 10.31        <NA>
    ## 5720   Ocean Spray Cranberry Cocktail     3 10.31        <NA>
    ## 5721   Energy StrawLemon Celsius 12oz     2 10.31        <NA>
    ## 5722  Tea Green Rasp Acai  Celsius 12     2 10.31        <NA>
    ## 5723   Soda Mandarin Jarritos 12.5 oz     3 10.31        <NA>
    ## 5724 Mountain Dew Kickstart Orange 16     2 10.31        <NA>
    ## 5725       Gatorade Fruit Punch 28 oz     2 10.31        <NA>
    ## 5726    Glacier Freeze Gatorade 28 oz     2 10.31        <NA>
    ## 5727 Juice Orange Homestyle Tropicana     2 10.31        <NA>
    ## 5728             Water Gatorade 700ml     2 10.31        <NA>
    ## 5729  Soda Pineapple Jarritos 12.5 oz     2 10.31        <NA>
    ## 5730   Soda Tamarind Jarritos 12.5 oz     2 10.31        <NA>
    ## 5731         Muscle Milk Choc PB 14oz     1 10.31        <NA>
    ## 5732     Soda Mountain Dew Zero 20 Oz     2 10.31        <NA>
    ## 5733     Tea Light Peach Lipton 20 oz     2 10.31        <NA>
    ## 5734 Starbucks Doubleshot Ener Coffee     1 10.31        <NA>
    ## 5735   Frappuccino Caramel SB 13.7 oz     1 10.31        <NA>
    ## 5736   Frappuccino Vanilla SB 13.7 Oz     1 10.31        <NA>
    ## 5737       Juice Naked PNCLDA 15.2 Oz     1 10.31        <NA>
    ## 5738 Gatorade Gatorlyte Zero Fruit Pu     1 10.31        <NA>
    ## 5739 Energy Mango Tango Celsius 16 oz     1 10.31        <NA>
    ## 5740 Energy Orangesicle Celsius 16 oz     1 10.31        <NA>
    ## 5741                 Cornell Lemonade     2 10.31        <NA>
    ## 5742  Energy Fast Twitch Cool Blue 12     1 10.31        <NA>
    ## 5743   Energy Fast Twitch Grape 12 oz     1 10.31        <NA>
    ## 5744 Energy Fast Twitch Watermelon St     1 10.31        <NA>
    ## 5745      Gatorade Berry Zero G 28 oz     1 10.31        <NA>
    ## 5746        Gatorade Zero Grape 28 oz     1 10.31        <NA>
    ## 5747       Lipton Pure Leaf Sweet Tea     1 10.31        <NA>
    ## 5748         Juice Apple Dole 15.2 oz     1 10.31        <NA>
    ## 5749        Juice Orange Dole 15.2 oz     1 10.31        <NA>
    ## 5750 Juice Ocean Spray Cranbe Grape 1     1 10.31        <NA>
    ## 5751 Yogurt 0% Fat Vanilla Greek Oiko    13 10.31        <NA>
    ## 5752   Cornell Low Fat Chocolate Milk    10 10.31        <NA>
    ## 5753       Cornell 2% Milk (70000381)     9 10.31        <NA>
    ## 5754       Cornell 2% Milk (70000380)     5 10.31        <NA>
    ## 5755  Yogurt Flip Peanut Butter Dream     5 10.31        <NA>
    ## 5756       Yogurt Plain Greek Chobani     5 10.31        <NA>
    ## 5757  Yogurt Mango Chobani Drink 7 oz     3 10.31        <NA>
    ## 5758 Yogurt Mixed Berry Chobani Drink     2 10.31        <NA>
    ## 5759               Cornell Whole Milk     4 10.31        <NA>
    ## 5760   Yogurt Blueberry Greek Chobani     3 10.31        <NA>
    ## 5761  Yogurt Strawberry Greek Chobani     3 10.31        <NA>
    ## 5762 Yogurt Strawberry Banana Greek C     2 10.31        <NA>
    ## 5763          Silk Chocolate Soy Milk     2 10.31        <NA>
    ## 5764 Yogurt Straw Banana Chobani Drin     1 10.31        <NA>
    ## 5765  Cornell Dairy Strawberry Yogurt     2 10.31        <NA>
    ## 5766   Milk Chocolate LF Cornell 8 Oz     2 10.31        <NA>
    ## 5767 Yogurt Flip Almond Coco Loco Cho     1 10.31        <NA>
    ## 5768 Yogurt Black Cherry Greek Choban     1 10.31        <NA>
    ## 5769       Yogurt Mango Greek Chobani     1 10.31        <NA>
    ## 5770       Yogurt Peach Greek Chobani     1 10.31        <NA>
    ## 5771       Cornell Dairy Peach Yogurt     1 10.31        <NA>
    ## 5772 Jelly Konjac Apple Grape Tastell     6 10.31        <NA>
    ## 5773 Jelly Konjac Mango Pineapple Tas     6 10.31        <NA>
    ## 5774      Jelly Konjac Peach Tastelli     4 10.31        <NA>
    ## 5775 Bar Choc Chip Cookie Dough Quest     2 10.31        <NA>
    ## 5776 Cookie Double Choc Lenny & Larry     2 10.31        <NA>
    ## 5777 Jelly Konjac Double Berry Tastel     2 10.31        <NA>
    ## 5778       Bar Chocolate Chip Clifbar     2 10.31        <NA>
    ## 5779 Bar Cookie & Cream Quest 2.12 oz     1 10.31        <NA>
    ## 5780  Bar Frosted Birthday Cake Quest     1 10.31        <NA>
    ## 5781        Bar S'Mores Quest 2.12 oz     1 10.31        <NA>
    ## 5782  Cookie Choc Chip Lenny & Larrys     1 10.31        <NA>
    ## 5783 Bar CHOC Mint Builders Clif bar2     1 10.31        <NA>
    ## 5784  Bar Peanut Butter Builders Clif     1 10.31        <NA>
    ## 5785   Bar Crunchy Peanut Butter Clif     1 10.31        <NA>
    ## 5786 Dark Choc Cherry Cashew Plus Kin     1 10.31        <NA>
    ## 5787   Kind Cranberry Almond Plus Bar     1 10.31        <NA>
    ## 5788 Bar That's It Apple+Blueberry 1.     1 10.31        <NA>
    ## 5789        Oat And Honey Granola Bar     1 10.31        <NA>
    ## 5790                      Fruit Whole    58 10.31        <NA>
    ## 5791           Chips Dirty Sea Salted     3 10.31        <NA>
    ## 5792                 Utz Regular Chip     3 10.31        <NA>
    ## 5793    Utz Sour Cream and Onion Chip     3 10.31        <NA>
    ## 5794 Chip Potato Dirty BBQ Mesquite 2     2 10.31        <NA>
    ## 5795  Chips Cracked Pepper & Sea Salt     2 10.31        <NA>
    ## 5796  Chip Sour Cream And Onion Dirty     2 10.31        <NA>
    ## 5797 Nuts Almonds Honey 1.5 oz Sahale     1 10.31        <NA>
    ## 5798 Nuts Fruit Nut Combo 1.5 oz Saha     1 10.31        <NA>
    ## 5799         Pretzel Thin 2.12 oz Utz     2 10.31        <NA>
    ## 5800 Chip Potato Kettle Honey BBQ 2oz     1 10.31        <NA>
    ## 5801   Chip Voodoo Limited Zapps 2 oz     1 10.31        <NA>
    ## 5802   Utz Honey Barbecue Chip 1.5 oz     1 10.31        <NA>
    ## 5803      Chewy Marshmallow GF 2.1 oz     5 10.31        <NA>
    ## 5804  Ice Cream Mochi Sweet Mango 1.5     3 10.31        <NA>
    ## 5805                 MochiCookCrm1.5o     2 10.31        <NA>
    ## 5806 Ice Cream Mochi Double Chocolate     1 10.31        <NA>
    ## 5807    Fresh Cut Pineapple Fruit Cup     2 10.31        <NA>
    ## 5808             Orbit Sweet Mint Gum     1 10.31        <NA>
    ## 5809               Golden Dragon Roll     3 10.31 grab 'n' go
    ## 5810                     Alaskan Roll     2 10.31 grab 'n' go
    ## 5811              Tempura Shrimp Roll     2 10.31 grab 'n' go
    ## 5812                      Salmon Roll     2 10.31 grab 'n' go
    ## 5813                  California Roll     2 10.31 grab 'n' go
    ## 5814              Tempura Crunch Roll     1 10.31 grab 'n' go
    ## 5815                         TSA Roll     1 10.31 grab 'n' go
    ## 5816                  Hawaiian Sunset     1 10.31 grab 'n' go
    ## 5817                  Spicy Tuna Roll     1 10.31 grab 'n' go
    ## 5818                     Avocado Roll     1 10.31 grab 'n' go
    ## 5819              Wrap Chicken Caesar     7 10.31 grab 'n' go
    ## 5820 Sandwich Prosciutto & Mozzarella     3 10.31 grab 'n' go
    ## 5821             Wrap Buffalo Chicken     3 10.31 grab 'n' go
    ## 5822 Sandwich Black Forrest Ham & Swi     2 10.31 grab 'n' go
    ## 5823 Sandwich Crispy Chicken Milanese     2 10.31 grab 'n' go
    ## 5824             Salad Chicken Caesar     2 10.31 grab 'n' go
    ## 5825     Sandwich Corned Beef & Swiss     1 10.31 grab 'n' go
    ## 5826                     Muffin Jumbo    19 10.31        <NA>
    ## 5827                           Cookie    21 10.31        <NA>
    ## 5828               Croissant Straw CC     7 10.31        <NA>
    ## 5829                     Cinnamon Bun     5 10.31        <NA>
    ## 5830                   Croissant Choc     3 10.31        <NA>
    ## 5831                      Coffee Cake     2 10.31        <NA>
    ## 5832                Croissant Blue CC     1 10.31        <NA>
    ## 5833      Combo 16 oz Coffee & Muffin     1 10.31        <NA>
    ## 5834                 BowlSesAsianNood     3 10.31 grab 'n' go
    ## 5835                   BowlMedProtein     2 10.31 grab 'n' go
    ## 5836                 BowlMexicanChick     2 10.31 grab 'n' go
    ## 5837                 BowlChickAlfrPen     1 10.31 grab 'n' go
    ## 5838                   BowlSouthChick     1 10.31 grab 'n' go
    ## 5839                     PBJ on Wheat     5 10.31 grab 'n' go
    ## 5840                 GFSunButterJelly     3 10.31 grab 'n' go
    ## 5841       Quesadilla Deluxe Trillium   167 10.30     mexican
    ## 5842                Grilled Hamburger    95 10.30       grill
    ## 5843            Fried Chicken Tenders    95 10.30       grill
    ## 5844    Burrito Una Mano Trillium BYO    64 10.30     mexican
    ## 5845                     French Fries   110 10.30       grill
    ## 5846  Grilled Chicken Breast Sandwich    19 10.30       grill
    ## 5847 Trillium Grill Impossible Burger     6 10.30       grill
    ## 5848             Seared Salmon Burger     7 10.30       grill
    ## 5849                Quesadilla Cheese     7 10.30     mexican
    ## 5850                Black Bean Burger     5 10.30       grill
    ## 5851               Sweet Potato Fries    12 10.30       grill
    ## 5852             ADD Beef Patty $2.99     4 10.30       grill
    ## 5853              Add Sausage 2 Patty     3 10.30       grill
    ## 5854                     Add Egg $.99     5 10.30       grill
    ## 5855                       ADD Cheese     7 10.30       grill
    ## 5856               ADD Chicken Breast     1 10.30       grill
    ## 5857                1 Entree + 1 Side   175 10.30         wok
    ## 5858                1 Entree + 2 Side    83 10.30         wok
    ## 5859               Bowl Ramen Chicken    70 10.30       ramen
    ## 5860              2 Entrees + 2 Sides    23 10.30         wok
    ## 5861                  Bowl Ramen Tofu    12 10.30       ramen
    ## 5862          Side Vegetarian Lo Mein    11 10.30         wok
    ## 5863                     1 Wok Entree     3 10.30         wok
    ## 5864         Side White or Brown Rice     5 10.30         wok
    ## 5865                  Side Vegetables     1 10.30         wok
    ## 5866      Create Your Pasta Bowl MEAT   136 10.30     italian
    ## 5867       Create Your Pasta Bowl VEG    23 10.30     italian
    ## 5868              Pizza with Toppings    31 10.30     italian
    ## 5869                     Pizza Cheese    17 10.30     italian
    ## 5870                   Add Extra Meat    28 10.30     italian
    ## 5871         Side Bread Pasta Station     1 10.30     italian
    ## 5872                Burrito Breakfast    80 10.30        <NA>
    ## 5873              Small French Omelet    57 10.30        <NA>
    ## 5874             Grand Slam Breakfast    21 10.30        <NA>
    ## 5875 Egg Cheese Bacon Breakfast Sandw    37 10.30        <NA>
    ## 5876 Egg Cheese Sausage Breakfast San    31 10.30        <NA>
    ## 5877                        Add Bacon    32 10.30        <NA>
    ## 5878                         Two Eggs    10 10.30        <NA>
    ## 5879              Trillium Home Fries     3 10.30        <NA>
    ## 5880                   Pancake Single     2 10.30        <NA>
    ## 5881                 PC Peanut Butter     2 10.30        <NA>
    ## 5882                            Toast     1 10.30        <NA>
    ## 5883                 Burrito Bowl BYO    96 10.30     mexican
    ## 5884                      Single Taco     9 10.30     mexican
    ## 5885                   Side Guacamole     3 10.30     mexican
    ## 5886                       Side Salsa     1 10.30     mexican
    ## 5887               Salad by the Pound    76 10.30   salad bar
    ## 5888                        8 oz Soup    63 10.30   salad bar
    ## 5889                       Soup 12 oz    32 10.30   salad bar
    ## 5890              Soda Fountain 24 oz    33 10.30        <NA>
    ## 5891              Soda Fountain 16 oz    35 10.30        <NA>
    ## 5892                  Coffee 12 oz SB    21 10.30        <NA>
    ## 5893                  Coffee 16 oz SB    19 10.30        <NA>
    ## 5894                    Hot Tea 20 oz     2 10.30        <NA>
    ## 5895                 Side Potato Tots    20 10.30        <NA>
    ## 5896               Open Miscellaneous     7 10.30        <NA>
    ## 5897          Add Extra Protein $2.99     1 10.30        <NA>
    ## 5898             Water Aquafina 20 oz    28 10.30        <NA>
    ## 5899 Yerba Mate Peach Revival 15.5 oz    11 10.30        <NA>
    ## 5900            Soda Pepsi Diet 20 Oz    17 10.30        <NA>
    ## 5901 Tea Golden Oolong Unsweet Ito En    10 10.30        <NA>
    ## 5902 Milk Chocolate LF BIG RED Refuel    18 10.30        <NA>
    ## 5903 Yerba Mate Enlighten Mint 15.5 o     9 10.30        <NA>
    ## 5904         Juice Naked Mighty Mango     7 10.30        <NA>
    ## 5905 Tea Jasmine Green Unsweet Ito En     8 10.30        <NA>
    ## 5906    Yerba Mate Bluephoria 15.5 oz     7 10.30        <NA>
    ## 5907                Water Aquafina 1L    11 10.30        <NA>
    ## 5908 Energy Mango PassFruit Celsius 1     8 10.30        <NA>
    ## 5909   Energy Artic Vibe Celsius 12oz     7 10.30        <NA>
    ## 5910   Yerba Mate Revel Berry 15.5 oz     6 10.30        <NA>
    ## 5911 Juice Orange Premium Topicana 11     8 10.30        <NA>
    ## 5912         Juice Naked Blue Machine     5 10.30        <NA>
    ## 5913        Juice Naked Green Machine     5 10.30        <NA>
    ## 5914                 Soda Pepsi 20 Oz     9 10.30        <NA>
    ## 5915 Energy Blue Raz Lemonade Celsius     6 10.30        <NA>
    ## 5916 Muscle Milk P40 Strawberry Cream     4 10.30        <NA>
    ## 5917     Water Life WTR Immune 700 ML     6 10.30        <NA>
    ## 5918            Soda Pepsi  Zero 20oz     8 10.30        <NA>
    ## 5919 Tea Tea & Lemon Pure Leaf Lipton     8 10.30        <NA>
    ## 5920  Soda Ginger Ale Schweppes 20 Oz     7 10.30        <NA>
    ## 5921  Tea Peach Pure Leaf Lipton 18.5     7 10.30        <NA>
    ## 5922 Tea Unsweet Black W Lemon 18.5 o     7 10.30        <NA>
    ## 5923 Gatorade Gatorlyte Glacier Freez     4 10.30        <NA>
    ## 5924 Juice Ocean Spray Cranbe Grape 1     7 10.30        <NA>
    ## 5925    Muscle Milk KO Chocolate 14oz     3 10.30        <NA>
    ## 5926 Muscle Milk PROSRS 40 Intense Va     3 10.30        <NA>
    ## 5927  Energy Blue Crush Celsius 16 oz     4 10.30        <NA>
    ## 5928 Juice Fuji Apple Red Jacket 12oz     4 10.30        <NA>
    ## 5929       Lipton Pure Leaf Sweet Tea     6 10.30        <NA>
    ## 5930          Soda Rootbeer Mug 20 oz     6 10.30        <NA>
    ## 5931   Tea Iced Rasberry Lipton 16 oz     6 10.30        <NA>
    ## 5932              Gatorade Blue 28 oz     5 10.30        <NA>
    ## 5933 Gatorade Galcier Freeze Zero G 2     5 10.30        <NA>
    ## 5934       Juice AppleTropicana 11 oz     5 10.30        <NA>
    ## 5935 Juice Zero Summer Splash Punch 1     5 10.30        <NA>
    ## 5936  Starbucks Doubleshot Energy Van     3 10.30        <NA>
    ## 5937 Gatorade Propel  Kiwi Straw Wate     5 10.30        <NA>
    ## 5938   Energy StrawGuava Celsius 12oz     4 10.30        <NA>
    ## 5939  Tea Green Rasp Acai  Celsius 12     4 10.30        <NA>
    ## 5940     Frappuccino Mocha 13.7 Oz SB     3 10.30        <NA>
    ## 5941       Juice Naked PNCLDA 15.2 Oz     3 10.30        <NA>
    ## 5942     Cider Apple Red Jacket 12 oz     4 10.30        <NA>
    ## 5943 Tea Unsweet Green Lipton 18.5 oz     5 10.30        <NA>
    ## 5944 Gatorade Gatorlyte Strawberry Ki     3 10.30        <NA>
    ## 5945 Gatorade Gatorlyte Zero Lemon Li     3 10.30        <NA>
    ## 5946    Glacier Freeze Gatorade 28 oz     4 10.30        <NA>
    ## 5947 Juice Orange Homestyle Tropicana     4 10.30        <NA>
    ## 5948 Cold Brew Chocolate Cream Starbu     3 10.30        <NA>
    ## 5949 Energy 222 Blue Raspberry Odysse     3 10.30        <NA>
    ## 5950 Gatorade Propel Berry Water 1 lt     4 10.30        <NA>
    ## 5951  Sparkling Lemonade Mango Kevita     4 10.30        <NA>
    ## 5952      Soda Mango Jarritos 12.5 oz     4 10.30        <NA>
    ## 5953 Energy Fast Twitch Watermelon St     3 10.30        <NA>
    ## 5954 Tea Green Peach Mango Celsius 12     3 10.30        <NA>
    ## 5955     Soda Pepsi Wild Cherry 20 Oz     4 10.30        <NA>
    ## 5956  Soda Rootbeer Zero Sugar Mug 20     4 10.30        <NA>
    ## 5957 Tea Pure Leaf Zero Sugar Sweet T     4 10.30        <NA>
    ## 5958   Frappuccino Caramel SB 13.7 oz     2 10.30        <NA>
    ## 5959   Frappuccino Vanilla SB 13.7 Oz     2 10.30        <NA>
    ## 5960          Juice Naked Berry Blast     2 10.30        <NA>
    ## 5961         Juice Protein Zone Naked     2 10.30        <NA>
    ## 5962    Naked Strawberry Banana Juice     2 10.30        <NA>
    ## 5963   Kombucha Ginger Kevita 15.2 oz     2 10.30        <NA>
    ## 5964    Glacier Cherry Gatorade 28 oz     3 10.30        <NA>
    ## 5965 Juice Raspberry Lemonade Tropica     3 10.30        <NA>
    ## 5966 Gatorade Propel Grape Water 1 lt     3 10.30        <NA>
    ## 5967 Gatorade Gatorlyte Cherry Lime 2     2 10.30        <NA>
    ## 5968 Gatorade Gatorlyte MixBerry 20 o     2 10.30        <NA>
    ## 5969    Tea Black Milk Ito En 11.8 oz     2 10.30        <NA>
    ## 5970    Water Aquafina Alumitek 16 oz     4 10.30        <NA>
    ## 5971 Cold Brew Vanilla Sweet Cream St     2 10.30        <NA>
    ## 5972          Soda Orange Crush 20 Oz     3 10.30        <NA>
    ## 5973     Soda Starry Lemon Lime 20 oz     3 10.30        <NA>
    ## 5974 Tea Blackberry Pure Leaf 18.5 oz     3 10.30        <NA>
    ## 5975   Rockstar Strawberry Punch 16oz     2 10.30        <NA>
    ## 5976         Juice Apple Dole 15.2 oz     3 10.30        <NA>
    ## 5977  Energy Fast Twitch Cool Blue 12     2 10.30        <NA>
    ## 5978   Energy Fast Twitch Grape 12 oz     2 10.30        <NA>
    ## 5979   Energy StrawLemon Celsius 12oz     2 10.30        <NA>
    ## 5980 Mountain Dew Kickstart Orange 16     2 10.30        <NA>
    ## 5981      Gatorade Berry Zero G 28 oz     2 10.30        <NA>
    ## 5982       Gatorade Fruit Punch 28 oz     2 10.30        <NA>
    ## 5983             Gatorade Orange 28oz     2 10.30        <NA>
    ## 5984 Sparkling lemonade Strawberry Ke     2 10.30        <NA>
    ## 5985                 Cornell Lemonade     3 10.30        <NA>
    ## 5986      Soda Guava Jarritos 12.5 oz     2 10.30        <NA>
    ## 5987   Soda Tamarind Jarritos 12.5 oz     2 10.30        <NA>
    ## 5988          Soda Mountain Dew 20 Oz     2 10.30        <NA>
    ## 5989     Tea Light Peach Lipton 20 oz     2 10.30        <NA>
    ## 5990    Frappuccino Coffee 13.7 oz SB     1 10.30        <NA>
    ## 5991   Soda Mandarin Jarritos 12.5 oz     2 10.30        <NA>
    ## 5992  Kombucha Pineapple Peach Kevita     1 10.30        <NA>
    ## 5993  Kombucha Rasp Lemon Kevita 15.2     1 10.30        <NA>
    ## 5994 Gatorade Gatorlyte Zero Fruit Pu     1 10.30        <NA>
    ## 5995         Yerba Mate Lemon 15.5 oz     1 10.30        <NA>
    ## 5996      Yerba Mate Tropical 15.5 oz     1 10.30        <NA>
    ## 5997     Starbucks Double Shot 6.5 Oz     1 10.30        <NA>
    ## 5998 Tea Green Matcha Milk Ito En 11.     1 10.30        <NA>
    ## 5999 Energy Cherry Limeade Celsius 16     1 10.30        <NA>
    ## 6000 Energy Dragonberry Celsius 16 oz     1 10.30        <NA>
    ## 6001 Energy Mango Tango Celsius 16 oz     1 10.30        <NA>
    ## 6002 Energy Passion Orange Guva Odyss     1 10.30        <NA>
    ## 6003    Rockstar Pure Zero Silver Ice     1 10.30        <NA>
    ## 6004 Energy Fuji Apple Pear Celsius 1     1 10.30        <NA>
    ## 6005        Gatorade Zero Grape 28 oz     1 10.30        <NA>
    ## 6006       Juice Grape Tropicana 11oz     1 10.30        <NA>
    ## 6007  Juice Lively Lemonade Tropicana     1 10.30        <NA>
    ## 6008             Water Gatorade 700ml     1 10.30        <NA>
    ## 6009     Soda Mountain Dew Zero 20 Oz     1 10.30        <NA>
    ## 6010 Tea Sweetened With Lemon Brisk 2     1 10.30        <NA>
    ## 6011 Tea Unsweetened Pure Leaf Lipton     1 10.30        <NA>
    ## 6012         Juice Lemonade Dole 20oz     1 10.30        <NA>
    ## 6013  Yogurt Mango Chobani Drink 7 oz     5 10.30        <NA>
    ## 6014 Yogurt Straw Banana Chobani Drin     5 10.30        <NA>
    ## 6015   Cornell Low Fat Chocolate Milk     5 10.30        <NA>
    ## 6016 Yogurt Black Cherry Greek Choban     6 10.30        <NA>
    ## 6017       Yogurt Mango Greek Chobani     6 10.30        <NA>
    ## 6018 Yogurt 0% Fat Vanilla Greek Oiko     5 10.30        <NA>
    ## 6019  Yogurt Flip Peanut Butter Dream     4 10.30        <NA>
    ## 6020   Milk Chocolate LF Cornell 8 Oz     5 10.30        <NA>
    ## 6021       Yogurt Peach Greek Chobani     4 10.30        <NA>
    ## 6022 Yogurt Flip Almond Coco Loco Cho     3 10.30        <NA>
    ## 6023       Yogurt Plain Greek Chobani     3 10.30        <NA>
    ## 6024       Cornell 2% Milk (70000380)     2 10.30        <NA>
    ## 6025  Cornell Dairy Strawberry Yogurt     3 10.30        <NA>
    ## 6026   Yogurt Blueberry Greek Chobani     2 10.30        <NA>
    ## 6027 Yogurt Strawberry Banana Greek C     2 10.30        <NA>
    ## 6028 Yogurt Mixed Berry Chobani Drink     1 10.30        <NA>
    ## 6029          Silk Chocolate Soy Milk     1 10.30        <NA>
    ## 6030       Cornell 2% Milk (70000381)     1 10.30        <NA>
    ## 6031       Cornell Dairy Peach Yogurt     1 10.30        <NA>
    ## 6032 Jelly Konjac Mango Pineapple Tas     7 10.30        <NA>
    ## 6033 Jelly Konjac Apple Grape Tastell     6 10.30        <NA>
    ## 6034 Jelly Konjac Double Berry Tastel     3 10.30        <NA>
    ## 6035      Jelly Konjac Peach Tastelli     3 10.30        <NA>
    ## 6036 Bar Choc Chip Cookie Dough Quest     2 10.30        <NA>
    ## 6037        Bar S'Mores Quest 2.12 oz     2 10.30        <NA>
    ## 6038  Cookie Choc Chip Lenny & Larrys     2 10.30        <NA>
    ## 6039 Bar Cookie & Cream Quest 2.12 oz     1 10.30        <NA>
    ## 6040 Bar CHOC Mint Builders Clif bar2     1 10.30        <NA>
    ## 6041   Fruit And Nut Delight Kind Bar     1 10.30        <NA>
    ## 6042       Bar Chocolate Chip Clifbar     1 10.30        <NA>
    ## 6043      Kind Almond And Coconut Bar     1 10.30        <NA>
    ## 6044   Kind Cranberry Almond Plus Bar     1 10.30        <NA>
    ## 6045 Bar That's It Apple+Blueberry 1.     1 10.30        <NA>
    ## 6046  Oats And Dark Chocolate Granola     1 10.30        <NA>
    ## 6047                      Fruit Whole    48 10.30        <NA>
    ## 6048        Fresh Cut Melon Fruit Cup     4 10.30        <NA>
    ## 6049    Fresh Cut Pineapple Fruit Cup     1 10.30        <NA>
    ## 6050    Chip Kettle Salt Vinegar 2 oz     2 10.30        <NA>
    ## 6051        Chip Kettle Sea Salt 2 oz     1 10.30        <NA>
    ## 6052     Chip Kettle Sweet Onion 2 oz     1 10.30        <NA>
    ## 6053 Chip Potato Kettle Honey BBQ 2oz     1 10.30        <NA>
    ## 6054 Chip Potato Dirty BBQ Mesquite 2     1 10.30        <NA>
    ## 6055   Chip Voodoo Limited Zapps 2 oz     1 10.30        <NA>
    ## 6056   Utz Honey Barbecue Chip 1.5 oz     1 10.30        <NA>
    ## 6057                 Utz Regular Chip     1 10.30        <NA>
    ## 6058       Chip Baked Jax  Utz 1.5 oz     1 10.30        <NA>
    ## 6059  Ice Cream Mochi Sweet Mango 1.5     3 10.30        <NA>
    ## 6060                 MochiCookCrm1.5o     2 10.30        <NA>
    ## 6061 Ice Cream Mochi Double Chocolate     2 10.30        <NA>
    ## 6062 Candy Milk Choc Caff Bar Awake 1     2 10.30        <NA>
    ## 6063             Orbit Wintermint Gum     2 10.30        <NA>
    ## 6064             Orbit Sweet Mint Gum     1 10.30        <NA>
    ## 6065 GF Sweet Street Choloate Brownie     2 10.30        <NA>
    ## 6066      Chewy Marshmallow GF 2.1 oz     2 10.30        <NA>
    ## 6067                  California Roll     3 10.30 grab 'n' go
    ## 6068                     Alaskan Roll     2 10.30 grab 'n' go
    ## 6069              Tempura Shrimp Roll     2 10.30 grab 'n' go
    ## 6070                         TSA Roll     2 10.30 grab 'n' go
    ## 6071               Golden Dragon Roll     2 10.30 grab 'n' go
    ## 6072                      Salmon Roll     2 10.30 grab 'n' go
    ## 6073                  Spicy Tuna Roll     2 10.30 grab 'n' go
    ## 6074            Hawaiian Volcano Roll     1 10.30 grab 'n' go
    ## 6075              Tempura Crunch Roll     1 10.30 grab 'n' go
    ## 6076                  Hawaiian Sunset     1 10.30 grab 'n' go
    ## 6077                     Avocado Roll     1 10.30 grab 'n' go
    ## 6078             Wrap Buffalo Chicken     3 10.30 grab 'n' go
    ## 6079              Wrap Chicken Caesar     3 10.30 grab 'n' go
    ## 6080 Sandwich Prosciutto & Mozzarella     2 10.30 grab 'n' go
    ## 6081 Sandwich Black Forrest Ham & Swi     1 10.30 grab 'n' go
    ## 6082 Sandwich Crispy Chicken Milanese     1 10.30 grab 'n' go
    ## 6083             Salad Chicken Caesar     1 10.30 grab 'n' go
    ## 6084                     Muffin Jumbo    24 10.30        <NA>
    ## 6085                           Cookie    24 10.30        <NA>
    ## 6086                Croissant Blue CC     5 10.30        <NA>
    ## 6087                   Croissant Choc     4 10.30        <NA>
    ## 6088               Croissant Straw CC     3 10.30        <NA>
    ## 6089                     Cinnamon Bun     3 10.30        <NA>
    ## 6090      Combo 16 oz Coffee & Muffin     1 10.30        <NA>
    ## 6091                            Scone     1 10.30        <NA>
    ## 6092                      Coffee Cake     1 10.30        <NA>
    ## 6093                   BowlSouthChick     3 10.30 grab 'n' go
    ## 6094                   BowlMedProtein     2 10.30 grab 'n' go
    ## 6095                 BowlSesAsianNood     2 10.30 grab 'n' go
    ## 6096                 BowlMexicanChick     1 10.30 grab 'n' go
    ## 6097                     PBJ on Wheat     8 10.30 grab 'n' go
    ## 6098                 GF ChicCaesarSld     3 10.30 grab 'n' go
    ## 6099                 GFSunButterJelly     1 10.30 grab 'n' go
    ## 6100       Quesadilla Deluxe Trillium   166 10.29     mexican
    ## 6101                Grilled Hamburger   102 10.29       grill
    ## 6102            Fried Chicken Tenders    89 10.29       grill
    ## 6103    Burrito Una Mano Trillium BYO    63 10.29     mexican
    ## 6104                     French Fries   135 10.29       grill
    ## 6105  Grilled Chicken Breast Sandwich    22 10.29       grill
    ## 6106                Quesadilla Cheese    19 10.29     mexican
    ## 6107 Trillium Grill Impossible Burger     9 10.29       grill
    ## 6108               Sweet Potato Fries    27 10.29       grill
    ## 6109                Black Bean Burger     6 10.29       grill
    ## 6110             Seared Salmon Burger     6 10.29       grill
    ## 6111             ADD Beef Patty $2.99    16 10.29       grill
    ## 6112               ADD Chicken Breast     3 10.29       grill
    ## 6113              Add Sausage 2 Patty     3 10.29       grill
    ## 6114                     Add Egg $.99     6 10.29       grill
    ## 6115                       ADD Cheese     9 10.29       grill
    ## 6116                1 Entree + 1 Side   206 10.29         wok
    ## 6117                1 Entree + 2 Side    94 10.29         wok
    ## 6118               Bowl Ramen Chicken    67 10.29       ramen
    ## 6119              2 Entrees + 2 Sides    23 10.29         wok
    ## 6120                  Bowl Ramen Tofu    13 10.29       ramen
    ## 6121          Side Vegetarian Lo Mein    12 10.29         wok
    ## 6122                     1 Wok Entree     6 10.29         wok
    ## 6123         Side White or Brown Rice     5 10.29         wok
    ## 6124  Side Vegetarian Fried Rice with     1 10.29         wok
    ## 6125                Burrito Breakfast    90 10.29        <NA>
    ## 6126              Small French Omelet    60 10.29        <NA>
    ## 6127 Egg Cheese Sausage Breakfast San    34 10.29        <NA>
    ## 6128 Egg Cheese Bacon Breakfast Sandw    32 10.29        <NA>
    ## 6129             Grand Slam Breakfast    17 10.29        <NA>
    ## 6130                        Add Bacon    29 10.29        <NA>
    ## 6131                         Two Eggs    14 10.29        <NA>
    ## 6132                   Pancake Single     8 10.29        <NA>
    ## 6133              Trillium Home Fries     1 10.29        <NA>
    ## 6134                   2 Slices Toast     1 10.29        <NA>
    ## 6135                            Toast     1 10.29        <NA>
    ## 6136      Create Your Pasta Bowl MEAT   108 10.29     italian
    ## 6137              Pizza with Toppings    39 10.29     italian
    ## 6138       Create Your Pasta Bowl VEG    25 10.29     italian
    ## 6139                     Pizza Cheese    22 10.29     italian
    ## 6140                   Add Extra Meat    20 10.29     italian
    ## 6141                 Burrito Bowl BYO   102 10.29     mexican
    ## 6142                      Single Taco     5 10.29     mexican
    ## 6143                       Side Salsa     1 10.29     mexican
    ## 6144      Add Extra Toppings Una Mano     1 10.29     mexican
    ## 6145               Salad by the Pound    67 10.29   salad bar
    ## 6146                       Soup 12 oz    58 10.29   salad bar
    ## 6147                        8 oz Soup    48 10.29   salad bar
    ## 6148              Soda Fountain 24 oz    30 10.29        <NA>
    ## 6149                  Coffee 12 oz SB    27 10.29        <NA>
    ## 6150                  Coffee 16 oz SB    23 10.29        <NA>
    ## 6151              Soda Fountain 16 oz    26 10.29        <NA>
    ## 6152                    Hot Tea 20 oz     3 10.29        <NA>
    ## 6153                 Side Potato Tots    18 10.29        <NA>
    ## 6154               Open Miscellaneous    12 10.29        <NA>
    ## 6155          Add Extra Protein $2.99     5 10.29        <NA>
    ## 6156             Water Aquafina 20 oz    19 10.29        <NA>
    ## 6157                Water Aquafina 1L    15 10.29        <NA>
    ## 6158            Soda Pepsi Diet 20 Oz    15 10.29        <NA>
    ## 6159    Yerba Mate Bluephoria 15.5 oz     7 10.29        <NA>
    ## 6160 Yerba Mate Enlighten Mint 15.5 o     7 10.29        <NA>
    ## 6161 Milk Chocolate LF BIG RED Refuel    13 10.29        <NA>
    ## 6162   Energy StrawGuava Celsius 12oz     8 10.29        <NA>
    ## 6163   Energy Artic Vibe Celsius 12oz     7 10.29        <NA>
    ## 6164 Yerba Mate Peach Revival 15.5 oz     6 10.29        <NA>
    ## 6165    Tea Black Milk Ito En 11.8 oz     6 10.29        <NA>
    ## 6166   Frappuccino Caramel SB 13.7 oz     5 10.29        <NA>
    ## 6167 Juice Fuji Apple Red Jacket 12oz     6 10.29        <NA>
    ## 6168  Tea Peach Pure Leaf Lipton 18.5     9 10.29        <NA>
    ## 6169 Energy Mango PassFruit Celsius 1     6 10.29        <NA>
    ## 6170              Gatorade Blue 28 oz     7 10.29        <NA>
    ## 6171 Juice Orange Premium Topicana 11     7 10.29        <NA>
    ## 6172     Water Life WTR Immune 700 ML     6 10.29        <NA>
    ## 6173  Starbucks Doubleshot Energy Van     4 10.29        <NA>
    ## 6174 Cold Brew Vanilla Sweet Cream St     5 10.29        <NA>
    ## 6175 Energy Mango Tango Celsius 16 oz     5 10.29        <NA>
    ## 6176  Kombucha Pineapple Peach Kevita     4 10.29        <NA>
    ## 6177 Gatorade Galcier Freeze Zero G 2     6 10.29        <NA>
    ## 6178                 Soda Pepsi 20 Oz     7 10.29        <NA>
    ## 6179      Yerba Mate Tropical 15.5 oz     4 10.29        <NA>
    ## 6180 Tea Green Matcha Milk Ito En 11.     4 10.29        <NA>
    ## 6181    Water Aquafina Alumitek 16 oz     8 10.29        <NA>
    ## 6182 Cold Brew Chocolate Cream Starbu     4 10.29        <NA>
    ## 6183  Energy Blue Crush Celsius 16 oz     4 10.29        <NA>
    ## 6184            Soda Pepsi  Zero 20oz     6 10.29        <NA>
    ## 6185 Tea Sweet W/ Le Pure Leaf Lipton     6 10.29        <NA>
    ## 6186 Energy Blue Raz Lemonade Celsius     4 10.29        <NA>
    ## 6187 Energy Fuji Apple Pear Celsius 1     4 10.29        <NA>
    ## 6188          Juice Naked Berry Blast     3 10.29        <NA>
    ## 6189        Juice Naked Green Machine     3 10.29        <NA>
    ## 6190 Juice Ocean Spray Cranbe Grape 1     6 10.29        <NA>
    ## 6191   Kombucha Ginger Kevita 15.2 oz     3 10.29        <NA>
    ## 6192      Soda Mango Jarritos 12.5 oz     5 10.29        <NA>
    ## 6193       Lipton Pure Leaf Sweet Tea     5 10.29        <NA>
    ## 6194 Tea Tea & Lemon Pure Leaf Lipton     5 10.29        <NA>
    ## 6195     Starbucks Double Shot 6.5 Oz     3 10.29        <NA>
    ## 6196 Tea Jasmine Green Unsweet Ito En     3 10.29        <NA>
    ## 6197        Gatorade Lemon Lime 28 oz     4 10.29        <NA>
    ## 6198    Glacier Cherry Gatorade 28 oz     4 10.29        <NA>
    ## 6199  Juice Lively Lemonade Tropicana     4 10.29        <NA>
    ## 6200 Gatorade Propel  Kiwi Straw Wate     4 10.29        <NA>
    ## 6201 Sparkling lemonade Strawberry Ke     4 10.29        <NA>
    ## 6202   Energy StrawLemon Celsius 12oz     3 10.29        <NA>
    ## 6203 Tea Green Peach Mango Celsius 12     3 10.29        <NA>
    ## 6204    Muscle Milk KO Chocolate 14oz     2 10.29        <NA>
    ## 6205 Mountain Dew Kickstart Black Che     3 10.29        <NA>
    ## 6206           Water Gatorade 33.8 oz     3 10.29        <NA>
    ## 6207  Soda Ginger Ale Schweppes 20 Oz     4 10.29        <NA>
    ## 6208          Soda Orange Crush 20 Oz     4 10.29        <NA>
    ## 6209     Soda Pepsi Wild Cherry 20 Oz     4 10.29        <NA>
    ## 6210   Tea Iced Rasberry Lipton 16 oz     4 10.29        <NA>
    ## 6211 Tea Unsweetened Pure Leaf Lipton     4 10.29        <NA>
    ## 6212         Juice Naked Blue Machine     2 10.29        <NA>
    ## 6213         Juice Protein Zone Naked     2 10.29        <NA>
    ## 6214    Naked Strawberry Banana Juice     2 10.29        <NA>
    ## 6215       Gatorade Fruit Punch 28 oz     3 10.29        <NA>
    ## 6216 Gatorade Propel Berry Water 1 lt     3 10.29        <NA>
    ## 6217 Gatorade Propel Grape Water 1 lt     3 10.29        <NA>
    ## 6218 Gatorade Gatorlyte Zero Fruit Pu     2 10.29        <NA>
    ## 6219         Yerba Mate Lemon 15.5 oz     2 10.29        <NA>
    ## 6220   Yerba Mate Revel Berry 15.5 oz     2 10.29        <NA>
    ## 6221     Soda Mountain Dew Zero 20 Oz     3 10.29        <NA>
    ## 6222          Soda Rootbeer Mug 20 oz     3 10.29        <NA>
    ## 6223 Tea Blackberry Pure Leaf 18.5 oz     3 10.29        <NA>
    ## 6224     Tea Light Peach Lipton 20 oz     3 10.29        <NA>
    ## 6225 Tea Unsweet Black W Lemon 18.5 o     3 10.29        <NA>
    ## 6226         Juice Apple Dole 15.2 oz     3 10.29        <NA>
    ## 6227   Ocean Spray Cranberry Cocktail     3 10.29        <NA>
    ## 6228 Energy Fast Twitch Watermelon St     2 10.29        <NA>
    ## 6229      Gatorade Berry Zero G 28 oz     2 10.29        <NA>
    ## 6230             Gatorade Orange 28oz     2 10.29        <NA>
    ## 6231    Glacier Freeze Gatorade 28 oz     2 10.29        <NA>
    ## 6232       Juice AppleTropicana 11 oz     2 10.29        <NA>
    ## 6233 Juice Orange Homestyle Tropicana     2 10.29        <NA>
    ## 6234 Kickstart Strawberry Start-up 16     2 10.29        <NA>
    ## 6235  Sparkling Lemonade Mango Kevita     2 10.29        <NA>
    ## 6236             Water Gatorade 700ml     2 10.29        <NA>
    ## 6237         Muscle Milk Choc PB 14oz     1 10.29        <NA>
    ## 6238          Soda Mountain Dew 20 Oz     2 10.29        <NA>
    ## 6239 Tea Sweetened With Lemon Brisk 2     2 10.29        <NA>
    ## 6240 Tea Unsweet Green Lipton 18.5 oz     2 10.29        <NA>
    ## 6241    Frappuccino Coffee 13.7 oz SB     1 10.29        <NA>
    ## 6242     Frappuccino Mocha 13.7 Oz SB     1 10.29        <NA>
    ## 6243   Frappuccino Vanilla SB 13.7 Oz     1 10.29        <NA>
    ## 6244         Juice Naked Mighty Mango     1 10.29        <NA>
    ## 6245          Naked Red Machine Juice     1 10.29        <NA>
    ## 6246   Soda Mandarin Jarritos 12.5 oz     2 10.29        <NA>
    ## 6247 Gatorade Gatorlyte Cherry Lime 2     1 10.29        <NA>
    ## 6248 Gatorade Gatorlyte Zero Lemon Li     1 10.29        <NA>
    ## 6249 Energy 222 Blue Raspberry Odysse     1 10.29        <NA>
    ## 6250 Energy Fruit Burst Celsius 16 oz     1 10.29        <NA>
    ## 6251 Energy Passion Orange Guva Odyss     1 10.29        <NA>
    ## 6252                 Cornell Lemonade     2 10.29        <NA>
    ## 6253    Rockstar Pure Zero Silver Ice     1 10.29        <NA>
    ## 6254  Energy Fast Twitch Cool Blue 12     1 10.29        <NA>
    ## 6255  Tea Green Rasp Acai  Celsius 12     1 10.29        <NA>
    ## 6256 Mountain Dew Kickstart Orange 16     1 10.29        <NA>
    ## 6257     Cider Apple Red Jacket 12 oz     1 10.29        <NA>
    ## 6258       Juice Grape Tropicana 11oz     1 10.29        <NA>
    ## 6259 Juice Raspberry Lemonade Tropica     1 10.29        <NA>
    ## 6260 Juice Zero Summer Splash Punch 1     1 10.29        <NA>
    ## 6261  Soda Rootbeer Zero Sugar Mug 20     1 10.29        <NA>
    ## 6262 Tea Pure Leaf Zero Sugar Sweet T     1 10.29        <NA>
    ## 6263        Juice Orange Dole 15.2 oz     1 10.29        <NA>
    ## 6264   Cornell Low Fat Chocolate Milk     9 10.29        <NA>
    ## 6265   Milk Chocolate LF Cornell 8 Oz    14 10.29        <NA>
    ## 6266 Yogurt Mixed Berry Chobani Drink     3 10.29        <NA>
    ## 6267 Yogurt Straw Banana Chobani Drin     3 10.29        <NA>
    ## 6268 Yogurt Black Cherry Greek Choban     4 10.29        <NA>
    ## 6269       Yogurt Mango Greek Chobani     3 10.29        <NA>
    ## 6270       Yogurt Plain Greek Chobani     3 10.29        <NA>
    ## 6271  Yogurt Strawberry Greek Chobani     3 10.29        <NA>
    ## 6272 Yogurt Flip Almond Coco Loco Cho     2 10.29        <NA>
    ## 6273  Yogurt Mango Chobani Drink 7 oz     1 10.29        <NA>
    ## 6274                  Cornell 2% Milk     2 10.29        <NA>
    ## 6275               Cornell Whole Milk     2 10.29        <NA>
    ## 6276  Yogurt Flip Peanut Butter Dream     1 10.29        <NA>
    ## 6277 Yogurt 0% Fat Vanilla Greek Oiko     1 10.29        <NA>
    ## 6278   Yogurt Blueberry Greek Chobani     1 10.29        <NA>
    ## 6279       Yogurt Peach Greek Chobani     1 10.29        <NA>
    ## 6280 Yogurt Strawberry Banana Greek C     1 10.29        <NA>
    ## 6281          Silk Chocolate Soy Milk     1 10.29        <NA>
    ## 6282  Cornell Dairy Strawberry Yogurt     1 10.29        <NA>
    ## 6283 Jelly Konjac Mango Pineapple Tas     4 10.29        <NA>
    ## 6284      Jelly Konjac Peach Tastelli     4 10.29        <NA>
    ## 6285 Jelly Konjac Double Berry Tastel     3 10.29        <NA>
    ## 6286       Bar Chocolate Chip Clifbar     3 10.29        <NA>
    ## 6287 Dark Choc Cherry Cashew Plus Kin     3 10.29        <NA>
    ## 6288        Bar S'Mores Quest 2.12 oz     2 10.29        <NA>
    ## 6289 Bar CHOC Mint Builders Clif bar2     2 10.29        <NA>
    ## 6290   Fruit And Nut Delight Kind Bar     2 10.29        <NA>
    ## 6291 Jelly Konjac Apple Grape Tastell     2 10.29        <NA>
    ## 6292 Bar Cookie & Cream Quest 2.12 oz     1 10.29        <NA>
    ## 6293  Cookie Choc Chip Lenny & Larrys     1 10.29        <NA>
    ## 6294 Cookie Double Choc Lenny & Larry     1 10.29        <NA>
    ## 6295      Kind Almond And Coconut Bar     1 10.29        <NA>
    ## 6296   Bar Blueberry Crisp Clif 2.4oz     1 10.29        <NA>
    ## 6297 Bar That's It Apple+Strawberry 1     1 10.29        <NA>
    ## 6298 Nature Valley Peanut Butter Gran     1 10.29        <NA>
    ## 6299        Oat And Honey Granola Bar     1 10.29        <NA>
    ## 6300   Fresh Cut Watermelon Fruit Cup     9 10.29        <NA>
    ## 6301        Fresh Cut Melon Fruit Cup     4 10.29        <NA>
    ## 6302    Fresh Cut Pineapple Fruit Cup     3 10.29        <NA>
    ## 6303 Chip Salt & Malt Vinegar Dirty 2     3 10.29        <NA>
    ## 6304       Jalapeno Heat Chips Kosher     3 10.29        <NA>
    ## 6305           Chips Dirty Sea Salted     2 10.29        <NA>
    ## 6306   Chip Voodoo Limited Zapps 2 oz     2 10.29        <NA>
    ## 6307    Utz Sour Cream and Onion Chip     2 10.29        <NA>
    ## 6308 Nuts Almonds Honey 1.5 oz Sahale     1 10.29        <NA>
    ## 6309       Chip Maui Onion Dirty 2 oz     1 10.29        <NA>
    ## 6310 Chip Potato Dirty BBQ Mesquite 2     1 10.29        <NA>
    ## 6311  Chips Cracked Pepper & Sea Salt     1 10.29        <NA>
    ## 6312                 Utz Regular Chip     1 10.29        <NA>
    ## 6313       Chip Baked Jax  Utz 1.5 oz     1 10.29        <NA>
    ## 6314         Pretzel Thin 2.12 oz Utz     1 10.29        <NA>
    ## 6315                      Fruit Whole    38 10.29        <NA>
    ## 6316             Orbit Wintermint Gum     4 10.29        <NA>
    ## 6317             Orbit Sweet Mint Gum     3 10.29        <NA>
    ## 6318     Chocolate Sunbutter Cups 2ct     2 10.29        <NA>
    ## 6319      Chewy Marshmallow GF 2.1 oz     4 10.29        <NA>
    ## 6320 GF Sweet Street Choloate Brownie     2 10.29        <NA>
    ## 6321  Ice Cream Mochi Sweet Mango 1.5     1 10.29        <NA>
    ## 6322                 MochiCookCrm1.5o     1 10.29        <NA>
    ## 6323               Golden Dragon Roll     3 10.29 grab 'n' go
    ## 6324              Tempura Shrimp Roll     2 10.29 grab 'n' go
    ## 6325                      Salmon Roll     2 10.29 grab 'n' go
    ## 6326                  California Roll     2 10.29 grab 'n' go
    ## 6327                     Alaskan Roll     1 10.29 grab 'n' go
    ## 6328              Tempura Crunch Roll     1 10.29 grab 'n' go
    ## 6329                         TSA Roll     1 10.29 grab 'n' go
    ## 6330                  Hawaiian Sunset     1 10.29 grab 'n' go
    ## 6331                  Spicy Tuna Roll     1 10.29 grab 'n' go
    ## 6332                     Avocado Roll     1 10.29 grab 'n' go
    ## 6333             Wrap Buffalo Chicken     5 10.29 grab 'n' go
    ## 6334              Wrap Chicken Caesar     5 10.29 grab 'n' go
    ## 6335     Sandwich Corned Beef & Swiss     3 10.29 grab 'n' go
    ## 6336 Sandwich Crispy Chicken Milanese     1 10.29 grab 'n' go
    ## 6337 Sandwich Prosciutto & Mozzarella     1 10.29 grab 'n' go
    ## 6338             Salad Chicken Caesar     1 10.29 grab 'n' go
    ## 6339                     Muffin Jumbo    28 10.29        <NA>
    ## 6340                           Cookie    26 10.29        <NA>
    ## 6341                   Croissant Choc     6 10.29        <NA>
    ## 6342                Croissant Blue CC     4 10.29        <NA>
    ## 6343               Croissant Straw CC     4 10.29        <NA>
    ## 6344                            Scone     4 10.29        <NA>
    ## 6345      Combo 16 oz Coffee & Muffin     2 10.29        <NA>
    ## 6346                     Cinnamon Bun     2 10.29        <NA>
    ## 6347                      Coffee Cake     1 10.29        <NA>
    ## 6348                 BowlChickAlfrPen     3 10.29 grab 'n' go
    ## 6349                   BowlSouthChick     3 10.29 grab 'n' go
    ## 6350                   BowlMedProtein     1 10.29 grab 'n' go
    ## 6351                 BowlMexicanChick     1 10.29 grab 'n' go
    ## 6352                   GF Turkey Sand     3 10.29 grab 'n' go
    ## 6353                 GFSunButterJelly     3 10.29 grab 'n' go
    ## 6354                     PBJ on Wheat     7 10.29 grab 'n' go
    ## 6355       Quesadilla Deluxe Trillium   156 10.28     mexican
    ## 6356                Grilled Hamburger    96 10.28       grill
    ## 6357            Fried Chicken Tenders    97 10.28       grill
    ## 6358    Burrito Una Mano Trillium BYO    71 10.28     mexican
    ## 6359                     French Fries   122 10.28       grill
    ## 6360  Grilled Chicken Breast Sandwich    15 10.28       grill
    ## 6361                Quesadilla Cheese    11 10.28     mexican
    ## 6362               Sweet Potato Fries    28 10.28       grill
    ## 6363             Seared Salmon Burger     8 10.28       grill
    ## 6364 Trillium Grill Impossible Burger     4 10.28       grill
    ## 6365             ADD Beef Patty $2.99    13 10.28       grill
    ## 6366                Black Bean Burger     4 10.28       grill
    ## 6367               ADD Chicken Breast     4 10.28       grill
    ## 6368              Add Sausage 2 Patty     6 10.28       grill
    ## 6369                     Add Egg $.99     5 10.28       grill
    ## 6370                       ADD Cheese     8 10.28       grill
    ## 6371                1 Entree + 1 Side   190 10.28         wok
    ## 6372                1 Entree + 2 Side    86 10.28         wok
    ## 6373               Bowl Ramen Chicken    65 10.28       ramen
    ## 6374              2 Entrees + 2 Sides    23 10.28         wok
    ## 6375                  Bowl Ramen Tofu    11 10.28       ramen
    ## 6376                     1 Wok Entree    10 10.28         wok
    ## 6377          Side Vegetarian Lo Mein    11 10.28         wok
    ## 6378                  Side Vegetables     2 10.28         wok
    ## 6379         Side White or Brown Rice     3 10.28         wok
    ## 6380  Side Vegetarian Fried Rice with     1 10.28         wok
    ## 6381      Create Your Pasta Bowl MEAT   127 10.28     italian
    ## 6382       Create Your Pasta Bowl VEG    38 10.28     italian
    ## 6383              Pizza with Toppings    34 10.28     italian
    ## 6384                     Pizza Cheese    22 10.28     italian
    ## 6385                   Add Extra Meat    27 10.28     italian
    ## 6386                Burrito Breakfast    89 10.28        <NA>
    ## 6387              Small French Omelet    57 10.28        <NA>
    ## 6388 Egg Cheese Sausage Breakfast San    35 10.28        <NA>
    ## 6389             Grand Slam Breakfast    17 10.28        <NA>
    ## 6390 Egg Cheese Bacon Breakfast Sandw    28 10.28        <NA>
    ## 6391                        Add Bacon    36 10.28        <NA>
    ## 6392                         Two Eggs    17 10.28        <NA>
    ## 6393              Trillium Home Fries     6 10.28        <NA>
    ## 6394                   2 Slices Toast     3 10.28        <NA>
    ## 6395                   Pancake Single     1 10.28        <NA>
    ## 6396                            Toast     1 10.28        <NA>
    ## 6397                         PC Jelly     1 10.28        <NA>
    ## 6398                 Burrito Bowl BYO   107 10.28     mexican
    ## 6399                      Single Taco     2 10.28     mexican
    ## 6400                   Side Guacamole     1 10.28     mexican
    ## 6401      Add Extra Toppings Una Mano     1 10.28     mexican
    ## 6402               Salad by the Pound    63 10.28   salad bar
    ## 6403                        8 oz Soup    46 10.28   salad bar
    ## 6404                       Soup 12 oz    40 10.28   salad bar
    ## 6405                  Coffee 16 oz SB    31 10.28        <NA>
    ## 6406              Soda Fountain 16 oz    32 10.28        <NA>
    ## 6407                  Coffee 12 oz SB    26 10.28        <NA>
    ## 6408              Soda Fountain 24 oz    28 10.28        <NA>
    ## 6409                    Hot Tea 20 oz     3 10.28        <NA>
    ## 6410                 Side Potato Tots    13 10.28        <NA>
    ## 6411               Open Miscellaneous    10 10.28        <NA>
    ## 6412            Soda Pepsi Diet 20 Oz    24 10.28        <NA>
    ## 6413             Water Aquafina 20 oz    26 10.28        <NA>
    ## 6414     Water Life WTR Immune 700 ML    11 10.28        <NA>
    ## 6415 Tea Golden Oolong Unsweet Ito En     9 10.28        <NA>
    ## 6416                 Soda Pepsi 20 Oz    14 10.28        <NA>
    ## 6417    Yerba Mate Bluephoria 15.5 oz     8 10.28        <NA>
    ## 6418 Yerba Mate Enlighten Mint 15.5 o     7 10.28        <NA>
    ## 6419    Naked Strawberry Banana Juice     6 10.28        <NA>
    ## 6420   Yerba Mate Revel Berry 15.5 oz     6 10.28        <NA>
    ## 6421      Yerba Mate Tropical 15.5 oz     6 10.28        <NA>
    ## 6422 Milk Chocolate LF BIG RED Refuel    11 10.28        <NA>
    ## 6423  Starbucks Doubleshot Energy Van     5 10.28        <NA>
    ## 6424   Tea Iced Rasberry Lipton 16 oz     9 10.28        <NA>
    ## 6425 Energy Mango PassFruit Celsius 1     6 10.28        <NA>
    ## 6426   Energy StrawGuava Celsius 12oz     6 10.28        <NA>
    ## 6427                Water Aquafina 1L     8 10.28        <NA>
    ## 6428     Soda Pepsi Wild Cherry 20 Oz     8 10.28        <NA>
    ## 6429 Energy Cherry Limeade Celsius 16     5 10.28        <NA>
    ## 6430          Juice Naked Berry Blast     4 10.28        <NA>
    ## 6431         Juice Naked Blue Machine     4 10.28        <NA>
    ## 6432  Soda Ginger Ale Schweppes 20 Oz     7 10.28        <NA>
    ## 6433         Yerba Mate Lemon 15.5 oz     4 10.28        <NA>
    ## 6434    Muscle Milk KO Chocolate 14oz     3 10.28        <NA>
    ## 6435            Soda Pepsi  Zero 20oz     6 10.28        <NA>
    ## 6436  Tea Peach Pure Leaf Lipton 18.5     6 10.28        <NA>
    ## 6437 Gatorade Galcier Freeze Zero G 2     5 10.28        <NA>
    ## 6438 Juice Orange Premium Topicana 11     5 10.28        <NA>
    ## 6439 Juice Raspberry Lemonade Tropica     5 10.28        <NA>
    ## 6440 Starbucks Doubleshot Ener Coffee     3 10.28        <NA>
    ## 6441 Starbucks Doubleshot Energy Moch     3 10.28        <NA>
    ## 6442   Energy Artic Vibe Celsius 12oz     4 10.28        <NA>
    ## 6443 Energy Blue Raz Lemonade Celsius     4 10.28        <NA>
    ## 6444   Energy StrawLemon Celsius 12oz     4 10.28        <NA>
    ## 6445 Tea Green Peach Mango Celsius 12     4 10.28        <NA>
    ## 6446   Frappuccino Vanilla SB 13.7 Oz     3 10.28        <NA>
    ## 6447         Juice Protein Zone Naked     3 10.28        <NA>
    ## 6448   Kombucha Ginger Kevita 15.2 oz     3 10.28        <NA>
    ## 6449 Tea Blackberry Pure Leaf 18.5 oz     5 10.28        <NA>
    ## 6450 Gatorade Gatorlyte Zero Fruit Pu     3 10.28        <NA>
    ## 6451 Yerba Mate Peach Revival 15.5 oz     3 10.28        <NA>
    ## 6452    Water Aquafina Alumitek 16 oz     6 10.28        <NA>
    ## 6453 Cold Brew Vanilla Sweet Cream St     3 10.28        <NA>
    ## 6454 Gatorade Propel Grape Water 1 lt     4 10.28        <NA>
    ## 6455 Gatorade Propel  Kiwi Straw Wate     4 10.28        <NA>
    ## 6456 Muscle Milk P40 Strawberry Cream     2 10.28        <NA>
    ## 6457 Muscle Milk PROSRS 40 Intense Va     2 10.28        <NA>
    ## 6458           Water Gatorade 33.8 oz     3 10.28        <NA>
    ## 6459          Soda Mountain Dew 20 Oz     4 10.28        <NA>
    ## 6460          Soda Orange Crush 20 Oz     4 10.28        <NA>
    ## 6461 Tea Tea & Lemon Pure Leaf Lipton     4 10.28        <NA>
    ## 6462 Tea Unsweet Green Lipton 18.5 oz     4 10.28        <NA>
    ## 6463         Juice Apple Dole 15.2 oz     4 10.28        <NA>
    ## 6464         Juice Naked Mighty Mango     2 10.28        <NA>
    ## 6465       Juice Naked PNCLDA 15.2 Oz     2 10.28        <NA>
    ## 6466  Kombucha Pineapple Peach Kevita     2 10.28        <NA>
    ## 6467       Juice AppleTropicana 11 oz     3 10.28        <NA>
    ## 6468  Sparkling Lemonade Mango Kevita     3 10.28        <NA>
    ## 6469 Gatorade Gatorlyte Glacier Freez     2 10.28        <NA>
    ## 6470     Starbucks Double Shot 6.5 Oz     2 10.28        <NA>
    ## 6471 Tea Green Matcha Milk Ito En 11.     2 10.28        <NA>
    ## 6472      Soda Mango Jarritos 12.5 oz     3 10.28        <NA>
    ## 6473  Energy Blue Crush Celsius 16 oz     2 10.28        <NA>
    ## 6474 Energy Mango Tango Celsius 16 oz     2 10.28        <NA>
    ## 6475 Red Jacket Strawberry Apple Juic     2 10.28        <NA>
    ## 6476  Soda Rootbeer Zero Sugar Mug 20     3 10.28        <NA>
    ## 6477     Tea Light Peach Lipton 20 oz     3 10.28        <NA>
    ## 6478 Tea Pure Leaf Zero Sugar Sweet T     3 10.28        <NA>
    ## 6479 Tea Unsweet Black W Lemon 18.5 o     3 10.28        <NA>
    ## 6480 Tea Unsweetened Pure Leaf Lipton     3 10.28        <NA>
    ## 6481  Energy Fast Twitch Cool Blue 12     2 10.28        <NA>
    ## 6482 Energy Fast Twitch Watermelon St     2 10.28        <NA>
    ## 6483 Energy Fuji Apple Pear Celsius 1     2 10.28        <NA>
    ## 6484  Tea Green Rasp Acai  Celsius 12     2 10.28        <NA>
    ## 6485       Gatorade Fruit Punch 28 oz     2 10.28        <NA>
    ## 6486        Gatorade Lemon Lime 28 oz     2 10.28        <NA>
    ## 6487        Gatorade Zero Grape 28 oz     2 10.28        <NA>
    ## 6488    Glacier Cherry Gatorade 28 oz     2 10.28        <NA>
    ## 6489 Juice Orange Homestyle Tropicana     2 10.28        <NA>
    ## 6490 Sparkling lemonade Strawberry Ke     2 10.28        <NA>
    ## 6491             Water Gatorade 700ml     2 10.28        <NA>
    ## 6492                 Cornell Lemonade     3 10.28        <NA>
    ## 6493      Soda Guava Jarritos 12.5 oz     2 10.28        <NA>
    ## 6494  Soda Pineapple Jarritos 12.5 oz     2 10.28        <NA>
    ## 6495         Muscle Milk Choc PB 14oz     1 10.28        <NA>
    ## 6496   Muscle Milk PP Chocolate 14 oz     1 10.28        <NA>
    ## 6497       Lipton Pure Leaf Sweet Tea     2 10.28        <NA>
    ## 6498     Soda Mountain Dew Zero 20 Oz     2 10.28        <NA>
    ## 6499          Soda Rootbeer Mug 20 oz     2 10.28        <NA>
    ## 6500 Tea Sweetened With Lemon Brisk 2     2 10.28        <NA>
    ## 6501   Ocean Spray Cranberry Cocktail     2 10.28        <NA>
    ## 6502    Frappuccino Coffee 13.7 oz SB     1 10.28        <NA>
    ## 6503     Frappuccino Mocha 13.7 Oz SB     1 10.28        <NA>
    ## 6504        Juice Naked Green Machine     1 10.28        <NA>
    ## 6505 Juice Ocean Spray Cranbe Grape 1     2 10.28        <NA>
    ## 6506   Soda Mandarin Jarritos 12.5 oz     2 10.28        <NA>
    ## 6507  Kombucha Rasp Lemon Kevita 15.2     1 10.28        <NA>
    ## 6508 Gatorade Gatorlyte MixBerry 20 o     1 10.28        <NA>
    ## 6509  Gatorade Gatorlyte Orange 20 oz     1 10.28        <NA>
    ## 6510 Gatorade Gatorlyte Strawberry Ki     1 10.28        <NA>
    ## 6511 Gatorade Gatorlyte Zero Lemon Li     1 10.28        <NA>
    ## 6512   Rockstar Energy Pure Zero PNCH     1 10.28        <NA>
    ## 6513    Tea Black Milk Ito En 11.8 oz     1 10.28        <NA>
    ## 6514 Cold Brew Chocolate Cream Starbu     1 10.28        <NA>
    ## 6515 Energy 222 Blue Raspberry Odysse     1 10.28        <NA>
    ## 6516 Energy Orangesicle Celsius 16 oz     1 10.28        <NA>
    ## 6517 Energy Passion Orange Guva Odyss     1 10.28        <NA>
    ## 6518    Rockstar Pure Zero Silver Ice     1 10.28        <NA>
    ## 6519     Cider Apple Red Jacket 12 oz     1 10.28        <NA>
    ## 6520      Gatorade Berry Zero G 28 oz     1 10.28        <NA>
    ## 6521              Gatorade Blue 28 oz     1 10.28        <NA>
    ## 6522             Gatorade Orange 28oz     1 10.28        <NA>
    ## 6523    Glacier Freeze Gatorade 28 oz     1 10.28        <NA>
    ## 6524  Juice Lively Lemonade Tropicana     1 10.28        <NA>
    ## 6525 Kickstart Strawberry Start-up 16     1 10.28        <NA>
    ## 6526 Gatorade Propel Berry Water 1 lt     1 10.28        <NA>
    ## 6527   Soda Tamarind Jarritos 12.5 oz     1 10.28        <NA>
    ## 6528     Soda Starry Lemon Lime 20 oz     1 10.28        <NA>
    ## 6529         Juice Lemonade Dole 20oz     1 10.28        <NA>
    ## 6530   Cornell Low Fat Chocolate Milk     9 10.28        <NA>
    ## 6531 Yogurt Straw Banana Chobani Drin     7 10.28        <NA>
    ## 6532 Yogurt 0% Fat Vanilla Greek Oiko     7 10.28        <NA>
    ## 6533 Yogurt Flip Almond Coco Loco Cho     6 10.28        <NA>
    ## 6534  Yogurt Mango Chobani Drink 7 oz     3 10.28        <NA>
    ## 6535 Yogurt Mixed Berry Chobani Drink     3 10.28        <NA>
    ## 6536               Cornell Whole Milk     5 10.28        <NA>
    ## 6537 Yogurt Strawberry Banana Greek C     4 10.28        <NA>
    ## 6538  Yogurt Flip Peanut Butter Dream     3 10.28        <NA>
    ## 6539   Milk Chocolate LF Cornell 8 Oz     4 10.28        <NA>
    ## 6540       Yogurt Peach Greek Chobani     3 10.28        <NA>
    ## 6541  Yogurt Strawberry Greek Chobani     3 10.28        <NA>
    ## 6542       Cornell 2% Milk (70000380)     2 10.28        <NA>
    ## 6543 Yogurt Black Cherry Greek Choban     2 10.28        <NA>
    ## 6544   Yogurt Blueberry Greek Chobani     2 10.28        <NA>
    ## 6545       Yogurt Mango Greek Chobani     2 10.28        <NA>
    ## 6546       Cornell 2% Milk (70000381)     2 10.28        <NA>
    ## 6547       Yogurt Plain Greek Chobani     1 10.28        <NA>
    ## 6548          Silk Chocolate Soy Milk     1 10.28        <NA>
    ## 6549  Cornell Dairy Strawberry Yogurt     1 10.28        <NA>
    ## 6550 Jelly Konjac Mango Pineapple Tas     6 10.28        <NA>
    ## 6551 Jelly Konjac Apple Grape Tastell     5 10.28        <NA>
    ## 6552      Jelly Konjac Peach Tastelli     5 10.28        <NA>
    ## 6553  Bar Frosted Birthday Cake Quest     3 10.28        <NA>
    ## 6554 Jelly Konjac Double Berry Tastel     3 10.28        <NA>
    ## 6555        Bar S'Mores Quest 2.12 oz     2 10.28        <NA>
    ## 6556 Bar That's It Apple+Blueberry 1.     2 10.28        <NA>
    ## 6557 Bar That's It Apple+Strawberry 1     2 10.28        <NA>
    ## 6558 Bar Choc Chip Cookie Dough Quest     1 10.28        <NA>
    ## 6559 Bar Cookie & Cream Quest 2.12 oz     1 10.28        <NA>
    ## 6560       Bar Chocolate Chip Clifbar     1 10.28        <NA>
    ## 6561      Kind Almond And Coconut Bar     1 10.28        <NA>
    ## 6562        Oat And Honey Granola Bar     1 10.28        <NA>
    ## 6563   Fresh Cut Watermelon Fruit Cup     8 10.28        <NA>
    ## 6564        Fresh Cut Melon Fruit Cup     3 10.28        <NA>
    ## 6565    Fresh Cut Pineapple Fruit Cup     2 10.28        <NA>
    ## 6566                      Fruit Whole    49 10.28        <NA>
    ## 6567 Chip Salt & Malt Vinegar Dirty 2     3 10.28        <NA>
    ## 6568        Chip Kettle Sea Salt 2 oz     2 10.28        <NA>
    ## 6569 Chip Potato Kettle Honey BBQ 2oz     2 10.28        <NA>
    ## 6570     Chip Funky Fusion Dirty 2 oz     1 10.28        <NA>
    ## 6571       Chip Maui Onion Dirty 2 oz     1 10.28        <NA>
    ## 6572  Chips Cracked Pepper & Sea Salt     1 10.28        <NA>
    ## 6573  Chip Sour Cream And Onion Dirty     1 10.28        <NA>
    ## 6574   Utz Honey Barbecue Chip 1.5 oz     1 10.28        <NA>
    ## 6575          Utz Salt N Vinegar Chip     1 10.28        <NA>
    ## 6576       Chip Baked Jax  Utz 1.5 oz     1 10.28        <NA>
    ## 6577      Chewy Marshmallow GF 2.1 oz     4 10.28        <NA>
    ## 6578 GF Sweet Street Choloate Brownie     1 10.28        <NA>
    ## 6579             Orbit Wintermint Gum     3 10.28        <NA>
    ## 6580     Chocolate Sunbutter Cups 2ct     1 10.28        <NA>
    ## 6581 Ice Cream Mochi Double Chocolate     2 10.28        <NA>
    ## 6582  Ice Cream Mochi Sweet Mango 1.5     1 10.28        <NA>
    ## 6583                     Muffin Jumbo    26 10.28        <NA>
    ## 6584                           Cookie    21 10.28        <NA>
    ## 6585                   Croissant Choc     5 10.28        <NA>
    ## 6586               Croissant Straw CC     2 10.28        <NA>
    ## 6587      Combo 16 oz Coffee & Muffin     2 10.28        <NA>
    ## 6588                     Cinnamon Bun     2 10.28        <NA>
    ## 6589                      Coffee Cake     3 10.28        <NA>
    ## 6590                            Scone     2 10.28        <NA>
    ## 6591                Croissant Blue CC     1 10.28        <NA>
    ## 6592                 BowlMexicanChick     5 10.28 grab 'n' go
    ## 6593                   BowlMedProtein     3 10.28 grab 'n' go
    ## 6594                   BowlSouthChick     3 10.28 grab 'n' go
    ## 6595                 BowlSesAsianNood     1 10.28 grab 'n' go
    ## 6596                     PBJ on Wheat     9 10.28 grab 'n' go
    ## 6597                   GF Turkey Sand     1 10.28 grab 'n' go
    ## 6598               Golden Dragon Roll     3 10.28 grab 'n' go
    ## 6599              Tempura Shrimp Roll     2 10.28 grab 'n' go
    ## 6600                         TSA Roll     2 10.28 grab 'n' go
    ## 6601              Tempura Crunch Roll     1 10.28 grab 'n' go
    ## 6602                  Hawaiian Sunset     1 10.28 grab 'n' go
    ## 6603                      Salmon Roll     1 10.28 grab 'n' go
    ## 6604                  Spicy Tuna Roll     1 10.28 grab 'n' go
    ## 6605                     Avocado Roll     1 10.28 grab 'n' go
    ## 6606                  California Roll     1 10.28 grab 'n' go
    ## 6607              Wrap Chicken Caesar     3 10.28 grab 'n' go
    ## 6608 Sandwich Black Forrest Ham & Swi     2 10.28 grab 'n' go
    ## 6609     Sandwich Corned Beef & Swiss     2 10.28 grab 'n' go
    ## 6610             Salad Chicken Caesar     2 10.28 grab 'n' go
    ## 6611 Sandwich Crispy Chicken Milanese     1 10.28 grab 'n' go
    ## 6612             Wrap Buffalo Chicken     1 10.28 grab 'n' go
    ## 6613       Quesadilla Deluxe Trillium   110 10.25     mexican
    ## 6614                Grilled Hamburger    71 10.25       grill
    ## 6615    Burrito Una Mano Trillium BYO    57 10.25     mexican
    ## 6616            Fried Chicken Tenders    59 10.25       grill
    ## 6617                     French Fries    82 10.25       grill
    ## 6618  Grilled Chicken Breast Sandwich     8 10.25       grill
    ## 6619             Seared Salmon Burger     8 10.25       grill
    ## 6620                Quesadilla Cheese     8 10.25     mexican
    ## 6621 Trillium Grill Impossible Burger     6 10.25       grill
    ## 6622                Black Bean Burger     4 10.25       grill
    ## 6623             ADD Beef Patty $2.99     4 10.25       grill
    ## 6624               ADD Chicken Breast     2 10.25       grill
    ## 6625                       ADD Cheese     4 10.25       grill
    ## 6626                     Add Egg $.99     1 10.25       grill
    ## 6627                1 Entree + 1 Side   105 10.25         wok
    ## 6628                1 Entree + 2 Side    59 10.25         wok
    ## 6629               Bowl Ramen Chicken    44 10.25       ramen
    ## 6630              2 Entrees + 2 Sides    19 10.25         wok
    ## 6631                  Bowl Ramen Tofu     9 10.25       ramen
    ## 6632          Side Vegetarian Lo Mein     8 10.25         wok
    ## 6633                     1 Wok Entree     4 10.25         wok
    ## 6634  Side Vegetarian Fried Rice with     3 10.25         wok
    ## 6635         Side White or Brown Rice     3 10.25         wok
    ## 6636      Side Vegetable Spring Rolls     1 10.25         wok
    ## 6637                Burrito Breakfast    80 10.25        <NA>
    ## 6638              Small French Omelet    51 10.25        <NA>
    ## 6639 Egg Cheese Sausage Breakfast San    33 10.25        <NA>
    ## 6640 Egg Cheese Bacon Breakfast Sandw    30 10.25        <NA>
    ## 6641             Grand Slam Breakfast    16 10.25        <NA>
    ## 6642                        Add Bacon    18 10.25        <NA>
    ## 6643                         Two Eggs     8 10.25        <NA>
    ## 6644              Trillium Home Fries     3 10.25        <NA>
    ## 6645                   Pancake Single     2 10.25        <NA>
    ## 6646                   2 Slices Toast     1 10.25        <NA>
    ## 6647      Create Your Pasta Bowl MEAT    80 10.25     italian
    ## 6648              Pizza with Toppings    24 10.25     italian
    ## 6649       Create Your Pasta Bowl VEG    14 10.25     italian
    ## 6650                     Pizza Cheese    11 10.25     italian
    ## 6651                   Add Extra Meat    12 10.25     italian
    ## 6652                 Burrito Bowl BYO    50 10.25     mexican
    ## 6653                      Single Taco     4 10.25     mexican
    ## 6654                   Side Guacamole     1 10.25     mexican
    ## 6655      Add Extra Toppings Una Mano     1 10.25     mexican
    ## 6656               Salad by the Pound    39 10.25   salad bar
    ## 6657                        8 oz Soup    38 10.25   salad bar
    ## 6658                       Soup 12 oz    25 10.25   salad bar
    ## 6659              Soda Fountain 24 oz    36 10.25        <NA>
    ## 6660                  Coffee 16 oz SB    28 10.25        <NA>
    ## 6661              Soda Fountain 16 oz    28 10.25        <NA>
    ## 6662                  Coffee 12 oz SB    20 10.25        <NA>
    ## 6663                    Hot Tea 20 oz     1 10.25        <NA>
    ## 6664                 Side Potato Tots    27 10.25        <NA>
    ## 6665               Open Miscellaneous     5 10.25        <NA>
    ## 6666         Juice Naked Mighty Mango     7 10.25        <NA>
    ## 6667  Kombucha Rasp Lemon Kevita 15.2     6 10.25        <NA>
    ## 6668             Water Aquafina 20 oz    11 10.25        <NA>
    ## 6669 Yerba Mate Peach Revival 15.5 oz     6 10.25        <NA>
    ## 6670 Yerba Mate Enlighten Mint 15.5 o     5 10.25        <NA>
    ## 6671 Tea Jasmine Green Unsweet Ito En     5 10.25        <NA>
    ## 6672            Soda Pepsi  Zero 20oz     8 10.25        <NA>
    ## 6673 Milk Chocolate LF BIG RED Refuel     9 10.25        <NA>
    ## 6674        Juice Naked Green Machine     4 10.25        <NA>
    ## 6675 Juice Orange Homestyle Tropicana     6 10.25        <NA>
    ## 6676                 Soda Pepsi 20 Oz     7 10.25        <NA>
    ## 6677            Soda Pepsi Diet 20 Oz     7 10.25        <NA>
    ## 6678     Water Life WTR Immune 700 ML     5 10.25        <NA>
    ## 6679    Yerba Mate Bluephoria 15.5 oz     4 10.25        <NA>
    ## 6680         Yerba Mate Lemon 15.5 oz     4 10.25        <NA>
    ## 6681      Yerba Mate Tropical 15.5 oz     4 10.25        <NA>
    ## 6682 Juice Fuji Apple Red Jacket 12oz     4 10.25        <NA>
    ## 6683   Tea Iced Rasberry Lipton 16 oz     6 10.25        <NA>
    ## 6684              Gatorade Blue 28 oz     5 10.25        <NA>
    ## 6685 Gatorade Propel  Kiwi Straw Wate     5 10.25        <NA>
    ## 6686   Energy Artic Vibe Celsius 12oz     4 10.25        <NA>
    ## 6687 Energy Mango PassFruit Celsius 1     4 10.25        <NA>
    ## 6688    Water Aquafina Alumitek 16 oz     7 10.25        <NA>
    ## 6689       Juice Naked PNCLDA 15.2 Oz     3 10.25        <NA>
    ## 6690 Mountain Dew Kickstart Orange 16     4 10.25        <NA>
    ## 6691 Tea Pure Leaf Zero Sugar Sweet T     5 10.25        <NA>
    ## 6692   Yerba Mate Revel Berry 15.5 oz     3 10.25        <NA>
    ## 6693     Starbucks Double Shot 6.5 Oz     3 10.25        <NA>
    ## 6694    Tea Black Milk Ito En 11.8 oz     3 10.25        <NA>
    ## 6695        Gatorade Lemon Lime 28 oz     4 10.25        <NA>
    ## 6696       Juice AppleTropicana 11 oz     4 10.25        <NA>
    ## 6697 Juice Orange Premium Topicana 11     4 10.25        <NA>
    ## 6698 Energy 222 Pineapple Mango Odyss     3 10.25        <NA>
    ## 6699 Energy Fuji Apple Pear Celsius 1     3 10.25        <NA>
    ## 6700  Energy Kiwi Guava Celsius 12 oz     3 10.25        <NA>
    ## 6701   Energy StrawLemon Celsius 12oz     3 10.25        <NA>
    ## 6702 Tea Green Peach Mango Celsius 12     3 10.25        <NA>
    ## 6703                Water Aquafina 1L     4 10.25        <NA>
    ## 6704       Lipton Pure Leaf Sweet Tea     4 10.25        <NA>
    ## 6705 Tea Sweet W/ Le Pure Leaf Lipton     4 10.25        <NA>
    ## 6706     Cider Apple Red Jacket 12 oz     3 10.25        <NA>
    ## 6707 Starbucks Doubleshot Ener Coffee     2 10.25        <NA>
    ## 6708    Frappuccino Coffee 13.7 oz SB     2 10.25        <NA>
    ## 6709    Naked Strawberry Banana Juice     2 10.25        <NA>
    ## 6710 Juice Ocean Spray Cranbe Grape 1     4 10.25        <NA>
    ## 6711       Gatorade Fruit Punch 28 oz     3 10.25        <NA>
    ## 6712 Gatorade Propel Berry Water 1 lt     3 10.25        <NA>
    ## 6713 Gatorade Gatorlyte Glacier Freez     2 10.25        <NA>
    ## 6714 Gatorade Gatorlyte MixBerry 20 o     2 10.25        <NA>
    ## 6715 Cold Brew Chocolate Cream Starbu     2 10.25        <NA>
    ## 6716        Gatorade Zero Grape 28 oz     2 10.25        <NA>
    ## 6717  Juice Lively Lemonade Tropicana     2 10.25        <NA>
    ## 6718 Juice Raspberry Lemonade Tropica     2 10.25        <NA>
    ## 6719 Gatorade Propel Grape Water 1 lt     2 10.25        <NA>
    ## 6720                 Cornell Lemonade     3 10.25        <NA>
    ## 6721         Muscle Milk Choc PB 14oz     1 10.25        <NA>
    ## 6722    Muscle Milk KO Chocolate 14oz     1 10.25        <NA>
    ## 6723          Soda Mountain Dew 20 Oz     2 10.25        <NA>
    ## 6724  Soda Rootbeer Zero Sugar Mug 20     2 10.25        <NA>
    ## 6725 Tea Blackberry Pure Leaf 18.5 oz     2 10.25        <NA>
    ## 6726  Tea Peach Pure Leaf Lipton 18.5     2 10.25        <NA>
    ## 6727 Tea Tea & Lemon Pure Leaf Lipton     2 10.25        <NA>
    ## 6728 Tea Unsweetened Pure Leaf Lipton     2 10.25        <NA>
    ## 6729 Starbucks Doubleshot Energy Moch     1 10.25        <NA>
    ## 6730         Juice Apple Dole 15.2 oz     2 10.25        <NA>
    ## 6731     Frappuccino Mocha 13.7 Oz SB     1 10.25        <NA>
    ## 6732   Frappuccino Vanilla SB 13.7 Oz     1 10.25        <NA>
    ## 6733          Juice Naked Berry Blast     1 10.25        <NA>
    ## 6734         Juice Naked Blue Machine     1 10.25        <NA>
    ## 6735         Juice Protein Zone Naked     1 10.25        <NA>
    ## 6736          Naked Red Machine Juice     1 10.25        <NA>
    ## 6737   Soda Mandarin Jarritos 12.5 oz     2 10.25        <NA>
    ## 6738  Kombucha Pineapple Peach Kevita     1 10.25        <NA>
    ## 6739 Gatorade Gatorlyte Strawberry Ki     1 10.25        <NA>
    ## 6740 Gatorade Gatorlyte Zero Fruit Pu     1 10.25        <NA>
    ## 6741 Cold Brew Vanilla Sweet Cream St     1 10.25        <NA>
    ## 6742 Energy 222 Blue Raspberry Odysse     1 10.25        <NA>
    ## 6743  Energy Blue Crush Celsius 16 oz     1 10.25        <NA>
    ## 6744 Energy Cherry Limeade Celsius 16     1 10.25        <NA>
    ## 6745 Energy Fruit Burst Celsius 16 oz     1 10.25        <NA>
    ## 6746 Energy Orangesicle Celsius 16 oz     1 10.25        <NA>
    ## 6747 Energy Fast Twitch Watermelon St     1 10.25        <NA>
    ## 6748   Energy StrawGuava Celsius 12oz     1 10.25        <NA>
    ## 6749 Mountain Dew Kickstart Black Che     1 10.25        <NA>
    ## 6750           Water Gatorade 33.8 oz     1 10.25        <NA>
    ## 6751 Tea Unsweet Green Lipton 18.5 oz     1 10.25        <NA>
    ## 6752      Gatorade Berry Zero G 28 oz     1 10.25        <NA>
    ## 6753 Gatorade Galcier Freeze Zero G 2     1 10.25        <NA>
    ## 6754             Gatorade Orange 28oz     1 10.25        <NA>
    ## 6755    Glacier Cherry Gatorade 28 oz     1 10.25        <NA>
    ## 6756    Glacier Freeze Gatorade 28 oz     1 10.25        <NA>
    ## 6757       Juice Grape Tropicana 11oz     1 10.25        <NA>
    ## 6758 Kickstart Strawberry Start-up 16     1 10.25        <NA>
    ## 6759  Sparkling Lemonade Mango Kevita     1 10.25        <NA>
    ## 6760 Sparkling lemonade Strawberry Ke     1 10.25        <NA>
    ## 6761             Water Gatorade 700ml     1 10.25        <NA>
    ## 6762          Soda Orange Crush 20 Oz     1 10.25        <NA>
    ## 6763     Soda Pepsi Wild Cherry 20 Oz     1 10.25        <NA>
    ## 6764          Soda Rootbeer Mug 20 oz     1 10.25        <NA>
    ## 6765     Soda Starry Lemon Lime 20 oz     1 10.25        <NA>
    ## 6766 Tea Unsweet Black W Lemon 18.5 o     1 10.25        <NA>
    ## 6767 Yogurt Flip Almond Coco Loco Cho     8 10.25        <NA>
    ## 6768   Cornell Low Fat Chocolate Milk     5 10.25        <NA>
    ## 6769  Yogurt Mango Chobani Drink 7 oz     4 10.25        <NA>
    ## 6770 Yogurt 0% Fat Vanilla Greek Oiko     4 10.25        <NA>
    ## 6771   Milk Chocolate LF Cornell 8 Oz     5 10.25        <NA>
    ## 6772 Yogurt Mixed Berry Chobani Drink     2 10.25        <NA>
    ## 6773 Yogurt Straw Banana Chobani Drin     2 10.25        <NA>
    ## 6774               Cornell Whole Milk     4 10.25        <NA>
    ## 6775 Yogurt Black Cherry Greek Choban     2 10.25        <NA>
    ## 6776       Yogurt Plain Greek Chobani     2 10.25        <NA>
    ## 6777 Yogurt Strawberry Banana Greek C     2 10.25        <NA>
    ## 6778                  Cornell 2% Milk     2 10.25        <NA>
    ## 6779   Yogurt Blueberry Greek Chobani     1 10.25        <NA>
    ## 6780       Yogurt Mango Greek Chobani     1 10.25        <NA>
    ## 6781       Yogurt Peach Greek Chobani     1 10.25        <NA>
    ## 6782            8 oz Vanilla Soy Silk     1 10.25        <NA>
    ## 6783          Silk Chocolate Soy Milk     1 10.25        <NA>
    ## 6784      Jelly Konjac Peach Tastelli     3 10.25        <NA>
    ## 6785 Bar Choc Chip Cookie Dough Quest     2 10.25        <NA>
    ## 6786 Jelly Konjac Apple Grape Tastell     2 10.25        <NA>
    ## 6787 Jelly Konjac Mango Pineapple Tas     2 10.25        <NA>
    ## 6788   Bar Crunchy Peanut Butter Clif     2 10.25        <NA>
    ## 6789  Cookie Choc Chip Lenny & Larrys     1 10.25        <NA>
    ## 6790   Fruit And Nut Delight Kind Bar     1 10.25        <NA>
    ## 6791      Kind Almond And Coconut Bar     1 10.25        <NA>
    ## 6792   Bar Blueberry Crisp Clif 2.4oz     1 10.25        <NA>
    ## 6793 Bar That's It Apple+Strawberry 1     1 10.25        <NA>
    ## 6794                      Fruit Whole    33 10.25        <NA>
    ## 6795           Chips Dirty Sea Salted     3 10.25        <NA>
    ## 6796   Chip Voodoo Limited Zapps 2 oz     2 10.25        <NA>
    ## 6797                 Utz Regular Chip     2 10.25        <NA>
    ## 6798       Chip Baked Jax  Utz 1.5 oz     2 10.25        <NA>
    ## 6799        Chip Kettle Sea Salt 2 oz     1 10.25        <NA>
    ## 6800 Chip Potato Dirty BBQ Mesquite 2     1 10.25        <NA>
    ## 6801 Chip Salt & Malt Vinegar Dirty 2     1 10.25        <NA>
    ## 6802  Chips Cracked Pepper & Sea Salt     1 10.25        <NA>
    ## 6803          Utz Salt N Vinegar Chip     1 10.25        <NA>
    ## 6804   Fresh Cut Watermelon Fruit Cup     3 10.25        <NA>
    ## 6805        Fresh Cut Melon Fruit Cup     2 10.25        <NA>
    ## 6806      Chewy Marshmallow GF 2.1 oz     4 10.25        <NA>
    ## 6807 GF Sweet Street Choloate Brownie     2 10.25        <NA>
    ## 6808   Candy CHOC Cara Caff Bar Awake     2 10.25        <NA>
    ## 6809             Orbit Sweet Mint Gum     1 10.25        <NA>
    ## 6810             Orbit Wintermint Gum     1 10.25        <NA>
    ## 6811                 MochiCookCrm1.5o     3 10.25        <NA>
    ## 6812  Ice Cream Mochi Sweet Mango 1.5     1 10.25        <NA>
    ## 6813                     Muffin Jumbo    20 10.25        <NA>
    ## 6814                           Cookie    16 10.25        <NA>
    ## 6815               Croissant Straw CC     3 10.25        <NA>
    ## 6816                     Cinnamon Bun     3 10.25        <NA>
    ## 6817                Croissant Blue CC     2 10.25        <NA>
    ## 6818                   Croissant Choc     2 10.25        <NA>
    ## 6819                      Coffee Cake     3 10.25        <NA>
    ## 6820      Combo 16 oz Coffee & Muffin     1 10.25        <NA>
    ## 6821                 GF ChicCaesarSld     2 10.25 grab 'n' go
    ## 6822                 GFSunButterJelly     1 10.25 grab 'n' go
    ## 6823                 BowlSesAsianNood     2 10.25 grab 'n' go
    ## 6824                     Alaskan Roll     2 10.25 grab 'n' go
    ## 6825              Tempura Crunch Roll     1 10.25 grab 'n' go
    ## 6826                         TSA Roll     1 10.25 grab 'n' go
    ## 6827               Golden Dragon Roll     1 10.25 grab 'n' go
    ## 6828                  Hawaiian Sunset     1 10.25 grab 'n' go
    ## 6829                      Salmon Roll     1 10.25 grab 'n' go
    ## 6830                  Spicy Tuna Roll     1 10.25 grab 'n' go
    ## 6831                     Avocado Roll     1 10.25 grab 'n' go
    ## 6832              Wrap Chicken Caesar     2 10.25 grab 'n' go
    ## 6833             Wrap Buffalo Chicken     1 10.25 grab 'n' go
    ## 6834       Quesadilla Deluxe Trillium   165 10.24     mexican
    ## 6835                Grilled Hamburger   107 10.24       grill
    ## 6836            Fried Chicken Tenders   107 10.24       grill
    ## 6837    Burrito Una Mano Trillium BYO    66 10.24     mexican
    ## 6838                     French Fries   132 10.24       grill
    ## 6839  Grilled Chicken Breast Sandwich    14 10.24       grill
    ## 6840 Trillium Grill Impossible Burger     9 10.24       grill
    ## 6841               Sweet Potato Fries    31 10.24       grill
    ## 6842             Seared Salmon Burger    10 10.24       grill
    ## 6843                Quesadilla Cheese     9 10.24     mexican
    ## 6844                Black Bean Burger     5 10.24       grill
    ## 6845             ADD Beef Patty $2.99    12 10.24       grill
    ## 6846               ADD Chicken Breast     5 10.24       grill
    ## 6847              Add Sausage 2 Patty     3 10.24       grill
    ## 6848                     Add Egg $.99     4 10.24       grill
    ## 6849                       ADD Cheese     4 10.24       grill
    ## 6850                1 Entree + 1 Side   195 10.24         wok
    ## 6851                1 Entree + 2 Side    93 10.24         wok
    ## 6852               Bowl Ramen Chicken    92 10.24       ramen
    ## 6853              2 Entrees + 2 Sides    39 10.24         wok
    ## 6854                  Bowl Ramen Tofu    15 10.24       ramen
    ## 6855          Side Vegetarian Lo Mein     7 10.24         wok
    ## 6856                     1 Wok Entree     2 10.24         wok
    ## 6857  Side Vegetarian Fried Rice with     3 10.24         wok
    ## 6858         Side White or Brown Rice     5 10.24         wok
    ## 6859      Create Your Pasta Bowl MEAT   131 10.24     italian
    ## 6860       Create Your Pasta Bowl VEG    32 10.24     italian
    ## 6861              Pizza with Toppings    39 10.24     italian
    ## 6862                     Pizza Cheese    32 10.24     italian
    ## 6863                   Add Extra Meat    16 10.24     italian
    ## 6864         Side Bread Pasta Station     1 10.24     italian
    ## 6865                Burrito Breakfast    86 10.24        <NA>
    ## 6866              Small French Omelet    64 10.24        <NA>
    ## 6867             Grand Slam Breakfast    27 10.24        <NA>
    ## 6868 Egg Cheese Sausage Breakfast San    37 10.24        <NA>
    ## 6869 Egg Cheese Bacon Breakfast Sandw    32 10.24        <NA>
    ## 6870                        Add Bacon    32 10.24        <NA>
    ## 6871                         Two Eggs    23 10.24        <NA>
    ## 6872              Trillium Home Fries     4 10.24        <NA>
    ## 6873                   2 Slices Toast     3 10.24        <NA>
    ## 6874                   Pancake Single     1 10.24        <NA>
    ## 6875                            Toast     3 10.24        <NA>
    ## 6876                         PC Jelly     3 10.24        <NA>
    ## 6877                 PC Peanut Butter     2 10.24        <NA>
    ## 6878               Salad by the Pound    82 10.24   salad bar
    ## 6879                 Burrito Bowl BYO    78 10.24     mexican
    ## 6880                   Side Guacamole     4 10.24     mexican
    ## 6881                      Single Taco     1 10.24     mexican
    ## 6882      Add Extra Toppings Una Mano     5 10.24     mexican
    ## 6883                       Side Salsa     1 10.24     mexican
    ## 6884                       Soup 12 oz    48 10.24   salad bar
    ## 6885                        8 oz Soup    41 10.24   salad bar
    ## 6886                  Coffee 16 oz SB    30 10.24        <NA>
    ## 6887                  Coffee 12 oz SB    28 10.24        <NA>
    ## 6888              Soda Fountain 16 oz    34 10.24        <NA>
    ## 6889              Soda Fountain 24 oz    29 10.24        <NA>
    ## 6890                    Hot Tea 20 oz     4 10.24        <NA>
    ## 6891               Open Miscellaneous    16 10.24        <NA>
    ## 6892                 Side Potato Tots    20 10.24        <NA>
    ## 6893          Add Extra Protein $2.99     1 10.24        <NA>
    ## 6894 Yerba Mate Enlighten Mint 15.5 o     9 10.24        <NA>
    ## 6895 Milk Chocolate LF BIG RED Refuel    16 10.24        <NA>
    ## 6896             Water Aquafina 20 oz    14 10.24        <NA>
    ## 6897            Soda Pepsi Diet 20 Oz    12 10.24        <NA>
    ## 6898                Water Aquafina 1L    11 10.24        <NA>
    ## 6899 Tea Jasmine Green Unsweet Ito En     7 10.24        <NA>
    ## 6900    Yerba Mate Bluephoria 15.5 oz     6 10.24        <NA>
    ## 6901    Glacier Freeze Gatorade 28 oz     8 10.24        <NA>
    ## 6902         Juice Naked Blue Machine     5 10.24        <NA>
    ## 6903         Juice Naked Mighty Mango     5 10.24        <NA>
    ## 6904   Tea Iced Rasberry Lipton 16 oz     9 10.24        <NA>
    ## 6905 Tea Sweet W/ Le Pure Leaf Lipton     9 10.24        <NA>
    ## 6906   Energy Artic Vibe Celsius 12oz     6 10.24        <NA>
    ## 6907        Gatorade Lemon Lime 28 oz     7 10.24        <NA>
    ## 6908     Water Life WTR Immune 700 ML     6 10.24        <NA>
    ## 6909         Yerba Mate Lemon 15.5 oz     5 10.24        <NA>
    ## 6910 Yerba Mate Peach Revival 15.5 oz     5 10.24        <NA>
    ## 6911           Water Gatorade 33.8 oz     6 10.24        <NA>
    ## 6912 Gatorade Propel Berry Water 1 lt     7 10.24        <NA>
    ## 6913 Tea Unsweet Black W Lemon 18.5 o     8 10.24        <NA>
    ## 6914 Cold Brew Chocolate Cream Starbu     5 10.24        <NA>
    ## 6915 Cold Brew Vanilla Sweet Cream St     5 10.24        <NA>
    ## 6916         Juice Protein Zone Naked     4 10.24        <NA>
    ## 6917    Naked Strawberry Banana Juice     4 10.24        <NA>
    ## 6918  Energy Kiwi Guava Celsius 12 oz     5 10.24        <NA>
    ## 6919 Energy Mango PassFruit Celsius 1     5 10.24        <NA>
    ## 6920 Tea Pure Leaf Zero Sugar Sweet T     7 10.24        <NA>
    ## 6921 Gatorade Gatorlyte Cherry Lime 2     4 10.24        <NA>
    ## 6922     Starbucks Double Shot 6.5 Oz     4 10.24        <NA>
    ## 6923                 Soda Pepsi 20 Oz     6 10.24        <NA>
    ## 6924            Soda Pepsi  Zero 20oz     6 10.24        <NA>
    ## 6925 Tea Unsweetened Pure Leaf Lipton     6 10.24        <NA>
    ## 6926      Gatorade Berry Zero G 28 oz     5 10.24        <NA>
    ## 6927 Juice Orange Premium Topicana 11     5 10.24        <NA>
    ## 6928 Juice Raspberry Lemonade Tropica     5 10.24        <NA>
    ## 6929 Energy Fuji Apple Pear Celsius 1     4 10.24        <NA>
    ## 6930     Frappuccino Mocha 13.7 Oz SB     3 10.24        <NA>
    ## 6931   Frappuccino Vanilla SB 13.7 Oz     3 10.24        <NA>
    ## 6932  Kombucha Pineapple Peach Kevita     3 10.24        <NA>
    ## 6933  Soda Ginger Ale Schweppes 20 Oz     5 10.24        <NA>
    ## 6934          Soda Mountain Dew 20 Oz     5 10.24        <NA>
    ## 6935  Soda Rootbeer Zero Sugar Mug 20     5 10.24        <NA>
    ## 6936 Tea Tea & Lemon Pure Leaf Lipton     5 10.24        <NA>
    ## 6937    Tea Black Milk Ito En 11.8 oz     3 10.24        <NA>
    ## 6938              Gatorade Blue 28 oz     4 10.24        <NA>
    ## 6939             Gatorade Orange 28oz     4 10.24        <NA>
    ## 6940       Juice AppleTropicana 11 oz     4 10.24        <NA>
    ## 6941 Juice Orange Homestyle Tropicana     4 10.24        <NA>
    ## 6942 Juice Zero Summer Splash Punch 1     4 10.24        <NA>
    ## 6943 Energy Orangesicle Celsius 16 oz     3 10.24        <NA>
    ## 6944 Red Jacket Strawberry Apple Juic     3 10.24        <NA>
    ## 6945 Energy Blue Raz Lemonade Celsius     3 10.24        <NA>
    ## 6946    Muscle Milk KO Chocolate 14oz     2 10.24        <NA>
    ## 6947 Muscle Milk PROSRS 40 Intense Va     2 10.24        <NA>
    ## 6948 Mountain Dew Kickstart Orange 16     3 10.24        <NA>
    ## 6949       Lipton Pure Leaf Sweet Tea     4 10.24        <NA>
    ## 6950          Soda Orange Crush 20 Oz     4 10.24        <NA>
    ## 6951     Soda Pepsi Wild Cherry 20 Oz     4 10.24        <NA>
    ## 6952          Soda Rootbeer Mug 20 oz     4 10.24        <NA>
    ## 6953     Soda Starry Lemon Lime 20 oz     4 10.24        <NA>
    ## 6954 Tea Sweetened With Lemon Brisk 2     4 10.24        <NA>
    ## 6955 Starbucks Doubleshot Energy Moch     2 10.24        <NA>
    ## 6956  Starbucks Doubleshot Energy Van     2 10.24        <NA>
    ## 6957         Juice Apple Dole 15.2 oz     4 10.24        <NA>
    ## 6958          Juice Naked Berry Blast     2 10.24        <NA>
    ## 6959        Juice Naked Green Machine     2 10.24        <NA>
    ## 6960       Juice Naked PNCLDA 15.2 Oz     2 10.24        <NA>
    ## 6961          Naked Red Machine Juice     2 10.24        <NA>
    ## 6962 Gatorade Galcier Freeze Zero G 2     3 10.24        <NA>
    ## 6963  Juice Lively Lemonade Tropicana     3 10.24        <NA>
    ## 6964 Gatorade Propel Grape Water 1 lt     3 10.24        <NA>
    ## 6965  Sparkling Lemonade Mango Kevita     3 10.24        <NA>
    ## 6966 Sparkling lemonade Strawberry Ke     3 10.24        <NA>
    ## 6967   Yerba Mate Revel Berry 15.5 oz     2 10.24        <NA>
    ## 6968      Yerba Mate Tropical 15.5 oz     2 10.24        <NA>
    ## 6969 Tea Golden Oolong Unsweet Ito En     2 10.24        <NA>
    ## 6970      Soda Guava Jarritos 12.5 oz     3 10.24        <NA>
    ## 6971      Soda Mango Jarritos 12.5 oz     3 10.24        <NA>
    ## 6972 Energy 222 Blue Raspberry Odysse     2 10.24        <NA>
    ## 6973  Energy Blue Crush Celsius 16 oz     2 10.24        <NA>
    ## 6974  Tea Peach Pure Leaf Lipton 18.5     3 10.24        <NA>
    ## 6975   Energy StrawLemon Celsius 12oz     2 10.24        <NA>
    ## 6976 Tea Green Peach Mango Celsius 12     2 10.24        <NA>
    ## 6977 Juice Ocean Spray Cranbe Grape 1     3 10.24        <NA>
    ## 6978 Mountain Dew Kickstart Black Che     2 10.24        <NA>
    ## 6979 Tea Unsweet Green Lipton 18.5 oz     2 10.24        <NA>
    ## 6980       Gatorade Fruit Punch 28 oz     2 10.24        <NA>
    ## 6981                 Cornell Lemonade     3 10.24        <NA>
    ## 6982  Soda Pineapple Jarritos 12.5 oz     2 10.24        <NA>
    ## 6983 Muscle Milk P40 Strawberry Cream     1 10.24        <NA>
    ## 6984   Muscle Milk PP Chocolate 14 oz     1 10.24        <NA>
    ## 6985 Tea Blackberry Pure Leaf 18.5 oz     2 10.24        <NA>
    ## 6986 Starbucks Doubleshot Ener Coffee     1 10.24        <NA>
    ## 6987   Kombucha Ginger Kevita 15.2 oz     1 10.24        <NA>
    ## 6988 Gatorade Gatorlyte Glacier Freez     1 10.24        <NA>
    ## 6989 Gatorade Gatorlyte Zero Fruit Pu     1 10.24        <NA>
    ## 6990                  Rockstar Energy     1 10.24        <NA>
    ## 6991   Rockstar Energy Pure Zero PNCH     1 10.24        <NA>
    ## 6992 Tea Green Matcha Milk Ito En 11.     1 10.24        <NA>
    ## 6993    Water Aquafina Alumitek 16 oz     2 10.24        <NA>
    ## 6994 Energy Dragonberry Celsius 16 oz     1 10.24        <NA>
    ## 6995 Energy Fruit Burst Celsius 16 oz     1 10.24        <NA>
    ## 6996 Energy Mango Tango Celsius 16 oz     1 10.24        <NA>
    ## 6997 Energy Passion Orange Guva Odyss     1 10.24        <NA>
    ## 6998 Juice Fuji Apple Red Jacket 12oz     1 10.24        <NA>
    ## 6999    Rockstar Pure Zero Silver Ice     1 10.24        <NA>
    ## 7000   Energy Fast Twitch Grape 12 oz     1 10.24        <NA>
    ## 7001 Energy Fast Twitch Watermelon St     1 10.24        <NA>
    ## 7002   Energy StrawGuava Celsius 12oz     1 10.24        <NA>
    ## 7003  Tea Green Rasp Acai  Celsius 12     1 10.24        <NA>
    ## 7004        Gatorade Zero Grape 28 oz     1 10.24        <NA>
    ## 7005 Gatorade Propel  Kiwi Straw Wate     1 10.24        <NA>
    ## 7006             Water Gatorade 700ml     1 10.24        <NA>
    ## 7007   Soda Tamarind Jarritos 12.5 oz     1 10.24        <NA>
    ## 7008     Soda Mountain Dew Zero 20 Oz     1 10.24        <NA>
    ## 7009     Tea Light Peach Lipton 20 oz     1 10.24        <NA>
    ## 7010         Juice Lemonade Dole 20oz     1 10.24        <NA>
    ## 7011        Juice Orange Dole 15.2 oz     1 10.24        <NA>
    ## 7012   Soda Mandarin Jarritos 12.5 oz     1 10.24        <NA>
    ## 7013   Cornell Low Fat Chocolate Milk     9 10.24        <NA>
    ## 7014 Yogurt Mixed Berry Chobani Drink     5 10.24        <NA>
    ## 7015 Yogurt 0% Fat Vanilla Greek Oiko     7 10.24        <NA>
    ## 7016 Yogurt Flip Almond Coco Loco Cho     4 10.24        <NA>
    ## 7017  Yogurt Flip Peanut Butter Dream     4 10.24        <NA>
    ## 7018       Cornell 2% Milk (70000381)     5 10.24        <NA>
    ## 7019   Yogurt Blueberry Greek Chobani     4 10.24        <NA>
    ## 7020 Yogurt Strawberry Banana Greek C     4 10.24        <NA>
    ## 7021 Yogurt Straw Banana Chobani Drin     2 10.24        <NA>
    ## 7022       Yogurt Mango Greek Chobani     3 10.24        <NA>
    ## 7023  Yogurt Strawberry Greek Chobani     3 10.24        <NA>
    ## 7024               Cornell Whole Milk     3 10.24        <NA>
    ## 7025   Milk Chocolate LF Cornell 8 Oz     3 10.24        <NA>
    ## 7026       Yogurt Peach Greek Chobani     2 10.24        <NA>
    ## 7027       Yogurt Plain Greek Chobani     2 10.24        <NA>
    ## 7028  Yogurt Mango Chobani Drink 7 oz     1 10.24        <NA>
    ## 7029       Cornell 2% Milk (70000380)     1 10.24        <NA>
    ## 7030 Yogurt Black Cherry Greek Choban     1 10.24        <NA>
    ## 7031            8 oz Vanilla Soy Silk     1 10.24        <NA>
    ## 7032          Silk Chocolate Soy Milk     1 10.24        <NA>
    ## 7033 Jelly Konjac Apple Grape Tastell     5 10.24        <NA>
    ## 7034      Jelly Konjac Peach Tastelli     5 10.24        <NA>
    ## 7035  Bar Peanut Butter Builders Clif     4 10.24        <NA>
    ## 7036 Bar CHOC Mint Builders Clif bar2     3 10.24        <NA>
    ## 7037       Bar Chocolate Chip Clifbar     3 10.24        <NA>
    ## 7038  Bar Frosted Birthday Cake Quest     2 10.24        <NA>
    ## 7039 Bar That's It Apple+Blueberry 1.     2 10.24        <NA>
    ## 7040        Bar S'Mores Quest 2.12 oz     1 10.24        <NA>
    ## 7041 Cookie Birthday Cake Lenny & Lar     1 10.24        <NA>
    ## 7042 Jelly Konjac Double Berry Tastel     1 10.24        <NA>
    ## 7043 Jelly Konjac Mango Pineapple Tas     1 10.24        <NA>
    ## 7044   Bar Crunchy Peanut Butter Clif     1 10.24        <NA>
    ## 7045      Kind Almond And Coconut Bar     1 10.24        <NA>
    ## 7046                      Fruit Whole    61 10.24        <NA>
    ## 7047   Fresh Cut Watermelon Fruit Cup     8 10.24        <NA>
    ## 7048        Fresh Cut Melon Fruit Cup     4 10.24        <NA>
    ## 7049        Chip Kettle Sea Salt 2 oz     2 10.24        <NA>
    ## 7050                 Utz Regular Chip     2 10.24        <NA>
    ## 7051          Utz Salt N Vinegar Chip     2 10.24        <NA>
    ## 7052    Chip Kettle Salt Vinegar 2 oz     1 10.24        <NA>
    ## 7053     Chip Funky Fusion Dirty 2 oz     1 10.24        <NA>
    ## 7054 Chip Potato Dirty BBQ Mesquite 2     1 10.24        <NA>
    ## 7055 Chip Salt & Malt Vinegar Dirty 2     1 10.24        <NA>
    ## 7056           Chips Dirty Sea Salted     1 10.24        <NA>
    ## 7057       Jalapeno Heat Chips Kosher     1 10.24        <NA>
    ## 7058 GF Sweet Street Choloate Brownie     4 10.24        <NA>
    ## 7059      Chewy Marshmallow GF 2.1 oz     3 10.24        <NA>
    ## 7060             Orbit Wintermint Gum     5 10.24        <NA>
    ## 7061             Orbit Sweet Mint Gum     2 10.24        <NA>
    ## 7062 Candy Milk Choc Caff Bar Awake 1     1 10.24        <NA>
    ## 7063                 MochiCookCrm1.5o     4 10.24        <NA>
    ## 7064  Ice Cream Mochi Sweet Mango 1.5     2 10.24        <NA>
    ## 7065 Ice Cream Mochi Double Chocolate     1 10.24        <NA>
    ## 7066                     Muffin Jumbo    34 10.24        <NA>
    ## 7067                           Cookie    30 10.24        <NA>
    ## 7068      Combo 16 oz Coffee & Muffin     4 10.24        <NA>
    ## 7069                            Scone     5 10.24        <NA>
    ## 7070                   Croissant Choc     3 10.24        <NA>
    ## 7071               Croissant Straw CC     3 10.24        <NA>
    ## 7072                Croissant Blue CC     2 10.24        <NA>
    ## 7073                     Cinnamon Bun     1 10.24        <NA>
    ## 7074                      Coffee Cake     1 10.24        <NA>
    ## 7075                 BowlMexicanChick     3 10.24 grab 'n' go
    ## 7076                   BowlSouthChick     2 10.24 grab 'n' go
    ## 7077                   BowlMedProtein     1 10.24 grab 'n' go
    ## 7078                 BowlSesAsianNood     1 10.24 grab 'n' go
    ## 7079                  Tofu Grain Bowl     1 10.24        <NA>
    ## 7080                   GF Turkey Sand     2 10.24 grab 'n' go
    ## 7081                 GF ChicCaesarSld     1 10.24 grab 'n' go
    ## 7082                 GFSunButterJelly     1 10.24 grab 'n' go
    ## 7083                     PBJ on Wheat     2 10.24 grab 'n' go
    ## 7084                  California Roll     4 10.24 grab 'n' go
    ## 7085                     Alaskan Roll     2 10.24 grab 'n' go
    ## 7086               Golden Dragon Roll     2 10.24 grab 'n' go
    ## 7087                  Spicy Tuna Roll     2 10.24 grab 'n' go
    ## 7088              Tempura Shrimp Roll     1 10.24 grab 'n' go
    ## 7089                         TSA Roll     1 10.24 grab 'n' go
    ## 7090                  Hawaiian Sunset     1 10.24 grab 'n' go
    ## 7091                      Salmon Roll     1 10.24 grab 'n' go
    ## 7092                     Avocado Roll     1 10.24 grab 'n' go
    ## 7093 Sandwich Black Forrest Ham & Swi     4 10.24 grab 'n' go
    ## 7094             Wrap Buffalo Chicken     3 10.24 grab 'n' go
    ## 7095              Wrap Chicken Caesar     1 10.24 grab 'n' go
    ## 7096             Salad Chicken Caesar     1 10.24 grab 'n' go
    ## 7097       Quesadilla Deluxe Trillium   149 10.23     mexican
    ## 7098                Grilled Hamburger   100 10.23       grill
    ## 7099            Fried Chicken Tenders   110 10.23       grill
    ## 7100    Burrito Una Mano Trillium BYO    59 10.23     mexican
    ## 7101                     French Fries   143 10.23       grill
    ## 7102  Grilled Chicken Breast Sandwich    20 10.23       grill
    ## 7103 Trillium Grill Impossible Burger    10 10.23       grill
    ## 7104                Quesadilla Cheese    11 10.23     mexican
    ## 7105               Sweet Potato Fries    27 10.23       grill
    ## 7106             ADD Beef Patty $2.99    15 10.23       grill
    ## 7107             Seared Salmon Burger     3 10.23       grill
    ## 7108               ADD Chicken Breast     5 10.23       grill
    ## 7109                Black Bean Burger     2 10.23       grill
    ## 7110              Add Sausage 2 Patty     5 10.23       grill
    ## 7111                       ADD Cheese     8 10.23       grill
    ## 7112                     Add Egg $.99     1 10.23       grill
    ## 7113                1 Entree + 1 Side   194 10.23         wok
    ## 7114                1 Entree + 2 Side    92 10.23         wok
    ## 7115               Bowl Ramen Chicken    83 10.23       ramen
    ## 7116              2 Entrees + 2 Sides    19 10.23         wok
    ## 7117                  Bowl Ramen Tofu     9 10.23       ramen
    ## 7118                     1 Wok Entree     3 10.23         wok
    ## 7119      Side Vegetable Spring Rolls     4 10.23         wok
    ## 7120  Side Vegetarian Fried Rice with     3 10.23         wok
    ## 7121          Side Vegetarian Lo Mein     3 10.23         wok
    ## 7122         Side White or Brown Rice     5 10.23         wok
    ## 7123                  Side Vegetables     2 10.23         wok
    ## 7124      Create Your Pasta Bowl MEAT   124 10.23     italian
    ## 7125       Create Your Pasta Bowl VEG    28 10.23     italian
    ## 7126              Pizza with Toppings    38 10.23     italian
    ## 7127                     Pizza Cheese    23 10.23     italian
    ## 7128                   Add Extra Meat    25 10.23     italian
    ## 7129         Side Bread Pasta Station     1 10.23     italian
    ## 7130                Burrito Breakfast    89 10.23        <NA>
    ## 7131              Small French Omelet    73 10.23        <NA>
    ## 7132 Egg Cheese Bacon Breakfast Sandw    30 10.23        <NA>
    ## 7133 Egg Cheese Sausage Breakfast San    29 10.23        <NA>
    ## 7134             Grand Slam Breakfast    10 10.23        <NA>
    ## 7135                        Add Bacon    34 10.23        <NA>
    ## 7136                         Two Eggs    16 10.23        <NA>
    ## 7137              Trillium Home Fries     6 10.23        <NA>
    ## 7138                   Pancake Single     2 10.23        <NA>
    ## 7139                            Toast     2 10.23        <NA>
    ## 7140                   2 Slices Toast     1 10.23        <NA>
    ## 7141                 Burrito Bowl BYO    92 10.23     mexican
    ## 7142                      Single Taco     5 10.23     mexican
    ## 7143                   Side Guacamole     1 10.23     mexican
    ## 7144      Add Extra Toppings Una Mano     1 10.23     mexican
    ## 7145               Salad by the Pound    77 10.23   salad bar
    ## 7146                       Soup 12 oz    36 10.23   salad bar
    ## 7147                        8 oz Soup    35 10.23   salad bar
    ## 7148              Soda Fountain 16 oz    40 10.23        <NA>
    ## 7149              Soda Fountain 24 oz    28 10.23        <NA>
    ## 7150                  Coffee 12 oz SB    20 10.23        <NA>
    ## 7151                  Coffee 16 oz SB    18 10.23        <NA>
    ## 7152                    Hot Tea 20 oz    10 10.23        <NA>
    ## 7153                 Side Potato Tots    17 10.23        <NA>
    ## 7154               Open Miscellaneous     5 10.23        <NA>
    ## 7155          Add Extra Protein $2.99     1 10.23        <NA>
    ## 7156             Water Aquafina 20 oz    22 10.23        <NA>
    ## 7157 Tea Golden Oolong Unsweet Ito En    11 10.23        <NA>
    ## 7158 Energy Mango PassFruit Celsius 1    12 10.23        <NA>
    ## 7159            Soda Pepsi Diet 20 Oz    15 10.23        <NA>
    ## 7160   Frappuccino Vanilla SB 13.7 Oz     8 10.23        <NA>
    ## 7161         Juice Naked Mighty Mango     8 10.23        <NA>
    ## 7162         Yerba Mate Lemon 15.5 oz     8 10.23        <NA>
    ## 7163                 Soda Pepsi 20 Oz    13 10.23        <NA>
    ## 7164        Juice Naked Green Machine     7 10.23        <NA>
    ## 7165 Gatorade Gatorlyte Glacier Freez     7 10.23        <NA>
    ## 7166    Yerba Mate Bluephoria 15.5 oz     7 10.23        <NA>
    ## 7167 Yerba Mate Enlighten Mint 15.5 o     7 10.23        <NA>
    ## 7168   Energy Artic Vibe Celsius 12oz     8 10.23        <NA>
    ## 7169 Tea Jasmine Green Unsweet Ito En     7 10.23        <NA>
    ## 7170 Milk Chocolate LF BIG RED Refuel    12 10.23        <NA>
    ## 7171                Water Aquafina 1L    10 10.23        <NA>
    ## 7172   Tea Iced Rasberry Lipton 16 oz    10 10.23        <NA>
    ## 7173      Yerba Mate Tropical 15.5 oz     6 10.23        <NA>
    ## 7174    Tea Black Milk Ito En 11.8 oz     6 10.23        <NA>
    ## 7175     Water Life WTR Immune 700 ML     7 10.23        <NA>
    ## 7176  Juice Lively Lemonade Tropicana     8 10.23        <NA>
    ## 7177 Energy Blue Raz Lemonade Celsius     6 10.23        <NA>
    ## 7178 Tea Green Matcha Milk Ito En 11.     5 10.23        <NA>
    ## 7179            Soda Pepsi  Zero 20oz     8 10.23        <NA>
    ## 7180  Starbucks Doubleshot Energy Van     4 10.23        <NA>
    ## 7181         Juice Naked Blue Machine     4 10.23        <NA>
    ## 7182       Juice Naked PNCLDA 15.2 Oz     4 10.23        <NA>
    ## 7183    Naked Strawberry Banana Juice     4 10.23        <NA>
    ## 7184  Kombucha Rasp Lemon Kevita 15.2     4 10.23        <NA>
    ## 7185 Juice Orange Premium Topicana 11     6 10.23        <NA>
    ## 7186  Energy Kiwi Guava Celsius 12 oz     5 10.23        <NA>
    ## 7187 Tea Green Peach Mango Celsius 12     5 10.23        <NA>
    ## 7188       Lipton Pure Leaf Sweet Tea     7 10.23        <NA>
    ## 7189          Soda Mountain Dew 20 Oz     7 10.23        <NA>
    ## 7190     Soda Pepsi Wild Cherry 20 Oz     7 10.23        <NA>
    ## 7191  Tea Peach Pure Leaf Lipton 18.5     7 10.23        <NA>
    ## 7192 Gatorade Propel Berry Water 1 lt     6 10.23        <NA>
    ## 7193 Muscle Milk PROSRS 40 Intense Va     3 10.23        <NA>
    ## 7194 Cold Brew Vanilla Sweet Cream St     4 10.23        <NA>
    ## 7195 Tea Unsweetened Pure Leaf Lipton     6 10.23        <NA>
    ## 7196       Juice AppleTropicana 11 oz     5 10.23        <NA>
    ## 7197 Gatorade Propel Grape Water 1 lt     5 10.23        <NA>
    ## 7198 Energy Fuji Apple Pear Celsius 1     4 10.23        <NA>
    ## 7199     Frappuccino Mocha 13.7 Oz SB     3 10.23        <NA>
    ## 7200      Soda Guava Jarritos 12.5 oz     5 10.23        <NA>
    ## 7201     Cider Apple Red Jacket 12 oz     4 10.23        <NA>
    ## 7202 Tea Unsweet Green Lipton 18.5 oz     4 10.23        <NA>
    ## 7203  Soda Ginger Ale Schweppes 20 Oz     5 10.23        <NA>
    ## 7204 Tea Tea & Lemon Pure Leaf Lipton     5 10.23        <NA>
    ## 7205 Yerba Mate Peach Revival 15.5 oz     3 10.23        <NA>
    ## 7206 Gatorade Galcier Freeze Zero G 2     4 10.23        <NA>
    ## 7207        Gatorade Lemon Lime 28 oz     4 10.23        <NA>
    ## 7208 Juice Orange Homestyle Tropicana     4 10.23        <NA>
    ## 7209 Sparkling lemonade Strawberry Ke     4 10.23        <NA>
    ## 7210 Energy Fast Twitch Watermelon St     3 10.23        <NA>
    ## 7211   Energy StrawLemon Celsius 12oz     3 10.23        <NA>
    ## 7212  Tea Green Rasp Acai  Celsius 12     3 10.23        <NA>
    ## 7213    Muscle Milk KO Chocolate 14oz     2 10.23        <NA>
    ## 7214 Mountain Dew Kickstart Orange 16     3 10.23        <NA>
    ## 7215             Soda Dr Pepper 20 Oz     4 10.23        <NA>
    ## 7216 Tea Pure Leaf Zero Sugar Sweet T     4 10.23        <NA>
    ## 7217 Tea Unsweet Black W Lemon 18.5 o     4 10.23        <NA>
    ## 7218 Starbucks Doubleshot Energy Moch     2 10.23        <NA>
    ## 7219         Juice Apple Dole 15.2 oz     4 10.23        <NA>
    ## 7220          Juice Naked Berry Blast     2 10.23        <NA>
    ## 7221   Kombucha Ginger Kevita 15.2 oz     2 10.23        <NA>
    ## 7222       Gatorade Fruit Punch 28 oz     3 10.23        <NA>
    ## 7223        Gatorade Zero Grape 28 oz     3 10.23        <NA>
    ## 7224       Juice Grape Tropicana 11oz     3 10.23        <NA>
    ## 7225 Gatorade Propel  Kiwi Straw Wate     3 10.23        <NA>
    ## 7226 Gatorade Gatorlyte Zero Fruit Pu     2 10.23        <NA>
    ## 7227 Gatorade Gatorlyte Zero Lemon Li     2 10.23        <NA>
    ## 7228                  Rockstar Energy     2 10.23        <NA>
    ## 7229  Energy Blue Crush Celsius 16 oz     2 10.23        <NA>
    ## 7230 Energy Mango Tango Celsius 16 oz     2 10.23        <NA>
    ## 7231 Juice Fuji Apple Red Jacket 12oz     2 10.23        <NA>
    ## 7232 Red Jacket Strawberry Apple Juic     2 10.23        <NA>
    ## 7233     Soda Mountain Dew Zero 20 Oz     3 10.23        <NA>
    ## 7234 Tea Blackberry Pure Leaf 18.5 oz     3 10.23        <NA>
    ## 7235 Tea Sweetened With Lemon Brisk 2     3 10.23        <NA>
    ## 7236 Tea Sweet W/ Le Pure Leaf Lipton     3 10.23        <NA>
    ## 7237        Juice Orange Dole 15.2 oz     3 10.23        <NA>
    ## 7238   Ocean Spray Cranberry Cocktail     3 10.23        <NA>
    ## 7239 Juice Ocean Spray Cranbe Grape 1     3 10.23        <NA>
    ## 7240           Water Gatorade 33.8 oz     2 10.23        <NA>
    ## 7241              Gatorade Blue 28 oz     2 10.23        <NA>
    ## 7242    Glacier Cherry Gatorade 28 oz     2 10.23        <NA>
    ## 7243    Glacier Freeze Gatorade 28 oz     2 10.23        <NA>
    ## 7244 Kickstart Strawberry Start-up 16     2 10.23        <NA>
    ## 7245  Sparkling Lemonade Mango Kevita     2 10.23        <NA>
    ## 7246             Water Gatorade 700ml     2 10.23        <NA>
    ## 7247   Soda Tamarind Jarritos 12.5 oz     2 10.23        <NA>
    ## 7248          Soda Orange Crush 20 Oz     2 10.23        <NA>
    ## 7249          Soda Rootbeer Mug 20 oz     2 10.23        <NA>
    ## 7250  Soda Rootbeer Zero Sugar Mug 20     2 10.23        <NA>
    ## 7251     Tea Light Peach Lipton 20 oz     2 10.23        <NA>
    ## 7252    Frappuccino Coffee 13.7 oz SB     1 10.23        <NA>
    ## 7253         Juice Protein Zone Naked     1 10.23        <NA>
    ## 7254   Soda Mandarin Jarritos 12.5 oz     2 10.23        <NA>
    ## 7255  Kombucha Pineapple Peach Kevita     1 10.23        <NA>
    ## 7256 Gatorade Gatorlyte MixBerry 20 o     1 10.23        <NA>
    ## 7257  Gatorade Gatorlyte Orange 20 oz     1 10.23        <NA>
    ## 7258 Gatorade Gatorlyte Strawberry Ki     1 10.23        <NA>
    ## 7259   Yerba Mate Revel Berry 15.5 oz     1 10.23        <NA>
    ## 7260   Rockstar Energy Pure Zero PNCH     1 10.23        <NA>
    ## 7261     Starbucks Double Shot 6.5 Oz     1 10.23        <NA>
    ## 7262 Energy Cherry Limeade Celsius 16     1 10.23        <NA>
    ## 7263 Energy Fruit Burst Celsius 16 oz     1 10.23        <NA>
    ## 7264 Energy Orangesicle Celsius 16 oz     1 10.23        <NA>
    ## 7265                 Cornell Lemonade     2 10.23        <NA>
    ## 7266    Rockstar Pure Zero Silver Ice     1 10.23        <NA>
    ## 7267  Energy Fast Twitch Cool Blue 12     1 10.23        <NA>
    ## 7268 Mountain Dew Kickstart Black Che     1 10.23        <NA>
    ## 7269      Gatorade Berry Zero G 28 oz     1 10.23        <NA>
    ## 7270             Gatorade Orange 28oz     1 10.23        <NA>
    ## 7271 Juice Zero Summer Splash Punch 1     1 10.23        <NA>
    ## 7272      Soda Mango Jarritos 12.5 oz     1 10.23        <NA>
    ## 7273  Soda Pineapple Jarritos 12.5 oz     1 10.23        <NA>
    ## 7274         Juice Lemonade Dole 20oz     1 10.23        <NA>
    ## 7275    Water Aquafina Alumitek 16 oz     1 10.23        <NA>
    ## 7276 Yogurt 0% Fat Vanilla Greek Oiko     9 10.23        <NA>
    ## 7277 Yogurt Mixed Berry Chobani Drink     5 10.23        <NA>
    ## 7278 Yogurt Straw Banana Chobani Drin     5 10.23        <NA>
    ## 7279       Cornell 2% Milk (70000380)     5 10.23        <NA>
    ## 7280   Milk Chocolate LF Cornell 8 Oz     8 10.23        <NA>
    ## 7281 Yogurt Flip Almond Coco Loco Cho     5 10.23        <NA>
    ## 7282 Yogurt Black Cherry Greek Choban     5 10.23        <NA>
    ## 7283       Cornell 2% Milk (70000381)     5 10.23        <NA>
    ## 7284  Yogurt Strawberry Greek Chobani     4 10.23        <NA>
    ## 7285  Yogurt Mango Chobani Drink 7 oz     2 10.23        <NA>
    ## 7286               Cornell Whole Milk     4 10.23        <NA>
    ## 7287       Yogurt Mango Greek Chobani     3 10.23        <NA>
    ## 7288   Cornell Low Fat Chocolate Milk     2 10.23        <NA>
    ## 7289       Yogurt Peach Greek Chobani     2 10.23        <NA>
    ## 7290 Yogurt Strawberry Banana Greek C     2 10.23        <NA>
    ## 7291       Yogurt Plain Greek Chobani     1 10.23        <NA>
    ## 7292  Cornell Dairy Strawberry Yogurt     1 10.23        <NA>
    ## 7293      Jelly Konjac Peach Tastelli     6 10.23        <NA>
    ## 7294 Bar Cookie & Cream Quest 2.12 oz     3 10.23        <NA>
    ## 7295 Jelly Konjac Mango Pineapple Tas     4 10.23        <NA>
    ## 7296 Bar CHOC Mint Builders Clif bar2     3 10.23        <NA>
    ## 7297 Bar That's It Apple+Blueberry 1.     4 10.23        <NA>
    ## 7298 Jelly Konjac Double Berry Tastel     3 10.23        <NA>
    ## 7299   Bar Crunchy Peanut Butter Clif     3 10.23        <NA>
    ## 7300 Bar White Chocolate Macadamia Cl     3 10.23        <NA>
    ## 7301 Jelly Konjac Apple Grape Tastell     2 10.23        <NA>
    ## 7302 Bar Choc Chip Cookie Dough Quest     1 10.23        <NA>
    ## 7303  Bar Frosted Birthday Cake Quest     1 10.23        <NA>
    ## 7304   Bar Blueberry Crisp Clif 2.4oz     1 10.23        <NA>
    ## 7305 Bar That's It Apple+Mango 1.2 oz     1 10.23        <NA>
    ## 7306 Nature Valley Peanut Butter Gran     1 10.23        <NA>
    ## 7307        Oat And Honey Granola Bar     1 10.23        <NA>
    ## 7308                      Fruit Whole    57 10.23        <NA>
    ## 7309           Chips Dirty Sea Salted     4 10.23        <NA>
    ## 7310 Nuts Fruit Nut Combo 1.5 oz Saha     2 10.23        <NA>
    ## 7311        Chip Kettle Sea Salt 2 oz     2 10.23        <NA>
    ## 7312     Chip Funky Fusion Dirty 2 oz     2 10.23        <NA>
    ## 7313   Chip Voodoo Limited Zapps 2 oz     2 10.23        <NA>
    ## 7314     Chip Kettle Sweet Onion 2 oz     1 10.23        <NA>
    ## 7315       Chip Maui Onion Dirty 2 oz     1 10.23        <NA>
    ## 7316  Chips Cracked Pepper & Sea Salt     1 10.23        <NA>
    ## 7317       Jalapeno Heat Chips Kosher     1 10.23        <NA>
    ## 7318       Chip Baked Jax  Utz 1.5 oz     1 10.23        <NA>
    ## 7319      Chewy Marshmallow GF 2.1 oz     7 10.23        <NA>
    ## 7320 GF Sweet Street Choloate Brownie     3 10.23        <NA>
    ## 7321                 MochiCookCrm1.5o     2 10.23        <NA>
    ## 7322  Ice Cream Mochi Sweet Mango 1.5     1 10.23        <NA>
    ## 7323 Ice Cream Mochi Double Chocolate     1 10.23        <NA>
    ## 7324   Candy CHOC Cara Caff Bar Awake     1 10.23        <NA>
    ## 7325             Orbit Sweet Mint Gum     1 10.23        <NA>
    ## 7326             Orbit Wintermint Gum     1 10.23        <NA>
    ## 7327              Tempura Shrimp Roll     3 10.23 grab 'n' go
    ## 7328               Golden Dragon Roll     3 10.23 grab 'n' go
    ## 7329                  California Roll     3 10.23 grab 'n' go
    ## 7330                     Alaskan Roll     2 10.23 grab 'n' go
    ## 7331              Tempura Crunch Roll     2 10.23 grab 'n' go
    ## 7332                         TSA Roll     2 10.23 grab 'n' go
    ## 7333                      Salmon Roll     2 10.23 grab 'n' go
    ## 7334                  Spicy Tuna Roll     2 10.23 grab 'n' go
    ## 7335            Hawaiian Volcano Roll     1 10.23 grab 'n' go
    ## 7336                  Hawaiian Sunset     1 10.23 grab 'n' go
    ## 7337                     Avocado Roll     1 10.23 grab 'n' go
    ## 7338              Wrap Chicken Caesar     4 10.23 grab 'n' go
    ## 7339 Sandwich Black Forrest Ham & Swi     3 10.23 grab 'n' go
    ## 7340             Wrap Buffalo Chicken     3 10.23 grab 'n' go
    ## 7341 Sandwich Prosciutto & Mozzarella     2 10.23 grab 'n' go
    ## 7342 Sandwich Crispy Chicken Milanese     1 10.23 grab 'n' go
    ## 7343                           Cookie    35 10.23        <NA>
    ## 7344                     Muffin Jumbo    27 10.23        <NA>
    ## 7345               Croissant Straw CC     5 10.23        <NA>
    ## 7346                   Croissant Choc     4 10.23        <NA>
    ## 7347                      Coffee Cake     5 10.23        <NA>
    ## 7348                     Cinnamon Bun     3 10.23        <NA>
    ## 7349      Combo 16 oz Coffee & Muffin     2 10.23        <NA>
    ## 7350                            Scone     2 10.23        <NA>
    ## 7351                Croissant Blue CC     1 10.23        <NA>
    ## 7352                   BowlMedProtein     3 10.23 grab 'n' go
    ## 7353                 BowlSesAsianNood     2 10.23 grab 'n' go
    ## 7354                  Tofu Grain Bowl     2 10.23        <NA>
    ## 7355                 BowlMexicanChick     1 10.23 grab 'n' go
    ## 7356                     PBJ on Wheat    12 10.23 grab 'n' go
    ## 7357                   GF Turkey Sand     3 10.23 grab 'n' go
    ## 7358                 GF ChicCaesarSld     1 10.23 grab 'n' go
    ## 7359                 GFSunButterJelly     2 10.23 grab 'n' go
    ## 7360       Quesadilla Deluxe Trillium   161 10.22     mexican
    ## 7361                Grilled Hamburger   127 10.22       grill
    ## 7362            Fried Chicken Tenders   120 10.22       grill
    ## 7363    Burrito Una Mano Trillium BYO    62 10.22     mexican
    ## 7364                     French Fries   146 10.22       grill
    ## 7365  Grilled Chicken Breast Sandwich    18 10.22       grill
    ## 7366               Sweet Potato Fries    34 10.22       grill
    ## 7367                Quesadilla Cheese    12 10.22     mexican
    ## 7368             Seared Salmon Burger    10 10.22       grill
    ## 7369 Trillium Grill Impossible Burger     6 10.22       grill
    ## 7370             ADD Beef Patty $2.99     9 10.22       grill
    ## 7371                Black Bean Burger     2 10.22       grill
    ## 7372               ADD Chicken Breast     3 10.22       grill
    ## 7373              Add Sausage 2 Patty     1 10.22       grill
    ## 7374                     Add Egg $.99     2 10.22       grill
    ## 7375                       ADD Cheese     2 10.22       grill
    ## 7376                1 Entree + 1 Side   197 10.22         wok
    ## 7377                1 Entree + 2 Side    87 10.22         wok
    ## 7378               Bowl Ramen Chicken    73 10.22       ramen
    ## 7379              2 Entrees + 2 Sides    36 10.22         wok
    ## 7380                  Bowl Ramen Tofu    10 10.22       ramen
    ## 7381          Side Vegetarian Lo Mein     4 10.22         wok
    ## 7382                     1 Wok Entree     1 10.22         wok
    ## 7383         Side White or Brown Rice     3 10.22         wok
    ## 7384  Side Vegetarian Fried Rice with     1 10.22         wok
    ## 7385      Create Your Pasta Bowl MEAT   132 10.22     italian
    ## 7386       Create Your Pasta Bowl VEG    26 10.22     italian
    ## 7387              Pizza with Toppings    35 10.22     italian
    ## 7388                     Pizza Cheese    24 10.22     italian
    ## 7389                   Add Extra Meat    21 10.22     italian
    ## 7390         Side Bread Pasta Station     1 10.22     italian
    ## 7391                Burrito Breakfast    89 10.22        <NA>
    ## 7392              Small French Omelet    60 10.22        <NA>
    ## 7393             Grand Slam Breakfast    19 10.22        <NA>
    ## 7394 Egg Cheese Sausage Breakfast San    28 10.22        <NA>
    ## 7395 Egg Cheese Bacon Breakfast Sandw    26 10.22        <NA>
    ## 7396                        Add Bacon    27 10.22        <NA>
    ## 7397                         Two Eggs    14 10.22        <NA>
    ## 7398                   Pancake Single     5 10.22        <NA>
    ## 7399              Trillium Home Fries     1 10.22        <NA>
    ## 7400                   2 Slices Toast     3 10.22        <NA>
    ## 7401                            Toast     2 10.22        <NA>
    ## 7402                         PC Jelly     1 10.22        <NA>
    ## 7403                 Burrito Bowl BYO   113 10.22     mexican
    ## 7404                      Single Taco     3 10.22     mexican
    ## 7405                   Side Guacamole     4 10.22     mexican
    ## 7406      Add Extra Toppings Una Mano     3 10.22     mexican
    ## 7407               Salad by the Pound    80 10.22   salad bar
    ## 7408                        8 oz Soup    51 10.22   salad bar
    ## 7409                       Soup 12 oz    33 10.22   salad bar
    ## 7410              Soda Fountain 16 oz    44 10.22        <NA>
    ## 7411                  Coffee 16 oz SB    31 10.22        <NA>
    ## 7412              Soda Fountain 24 oz    37 10.22        <NA>
    ## 7413                  Coffee 12 oz SB    28 10.22        <NA>
    ## 7414               Open Miscellaneous    16 10.22        <NA>
    ## 7415                 Side Potato Tots    14 10.22        <NA>
    ## 7416             Water Aquafina 20 oz    25 10.22        <NA>
    ## 7417 Yerba Mate Peach Revival 15.5 oz    10 10.22        <NA>
    ## 7418            Soda Pepsi Diet 20 Oz    16 10.22        <NA>
    ## 7419      Yerba Mate Tropical 15.5 oz     8 10.22        <NA>
    ## 7420 Tea Jasmine Green Unsweet Ito En     8 10.22        <NA>
    ## 7421 Yerba Mate Enlighten Mint 15.5 o     7 10.22        <NA>
    ## 7422              Gatorade Blue 28 oz     9 10.22        <NA>
    ## 7423                Water Aquafina 1L    10 10.22        <NA>
    ## 7424 Tea Green Peach Mango Celsius 12     7 10.22        <NA>
    ## 7425        Juice Naked Green Machine     5 10.22        <NA>
    ## 7426         Juice Protein Zone Naked     5 10.22        <NA>
    ## 7427   Kombucha Ginger Kevita 15.2 oz     5 10.22        <NA>
    ## 7428  Kombucha Rasp Lemon Kevita 15.2     5 10.22        <NA>
    ## 7429                 Soda Pepsi 20 Oz     9 10.22        <NA>
    ## 7430 Energy Mango PassFruit Celsius 1     6 10.22        <NA>
    ## 7431   Energy StrawLemon Celsius 12oz     6 10.22        <NA>
    ## 7432  Tea Green Rasp Acai  Celsius 12     6 10.22        <NA>
    ## 7433            Soda Pepsi  Zero 20oz     8 10.22        <NA>
    ## 7434 Juice Orange Premium Topicana 11     6 10.22        <NA>
    ## 7435 Juice Raspberry Lemonade Tropica     6 10.22        <NA>
    ## 7436  Energy Kiwi Guava Celsius 12 oz     5 10.22        <NA>
    ## 7437   Tea Iced Rasberry Lipton 16 oz     7 10.22        <NA>
    ## 7438    Yerba Mate Bluephoria 15.5 oz     4 10.22        <NA>
    ## 7439 Cold Brew Vanilla Sweet Cream St     4 10.22        <NA>
    ## 7440 Energy Dragonberry Celsius 16 oz     4 10.22        <NA>
    ## 7441             Soda Dr Pepper 20 Oz     6 10.22        <NA>
    ## 7442  Soda Ginger Ale Schweppes 20 Oz     6 10.22        <NA>
    ## 7443     Soda Mountain Dew Zero 20 Oz     6 10.22        <NA>
    ## 7444  Tea Peach Pure Leaf Lipton 18.5     6 10.22        <NA>
    ## 7445 Gatorade Galcier Freeze Zero G 2     5 10.22        <NA>
    ## 7446 Juice Orange Homestyle Tropicana     5 10.22        <NA>
    ## 7447  Starbucks Doubleshot Energy Van     3 10.22        <NA>
    ## 7448 Energy Fast Twitch Watermelon St     4 10.22        <NA>
    ## 7449     Frappuccino Mocha 13.7 Oz SB     3 10.22        <NA>
    ## 7450   Frappuccino Vanilla SB 13.7 Oz     3 10.22        <NA>
    ## 7451         Juice Naked Blue Machine     3 10.22        <NA>
    ## 7452    Naked Strawberry Banana Juice     3 10.22        <NA>
    ## 7453      Soda Guava Jarritos 12.5 oz     5 10.22        <NA>
    ## 7454       Lipton Pure Leaf Sweet Tea     5 10.22        <NA>
    ## 7455  Soda Rootbeer Zero Sugar Mug 20     5 10.22        <NA>
    ## 7456 Tea Sweet W/ Le Pure Leaf Lipton     5 10.22        <NA>
    ## 7457 Gatorade Gatorlyte Glacier Freez     3 10.22        <NA>
    ## 7458   Yerba Mate Revel Berry 15.5 oz     3 10.22        <NA>
    ## 7459    Tea Black Milk Ito En 11.8 oz     3 10.22        <NA>
    ## 7460 Tea Golden Oolong Unsweet Ito En     3 10.22        <NA>
    ## 7461 Tea Green Matcha Milk Ito En 11.     3 10.22        <NA>
    ## 7462         Juice Apple Dole 15.2 oz     5 10.22        <NA>
    ## 7463      Gatorade Berry Zero G 28 oz     4 10.22        <NA>
    ## 7464    Glacier Freeze Gatorade 28 oz     4 10.22        <NA>
    ## 7465       Juice AppleTropicana 11 oz     4 10.22        <NA>
    ## 7466  Juice Lively Lemonade Tropicana     4 10.22        <NA>
    ## 7467 Energy Cherry Limeade Celsius 16     3 10.22        <NA>
    ## 7468 Red Jacket Strawberry Apple Juic     3 10.22        <NA>
    ## 7469 Gatorade Propel  Kiwi Straw Wate     4 10.22        <NA>
    ## 7470             Water Gatorade 700ml     4 10.22        <NA>
    ## 7471 Energy Blue Raz Lemonade Celsius     3 10.22        <NA>
    ## 7472 Energy Fuji Apple Pear Celsius 1     3 10.22        <NA>
    ## 7473     Water Life WTR Immune 700 ML     3 10.22        <NA>
    ## 7474 Mountain Dew Kickstart Black Che     3 10.22        <NA>
    ## 7475    Water Aquafina Alumitek 16 oz     5 10.22        <NA>
    ## 7476          Soda Mountain Dew 20 Oz     4 10.22        <NA>
    ## 7477          Soda Orange Crush 20 Oz     4 10.22        <NA>
    ## 7478     Cider Apple Red Jacket 12 oz     3 10.22        <NA>
    ## 7479 Tea Unsweet Green Lipton 18.5 oz     3 10.22        <NA>
    ## 7480    Frappuccino Coffee 13.7 oz SB     2 10.22        <NA>
    ## 7481          Juice Naked Berry Blast     2 10.22        <NA>
    ## 7482         Juice Naked Mighty Mango     2 10.22        <NA>
    ## 7483       Gatorade Fruit Punch 28 oz     3 10.22        <NA>
    ## 7484        Gatorade Lemon Lime 28 oz     3 10.22        <NA>
    ## 7485    Glacier Cherry Gatorade 28 oz     3 10.22        <NA>
    ## 7486 Gatorade Propel Berry Water 1 lt     3 10.22        <NA>
    ## 7487 Gatorade Propel Grape Water 1 lt     3 10.22        <NA>
    ## 7488 Gatorade Gatorlyte Cherry Lime 2     2 10.22        <NA>
    ## 7489 Gatorade Gatorlyte MixBerry 20 o     2 10.22        <NA>
    ## 7490 Gatorade Gatorlyte Strawberry Ki     2 10.22        <NA>
    ## 7491   Rockstar Energy Pure Zero PNCH     2 10.22        <NA>
    ## 7492     Starbucks Double Shot 6.5 Oz     2 10.22        <NA>
    ## 7493  Energy Blue Crush Celsius 16 oz     2 10.22        <NA>
    ## 7494 Energy Fruit Burst Celsius 16 oz     2 10.22        <NA>
    ## 7495 Juice Fuji Apple Red Jacket 12oz     2 10.22        <NA>
    ## 7496     Soda Pepsi Wild Cherry 20 Oz     3 10.22        <NA>
    ## 7497          Soda Rootbeer Mug 20 oz     3 10.22        <NA>
    ## 7498 Tea Blackberry Pure Leaf 18.5 oz     3 10.22        <NA>
    ## 7499 Tea Sweetened With Lemon Brisk 2     3 10.22        <NA>
    ## 7500 Tea Tea & Lemon Pure Leaf Lipton     3 10.22        <NA>
    ## 7501         Juice Lemonade Dole 20oz     3 10.22        <NA>
    ## 7502   Ocean Spray Cranberry Cocktail     3 10.22        <NA>
    ## 7503   Energy Artic Vibe Celsius 12oz     2 10.22        <NA>
    ## 7504           Water Gatorade 33.8 oz     2 10.22        <NA>
    ## 7505             Gatorade Orange 28oz     2 10.22        <NA>
    ## 7506        Gatorade Zero Grape 28 oz     2 10.22        <NA>
    ## 7507 Juice Zero Summer Splash Punch 1     2 10.22        <NA>
    ## 7508 Sparkling lemonade Strawberry Ke     2 10.22        <NA>
    ## 7509                 Cornell Lemonade     3 10.22        <NA>
    ## 7510 Muscle Milk P40 Strawberry Cream     1 10.22        <NA>
    ## 7511 Muscle Milk PROSRS 40 Intense Va     1 10.22        <NA>
    ## 7512     Tea Light Peach Lipton 20 oz     2 10.22        <NA>
    ## 7513 Starbucks Doubleshot Ener Coffee     1 10.22        <NA>
    ## 7514 Starbucks Doubleshot Energy Moch     1 10.22        <NA>
    ## 7515        Juice Orange Dole 15.2 oz     2 10.22        <NA>
    ## 7516       Juice Naked PNCLDA 15.2 Oz     1 10.22        <NA>
    ## 7517 Juice Ocean Spray Cranbe Grape 1     2 10.22        <NA>
    ## 7518  Gatorade Gatorlyte Orange 20 oz     1 10.22        <NA>
    ## 7519 Gatorade Gatorlyte Zero Fruit Pu     1 10.22        <NA>
    ## 7520 Gatorade Gatorlyte Zero Lemon Li     1 10.22        <NA>
    ## 7521         Yerba Mate Lemon 15.5 oz     1 10.22        <NA>
    ## 7522 Cold Brew Chocolate Cream Starbu     1 10.22        <NA>
    ## 7523 Energy Mango Tango Celsius 16 oz     1 10.22        <NA>
    ## 7524 Energy Orangesicle Celsius 16 oz     1 10.22        <NA>
    ## 7525    Rockstar Pure Zero Silver Ice     1 10.22        <NA>
    ## 7526 Mountain Dew Kickstart Orange 16     1 10.22        <NA>
    ## 7527       Juice Grape Tropicana 11oz     1 10.22        <NA>
    ## 7528  Sparkling Lemonade Mango Kevita     1 10.22        <NA>
    ## 7529  Soda Pineapple Jarritos 12.5 oz     1 10.22        <NA>
    ## 7530   Soda Tamarind Jarritos 12.5 oz     1 10.22        <NA>
    ## 7531     Soda Starry Lemon Lime 20 oz     1 10.22        <NA>
    ## 7532 Tea Pure Leaf Zero Sugar Sweet T     1 10.22        <NA>
    ## 7533 Tea Unsweet Black W Lemon 18.5 o     1 10.22        <NA>
    ## 7534   Soda Mandarin Jarritos 12.5 oz     1 10.22        <NA>
    ## 7535   Cornell Low Fat Chocolate Milk     9 10.22        <NA>
    ## 7536   Milk Chocolate LF Cornell 8 Oz    10 10.22        <NA>
    ## 7537 Yogurt 0% Fat Vanilla Greek Oiko     6 10.22        <NA>
    ## 7538  Yogurt Flip Peanut Butter Dream     5 10.22        <NA>
    ## 7539       Yogurt Mango Greek Chobani     5 10.22        <NA>
    ## 7540 Yogurt Straw Banana Chobani Drin     3 10.22        <NA>
    ## 7541 Yogurt Flip Almond Coco Loco Cho     4 10.22        <NA>
    ## 7542                  Cornell 2% Milk     6 10.22        <NA>
    ## 7543       Yogurt Plain Greek Chobani     4 10.22        <NA>
    ## 7544               Cornell Whole Milk     4 10.22        <NA>
    ## 7545  Yogurt Strawberry Greek Chobani     3 10.22        <NA>
    ## 7546 Yogurt Black Cherry Greek Choban     2 10.22        <NA>
    ## 7547       Yogurt Peach Greek Chobani     2 10.22        <NA>
    ## 7548            8 oz Vanilla Soy Silk     2 10.22        <NA>
    ## 7549  Yogurt Mango Chobani Drink 7 oz     1 10.22        <NA>
    ## 7550 Yogurt Mixed Berry Chobani Drink     1 10.22        <NA>
    ## 7551   Yogurt Blueberry Greek Chobani     1 10.22        <NA>
    ## 7552 Yogurt Strawberry Banana Greek C     1 10.22        <NA>
    ## 7553          Silk Chocolate Soy Milk     1 10.22        <NA>
    ## 7554 Bar CHOC Mint Builders Clif bar2     7 10.22        <NA>
    ## 7555 Bar Cookie & Cream Quest 2.12 oz     5 10.22        <NA>
    ## 7556 Jelly Konjac Double Berry Tastel     4 10.22        <NA>
    ## 7557      Jelly Konjac Peach Tastelli     3 10.22        <NA>
    ## 7558 Bar That's It Apple+Strawberry 1     3 10.22        <NA>
    ## 7559  Bar Peanut Butter Builders Clif     2 10.22        <NA>
    ## 7560 Jelly Konjac Apple Grape Tastell     2 10.22        <NA>
    ## 7561  Bar Frosted Birthday Cake Quest     1 10.22        <NA>
    ## 7562  Cookie Choc Chip Lenny & Larrys     1 10.22        <NA>
    ## 7563 Cookie Peanut Butter Lenny & Lar     1 10.22        <NA>
    ## 7564   Fruit And Nut Delight Kind Bar     1 10.22        <NA>
    ## 7565 Jelly Konjac Mango Pineapple Tas     1 10.22        <NA>
    ## 7566       Bar Chocolate Chip Clifbar     1 10.22        <NA>
    ## 7567   Bar Crunchy Peanut Butter Clif     1 10.22        <NA>
    ## 7568 Dark Choc Cherry Cashew Plus Kin     1 10.22        <NA>
    ## 7569      Kind Almond And Coconut Bar     1 10.22        <NA>
    ## 7570   Kind Cranberry Almond Plus Bar     1 10.22        <NA>
    ## 7571 Bar That's It Apple+Blueberry 1.     1 10.22        <NA>
    ## 7572        Oat And Honey Granola Bar     1 10.22        <NA>
    ## 7573  Oats And Dark Chocolate Granola     1 10.22        <NA>
    ## 7574                      Fruit Whole    75 10.22        <NA>
    ## 7575 Nuts Fruit Nut Combo 1.5 oz Saha     2 10.22        <NA>
    ## 7576    Chip Kettle Salt Vinegar 2 oz     2 10.22        <NA>
    ## 7577 Chip Potato Dirty BBQ Mesquite 2     2 10.22        <NA>
    ## 7578   Chip Voodoo Limited Zapps 2 oz     2 10.22        <NA>
    ## 7579   Utz Honey Barbecue Chip 1.5 oz     2 10.22        <NA>
    ## 7580    Utz Sour Cream and Onion Chip     2 10.22        <NA>
    ## 7581       Chip Baked Jax  Utz 1.5 oz     2 10.22        <NA>
    ## 7582         Pretzel Thin 2.12 oz Utz     2 10.22        <NA>
    ## 7583     Chip Kettle Sweet Onion 2 oz     1 10.22        <NA>
    ## 7584     Chip Funky Fusion Dirty 2 oz     1 10.22        <NA>
    ## 7585           Chips Dirty Sea Salted     1 10.22        <NA>
    ## 7586  Chip Sour Cream And Onion Dirty     1 10.22        <NA>
    ## 7587       Jalapeno Heat Chips Kosher     1 10.22        <NA>
    ## 7588          Utz Salt N Vinegar Chip     1 10.22        <NA>
    ## 7589        Fresh Cut Melon Fruit Cup     8 10.22        <NA>
    ## 7590 GF Sweet Street Choloate Brownie     6 10.22        <NA>
    ## 7591      Chewy Marshmallow GF 2.1 oz     5 10.22        <NA>
    ## 7592             Orbit Wintermint Gum     4 10.22        <NA>
    ## 7593 Candy Milk Choc Caff Bar Awake 1     3 10.22        <NA>
    ## 7594   Candy CHOC Cara Caff Bar Awake     2 10.22        <NA>
    ## 7595     Chocolate Sunbutter Cups 2ct     1 10.22        <NA>
    ## 7596             Orbit Sweet Mint Gum     1 10.22        <NA>
    ## 7597 Ice Cream Mochi Double Chocolate     2 10.22        <NA>
    ## 7598                 MochiCookCrm1.5o     1 10.22        <NA>
    ## 7599              Wrap Chicken Caesar     8 10.22 grab 'n' go
    ## 7600             Wrap Buffalo Chicken     7 10.22 grab 'n' go
    ## 7601     Sandwich Corned Beef & Swiss     3 10.22 grab 'n' go
    ## 7602 Sandwich Crispy Chicken Milanese     3 10.22 grab 'n' go
    ## 7603 Sandwich Prosciutto & Mozzarella     3 10.22 grab 'n' go
    ## 7604 Sandwich Black Forrest Ham & Swi     2 10.22 grab 'n' go
    ## 7605             Salad Chicken Caesar     1 10.22 grab 'n' go
    ## 7606              Tempura Shrimp Roll     3 10.22 grab 'n' go
    ## 7607                  California Roll     3 10.22 grab 'n' go
    ## 7608                     Alaskan Roll     2 10.22 grab 'n' go
    ## 7609                         TSA Roll     2 10.22 grab 'n' go
    ## 7610               Golden Dragon Roll     2 10.22 grab 'n' go
    ## 7611                      Salmon Roll     2 10.22 grab 'n' go
    ## 7612                  Spicy Tuna Roll     2 10.22 grab 'n' go
    ## 7613            Hawaiian Volcano Roll     1 10.22 grab 'n' go
    ## 7614              Tempura Crunch Roll     1 10.22 grab 'n' go
    ## 7615                  Hawaiian Sunset     1 10.22 grab 'n' go
    ## 7616                     Avocado Roll     1 10.22 grab 'n' go
    ## 7617                     Muffin Jumbo    34 10.22        <NA>
    ## 7618                           Cookie    22 10.22        <NA>
    ## 7619                Croissant Blue CC     4 10.22        <NA>
    ## 7620                   Croissant Choc     4 10.22        <NA>
    ## 7621               Croissant Straw CC     4 10.22        <NA>
    ## 7622                     Cinnamon Bun     3 10.22        <NA>
    ## 7623                      Coffee Cake     4 10.22        <NA>
    ## 7624                            Scone     2 10.22        <NA>
    ## 7625                 BowlSesAsianNood     3 10.22 grab 'n' go
    ## 7626                  Tofu Grain Bowl     2 10.22        <NA>
    ## 7627                 BowlMexicanChick     1 10.22 grab 'n' go
    ## 7628                   BowlSouthChick     0 10.22 grab 'n' go
    ## 7629                 GF ChicCaesarSld     2 10.22 grab 'n' go
    ## 7630                   GF Turkey Sand     2 10.22 grab 'n' go
    ## 7631                 GFSunButterJelly     1 10.22 grab 'n' go
    ## 7632                     PBJ on Wheat     3 10.22 grab 'n' go
    ## 7633       Quesadilla Deluxe Trillium   154 10.21     mexican
    ## 7634                Grilled Hamburger   105 10.21       grill
    ## 7635    Burrito Una Mano Trillium BYO    79 10.21     mexican
    ## 7636            Fried Chicken Tenders    91 10.21       grill
    ## 7637                     French Fries   114 10.21       grill
    ## 7638  Grilled Chicken Breast Sandwich    18 10.21       grill
    ## 7639               Sweet Potato Fries    36 10.21       grill
    ## 7640             Seared Salmon Burger    11 10.21       grill
    ## 7641 Trillium Grill Impossible Burger     8 10.21       grill
    ## 7642                Quesadilla Cheese     6 10.21     mexican
    ## 7643             ADD Beef Patty $2.99    14 10.21       grill
    ## 7644               ADD Chicken Breast     4 10.21       grill
    ## 7645              Add Sausage 2 Patty     5 10.21       grill
    ## 7646                Black Bean Burger     1 10.21       grill
    ## 7647                       ADD Cheese     4 10.21       grill
    ## 7648                     Add Egg $.99     2 10.21       grill
    ## 7649                1 Entree + 1 Side   187 10.21         wok
    ## 7650                1 Entree + 2 Side    87 10.21         wok
    ## 7651               Bowl Ramen Chicken    71 10.21       ramen
    ## 7652              2 Entrees + 2 Sides    20 10.21         wok
    ## 7653                  Bowl Ramen Tofu    14 10.21       ramen
    ## 7654          Side Vegetarian Lo Mein     7 10.21         wok
    ## 7655      Side Vegetable Spring Rolls     5 10.21         wok
    ## 7656                     1 Wok Entree     3 10.21         wok
    ## 7657  Side Vegetarian Fried Rice with     1 10.21         wok
    ## 7658         Side White or Brown Rice     1 10.21         wok
    ## 7659      Create Your Pasta Bowl MEAT   124 10.21     italian
    ## 7660       Create Your Pasta Bowl VEG    37 10.21     italian
    ## 7661              Pizza with Toppings    38 10.21     italian
    ## 7662                     Pizza Cheese    23 10.21     italian
    ## 7663                   Add Extra Meat    22 10.21     italian
    ## 7664         Side Bread Pasta Station     1 10.21     italian
    ## 7665                Burrito Breakfast    80 10.21        <NA>
    ## 7666              Small French Omelet    51 10.21        <NA>
    ## 7667 Egg Cheese Sausage Breakfast San    32 10.21        <NA>
    ## 7668             Grand Slam Breakfast    16 10.21        <NA>
    ## 7669 Egg Cheese Bacon Breakfast Sandw    28 10.21        <NA>
    ## 7670                        Add Bacon    35 10.21        <NA>
    ## 7671                         Two Eggs    15 10.21        <NA>
    ## 7672              Trillium Home Fries     6 10.21        <NA>
    ## 7673                   2 Slices Toast     3 10.21        <NA>
    ## 7674                            Toast     2 10.21        <NA>
    ## 7675                 Burrito Bowl BYO   101 10.21     mexican
    ## 7676                      Single Taco     7 10.21     mexican
    ## 7677                   Side Guacamole     6 10.21     mexican
    ## 7678      Add Extra Toppings Una Mano     1 10.21     mexican
    ## 7679               Salad by the Pound    59 10.21   salad bar
    ## 7680                       Soup 12 oz    43 10.21   salad bar
    ## 7681                        8 oz Soup    27 10.21   salad bar
    ## 7682                  Coffee 16 oz SB    30 10.21        <NA>
    ## 7683              Soda Fountain 16 oz    36 10.21        <NA>
    ## 7684              Soda Fountain 24 oz    31 10.21        <NA>
    ## 7685                  Coffee 12 oz SB    15 10.21        <NA>
    ## 7686                    Hot Tea 20 oz     5 10.21        <NA>
    ## 7687                 Side Potato Tots    14 10.21        <NA>
    ## 7688               Open Miscellaneous     5 10.21        <NA>
    ## 7689            Soda Pepsi Diet 20 Oz    17 10.21        <NA>
    ## 7690             Water Aquafina 20 oz    18 10.21        <NA>
    ## 7691    Yerba Mate Bluephoria 15.5 oz     9 10.21        <NA>
    ## 7692 Yerba Mate Enlighten Mint 15.5 o     9 10.21        <NA>
    ## 7693 Energy Mango PassFruit Celsius 1    10 10.21        <NA>
    ## 7694    Naked Strawberry Banana Juice     7 10.21        <NA>
    ## 7695 Tea Jasmine Green Unsweet Ito En     8 10.21        <NA>
    ## 7696   Energy Artic Vibe Celsius 12oz     9 10.21        <NA>
    ## 7697                Water Aquafina 1L    11 10.21        <NA>
    ## 7698         Juice Naked Mighty Mango     6 10.21        <NA>
    ## 7699     Water Life WTR Immune 700 ML     8 10.21        <NA>
    ## 7700   Energy StrawLemon Celsius 12oz     7 10.21        <NA>
    ## 7701  Kombucha Pineapple Peach Kevita     5 10.21        <NA>
    ## 7702 Muscle Milk PROSRS 40 Intense Va     4 10.21        <NA>
    ## 7703        Gatorade Lemon Lime 28 oz     7 10.21        <NA>
    ## 7704 Juice Orange Premium Topicana 11     7 10.21        <NA>
    ## 7705         Yerba Mate Lemon 15.5 oz     5 10.21        <NA>
    ## 7706   Yerba Mate Revel Berry 15.5 oz     5 10.21        <NA>
    ## 7707 Tea Golden Oolong Unsweet Ito En     5 10.21        <NA>
    ## 7708             Soda Dr Pepper 20 Oz     8 10.21        <NA>
    ## 7709            Soda Pepsi  Zero 20oz     8 10.21        <NA>
    ## 7710  Tea Peach Pure Leaf Lipton 18.5     8 10.21        <NA>
    ## 7711  Starbucks Doubleshot Energy Van     4 10.21        <NA>
    ## 7712 Tea Unsweet Green Lipton 18.5 oz     6 10.21        <NA>
    ## 7713   Kombucha Ginger Kevita 15.2 oz     4 10.21        <NA>
    ## 7714 Energy Fuji Apple Pear Celsius 1     5 10.21        <NA>
    ## 7715                 Soda Pepsi 20 Oz     7 10.21        <NA>
    ## 7716   Muscle Milk PP Chocolate 14 oz     3 10.21        <NA>
    ## 7717 Milk Chocolate LF BIG RED Refuel     7 10.21        <NA>
    ## 7718 Juice Fuji Apple Red Jacket 12oz     4 10.21        <NA>
    ## 7719   Tea Iced Rasberry Lipton 16 oz     6 10.21        <NA>
    ## 7720     Tea Light Peach Lipton 20 oz     6 10.21        <NA>
    ## 7721 Tea Sweet W/ Le Pure Leaf Lipton     6 10.21        <NA>
    ## 7722              Gatorade Blue 28 oz     5 10.21        <NA>
    ## 7723 Juice Raspberry Lemonade Tropica     5 10.21        <NA>
    ## 7724 Tea Green Peach Mango Celsius 12     4 10.21        <NA>
    ## 7725  Tea Green Rasp Acai  Celsius 12     4 10.21        <NA>
    ## 7726        Juice Naked Green Machine     3 10.21        <NA>
    ## 7727         Juice Protein Zone Naked     3 10.21        <NA>
    ## 7728 Mountain Dew Kickstart Black Che     4 10.21        <NA>
    ## 7729 Tea Unsweetened Pure Leaf Lipton     5 10.21        <NA>
    ## 7730 Gatorade Gatorlyte Glacier Freez     3 10.21        <NA>
    ## 7731 Gatorade Gatorlyte MixBerry 20 o     3 10.21        <NA>
    ## 7732 Yerba Mate Peach Revival 15.5 oz     3 10.21        <NA>
    ## 7733      Yerba Mate Tropical 15.5 oz     3 10.21        <NA>
    ## 7734    Water Aquafina Alumitek 16 oz     6 10.21        <NA>
    ## 7735         Juice Apple Dole 15.2 oz     5 10.21        <NA>
    ## 7736       Juice Grape Tropicana 11oz     4 10.21        <NA>
    ## 7737 Cold Brew Chocolate Cream Starbu     3 10.21        <NA>
    ## 7738 Energy Cherry Limeade Celsius 16     3 10.21        <NA>
    ## 7739 Gatorade Propel Grape Water 1 lt     4 10.21        <NA>
    ## 7740 Gatorade Propel  Kiwi Straw Wate     4 10.21        <NA>
    ## 7741 Sparkling lemonade Strawberry Ke     4 10.21        <NA>
    ## 7742  Energy Kiwi Guava Celsius 12 oz     3 10.21        <NA>
    ## 7743    Muscle Milk KO Chocolate 14oz     2 10.21        <NA>
    ## 7744 Muscle Milk P40 Strawberry Cream     2 10.21        <NA>
    ## 7745       Lipton Pure Leaf Sweet Tea     4 10.21        <NA>
    ## 7746  Soda Ginger Ale Schweppes 20 Oz     4 10.21        <NA>
    ## 7747     Soda Mountain Dew Zero 20 Oz     4 10.21        <NA>
    ## 7748 Tea Tea & Lemon Pure Leaf Lipton     4 10.21        <NA>
    ## 7749     Cider Apple Red Jacket 12 oz     3 10.21        <NA>
    ## 7750    Frappuccino Coffee 13.7 oz SB     2 10.21        <NA>
    ## 7751          Juice Naked Berry Blast     2 10.21        <NA>
    ## 7752         Juice Naked Blue Machine     2 10.21        <NA>
    ## 7753    Glacier Cherry Gatorade 28 oz     3 10.21        <NA>
    ## 7754 Gatorade Propel Berry Water 1 lt     3 10.21        <NA>
    ## 7755 Gatorade Gatorlyte Cherry Lime 2     2 10.21        <NA>
    ## 7756 Gatorade Gatorlyte Zero Fruit Pu     2 10.21        <NA>
    ## 7757    Tea Black Milk Ito En 11.8 oz     2 10.21        <NA>
    ## 7758 Tea Green Matcha Milk Ito En 11.     2 10.21        <NA>
    ## 7759   Soda Tamarind Jarritos 12.5 oz     3 10.21        <NA>
    ## 7760 Cold Brew Vanilla Sweet Cream St     2 10.21        <NA>
    ## 7761 Energy Dragonberry Celsius 16 oz     2 10.21        <NA>
    ## 7762 Energy Fruit Burst Celsius 16 oz     2 10.21        <NA>
    ## 7763          Soda Mountain Dew 20 Oz     3 10.21        <NA>
    ## 7764     Soda Pepsi Wild Cherry 20 Oz     3 10.21        <NA>
    ## 7765  Soda Rootbeer Zero Sugar Mug 20     3 10.21        <NA>
    ## 7766         Juice Lemonade Dole 20oz     3 10.21        <NA>
    ## 7767   Ocean Spray Cranberry Cocktail     3 10.21        <NA>
    ## 7768 Energy Fast Twitch Watermelon St     2 10.21        <NA>
    ## 7769 Mountain Dew Kickstart Orange 16     2 10.21        <NA>
    ## 7770        Gatorade Zero Grape 28 oz     2 10.21        <NA>
    ## 7771       Juice AppleTropicana 11 oz     2 10.21        <NA>
    ## 7772 Juice Orange Homestyle Tropicana     2 10.21        <NA>
    ## 7773                 Cornell Lemonade     3 10.21        <NA>
    ## 7774         Muscle Milk Choc PB 14oz     1 10.21        <NA>
    ## 7775          Soda Rootbeer Mug 20 oz     2 10.21        <NA>
    ## 7776     Soda Starry Lemon Lime 20 oz     2 10.21        <NA>
    ## 7777 Tea Pure Leaf Zero Sugar Sweet T     2 10.21        <NA>
    ## 7778 Tea Unsweet Black W Lemon 18.5 o     2 10.21        <NA>
    ## 7779 Starbucks Doubleshot Energy Moch     1 10.21        <NA>
    ## 7780        Juice Orange Dole 15.2 oz     2 10.21        <NA>
    ## 7781     Frappuccino Mocha 13.7 Oz SB     1 10.21        <NA>
    ## 7782   Frappuccino Vanilla SB 13.7 Oz     1 10.21        <NA>
    ## 7783       Juice Naked PNCLDA 15.2 Oz     1 10.21        <NA>
    ## 7784          Naked Red Machine Juice     1 10.21        <NA>
    ## 7785 Juice Ocean Spray Cranbe Grape 1     2 10.21        <NA>
    ## 7786  Kombucha Rasp Lemon Kevita 15.2     1 10.21        <NA>
    ## 7787 Gatorade Gatorlyte Strawberry Ki     1 10.21        <NA>
    ## 7788 Gatorade Gatorlyte Zero Lemon Li     1 10.21        <NA>
    ## 7789     Starbucks Double Shot 6.5 Oz     1 10.21        <NA>
    ## 7790  Energy Blue Crush Celsius 16 oz     1 10.21        <NA>
    ## 7791 Energy Blue Raz Lemonade Celsius     1 10.21        <NA>
    ## 7792  Energy Fast Twitch Cool Blue 12     1 10.21        <NA>
    ## 7793           Water Gatorade 33.8 oz     1 10.21        <NA>
    ## 7794      Gatorade Berry Zero G 28 oz     1 10.21        <NA>
    ## 7795 Gatorade Galcier Freeze Zero G 2     1 10.21        <NA>
    ## 7796             Gatorade Orange 28oz     1 10.21        <NA>
    ## 7797  Juice Lively Lemonade Tropicana     1 10.21        <NA>
    ## 7798 Kickstart Strawberry Start-up 16     1 10.21        <NA>
    ## 7799             Water Gatorade 700ml     1 10.21        <NA>
    ## 7800      Soda Guava Jarritos 12.5 oz     1 10.21        <NA>
    ## 7801      Soda Mango Jarritos 12.5 oz     1 10.21        <NA>
    ## 7802          Soda Orange Crush 20 Oz     1 10.21        <NA>
    ## 7803 Tea Blackberry Pure Leaf 18.5 oz     1 10.21        <NA>
    ## 7804 Tea Sweetened With Lemon Brisk 2     1 10.21        <NA>
    ## 7805   Soda Mandarin Jarritos 12.5 oz     1 10.21        <NA>
    ## 7806 Yogurt Mixed Berry Chobani Drink     5 10.21        <NA>
    ## 7807       Yogurt Mango Greek Chobani     6 10.21        <NA>
    ## 7808  Yogurt Mango Chobani Drink 7 oz     3 10.21        <NA>
    ## 7809 Yogurt 0% Fat Vanilla Greek Oiko     4 10.21        <NA>
    ## 7810 Yogurt Flip Almond Coco Loco Cho     3 10.21        <NA>
    ## 7811  Yogurt Flip Peanut Butter Dream     3 10.21        <NA>
    ## 7812 Yogurt Straw Banana Chobani Drin     2 10.21        <NA>
    ## 7813 Yogurt Black Cherry Greek Choban     3 10.21        <NA>
    ## 7814 Yogurt Strawberry Banana Greek C     3 10.21        <NA>
    ## 7815               Cornell Whole Milk     3 10.21        <NA>
    ## 7816   Yogurt Blueberry Greek Chobani     2 10.21        <NA>
    ## 7817       Yogurt Peach Greek Chobani     2 10.21        <NA>
    ## 7818  Yogurt Strawberry Greek Chobani     2 10.21        <NA>
    ## 7819            8 oz Vanilla Soy Silk     2 10.21        <NA>
    ## 7820          Silk Chocolate Soy Milk     1 10.21        <NA>
    ## 7821        Fresh Cut Melon Fruit Cup     8 10.21        <NA>
    ## 7822   Fresh Cut Watermelon Fruit Cup     8 10.21        <NA>
    ## 7823 Jelly Konjac Mango Pineapple Tas     4 10.21        <NA>
    ## 7824 Bar CHOC Mint Builders Clif bar2     3 10.21        <NA>
    ## 7825 Jelly Konjac Apple Grape Tastell     3 10.21        <NA>
    ## 7826  Bar Frosted Birthday Cake Quest     1 10.21        <NA>
    ## 7827 Cookie Double Choc Lenny & Larry     1 10.21        <NA>
    ## 7828 Cookie Peanut Butter Lenny & Lar     1 10.21        <NA>
    ## 7829 Jelly Konjac Double Berry Tastel     1 10.21        <NA>
    ## 7830      Jelly Konjac Peach Tastelli     1 10.21        <NA>
    ## 7831   Bar Crunchy Peanut Butter Clif     1 10.21        <NA>
    ## 7832 Dark Choc Cherry Cashew Plus Kin     1 10.21        <NA>
    ## 7833      Kind Almond And Coconut Bar     1 10.21        <NA>
    ## 7834 Nature Valley Peanut Butter Gran     1 10.21        <NA>
    ## 7835                      Fruit Whole    40 10.21        <NA>
    ## 7836    Utz Sour Cream and Onion Chip     3 10.21        <NA>
    ## 7837           Chips Dirty Sea Salted     2 10.21        <NA>
    ## 7838   Chip Voodoo Limited Zapps 2 oz     2 10.21        <NA>
    ## 7839           Crunchsters BBQ 1.3 oz     1 10.21        <NA>
    ## 7840    Chip Kettle Salt Vinegar 2 oz     1 10.21        <NA>
    ## 7841 Chip Potato Dirty BBQ Mesquite 2     1 10.21        <NA>
    ## 7842  Chips Cracked Pepper & Sea Salt     1 10.21        <NA>
    ## 7843       Jalapeno Heat Chips Kosher     1 10.21        <NA>
    ## 7844                 Utz Regular Chip     1 10.21        <NA>
    ## 7845          Utz Salt N Vinegar Chip     1 10.21        <NA>
    ## 7846       Chip Baked Jax  Utz 1.5 oz     1 10.21        <NA>
    ## 7847         Pretzel Thin 2.12 oz Utz     1 10.21        <NA>
    ## 7848      Chewy Marshmallow GF 2.1 oz     7 10.21        <NA>
    ## 7849 GF Sweet Street Choloate Brownie     4 10.21        <NA>
    ## 7850             Orbit Sweet Mint Gum     3 10.21        <NA>
    ## 7851             Orbit Wintermint Gum     3 10.21        <NA>
    ## 7852     Chocolate Sunbutter Cups 2ct     1 10.21        <NA>
    ## 7853 Candy Milk Choc Caff Bar Awake 1     1 10.21        <NA>
    ## 7854  Ice Cream Mochi Sweet Mango 1.5     3 10.21        <NA>
    ## 7855                 MochiCookCrm1.5o     2 10.21        <NA>
    ## 7856 Ice Cream Mochi Double Chocolate     2 10.21        <NA>
    ## 7857               Golden Dragon Roll     3 10.21 grab 'n' go
    ## 7858                  California Roll     3 10.21 grab 'n' go
    ## 7859                     Alaskan Roll     2 10.21 grab 'n' go
    ## 7860              Tempura Shrimp Roll     2 10.21 grab 'n' go
    ## 7861                         TSA Roll     2 10.21 grab 'n' go
    ## 7862                      Salmon Roll     2 10.21 grab 'n' go
    ## 7863            Hawaiian Volcano Roll     1 10.21 grab 'n' go
    ## 7864                  Spicy Tuna Roll     1 10.21 grab 'n' go
    ## 7865                     Avocado Roll     1 10.21 grab 'n' go
    ## 7866              Wrap Chicken Caesar     5 10.21 grab 'n' go
    ## 7867             Wrap Buffalo Chicken     3 10.21 grab 'n' go
    ## 7868             Salad Chicken Caesar     3 10.21 grab 'n' go
    ## 7869 Sandwich Black Forrest Ham & Swi     2 10.21 grab 'n' go
    ## 7870 Sandwich Crispy Chicken Milanese     2 10.21 grab 'n' go
    ## 7871 Sandwich Prosciutto & Mozzarella     1 10.21 grab 'n' go
    ## 7872                           Cookie    25 10.21        <NA>
    ## 7873                     Muffin Jumbo    14 10.21        <NA>
    ## 7874               Croissant Straw CC     4 10.21        <NA>
    ## 7875                     Cinnamon Bun     3 10.21        <NA>
    ## 7876                      Coffee Cake     4 10.21        <NA>
    ## 7877                Croissant Blue CC     2 10.21        <NA>
    ## 7878      Combo 16 oz Coffee & Muffin     2 10.21        <NA>
    ## 7879                            Scone     2 10.21        <NA>
    ## 7880                   Croissant Choc     1 10.21        <NA>
    ## 7881                   BowlMedProtein     4 10.21 grab 'n' go
    ## 7882                 BowlMexicanChick     3 10.21 grab 'n' go
    ## 7883                   BowlSouthChick     3 10.21 grab 'n' go
    ## 7884                  Tofu Grain Bowl     2 10.21        <NA>
    ## 7885                     PBJ on Wheat    11 10.21 grab 'n' go
    ## 7886                   GF Turkey Sand     1 10.21 grab 'n' go
    ## 7887                 GFSunButterJelly     1 10.21 grab 'n' go
    ## 7888       Quesadilla Deluxe Trillium   105 10.18     mexican
    ## 7889                Grilled Hamburger    66 10.18       grill
    ## 7890            Fried Chicken Tenders    54 10.18       grill
    ## 7891    Burrito Una Mano Trillium BYO    42 10.18     mexican
    ## 7892                     French Fries   101 10.18       grill
    ## 7893                Quesadilla Cheese    14 10.18     mexican
    ## 7894  Grilled Chicken Breast Sandwich    11 10.18       grill
    ## 7895             Seared Salmon Burger     8 10.18       grill
    ## 7896             ADD Beef Patty $2.99    11 10.18       grill
    ## 7897 Trillium Grill Impossible Burger     2 10.18       grill
    ## 7898                Black Bean Burger     2 10.18       grill
    ## 7899              Add Sausage 2 Patty     2 10.18       grill
    ## 7900               ADD Chicken Breast     1 10.18       grill
    ## 7901                       ADD Cheese     4 10.18       grill
    ## 7902                     Add Egg $.99     1 10.18       grill
    ## 7903                1 Entree + 1 Side   103 10.18         wok
    ## 7904                1 Entree + 2 Side    63 10.18         wok
    ## 7905               Bowl Ramen Chicken    44 10.18       ramen
    ## 7906              2 Entrees + 2 Sides    20 10.18         wok
    ## 7907                  Bowl Ramen Tofu    21 10.18       ramen
    ## 7908         Side White or Brown Rice    12 10.18         wok
    ## 7909          Side Vegetarian Lo Mein     4 10.18         wok
    ## 7910                     1 Wok Entree     2 10.18         wok
    ## 7911      Side Vegetable Spring Rolls     1 10.18         wok
    ## 7912  Side Vegetarian Fried Rice with     1 10.18         wok
    ## 7913                Burrito Breakfast    84 10.18        <NA>
    ## 7914              Small French Omelet    47 10.18        <NA>
    ## 7915 Egg Cheese Sausage Breakfast San    36 10.18        <NA>
    ## 7916             Grand Slam Breakfast    15 10.18        <NA>
    ## 7917 Egg Cheese Bacon Breakfast Sandw    23 10.18        <NA>
    ## 7918                        Add Bacon    21 10.18        <NA>
    ## 7919                         Two Eggs    10 10.18        <NA>
    ## 7920                   Pancake Single     6 10.18        <NA>
    ## 7921              Trillium Home Fries     2 10.18        <NA>
    ## 7922                         PC Jelly     1 10.18        <NA>
    ## 7923      Create Your Pasta Bowl MEAT    83 10.18     italian
    ## 7924              Pizza with Toppings    23 10.18     italian
    ## 7925                     Pizza Cheese    27 10.18     italian
    ## 7926       Create Your Pasta Bowl VEG    13 10.18     italian
    ## 7927                   Add Extra Meat    14 10.18     italian
    ## 7928                 Burrito Bowl BYO    81 10.18     mexican
    ## 7929                      Single Taco     4 10.18     mexican
    ## 7930                        8 oz Soup    39 10.18   salad bar
    ## 7931                       Soup 12 oz    24 10.18   salad bar
    ## 7932               Salad by the Pound    34 10.18   salad bar
    ## 7933              Soda Fountain 16 oz    30 10.18        <NA>
    ## 7934                  Coffee 16 oz SB    22 10.18        <NA>
    ## 7935              Soda Fountain 24 oz    20 10.18        <NA>
    ## 7936                  Coffee 12 oz SB    15 10.18        <NA>
    ## 7937                    Hot Tea 20 oz     2 10.18        <NA>
    ## 7938                 Side Potato Tots    15 10.18        <NA>
    ## 7939               Open Miscellaneous    12 10.18        <NA>
    ## 7940          Add Extra Protein $2.99     1 10.18        <NA>
    ## 7941             Water Aquafina 20 oz    18 10.18        <NA>
    ## 7942            Soda Pepsi Diet 20 Oz    12 10.18        <NA>
    ## 7943 Milk Chocolate LF BIG RED Refuel    12 10.18        <NA>
    ## 7944   Yerba Mate Revel Berry 15.5 oz     6 10.18        <NA>
    ## 7945         Juice Naked Mighty Mango     5 10.18        <NA>
    ## 7946 Energy Mango PassFruit Celsius 1     6 10.18        <NA>
    ## 7947 Yerba Mate Enlighten Mint 15.5 o     5 10.18        <NA>
    ## 7948 Gatorade Propel Berry Water 1 lt     7 10.18        <NA>
    ## 7949            Soda Pepsi  Zero 20oz     8 10.18        <NA>
    ## 7950  Tea Green Rasp Acai  Celsius 12     5 10.18        <NA>
    ## 7951                 Soda Pepsi 20 Oz     7 10.18        <NA>
    ## 7952     Water Life WTR Immune 700 ML     5 10.18        <NA>
    ## 7953 Gatorade Gatorlyte MixBerry 20 o     4 10.18        <NA>
    ## 7954 Gatorade Gatorlyte Strawberry Ki     4 10.18        <NA>
    ## 7955    Yerba Mate Bluephoria 15.5 oz     4 10.18        <NA>
    ## 7956 Yerba Mate Peach Revival 15.5 oz     4 10.18        <NA>
    ## 7957 Tea Unsweet Green Lipton 18.5 oz     5 10.18        <NA>
    ## 7958              Gatorade Blue 28 oz     5 10.18        <NA>
    ## 7959 Juice Orange Homestyle Tropicana     5 10.18        <NA>
    ## 7960 Juice Raspberry Lemonade Tropica     5 10.18        <NA>
    ## 7961 Gatorade Propel Grape Water 1 lt     5 10.18        <NA>
    ## 7962   Energy StrawLemon Celsius 12oz     4 10.18        <NA>
    ## 7963 Tea Green Peach Mango Celsius 12     4 10.18        <NA>
    ## 7964         Juice Naked Blue Machine     3 10.18        <NA>
    ## 7965       Juice Naked PNCLDA 15.2 Oz     3 10.18        <NA>
    ## 7966    Naked Strawberry Banana Juice     3 10.18        <NA>
    ## 7967  Kombucha Rasp Lemon Kevita 15.2     3 10.18        <NA>
    ## 7968      Soda Guava Jarritos 12.5 oz     5 10.18        <NA>
    ## 7969             Soda Dr Pepper 20 Oz     5 10.18        <NA>
    ## 7970     Soda Mountain Dew Zero 20 Oz     5 10.18        <NA>
    ## 7971 Gatorade Gatorlyte Cherry Lime 2     3 10.18        <NA>
    ## 7972 Gatorade Gatorlyte Zero Fruit Pu     3 10.18        <NA>
    ## 7973 Tea Golden Oolong Unsweet Ito En     3 10.18        <NA>
    ## 7974 Tea Jasmine Green Unsweet Ito En     3 10.18        <NA>
    ## 7975        Gatorade Lemon Lime 28 oz     4 10.18        <NA>
    ## 7976    Glacier Freeze Gatorade 28 oz     4 10.18        <NA>
    ## 7977 Cold Brew Vanilla Sweet Cream St     3 10.18        <NA>
    ## 7978                 Cornell Lemonade     6 10.18        <NA>
    ## 7979   Energy Artic Vibe Celsius 12oz     3 10.18        <NA>
    ## 7980   Tea Iced Rasberry Lipton 16 oz     4 10.18        <NA>
    ## 7981 Tea Sweet W/ Le Pure Leaf Lipton     4 10.18        <NA>
    ## 7982   Frappuccino Vanilla SB 13.7 Oz     2 10.18        <NA>
    ## 7983          Juice Naked Berry Blast     2 10.18        <NA>
    ## 7984        Juice Naked Green Machine     2 10.18        <NA>
    ## 7985         Juice Protein Zone Naked     2 10.18        <NA>
    ## 7986   Kombucha Ginger Kevita 15.2 oz     2 10.18        <NA>
    ## 7987  Kombucha Pineapple Peach Kevita     2 10.18        <NA>
    ## 7988        Gatorade Zero Grape 28 oz     3 10.18        <NA>
    ## 7989 Juice Orange Premium Topicana 11     3 10.18        <NA>
    ## 7990  Sparkling Lemonade Mango Kevita     3 10.18        <NA>
    ## 7991             Water Gatorade 700ml     3 10.18        <NA>
    ## 7992 Gatorade Gatorlyte Glacier Freez     2 10.18        <NA>
    ## 7993      Yerba Mate Tropical 15.5 oz     2 10.18        <NA>
    ## 7994  Soda Pineapple Jarritos 12.5 oz     3 10.18        <NA>
    ## 7995                Water Aquafina 1L     3 10.18        <NA>
    ## 7996 Cold Brew Chocolate Cream Starbu     2 10.18        <NA>
    ## 7997 Tea Unsweetened Pure Leaf Lipton     3 10.18        <NA>
    ## 7998        Juice Orange Dole 15.2 oz     3 10.18        <NA>
    ## 7999 Energy Fast Twitch Watermelon St     2 10.18        <NA>
    ## 8000 Juice Ocean Spray Cranbe Grape 1     3 10.18        <NA>
    ## 8001           Water Gatorade 33.8 oz     2 10.18        <NA>
    ## 8002    Glacier Cherry Gatorade 28 oz     2 10.18        <NA>
    ## 8003       Juice AppleTropicana 11 oz     2 10.18        <NA>
    ## 8004 Sparkling lemonade Strawberry Ke     2 10.18        <NA>
    ## 8005      Soda Mango Jarritos 12.5 oz     2 10.18        <NA>
    ## 8006         Muscle Milk Choc PB 14oz     1 10.18        <NA>
    ## 8007    Muscle Milk KO Chocolate 14oz     1 10.18        <NA>
    ## 8008 Muscle Milk PROSRS 40 Intense Va     1 10.18        <NA>
    ## 8009  Soda Ginger Ale Schweppes 20 Oz     2 10.18        <NA>
    ## 8010  Tea Peach Pure Leaf Lipton 18.5     2 10.18        <NA>
    ## 8011 Tea Sweetened With Lemon Brisk 2     2 10.18        <NA>
    ## 8012 Tea Tea & Lemon Pure Leaf Lipton     2 10.18        <NA>
    ## 8013 Tea Unsweet Black W Lemon 18.5 o     2 10.18        <NA>
    ## 8014 Starbucks Doubleshot Ener Coffee     1 10.18        <NA>
    ## 8015 Starbucks Doubleshot Energy Moch     1 10.18        <NA>
    ## 8016  Starbucks Doubleshot Energy Van     1 10.18        <NA>
    ## 8017         Juice Apple Dole 15.2 oz     2 10.18        <NA>
    ## 8018   Soda Mandarin Jarritos 12.5 oz     2 10.18        <NA>
    ## 8019         Yerba Mate Lemon 15.5 oz     1 10.18        <NA>
    ## 8020   Rockstar Energy Pure Zero PNCH     1 10.18        <NA>
    ## 8021     Starbucks Double Shot 6.5 Oz     1 10.18        <NA>
    ## 8022    Tea Black Milk Ito En 11.8 oz     1 10.18        <NA>
    ## 8023 Tea Green Matcha Milk Ito En 11.     1 10.18        <NA>
    ## 8024 Energy Dragonberry Celsius 16 oz     1 10.18        <NA>
    ## 8025 Juice Fuji Apple Red Jacket 12oz     1 10.18        <NA>
    ## 8026 Red Jacket Strawberry Apple Juic     1 10.18        <NA>
    ## 8027 Energy Blue Raz Lemonade Celsius     1 10.18        <NA>
    ## 8028   Energy Fast Twitch Grape 12 oz     1 10.18        <NA>
    ## 8029 Mountain Dew Kickstart Black Che     1 10.18        <NA>
    ## 8030 Mountain Dew Kickstart Orange 16     1 10.18        <NA>
    ## 8031     Cider Apple Red Jacket 12 oz     1 10.18        <NA>
    ## 8032      Gatorade Berry Zero G 28 oz     1 10.18        <NA>
    ## 8033       Gatorade Fruit Punch 28 oz     1 10.18        <NA>
    ## 8034       Juice Grape Tropicana 11oz     1 10.18        <NA>
    ## 8035  Juice Lively Lemonade Tropicana     1 10.18        <NA>
    ## 8036       Lipton Pure Leaf Sweet Tea     1 10.18        <NA>
    ## 8037          Soda Mountain Dew 20 Oz     1 10.18        <NA>
    ## 8038          Soda Orange Crush 20 Oz     1 10.18        <NA>
    ## 8039     Soda Pepsi Wild Cherry 20 Oz     1 10.18        <NA>
    ## 8040          Soda Rootbeer Mug 20 oz     1 10.18        <NA>
    ## 8041  Soda Rootbeer Zero Sugar Mug 20     1 10.18        <NA>
    ## 8042 Tea Pure Leaf Zero Sugar Sweet T     1 10.18        <NA>
    ## 8043   Ocean Spray Cranberry Cocktail     1 10.18        <NA>
    ## 8044    Water Aquafina Alumitek 16 oz     1 10.18        <NA>
    ## 8045       Cornell 2% Milk (70000380)     4 10.18        <NA>
    ## 8046   Cornell Low Fat Chocolate Milk     4 10.18        <NA>
    ## 8047 Yogurt 0% Fat Vanilla Greek Oiko     4 10.18        <NA>
    ## 8048 Yogurt Flip Almond Coco Loco Cho     3 10.18        <NA>
    ## 8049  Yogurt Flip Peanut Butter Dream     3 10.18        <NA>
    ## 8050 Yogurt Mixed Berry Chobani Drink     2 10.18        <NA>
    ## 8051       Cornell 2% Milk (70000381)     3 10.18        <NA>
    ## 8052 Yogurt Black Cherry Greek Choban     2 10.18        <NA>
    ## 8053   Yogurt Blueberry Greek Chobani     2 10.18        <NA>
    ## 8054       Yogurt Mango Greek Chobani     2 10.18        <NA>
    ## 8055       Yogurt Plain Greek Chobani     2 10.18        <NA>
    ## 8056  Yogurt Strawberry Greek Chobani     2 10.18        <NA>
    ## 8057            8 oz Vanilla Soy Silk     2 10.18        <NA>
    ## 8058  Yogurt Mango Chobani Drink 7 oz     1 10.18        <NA>
    ## 8059 Yogurt Straw Banana Chobani Drin     1 10.18        <NA>
    ## 8060               Cornell Whole Milk     2 10.18        <NA>
    ## 8061   Milk Chocolate LF Cornell 8 Oz     2 10.18        <NA>
    ## 8062       Yogurt Peach Greek Chobani     1 10.18        <NA>
    ## 8063                      Fruit Whole    39 10.18        <NA>
    ## 8064   Fresh Cut Watermelon Fruit Cup     5 10.18        <NA>
    ## 8065        Fresh Cut Melon Fruit Cup     3 10.18        <NA>
    ## 8066 Jelly Konjac Mango Pineapple Tas     3 10.18        <NA>
    ## 8067 Bar That's It Apple+Blueberry 1.     2 10.18        <NA>
    ## 8068 Bar Choc Chip Cookie Dough Quest     1 10.18        <NA>
    ## 8069 Bar Cookie & Cream Quest 2.12 oz     1 10.18        <NA>
    ## 8070        Bar S'Mores Quest 2.12 oz     1 10.18        <NA>
    ## 8071 Bar CHOC Mint Builders Clif bar2     1 10.18        <NA>
    ## 8072  Bar Peanut Butter Builders Clif     1 10.18        <NA>
    ## 8073 Chip Potato Dirty BBQ Mesquite 2     2 10.18        <NA>
    ## 8074                 Utz Regular Chip     2 10.18        <NA>
    ## 8075    Utz Sour Cream and Onion Chip     2 10.18        <NA>
    ## 8076 Nuts Berry Almond Macaroon 1.5 o     1 10.18        <NA>
    ## 8077 Chip Potato Kettle Honey BBQ 2oz     1 10.18        <NA>
    ## 8078 Chip Salt & Malt Vinegar Dirty 2     1 10.18        <NA>
    ## 8079  Chips Cracked Pepper & Sea Salt     1 10.18        <NA>
    ## 8080           Chips Dirty Sea Salted     1 10.18        <NA>
    ## 8081   Chip Voodoo Limited Zapps 2 oz     1 10.18        <NA>
    ## 8082       Jalapeno Heat Chips Kosher     1 10.18        <NA>
    ## 8083       Chip Baked Jax  Utz 1.5 oz     1 10.18        <NA>
    ## 8084         Pretzel Thin 2.12 oz Utz     1 10.18        <NA>
    ## 8085      Chewy Marshmallow GF 2.1 oz     3 10.18        <NA>
    ## 8086 GF Sweet Street Choloate Brownie     2 10.18        <NA>
    ## 8087                 MochiCookCrm1.5o     2 10.18        <NA>
    ## 8088  Ice Cream Mochi Sweet Mango 1.5     1 10.18        <NA>
    ## 8089 Ice Cream Mochi Double Chocolate     1 10.18        <NA>
    ## 8090     Chocolate Sunbutter Cups 2ct     1 10.18        <NA>
    ## 8091             Orbit Sweet Mint Gum     1 10.18        <NA>
    ## 8092             Orbit Wintermint Gum     1 10.18        <NA>
    ## 8093               Golden Dragon Roll     3 10.18 grab 'n' go
    ## 8094                  California Roll     3 10.18 grab 'n' go
    ## 8095                  Hawaiian Sunset     2 10.18 grab 'n' go
    ## 8096            Hawaiian Volcano Roll     1 10.18 grab 'n' go
    ## 8097                     Alaskan Roll     1 10.18 grab 'n' go
    ## 8098              Tempura Crunch Roll     1 10.18 grab 'n' go
    ## 8099                      Salmon Roll     1 10.18 grab 'n' go
    ## 8100                  Spicy Tuna Roll     1 10.18 grab 'n' go
    ## 8101                     Avocado Roll     1 10.18 grab 'n' go
    ## 8102             Salad Chicken Caesar     4 10.18 grab 'n' go
    ## 8103 Sandwich Black Forrest Ham & Swi     3 10.18 grab 'n' go
    ## 8104     Sandwich Corned Beef & Swiss     2 10.18 grab 'n' go
    ## 8105 Sandwich Crispy Chicken Milanese     2 10.18 grab 'n' go
    ## 8106              Wrap Chicken Caesar     2 10.18 grab 'n' go
    ## 8107 Sandwich Prosciutto & Mozzarella     1 10.18 grab 'n' go
    ## 8108             Wrap Buffalo Chicken     1 10.18 grab 'n' go
    ## 8109                     Muffin Jumbo    19 10.18        <NA>
    ## 8110                           Cookie    22 10.18        <NA>
    ## 8111                   Croissant Choc     5 10.18        <NA>
    ## 8112                Croissant Blue CC     4 10.18        <NA>
    ## 8113                     Cinnamon Bun     4 10.18        <NA>
    ## 8114                      Coffee Cake     3 10.18        <NA>
    ## 8115                            Scone     2 10.18        <NA>
    ## 8116                 BowlChickAlfrPen     2 10.18 grab 'n' go
    ## 8117                 GF ChicCaesarSld     1 10.18 grab 'n' go
    ## 8118                 GFSunButterJelly     2 10.18 grab 'n' go
    ## 8119       Quesadilla Deluxe Trillium   159 10.17     mexican
    ## 8120                Grilled Hamburger   109 10.17       grill
    ## 8121            Fried Chicken Tenders   109 10.17       grill
    ## 8122    Burrito Una Mano Trillium BYO    51 10.17     mexican
    ## 8123                     French Fries   136 10.17       grill
    ## 8124  Grilled Chicken Breast Sandwich    22 10.17       grill
    ## 8125 Trillium Grill Impossible Burger    12 10.17       grill
    ## 8126                Quesadilla Cheese    13 10.17     mexican
    ## 8127               Sweet Potato Fries    27 10.17       grill
    ## 8128             Seared Salmon Burger     9 10.17       grill
    ## 8129             ADD Beef Patty $2.99    19 10.17       grill
    ## 8130                Black Bean Burger     1 10.17       grill
    ## 8131                       ADD Cheese    10 10.17       grill
    ## 8132              Add Sausage 2 Patty     2 10.17       grill
    ## 8133               ADD Chicken Breast     1 10.17       grill
    ## 8134                     Add Egg $.99     1 10.17       grill
    ## 8135                1 Entree + 1 Side   189 10.17         wok
    ## 8136                1 Entree + 2 Side    81 10.17         wok
    ## 8137               Bowl Ramen Chicken    68 10.17       ramen
    ## 8138              2 Entrees + 2 Sides    35 10.17         wok
    ## 8139                  Bowl Ramen Tofu    14 10.17       ramen
    ## 8140                     1 Wok Entree     5 10.17         wok
    ## 8141      Side Vegetable Spring Rolls     3 10.17         wok
    ## 8142          Side Vegetarian Lo Mein     3 10.17         wok
    ## 8143                  Side Vegetables     2 10.17         wok
    ## 8144         Side White or Brown Rice     2 10.17         wok
    ## 8145      Create Your Pasta Bowl MEAT   128 10.17     italian
    ## 8146       Create Your Pasta Bowl VEG    33 10.17     italian
    ## 8147              Pizza with Toppings    37 10.17     italian
    ## 8148                     Pizza Cheese    27 10.17     italian
    ## 8149                   Add Extra Meat    24 10.17     italian
    ## 8150         Side Bread Pasta Station     2 10.17     italian
    ## 8151                Burrito Breakfast    85 10.17        <NA>
    ## 8152              Small French Omelet    61 10.17        <NA>
    ## 8153 Egg Cheese Sausage Breakfast San    36 10.17        <NA>
    ## 8154 Egg Cheese Bacon Breakfast Sandw    34 10.17        <NA>
    ## 8155             Grand Slam Breakfast    15 10.17        <NA>
    ## 8156                        Add Bacon    28 10.17        <NA>
    ## 8157                         Two Eggs    17 10.17        <NA>
    ## 8158              Trillium Home Fries     1 10.17        <NA>
    ## 8159                   Pancake Single     1 10.17        <NA>
    ## 8160                   2 Slices Toast     1 10.17        <NA>
    ## 8161                            Toast     1 10.17        <NA>
    ## 8162                 Burrito Bowl BYO   103 10.17     mexican
    ## 8163                      Single Taco     4 10.17     mexican
    ## 8164                   Side Guacamole     4 10.17     mexican
    ## 8165                       Side Salsa     3 10.17     mexican
    ## 8166      Add Extra Toppings Una Mano     2 10.17     mexican
    ## 8167               Salad by the Pound    57 10.17   salad bar
    ## 8168                        8 oz Soup    35 10.17   salad bar
    ## 8169                       Soup 12 oz    28 10.17   salad bar
    ## 8170              Soda Fountain 24 oz    31 10.17        <NA>
    ## 8171                  Coffee 16 oz SB    24 10.17        <NA>
    ## 8172              Soda Fountain 16 oz    32 10.17        <NA>
    ## 8173                  Coffee 12 oz SB    25 10.17        <NA>
    ## 8174                    Hot Tea 20 oz     6 10.17        <NA>
    ## 8175                 Side Potato Tots    18 10.17        <NA>
    ## 8176               Open Miscellaneous    15 10.17        <NA>
    ## 8177          Add Extra Protein $2.99     1 10.17        <NA>
    ## 8178             Water Aquafina 20 oz    22 10.17        <NA>
    ## 8179 Yerba Mate Peach Revival 15.5 oz     9 10.17        <NA>
    ## 8180            Soda Pepsi Diet 20 Oz    14 10.17        <NA>
    ## 8181     Water Life WTR Immune 700 ML    10 10.17        <NA>
    ## 8182 Tea Green Peach Mango Celsius 12     9 10.17        <NA>
    ## 8183    Naked Strawberry Banana Juice     6 10.17        <NA>
    ## 8184   Energy Artic Vibe Celsius 12oz     7 10.17        <NA>
    ## 8185 Energy Mango PassFruit Celsius 1     7 10.17        <NA>
    ## 8186                Water Aquafina 1L     9 10.17        <NA>
    ## 8187         Juice Naked Mighty Mango     5 10.17        <NA>
    ## 8188 Milk Chocolate LF BIG RED Refuel    10 10.17        <NA>
    ## 8189 Yerba Mate Enlighten Mint 15.5 o     5 10.17        <NA>
    ## 8190    Tea Black Milk Ito En 11.8 oz     5 10.17        <NA>
    ## 8191 Tea Golden Oolong Unsweet Ito En     5 10.17        <NA>
    ## 8192          Soda Mountain Dew 20 Oz     8 10.17        <NA>
    ## 8193 Starbucks Doubleshot Energy Moch     4 10.17        <NA>
    ## 8194       Juice Naked PNCLDA 15.2 Oz     4 10.17        <NA>
    ## 8195              Gatorade Blue 28 oz     6 10.17        <NA>
    ## 8196                 Soda Pepsi 20 Oz     7 10.17        <NA>
    ## 8197    Yerba Mate Bluephoria 15.5 oz     4 10.17        <NA>
    ## 8198      Yerba Mate Tropical 15.5 oz     4 10.17        <NA>
    ## 8199 Muscle Milk P40 Strawberry Cream     3 10.17        <NA>
    ## 8200 Tea Unsweet Green Lipton 18.5 oz     5 10.17        <NA>
    ## 8201 Cold Brew Vanilla Sweet Cream St     4 10.17        <NA>
    ## 8202 Tea Pure Leaf Zero Sugar Sweet T     6 10.17        <NA>
    ## 8203        Gatorade Zero Grape 28 oz     5 10.17        <NA>
    ## 8204 Gatorade Propel  Kiwi Straw Wate     5 10.17        <NA>
    ## 8205 Energy Blue Raz Lemonade Celsius     4 10.17        <NA>
    ## 8206   Energy StrawLemon Celsius 12oz     4 10.17        <NA>
    ## 8207  Tea Green Rasp Acai  Celsius 12     4 10.17        <NA>
    ## 8208   Frappuccino Vanilla SB 13.7 Oz     3 10.17        <NA>
    ## 8209         Juice Protein Zone Naked     3 10.17        <NA>
    ## 8210             Soda Dr Pepper 20 Oz     5 10.17        <NA>
    ## 8211            Soda Pepsi  Zero 20oz     5 10.17        <NA>
    ## 8212   Tea Iced Rasberry Lipton 16 oz     5 10.17        <NA>
    ## 8213         Yerba Mate Lemon 15.5 oz     3 10.17        <NA>
    ## 8214   Yerba Mate Revel Berry 15.5 oz     3 10.17        <NA>
    ## 8215 Tea Jasmine Green Unsweet Ito En     3 10.17        <NA>
    ## 8216         Juice Apple Dole 15.2 oz     5 10.17        <NA>
    ## 8217  Juice Lively Lemonade Tropicana     4 10.17        <NA>
    ## 8218 Gatorade Propel Berry Water 1 lt     4 10.17        <NA>
    ## 8219 Juice Ocean Spray Cranbe Grape 1     5 10.17        <NA>
    ## 8220      Soda Mango Jarritos 12.5 oz     4 10.17        <NA>
    ## 8221    Muscle Milk KO Chocolate 14oz     2 10.17        <NA>
    ## 8222 Muscle Milk PROSRS 40 Intense Va     2 10.17        <NA>
    ## 8223    Water Aquafina Alumitek 16 oz     5 10.17        <NA>
    ## 8224     Soda Pepsi Wild Cherry 20 Oz     4 10.17        <NA>
    ## 8225 Tea Unsweet Black W Lemon 18.5 o     4 10.17        <NA>
    ## 8226 Tea Unsweetened Pure Leaf Lipton     4 10.17        <NA>
    ## 8227  Starbucks Doubleshot Energy Van     2 10.17        <NA>
    ## 8228     Frappuccino Mocha 13.7 Oz SB     2 10.17        <NA>
    ## 8229         Juice Naked Blue Machine     2 10.17        <NA>
    ## 8230   Kombucha Ginger Kevita 15.2 oz     2 10.17        <NA>
    ## 8231  Kombucha Pineapple Peach Kevita     2 10.17        <NA>
    ## 8232 Juice Raspberry Lemonade Tropica     3 10.17        <NA>
    ## 8233  Sparkling Lemonade Mango Kevita     3 10.17        <NA>
    ## 8234 Gatorade Gatorlyte Cherry Lime 2     2 10.17        <NA>
    ## 8235 Gatorade Gatorlyte Zero Fruit Pu     2 10.17        <NA>
    ## 8236     Starbucks Double Shot 6.5 Oz     2 10.17        <NA>
    ## 8237 Tea Green Matcha Milk Ito En 11.     2 10.17        <NA>
    ## 8238 Energy Orangesicle Celsius 16 oz     2 10.17        <NA>
    ## 8239       Lipton Pure Leaf Sweet Tea     3 10.17        <NA>
    ## 8240  Soda Ginger Ale Schweppes 20 Oz     3 10.17        <NA>
    ## 8241          Soda Rootbeer Mug 20 oz     3 10.17        <NA>
    ## 8242     Soda Starry Lemon Lime 20 oz     3 10.17        <NA>
    ## 8243     Tea Light Peach Lipton 20 oz     3 10.17        <NA>
    ## 8244  Tea Peach Pure Leaf Lipton 18.5     3 10.17        <NA>
    ## 8245 Tea Tea & Lemon Pure Leaf Lipton     3 10.17        <NA>
    ## 8246 Energy Fast Twitch Watermelon St     2 10.17        <NA>
    ## 8247           Water Gatorade 33.8 oz     2 10.17        <NA>
    ## 8248      Gatorade Berry Zero G 28 oz     2 10.17        <NA>
    ## 8249       Gatorade Fruit Punch 28 oz     2 10.17        <NA>
    ## 8250             Gatorade Orange 28oz     2 10.17        <NA>
    ## 8251    Glacier Freeze Gatorade 28 oz     2 10.17        <NA>
    ## 8252       Juice AppleTropicana 11 oz     2 10.17        <NA>
    ## 8253 Juice Orange Premium Topicana 11     2 10.17        <NA>
    ## 8254 Juice Zero Summer Splash Punch 1     2 10.17        <NA>
    ## 8255 Gatorade Propel Grape Water 1 lt     2 10.17        <NA>
    ## 8256             Water Gatorade 700ml     2 10.17        <NA>
    ## 8257   Soda Tamarind Jarritos 12.5 oz     2 10.17        <NA>
    ## 8258   Muscle Milk PP Chocolate 14 oz     1 10.17        <NA>
    ## 8259 Tea Blackberry Pure Leaf 18.5 oz     2 10.17        <NA>
    ## 8260 Tea Sweetened With Lemon Brisk 2     2 10.17        <NA>
    ## 8261 Tea Sweet W/ Le Pure Leaf Lipton     2 10.17        <NA>
    ## 8262 Starbucks Doubleshot Ener Coffee     1 10.17        <NA>
    ## 8263         Juice Lemonade Dole 20oz     2 10.17        <NA>
    ## 8264   Ocean Spray Cranberry Cocktail     2 10.17        <NA>
    ## 8265    Frappuccino Coffee 13.7 oz SB     1 10.17        <NA>
    ## 8266        Juice Naked Green Machine     1 10.17        <NA>
    ## 8267          Naked Red Machine Juice     1 10.17        <NA>
    ## 8268   Soda Mandarin Jarritos 12.5 oz     2 10.17        <NA>
    ## 8269  Kombucha Rasp Lemon Kevita 15.2     1 10.17        <NA>
    ## 8270 Gatorade Gatorlyte Glacier Freez     1 10.17        <NA>
    ## 8271  Gatorade Gatorlyte Orange 20 oz     1 10.17        <NA>
    ## 8272 Gatorade Gatorlyte Strawberry Ki     1 10.17        <NA>
    ## 8273  Energy Blue Crush Celsius 16 oz     1 10.17        <NA>
    ## 8274 Energy Fruit Burst Celsius 16 oz     1 10.17        <NA>
    ## 8275 Juice Fuji Apple Red Jacket 12oz     1 10.17        <NA>
    ## 8276 Red Jacket Strawberry Apple Juic     1 10.17        <NA>
    ## 8277 Energy Fuji Apple Pear Celsius 1     1 10.17        <NA>
    ## 8278 Mountain Dew Kickstart Black Che     1 10.17        <NA>
    ## 8279 Mountain Dew Kickstart Orange 16     1 10.17        <NA>
    ## 8280     Cider Apple Red Jacket 12 oz     1 10.17        <NA>
    ## 8281    Glacier Cherry Gatorade 28 oz     1 10.17        <NA>
    ## 8282       Juice Grape Tropicana 11oz     1 10.17        <NA>
    ## 8283 Juice Orange Homestyle Tropicana     1 10.17        <NA>
    ## 8284 Kickstart Strawberry Start-up 16     1 10.17        <NA>
    ## 8285 Sparkling lemonade Strawberry Ke     1 10.17        <NA>
    ## 8286      Soda Guava Jarritos 12.5 oz     1 10.17        <NA>
    ## 8287  Soda Pineapple Jarritos 12.5 oz     1 10.17        <NA>
    ## 8288     Soda Mountain Dew Zero 20 Oz     1 10.17        <NA>
    ## 8289          Soda Orange Crush 20 Oz     1 10.17        <NA>
    ## 8290  Soda Rootbeer Zero Sugar Mug 20     1 10.17        <NA>
    ## 8291                 Cornell Lemonade     1 10.17        <NA>
    ## 8292   Milk Chocolate LF Cornell 8 Oz     9 10.17        <NA>
    ## 8293 Yogurt 0% Fat Vanilla Greek Oiko     6 10.17        <NA>
    ## 8294   Cornell Low Fat Chocolate Milk     5 10.17        <NA>
    ## 8295  Yogurt Flip Peanut Butter Dream     5 10.17        <NA>
    ## 8296 Yogurt Mixed Berry Chobani Drink     3 10.17        <NA>
    ## 8297  Yogurt Mango Chobani Drink 7 oz     2 10.17        <NA>
    ## 8298   Yogurt Blueberry Greek Chobani     3 10.17        <NA>
    ## 8299 Yogurt Strawberry Banana Greek C     3 10.17        <NA>
    ## 8300  Yogurt Strawberry Greek Chobani     3 10.17        <NA>
    ## 8301          Silk Chocolate Soy Milk     3 10.17        <NA>
    ## 8302               Cornell Whole Milk     3 10.17        <NA>
    ## 8303       Yogurt Mango Greek Chobani     2 10.17        <NA>
    ## 8304 Yogurt Straw Banana Chobani Drin     1 10.17        <NA>
    ## 8305       Cornell 2% Milk (70000380)     1 10.17        <NA>
    ## 8306 Yogurt Flip Almond Coco Loco Cho     1 10.17        <NA>
    ## 8307       Yogurt Peach Greek Chobani     1 10.17        <NA>
    ## 8308       Cornell 2% Milk (70000381)     1 10.17        <NA>
    ## 8309  Cornell Dairy Strawberry Yogurt     1 10.17        <NA>
    ## 8310        Bar S'Mores Quest 2.12 oz     5 10.17        <NA>
    ## 8311 Jelly Konjac Mango Pineapple Tas     5 10.17        <NA>
    ## 8312 Bar CHOC Mint Builders Clif bar2     3 10.17        <NA>
    ## 8313       Bar Chocolate Chip Clifbar     3 10.17        <NA>
    ## 8314 Bar Cookie & Cream Quest 2.12 oz     2 10.17        <NA>
    ## 8315  Bar Frosted Birthday Cake Quest     2 10.17        <NA>
    ## 8316 Bar White Chocolate Macadamia Cl     2 10.17        <NA>
    ## 8317   Kind Cranberry Almond Plus Bar     2 10.17        <NA>
    ## 8318 Bar That's It Apple+Blueberry 1.     2 10.17        <NA>
    ## 8319 Bar Choc Chip Cookie Dough Quest     1 10.17        <NA>
    ## 8320 Cookie Birthday Cake Lenny & Lar     1 10.17        <NA>
    ## 8321  Cookie Choc Chip Lenny & Larrys     1 10.17        <NA>
    ## 8322 Cookie Double Choc Lenny & Larry     1 10.17        <NA>
    ## 8323 Jelly Konjac Double Berry Tastel     1 10.17        <NA>
    ## 8324      Jelly Konjac Peach Tastelli     1 10.17        <NA>
    ## 8325   Bar Crunchy Peanut Butter Clif     1 10.17        <NA>
    ## 8326 Bar That's It Apple+Mango 1.2 oz     1 10.17        <NA>
    ## 8327   Chip Voodoo Limited Zapps 2 oz     4 10.17        <NA>
    ## 8328 Nuts Almonds Honey 1.5 oz Sahale     2 10.17        <NA>
    ## 8329 Nuts Fruit Nut Combo 1.5 oz Saha     2 10.17        <NA>
    ## 8330   Utz Honey Barbecue Chip 1.5 oz     3 10.17        <NA>
    ## 8331    Utz Sour Cream and Onion Chip     3 10.17        <NA>
    ## 8332        Chip Kettle Sea Salt 2 oz     2 10.17        <NA>
    ## 8333 Chip Potato Dirty BBQ Mesquite 2     2 10.17        <NA>
    ## 8334 Chip Salt & Malt Vinegar Dirty 2     2 10.17        <NA>
    ## 8335  Chips Cracked Pepper & Sea Salt     2 10.17        <NA>
    ## 8336           Chips Dirty Sea Salted     1 10.17        <NA>
    ## 8337       Jalapeno Heat Chips Kosher     1 10.17        <NA>
    ## 8338         Pretzel Thin 2.12 oz Utz     1 10.17        <NA>
    ## 8339   Fresh Cut Watermelon Fruit Cup    11 10.17        <NA>
    ## 8340        Fresh Cut Melon Fruit Cup     2 10.17        <NA>
    ## 8341                      Fruit Whole    39 10.17        <NA>
    ## 8342             Orbit Wintermint Gum     7 10.17        <NA>
    ## 8343             Orbit Sweet Mint Gum     5 10.17        <NA>
    ## 8344   Candy CHOC Cara Caff Bar Awake     1 10.17        <NA>
    ## 8345 Candy Milk Choc Caff Bar Awake 1     1 10.17        <NA>
    ## 8346 GF Sweet Street Choloate Brownie     3 10.17        <NA>
    ## 8347      Chewy Marshmallow GF 2.1 oz     3 10.17        <NA>
    ## 8348                 MochiCookCrm1.5o     3 10.17        <NA>
    ## 8349  Ice Cream Mochi Sweet Mango 1.5     2 10.17        <NA>
    ## 8350 Ice Cream Mochi Double Chocolate     2 10.17        <NA>
    ## 8351                     Muffin Jumbo    36 10.17        <NA>
    ## 8352                           Cookie    25 10.17        <NA>
    ## 8353                   Croissant Choc     8 10.17        <NA>
    ## 8354                Croissant Blue CC     6 10.17        <NA>
    ## 8355               Croissant Straw CC     3 10.17        <NA>
    ## 8356      Combo 16 oz Coffee & Muffin     3 10.17        <NA>
    ## 8357                     Cinnamon Bun     3 10.17        <NA>
    ## 8358                      Coffee Cake     4 10.17        <NA>
    ## 8359                            Scone     1 10.17        <NA>
    ## 8360                 BowlMexicanChick     6 10.17 grab 'n' go
    ## 8361                   BowlMedProtein     3 10.17 grab 'n' go
    ## 8362                 BowlSesAsianNood     3 10.17 grab 'n' go
    ## 8363                 BowlChickAlfrPen     1 10.17 grab 'n' go
    ## 8364                   BowlSouthChick     1 10.17 grab 'n' go
    ## 8365                   GF Turkey Sand     3 10.17 grab 'n' go
    ## 8366                 GF ChicCaesarSld     2 10.17 grab 'n' go
    ## 8367                     PBJ on Wheat     5 10.17 grab 'n' go
    ## 8368              Tempura Shrimp Roll     3 10.17 grab 'n' go
    ## 8369                  California Roll     4 10.17 grab 'n' go
    ## 8370                     Alaskan Roll     2 10.17 grab 'n' go
    ## 8371                         TSA Roll     2 10.17 grab 'n' go
    ## 8372               Golden Dragon Roll     2 10.17 grab 'n' go
    ## 8373                     Phoenix Roll     1 10.17        <NA>
    ## 8374              Tempura Crunch Roll     1 10.17 grab 'n' go
    ## 8375                  Hawaiian Sunset     1 10.17 grab 'n' go
    ## 8376                      Salmon Roll     1 10.17 grab 'n' go
    ## 8377                  Spicy Tuna Roll     1 10.17 grab 'n' go
    ## 8378                     Avocado Roll     1 10.17 grab 'n' go
    ## 8379              Wrap Chicken Caesar     6 10.17 grab 'n' go
    ## 8380             Wrap Buffalo Chicken     4 10.17 grab 'n' go
    ## 8381     Sandwich Corned Beef & Swiss     2 10.17 grab 'n' go
    ## 8382 Sandwich Crispy Chicken Milanese     2 10.17 grab 'n' go
    ## 8383 Sandwich Prosciutto & Mozzarella     2 10.17 grab 'n' go
    ## 8384 Sandwich Black Forrest Ham & Swi     1 10.17 grab 'n' go
    ## 8385       Quesadilla Deluxe Trillium   140 10.16     mexican
    ## 8386                Grilled Hamburger    91 10.16       grill
    ## 8387            Fried Chicken Tenders    92 10.16       grill
    ## 8388    Burrito Una Mano Trillium BYO    62 10.16     mexican
    ## 8389                     French Fries   113 10.16       grill
    ## 8390                Quesadilla Cheese    35 10.16     mexican
    ## 8391  Grilled Chicken Breast Sandwich    14 10.16       grill
    ## 8392             Seared Salmon Burger     9 10.16       grill
    ## 8393 Trillium Grill Impossible Burger     7 10.16       grill
    ## 8394               Sweet Potato Fries    21 10.16       grill
    ## 8395             ADD Beef Patty $2.99    13 10.16       grill
    ## 8396                Black Bean Burger     2 10.16       grill
    ## 8397      Add Impossible Burger Patty     1 10.16        <NA>
    ## 8398                     Add Egg $.99     6 10.16       grill
    ## 8399                       ADD Cheese     9 10.16       grill
    ## 8400              Add Sausage 2 Patty     2 10.16       grill
    ## 8401        ADD Burger Salmon Grilled     1 10.16       grill
    ## 8402                1 Entree + 1 Side   206 10.16         wok
    ## 8403                1 Entree + 2 Side    79 10.16         wok
    ## 8404               Bowl Ramen Chicken    83 10.16       ramen
    ## 8405              2 Entrees + 2 Sides    20 10.16         wok
    ## 8406                  Bowl Ramen Tofu    16 10.16       ramen
    ## 8407                  Bowl Ramen Pork     5 10.16        <NA>
    ## 8408          Side Vegetarian Lo Mein     6 10.16         wok
    ## 8409           Side Fried Spring Roll     4 10.16         wok
    ## 8410         Side White or Brown Rice     4 10.16         wok
    ## 8411      Side Vegetable Spring Rolls     2 10.16         wok
    ## 8412                     1 Wok Entree     1 10.16         wok
    ## 8413                  Side Vegetables     1 10.16         wok
    ## 8414  Side Vegetarian Fried Rice with     1 10.16         wok
    ## 8415      Create Your Pasta Bowl MEAT   149 10.16     italian
    ## 8416       Create Your Pasta Bowl VEG    24 10.16     italian
    ## 8417                     Pizza Cheese    28 10.16     italian
    ## 8418                   Add Extra Meat    34 10.16     italian
    ## 8419              Pizza with Toppings    16 10.16     italian
    ## 8420         Side Bread Pasta Station     2 10.16     italian
    ## 8421                Burrito Breakfast    87 10.16        <NA>
    ## 8422              Small French Omelet    53 10.16        <NA>
    ## 8423 Egg Cheese Sausage Breakfast San    34 10.16        <NA>
    ## 8424 Egg Cheese Bacon Breakfast Sandw    26 10.16        <NA>
    ## 8425             Grand Slam Breakfast     9 10.16        <NA>
    ## 8426                        Add Bacon    26 10.16        <NA>
    ## 8427                         Two Eggs    10 10.16        <NA>
    ## 8428                   Pancake Single     4 10.16        <NA>
    ## 8429                   2 Slices Toast     4 10.16        <NA>
    ## 8430              Trillium Home Fries     1 10.16        <NA>
    ## 8431                 Burrito Bowl BYO   105 10.16     mexican
    ## 8432                      Single Taco     3 10.16     mexican
    ## 8433                  Side Sour Cream     1 10.16        <NA>
    ## 8434               Salad by the Pound    79 10.16   salad bar
    ## 8435                       Soup 12 oz    67 10.16   salad bar
    ## 8436                        8 oz Soup    60 10.16   salad bar
    ## 8437                  Coffee 12 oz SB    30 10.16        <NA>
    ## 8438                  Coffee 16 oz SB    27 10.16        <NA>
    ## 8439              Soda Fountain 24 oz    24 10.16        <NA>
    ## 8440              Soda Fountain 16 oz    24 10.16        <NA>
    ## 8441                    Hot Tea 20 oz    10 10.16        <NA>
    ## 8442                 Side Potato Tots    12 10.16        <NA>
    ## 8443               Open Miscellaneous     6 10.16        <NA>
    ## 8444          Add Extra Protein $2.99     1 10.16        <NA>
    ## 8445             Water Aquafina 20 oz    26 10.16        <NA>
    ## 8446         Juice Naked Mighty Mango     9 10.16        <NA>
    ## 8447 Yerba Mate Peach Revival 15.5 oz    10 10.16        <NA>
    ## 8448 Milk Chocolate LF BIG RED Refuel    16 10.16        <NA>
    ## 8449            Soda Pepsi Diet 20 Oz    13 10.16        <NA>
    ## 8450 Energy Mango PassFruit Celsius 1     9 10.16        <NA>
    ## 8451 Energy Fuji Apple Pear Celsius 1     8 10.16        <NA>
    ## 8452     Water Life WTR Immune 700 ML     8 10.16        <NA>
    ## 8453 Tea Jasmine Green Unsweet Ito En     6 10.16        <NA>
    ## 8454    Naked Strawberry Banana Juice     5 10.16        <NA>
    ## 8455  Kombucha Pineapple Peach Kevita     5 10.16        <NA>
    ## 8456                 Soda Pepsi 20 Oz     9 10.16        <NA>
    ## 8457 Gatorade Gatorlyte Glacier Freez     5 10.16        <NA>
    ## 8458 Yerba Mate Enlighten Mint 15.5 o     5 10.16        <NA>
    ## 8459 Tea Golden Oolong Unsweet Ito En     5 10.16        <NA>
    ## 8460 Gatorade Propel  Kiwi Straw Wate     7 10.16        <NA>
    ## 8461            Soda Pepsi  Zero 20oz     8 10.16        <NA>
    ## 8462 Starbucks Doubleshot Energy Moch     4 10.16        <NA>
    ## 8463   Frappuccino Vanilla SB 13.7 Oz     4 10.16        <NA>
    ## 8464       Juice Naked PNCLDA 15.2 Oz     4 10.16        <NA>
    ## 8465 Gatorade Galcier Freeze Zero G 2     6 10.16        <NA>
    ## 8466   Energy Artic Vibe Celsius 12oz     5 10.16        <NA>
    ## 8467 Tea Tea & Lemon Pure Leaf Lipton     7 10.16        <NA>
    ## 8468    Water Aquafina Alumitek 16 oz     8 10.16        <NA>
    ## 8469 Juice Joes Lemonade  Red Jacket1     4 10.16        <NA>
    ## 8470   Ocean Spray Cranberry Cocktail     6 10.16        <NA>
    ## 8471  Tea Green Rasp Acai  Celsius 12     4 10.16        <NA>
    ## 8472     Frappuccino Mocha 13.7 Oz SB     3 10.16        <NA>
    ## 8473        Juice Naked Green Machine     3 10.16        <NA>
    ## 8474         Juice Protein Zone Naked     3 10.16        <NA>
    ## 8475   Kombucha Ginger Kevita 15.2 oz     3 10.16        <NA>
    ## 8476 Tea Unsweet Green Lipton 18.5 oz     4 10.16        <NA>
    ## 8477             Soda Dr Pepper 20 Oz     5 10.16        <NA>
    ## 8478  Soda Rootbeer Zero Sugar Mug 20     5 10.16        <NA>
    ## 8479  Tea Peach Pure Leaf Lipton 18.5     5 10.16        <NA>
    ## 8480 Tea Unsweet Black W Lemon 18.5 o     5 10.16        <NA>
    ## 8481 Tea Unsweetened Pure Leaf Lipton     5 10.16        <NA>
    ## 8482   Yerba Mate Revel Berry 15.5 oz     3 10.16        <NA>
    ## 8483              Gatorade Blue 28 oz     4 10.16        <NA>
    ## 8484 Gatorade Propel Berry Water 1 lt     4 10.16        <NA>
    ## 8485 Energy Blue Raz Lemonade Celsius     3 10.16        <NA>
    ## 8486 Energy Fast Twitch Watermelon St     3 10.16        <NA>
    ## 8487  Energy Kiwi Guava Celsius 12 oz     3 10.16        <NA>
    ## 8488 Tea Green Peach Mango Celsius 12     3 10.16        <NA>
    ## 8489         Muscle Milk Choc PB 14oz     2 10.16        <NA>
    ## 8490           Water Gatorade 33.8 oz     3 10.16        <NA>
    ## 8491          Soda Mountain Dew 20 Oz     4 10.16        <NA>
    ## 8492          Soda Rootbeer Mug 20 oz     4 10.16        <NA>
    ## 8493  Starbucks Doubleshot Energy Van     2 10.16        <NA>
    ## 8494         Juice Naked Blue Machine     2 10.16        <NA>
    ## 8495        Gatorade Lemon Lime 28 oz     3 10.16        <NA>
    ## 8496 Gatorade Propel Grape Water 1 lt     3 10.16        <NA>
    ## 8497      Yerba Mate Tropical 15.5 oz     2 10.16        <NA>
    ## 8498                  Rockstar Energy     2 10.16        <NA>
    ## 8499   Rockstar Energy Pure Zero PNCH     2 10.16        <NA>
    ## 8500     Starbucks Double Shot 6.5 Oz     2 10.16        <NA>
    ## 8501                Water Aquafina 1L     3 10.16        <NA>
    ## 8502 Cold Brew Chocolate Cream Starbu     2 10.16        <NA>
    ## 8503 Juice Fuji Apple Red Jacket 12oz     2 10.16        <NA>
    ## 8504 Red Jacket Strawberry Apple Juic     2 10.16        <NA>
    ## 8505                 Cornell Lemonade     4 10.16        <NA>
    ## 8506       Lipton Pure Leaf Sweet Tea     3 10.16        <NA>
    ## 8507 Tea Blackberry Pure Leaf 18.5 oz     3 10.16        <NA>
    ## 8508 Tea Sweet W/ Le Pure Leaf Lipton     3 10.16        <NA>
    ## 8509         Juice Apple Dole 15.2 oz     3 10.16        <NA>
    ## 8510 Juice Ocean Spray Cranbe Grape 1     3 10.16        <NA>
    ## 8511 Mountain Dew Kickstart Black Che     2 10.16        <NA>
    ## 8512     Cider Apple Red Jacket 12 oz     2 10.16        <NA>
    ## 8513      Gatorade Berry Zero G 28 oz     2 10.16        <NA>
    ## 8514             Gatorade Orange 28oz     2 10.16        <NA>
    ## 8515        Gatorade Zero Grape 28 oz     2 10.16        <NA>
    ## 8516    Glacier Cherry Gatorade 28 oz     2 10.16        <NA>
    ## 8517    Glacier Freeze Gatorade 28 oz     2 10.16        <NA>
    ## 8518       Juice Grape Tropicana 11oz     2 10.16        <NA>
    ## 8519  Juice Lively Lemonade Tropicana     2 10.16        <NA>
    ## 8520 Juice Orange Homestyle Tropicana     2 10.16        <NA>
    ## 8521 Juice Orange Premium Topicana 11     2 10.16        <NA>
    ## 8522 Juice Raspberry Lemonade Tropica     2 10.16        <NA>
    ## 8523             Water Gatorade 700ml     2 10.16        <NA>
    ## 8524   Soda Tamarind Jarritos 12.5 oz     2 10.16        <NA>
    ## 8525    Muscle Milk KO Chocolate 14oz     1 10.16        <NA>
    ## 8526 Muscle Milk P40 Strawberry Cream     1 10.16        <NA>
    ## 8527 Muscle Milk PROSRS 40 Intense Va     1 10.16        <NA>
    ## 8528  Soda Ginger Ale Schweppes 20 Oz     2 10.16        <NA>
    ## 8529     Soda Mountain Dew Zero 20 Oz     2 10.16        <NA>
    ## 8530          Soda Orange Crush 20 Oz     2 10.16        <NA>
    ## 8531     Soda Starry Lemon Lime 20 oz     2 10.16        <NA>
    ## 8532   Tea Iced Rasberry Lipton 16 oz     2 10.16        <NA>
    ## 8533     Tea Light Peach Lipton 20 oz     2 10.16        <NA>
    ## 8534         Juice Lemonade Dole 20oz     2 10.16        <NA>
    ## 8535        Juice Orange Dole 15.2 oz     2 10.16        <NA>
    ## 8536    Frappuccino Coffee 13.7 oz SB     1 10.16        <NA>
    ## 8537          Juice Naked Berry Blast     1 10.16        <NA>
    ## 8538          Naked Red Machine Juice     1 10.16        <NA>
    ## 8539 Gatorade Gatorlyte Strawberry Ki     1 10.16        <NA>
    ## 8540 Gatorade Gatorlyte Zero Fruit Pu     1 10.16        <NA>
    ## 8541 Gatorade Gatorlyte Zero Lemon Li     1 10.16        <NA>
    ## 8542    Tea Black Milk Ito En 11.8 oz     1 10.16        <NA>
    ## 8543 Tea Green Matcha Milk Ito En 11.     1 10.16        <NA>
    ## 8544  Energy Blue Crush Celsius 16 oz     1 10.16        <NA>
    ## 8545 Energy Cherry Limeade Celsius 16     1 10.16        <NA>
    ## 8546 Energy Dragonberry Celsius 16 oz     1 10.16        <NA>
    ## 8547 Energy Mango Tango Celsius 16 oz     1 10.16        <NA>
    ## 8548   Energy StrawLemon Celsius 12oz     1 10.16        <NA>
    ## 8549 Mountain Dew Kickstart Orange 16     1 10.16        <NA>
    ## 8550       Gatorade Fruit Punch 28 oz     1 10.16        <NA>
    ## 8551       Juice AppleTropicana 11 oz     1 10.16        <NA>
    ## 8552      Soda Mango Jarritos 12.5 oz     1 10.16        <NA>
    ## 8553     Soda Pepsi Wild Cherry 20 Oz     1 10.16        <NA>
    ## 8554 Tea Pure Leaf Zero Sugar Sweet T     1 10.16        <NA>
    ## 8555   Soda Mandarin Jarritos 12.5 oz     1 10.16        <NA>
    ## 8556   Cornell Low Fat Chocolate Milk     8 10.16        <NA>
    ## 8557 Yogurt 0% Fat Vanilla Greek Oiko     7 10.16        <NA>
    ## 8558 Yogurt Straw Banana Chobani Drin     4 10.16        <NA>
    ## 8559       Cornell 2% Milk (70000380)     4 10.16        <NA>
    ## 8560 Yogurt Strawberry Banana Greek C     5 10.16        <NA>
    ## 8561 Yogurt Mixed Berry Chobani Drink     3 10.16        <NA>
    ## 8562 Yogurt Flip Almond Coco Loco Cho     4 10.16        <NA>
    ## 8563   Milk Chocolate LF Cornell 8 Oz     6 10.16        <NA>
    ## 8564  Yogurt Strawberry Greek Chobani     4 10.16        <NA>
    ## 8565  Yogurt Flip Peanut Butter Dream     3 10.16        <NA>
    ## 8566       Yogurt Mango Greek Chobani     3 10.16        <NA>
    ## 8567       Yogurt Peach Greek Chobani     3 10.16        <NA>
    ## 8568       Cornell 2% Milk (70000381)     3 10.16        <NA>
    ## 8569 Yogurt Black Cherry Greek Choban     2 10.16        <NA>
    ## 8570   Yogurt Blueberry Greek Chobani     2 10.16        <NA>
    ## 8571  Yogurt Mango Chobani Drink 7 oz     1 10.16        <NA>
    ## 8572  Cornell Dairy Strawberry Yogurt     2 10.16        <NA>
    ## 8573       Yogurt Plain Greek Chobani     1 10.16        <NA>
    ## 8574            8 oz Vanilla Soy Silk     1 10.16        <NA>
    ## 8575          Silk Chocolate Soy Milk     1 10.16        <NA>
    ## 8576       Cornell Dairy Mango Yogurt     1 10.16        <NA>
    ## 8577               Cornell Whole Milk     1 10.16        <NA>
    ## 8578        Bar S'Mores Quest 2.12 oz     5 10.16        <NA>
    ## 8579  Bar Frosted Birthday Cake Quest     3 10.16        <NA>
    ## 8580 Bar White Chocolate Macadamia Cl     3 10.16        <NA>
    ## 8581 Jelly Konjac Apple Grape Tastell     2 10.16        <NA>
    ## 8582      Kind Almond And Coconut Bar     2 10.16        <NA>
    ## 8583 Bar Cookie & Cream Quest 2.12 oz     1 10.16        <NA>
    ## 8584 Bar CHOC Mint Builders Clif bar2     1 10.16        <NA>
    ## 8585  Bar Peanut Butter Builders Clif     1 10.16        <NA>
    ## 8586   Fruit And Nut Delight Kind Bar     1 10.16        <NA>
    ## 8587        Oat And Honey Granola Bar     2 10.16        <NA>
    ## 8588 Jelly Konjac Mango Pineapple Tas     1 10.16        <NA>
    ## 8589      Jelly Konjac Peach Tastelli     1 10.16        <NA>
    ## 8590 Bar That's It Apple+Strawberry 1     1 10.16        <NA>
    ## 8591                      Fruit Whole    50 10.16        <NA>
    ## 8592     Chip Funky Fusion Dirty 2 oz     3 10.16        <NA>
    ## 8593        Chip Kettle Sea Salt 2 oz     2 10.16        <NA>
    ## 8594           Chips Dirty Sea Salted     2 10.16        <NA>
    ## 8595       Jalapeno Heat Chips Kosher     2 10.16        <NA>
    ## 8596   Utz Honey Barbecue Chip 1.5 oz     2 10.16        <NA>
    ## 8597 Nuts Almonds Honey 1.5 oz Sahale     1 10.16        <NA>
    ## 8598         Pretzel Thin 2.12 oz Utz     2 10.16        <NA>
    ## 8599    Chip Kettle Salt Vinegar 2 oz     1 10.16        <NA>
    ## 8600       Chip Maui Onion Dirty 2 oz     1 10.16        <NA>
    ## 8601 Chip Potato Dirty BBQ Mesquite 2     1 10.16        <NA>
    ## 8602  Chip Sour Cream And Onion Dirty     1 10.16        <NA>
    ## 8603                 Utz Regular Chip     1 10.16        <NA>
    ## 8604 GF Sweet Street Choloate Brownie     4 10.16        <NA>
    ## 8605      Chewy Marshmallow GF 2.1 oz     4 10.16        <NA>
    ## 8606   Candy CHOC Cara Caff Bar Awake     1 10.16        <NA>
    ## 8607             Orbit Sweet Mint Gum     1 10.16        <NA>
    ## 8608   Fresh Cut Watermelon Fruit Cup     1 10.16        <NA>
    ## 8609                 MochiCookCrm1.5o     1 10.16        <NA>
    ## 8610 Ice Cream Mochi Double Chocolate     1 10.16        <NA>
    ## 8611               Golden Dragon Roll     4 10.16 grab 'n' go
    ## 8612                  California Roll     4 10.16 grab 'n' go
    ## 8613              Tempura Shrimp Roll     2 10.16 grab 'n' go
    ## 8614                      Salmon Roll     2 10.16 grab 'n' go
    ## 8615                  Spicy Tuna Roll     2 10.16 grab 'n' go
    ## 8616                     Alaskan Roll     1 10.16 grab 'n' go
    ## 8617              Tempura Crunch Roll     1 10.16 grab 'n' go
    ## 8618                         TSA Roll     1 10.16 grab 'n' go
    ## 8619                  Hawaiian Sunset     1 10.16 grab 'n' go
    ## 8620                     Avocado Roll     1 10.16 grab 'n' go
    ## 8621              Wrap Chicken Caesar     4 10.16 grab 'n' go
    ## 8622 Sandwich Black Forrest Ham & Swi     3 10.16 grab 'n' go
    ## 8623 Sandwich Crispy Chicken Milanese     2 10.16 grab 'n' go
    ## 8624 Sandwich Prosciutto & Mozzarella     2 10.16 grab 'n' go
    ## 8625             Wrap Buffalo Chicken     2 10.16 grab 'n' go
    ## 8626             Salad Chicken Caesar     1 10.16 grab 'n' go
    ## 8627                     Muffin Jumbo    20 10.16        <NA>
    ## 8628                           Cookie    23 10.16        <NA>
    ## 8629                   Croissant Choc     4 10.16        <NA>
    ## 8630                     Cinnamon Bun     3 10.16        <NA>
    ## 8631                          Brownie     3 10.16        <NA>
    ## 8632                Croissant Blue CC     1 10.16        <NA>
    ## 8633               Croissant Straw CC     1 10.16        <NA>
    ## 8634      Combo 16 oz Coffee & Muffin     1 10.16        <NA>
    ## 8635                            Scone     1 10.16        <NA>
    ## 8636                      Coffee Cake     1 10.16        <NA>
    ## 8637                   BowlSouthChick     6 10.16 grab 'n' go
    ## 8638                 BowlChickAlfrPen     1 10.16 grab 'n' go
    ## 8639                   BowlMedProtein     1 10.16 grab 'n' go
    ## 8640                     PBJ on Wheat     8 10.16 grab 'n' go
    ## 8641                 GF ChicCaesarSld     1 10.16 grab 'n' go
    ## 8642                   GF Turkey Sand     1 10.16 grab 'n' go
    ## 8643                 GFSunButterJelly     1 10.16 grab 'n' go
