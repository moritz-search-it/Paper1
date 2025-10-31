
- [Visualisation Paper BraveNewMeat](#visualisation-paper-bravenewmeat)
  - [Inroduction](#inroduction)
    - [Contribution analysis](#contribution-analysis)
  - [Results](#results)
    - [Preparing the data](#preparing-the-data)
    - [nFU (qc-Protein and NRF)
      Comparison](#nfu-qc-protein-and-nrf-comparison)
    - [Visualisation of the environmental
      impacts](#visualisation-of-the-environmental-impacts)
    - [Summary of Metrics](#summary-of-metrics)
  - [Graphical abstract and Poster](#graphical-abstract-and-poster)

------------------------------------------------------------------------

title: “Visualisation Paper BraveNewMeat” date: “31 October, 2025”
output: github_document: toc: true

------------------------------------------------------------------------

# Visualisation Paper BraveNewMeat

``` r
#load all needed packages
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(ggplot2)
library(knitr)
library(openxlsx)
library(RColorBrewer)
library(scales)
library(patchwork)

theme_set(theme_bw(base_size = 45))
#  scale_fill_manual(values = cbp1)+
#theme_set(legend.position = "top")
```

``` r
rm(list = ls())


#set project path to current path 
projectPath <- here::here()

#working directories input and output
input_path <- file.path("~/Documents/Projekte/Paper1")
working_dir_output <- file.path(projectPath, "results")

# if no folder with results exists, create a folder with the name results
if (!dir.exists("results")) {
  dir.create("results")
}


# The colorblind palette
cbp1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#999999",
          "#0072B2","#F0E442","#D55E00", "#CC79A7", "yellowgreen")
# "rgba(255, 255, 255, 0)"
```

## Inroduction

This markdown file presents the visualization of results from the
scientific research project **“BraveNewMeat.”** The data were curated
and validated by Morit Herrmann, who takes full responsibility for their
accuracy.

### Contribution analysis

The PBMA is composed of six ingredients and three processing steps.
Their contributions to **GWP100, total land use, freshwater
eutrophication, acidification, and water scarcity** are shown below.
Minor ingredients, such as seasonings were omitted if env. impact was
below cut-off (\< 1 %), but the binding agent, methylcellulose, was
retained. In addition to the extruded soybean protein concentrate,
**refined rapeseed oil** contributes substantially to the overall
environmental impacts. Varying the oil source (sunflower, olive, or
soybean) did not produce significant changes in the total contribution
(data not shown). For the soy-based drink and tofu, **soybeans** are the
primary contributor.

``` r
# Load data
data_cont <- read_excel(
  file.path(input_path, "Results_v4.xlsx"),
  sheet = "ForR_cont",
  col_types = c(rep("text", 4), rep("numeric", 42))
)

# Distinguish between raw material and processing
# Do not use the data simultaneously
data_cont <- data_cont %>%
  mutate(rawm = ifelse(is.na(rawm), "processing", "raw material"))

# Select and tidy data
data_cont_tidy <- data_cont %>%
  select(
    1:4,
    "CED [MJ]",
    "GWP100 [kg CO2-eq]",
    "Land-occ [m2a]",
    "Water-scar [m3]",
    "Eutr-fw [kg P-eq]",
    "Acid-terr [kg SO2-eq]"
  ) %>%
  pivot_longer(
    cols = 5:10,
    names_to = "impact",
    values_to = "value"
  ) %>%
  mutate(
    impact = factor(
      impact,
      levels = c(
        "CED [MJ]",
        "GWP100 [kg CO2-eq]",
        "Water-scar [m3]",
        "Land-occ [m2a]",
        "Acid-terr [kg SO2-eq]",
        "Eutr-fw [kg P-eq]"
      )
    )
  )

# Calculate percentage for stacked columns
data_cont_tidy_perc <- data_cont_tidy %>%
  group_by(item, impact, rawm, country) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(percentage = (value / total) * 100)
```

Graphs are saved in the `results` folder in **JPEG and EPS** formats,
separately for CH and for the sensitivity analysis using Brazilian
soybeans.

<figure>
<img src="./results/contribution_PBMA_CH.jpeg"
alt="Contribution Analysis for the PBMA." />
<figcaption aria-hidden="true">Contribution Analysis for the
PBMA.</figcaption>
</figure>

## Results

The dataset contains **7** items, including three reference items
(chicken, beef, milk) and plant-based alternatives:

- Soybeans, cooked  
- Tofu, plain  
- Plant-based meat analogue (PBMA) beef burger  
- Soy-drink

Environmental data were calculated using **SimaPro** with the **SALCA
v.2** impact assessment method (unpublished). A total of **45** impact
categories were included.

The selected **nutritional functional unit (nFU)** is either:

- 1 g of **quality-corrected (qc-) protein**  
- 1 **Nutrient Rich subscore (NRprot7)** per 100 kcal

Additionally, a **sensitivity analysis** was conducted:

- ±20% variation in raw material inputs  
- Changing the country of origin from Switzerland to Brazil for
  plant-based alternatives  
- Exploring variations when different reference values for qc-protein
  and NR calculations (age, gender) were applied

This results in **84** unique entries. Below, results for **GWP100** are
shown.

``` r
knitr::kable(data_full_tidy%>% filter(impact == "GWP100 [kg CO2-eq]") %>% select(-mm), format="markdown")
```

| item | nfu | country | reference | gender | impact | ymin | mean | ymax |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|
| Soybeans, cooked | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0033425 | 0.0038592 | 0.0043759 |
| Tofu, plain | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0044760 | 0.0048418 | 0.0052076 |
| SBMA | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0033404 | 0.0040803 | 0.0048201 |
| Soydrink, UHT | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0058061 | 0.0063414 | 0.0068767 |
| Chicken (CH) meat | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0074751 | NA |
| Beef (CH), minced | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0384791 | NA |
| Cow milk (CH), UHT | qc-prot | CH | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0265293 | NA |
| Soybeans, cooked | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0028818 | 0.0033273 | 0.0037728 |
| Tofu, plain | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0055658 | 0.0060206 | 0.0064755 |
| SBMA | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0036571 | 0.0044670 | 0.0052770 |
| Soydrink, UHT | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0014691 | 0.0016046 | 0.0017400 |
| Chicken (CH) meat | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0216549 | NA |
| Beef (CH), minced | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0841381 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | CH | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0139082 | NA |
| Soybeans, cooked | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | 0.0027374 | 0.0031606 | 0.0035837 |
| Tofu, plain | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | 0.0051992 | 0.0056241 | 0.0060490 |
| SBMA | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | 0.0036378 | 0.0044435 | 0.0052492 |
| Soydrink, UHT | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | 0.0013752 | 0.0015020 | 0.0016288 |
| Chicken (CH) meat | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | NA | 0.0196975 | NA |
| Beef (CH), minced | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | NA | 0.0785123 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | CH | adult | women | GWP100 \[kg CO2-eq\] | NA | 0.0131378 | NA |
| Soybeans, cooked | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | 0.0029505 | 0.0034066 | 0.0038627 |
| Tofu, plain | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | 0.0058055 | 0.0062800 | 0.0067545 |
| SBMA | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | 0.0035513 | 0.0043378 | 0.0051243 |
| Soydrink, UHT | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | 0.0015271 | 0.0016678 | 0.0018086 |
| Chicken (CH) meat | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | NA | 0.0234520 | NA |
| Beef (CH), minced | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | NA | 0.0882470 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | CH | adult | men | GWP100 \[kg CO2-eq\] | NA | 0.0145961 | NA |
| Soybeans, cooked | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0071838 | 0.0086608 | 0.0101378 |
| Tofu, plain | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0071956 | 0.0082413 | 0.0092870 |
| SBMA | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0054072 | 0.0066638 | 0.0079203 |
| Soydrink, UHT | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0097858 | 0.0113160 | 0.0128463 |
| Chicken (CH) meat | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0074751 | NA |
| Beef (CH), minced | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0384791 | NA |
| Cow milk (CH), UHT | qc-prot | BR | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0265293 | NA |
| Soybeans, cooked | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0061937 | 0.0074671 | 0.0087406 |
| Tofu, plain | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0089475 | 0.0102478 | 0.0115482 |
| SBMA | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0059198 | 0.0072954 | 0.0086711 |
| Soydrink, UHT | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | 0.0024761 | 0.0028633 | 0.0032504 |
| Chicken (CH) meat | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0216549 | NA |
| Beef (CH), minced | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0841381 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | BR | adult | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0139082 | NA |
| Soybeans, cooked | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | 0.0058833 | 0.0070930 | 0.0083026 |
| Tofu, plain | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | 0.0083582 | 0.0095729 | 0.0107876 |
| SBMA | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | 0.0058886 | 0.0072570 | 0.0086253 |
| Soydrink, UHT | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | 0.0023178 | 0.0026802 | 0.0030427 |
| Chicken (CH) meat | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | NA | 0.0196975 | NA |
| Beef (CH), minced | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | NA | 0.0785123 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | BR | adult | women | GWP100 \[kg CO2-eq\] | NA | 0.0131378 | NA |
| Soybeans, cooked | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | 0.0063413 | 0.0076451 | 0.0089489 |
| Tofu, plain | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | 0.0089475 | 0.0106893 | 0.0120456 |
| SBMA | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | 0.0057485 | 0.0070843 | 0.0084202 |
| Soydrink, UHT | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | 0.0025737 | 0.0029762 | 0.0033787 |
| Chicken (CH) meat | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | NA | 0.0234520 | NA |
| Beef (CH), minced | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | NA | 0.0882470 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | BR | adult | men | GWP100 \[kg CO2-eq\] | NA | 0.0145961 | NA |
| Soybeans, cooked | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0038682 | 0.0044661 | 0.0050640 |
| Tofu, plain | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0052544 | 0.0056839 | 0.0061133 |
| SBMA | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0039214 | 0.0047899 | 0.0056584 |
| Soydrink, UHT | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0068008 | 0.0074278 | 0.0080548 |
| Chicken (CH) meat | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0081464 | NA |
| Beef (CH), minced | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0416331 | NA |
| Cow milk (CH), UHT | qc-prot | CH | child | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0311431 | NA |
| Soybeans, cooked | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0009876 | 0.0011402 | 0.0012929 |
| Tofu, plain | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0018222 | 0.0019711 | 0.0021200 |
| SBMA | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0015142 | 0.0018496 | 0.0021850 |
| Soydrink, UHT | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | 0.0004774 | 0.0005214 | 0.0005655 |
| Chicken (CH) meat | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0054750 | NA |
| Beef (CH), minced | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0270462 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | CH | child | unisex | GWP100 \[kg CO2-eq\] | NA | 0.0051299 | NA |
| Soybeans, cooked | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | 0.0009748 | 0.0011255 | 0.0012761 |
| Tofu, plain | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | 0.0017962 | 0.0019430 | 0.0020898 |
| SBMA | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | 0.0015049 | 0.0018382 | 0.0021715 |
| Soydrink, UHT | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | 0.0004716 | 0.0005150 | 0.0005585 |
| Chicken (CH) meat | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | NA | 0.0054724 | NA |
| Beef (CH), minced | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | NA | 0.0270274 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | CH | child | women | GWP100 \[kg CO2-eq\] | NA | 0.0051225 | NA |
| Soybeans, cooked | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | 0.0009997 | 0.0011543 | 0.0013088 |
| Tofu, plain | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | 0.0018469 | 0.0019979 | 0.0021488 |
| SBMA | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | 0.0015229 | 0.0018602 | 0.0021975 |
| Soydrink, UHT | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | 0.0004830 | 0.0005275 | 0.0005720 |
| Chicken (CH) meat | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | NA | 0.0054774 | NA |
| Beef (CH), minced | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | NA | 0.0270636 | NA |
| Cow milk (CH), UHT | NRprot7_2000kcal | CH | child | men | GWP100 \[kg CO2-eq\] | NA | 0.0051368 | NA |

### Preparing the data

Preparing the results for visualisation, adding reference values to each
products impact and split datasets by country/reference for further
analysis or plotting

``` r
########### Reference-based normalization ###########
# Compare each product's impacts to a reference (Beef or Cow milk)
# Values are expressed as % of the reference mean

# Extract reference mean data
# For meat: Beef (CH), minced
# For milk: Cow milk (CH), UHT
Reference <- selected_data_tidy %>%
  group_by(nfu, country, reference, gender, impact, mm) %>%
  filter(item %in% c("Beef (CH), minced", "Cow milk (CH), UHT")) %>%
  select(-ymin, -ymax, -item) %>%
  rename(mean_m = mean)

# Combine original data with reference means and calculate % differences
selected_data_tidy_d1 <- selected_data_tidy %>%
  group_by(item, nfu, country, reference, gender, impact, mm) %>%
  left_join(
    Reference,
    by = c("nfu", "country", "reference", "gender", "impact", "mm")
  ) %>%
  mutate(
    ymin_1 = (ymin / mean_m) * 100,  # Lower bound as % of reference
    mean_1 = (mean / mean_m) * 100,  # Mean as % of reference
    ymax_1 = (ymax / mean_m) * 100   # Upper bound as % of reference
  ) %>%
  ungroup()

# Account for gender variation (sensitivity analysis)
# Combine male/female data to derive overall min, median, and max
selected_data_tidy_d1 <- selected_data_tidy_d1 %>%
  group_by(item, nfu, country, reference, impact, mm) %>%
  mutate(
    ymin_new1 = min(ymin_1),
    ymax_new1 = max(ymax_1),
    mean_new1 = median(mean_1)
  ) %>%
  ungroup()

# Create separate datasets by country and reference group
tidy_d1_CH    <- selected_data_tidy_d1 %>% filter(country == "CH", reference == "adult")   # Swiss adults
tidy_d1_BR    <- selected_data_tidy_d1 %>% filter(country == "BR", reference == "adult")   # Brazilian adults
tidy_d1_child <- selected_data_tidy_d1 %>% filter(reference == "child")                    # Children (both countries)

# Collect all three datasets into a list for batch processing or plotting
all_d1_datasets <- list(tidy_d1_CH, tidy_d1_BR, tidy_d1_child)


########### Max-based normalization ###########
# Normalize by the highest mean value in each group
# (Often beef, sometimes chicken, depending on data)
# Used as an alternative reference scenario

selected_data_tidy_max <- selected_data_tidy %>%
  group_by(nfu, country, reference, gender, impact, mm) %>%
  mutate(
    mean_m  = max(mean),             # Identify the maximum mean in group
    ymin_1  = (ymin / mean_m) * 100, # Lower bound as % of max
    mean_1  = (mean / mean_m) * 100, # Mean as % of max
    ymax_1  = (ymax / mean_m) * 100  # Upper bound as % of max
  ) %>%
  ungroup()

# Split datasets by country/reference for further analysis or plotting
tidy_max_CH    <- selected_data_tidy_max %>% filter(country == "CH", reference == "adult")   # Swiss adults
tidy_max_BR    <- selected_data_tidy_max %>% filter(country == "BR", reference == "adult")   # Brazilian adults
tidy_max_child <- selected_data_tidy_max %>% filter(reference == "child")                    # Children (both countries)
```

### nFU (qc-Protein and NRF) Comparison

We examined the effect of using different nFU metrics. As shown in the
data, **when using NRF instead of protein only**, chicken meat—a
previously efficient animal product—performs worse relative to
plant-based products. This is because plant-based products include other
beneficial nutrients such as fiber and unsaturated fats. However,
outcomes are highly sensitive to the choice of nutrients included in the
NRF.

``` r
########### nFU comparison plot (Switzerland, adults) ###########

# Set position for error bars (grouped bars)
dodge <- position_dodge(width = 0.9)

# Prepare composition data for plotting
# - Start with CH adult data
# - Pivot gender columns wide (so 'men' and 'women' are separate)
# - Keep only GWP impact (climate change)
# - Exclude reference items (Beef, Cow milk)
# - Simplify nFU labels for clarity
composition <- tidy_d1_CH %>%
  ungroup() %>%
  pivot_wider(
    names_from = gender,
    values_from = mean_1
  ) %>%
  group_by(nfu, item) %>%
  filter(
    !item %in% c("Beef (CH), minced", "Cow milk (CH), UHT"),
    impact == "GWP100 [kg CO2-eq]"
  ) %>%
  mutate(
    nfu = ifelse(nfu == "NRprot7_2000kcal", "NRprot7", "qc-protein")
  )

# Create bar plot comparing nFU systems across food items
# - Bars show mean values (unisex)
# - Error bars span from women's to men's values
nfu_plot <- composition %>%
  ggplot(aes(item, unisex, fill = nfu)) +
  geom_col(position = dodge) +
  geom_errorbar(
    aes(ymin = women, ymax = men),
    position = dodge,
    width = 0.4,
    linewidth = 1.5
  ) +
  scale_fill_brewer(palette = "Dark2") +   # Use a qualitative color palette
  ylim(0, 30) +                             # Limit y-axis to 0–30%
  labs(
    x = "",
    y = "GWP100, relative to beef and cow milk [%]"
  ) +
  scale_x_discrete(labels = label_wrap(9))  # Wrap long x-axis labels

# Save plot in both JPEG and EPS formats
ggsave(
  plot = nfu_plot,
  file.path(working_dir_output, "nFU_comp.jpeg"),
  device = "jpeg",
  width = 30, height = 20, dpi = "retina"
)
ggsave(
  plot = nfu_plot,
  file.path(working_dir_output, "nFU_comp.eps"),
  device = "eps",
  width = 30, height = 20
)
```

<figure>
<img src="./results/nfu_comp.jpeg" style="width:70.0%"
alt="Change of nutritional Functional Unit." />
<figcaption aria-hidden="true">Change of nutritional Functional
Unit.</figcaption>
</figure>

### Visualisation of the environmental impacts

Detailed visualizations are provided for each product and for the three
different scenarios. The storage order for the plots is as follows and
is found in results:

- scenario CH soy, adult reference (qc-prot and NRF): ending -1
- scenario BR soy, adult reference (qc-prot and NRF): ending -2  
- scenario CH soy, child reference (qc-prot and NRF): ending -3

``` r
# =============================================================================
# Plot generation loop for relative impact comparisons
# =============================================================================
# Input:
#   all_d1_datasets – list of 3 tidy datasets (CH, BR, child)
#   Each element ('tidy_d1') must contain:
#       • item       = food item (factor: e.g. "Beef (CH), minced", "Tofu, plain", etc.)
#       • nfu        = nutritional functional unit ("qc-prot", "NRprot7_2000kcal", ...)
#       • country    = country code ("CH", "BR", ...)
#       • reference  = dietary reference group ("adult", "child")
#       • gender     = gender category ("unisex", "men", "women")
#       • impact     = environmental impact category (e.g. "GWP100 [kg CO2-eq]")
#       • mm         = "meat" or "milk" (product category)
#       • mean_1, ymin_1, ymax_1 = relative values in % (normalized to reference)
# -----------------------------------------------------------------------------

# Define color palette for plotting
cbp1 <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#999999",
  "#0072B2", "#F0E442", "#D55E00", "#CC79A7", "yellowgreen"
)

# Loop through each dataset (CH, BR, child)
for (i in seq_along(all_d1_datasets)) {

  # Extract one tidy dataset from the list
  tidy_d1 <- all_d1_datasets[[i]]

  # Loop through both nutritional functional units
  for (nfu_value in c("qc-prot", "NRprot7_2000kcal")) {

    # Sub-palettes for item coloring
    cbp1_new  <- cbp1[c(3, 4, 5)]             # small palette for meat-only plots
    cbp1_new2 <- cbp1[c(1, 2, 3, 4, 5, 6, 7)] # larger palette for faceted plots

    # Define dodging for grouped bars and error bars
    dodge <- position_dodge(width = 0.9)

    # ---------------------------
    # Plot A (aa): Meat-only products
    # - Filters for unisex data
    # - Excludes NRF, LIM2, and NRprot7 impact categories
    # - Selects only meat items except reference items (beef & chicken)
    # - Plots relative impacts vs. impact categories
    # ---------------------------
    aa <- tidy_d1 %>%
      filter(
        gender == "unisex",
        impact != "NRprot7" & impact != "LIM2" & impact != "NRF"
      ) %>%
      filter(
        nfu == nfu_value,
        mm == "meat",
        item != "Beef (CH), minced" & item != "Chicken (CH) meat"
      ) %>%
      ggplot(aes(impact, mean_1, fill = item)) +
      geom_col(position = dodge) +
      scale_fill_manual(values = cbp1_new) +
      geom_errorbar(aes(ymin = ymin_1, ymax = ymax_1), position = dodge, width = 0.5) +
      labs(x = "", y = "Impacts relative to beef or milk [%]") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    # ---------------------------
    # Plot B (ab): All items (meat + milk)
    # - Filters for unisex data and valid impacts
    # - Facets by 'mm' (meat vs. milk products)
    # - Adds reference line at 100% (reference impact)
    # ---------------------------
    ab <- tidy_d1 %>%
      filter(
        gender == "unisex",
        impact != "NRprot7" & impact != "LIM2" & impact != "NRF"
      ) %>%
      filter(nfu == nfu_value) %>%
      ggplot(aes(impact, mean_1, fill = item)) +
      geom_col(position = dodge) +
      facet_grid(rows = vars(mm), scales = "free", space = "free_y") +
      scale_fill_manual(values = cbp1_new2) +
      geom_hline(yintercept = 100) +
      geom_errorbar(aes(ymin = ymin_1, ymax = ymax_1), position = dodge, width = 0.5) +
      labs(x = "", y = "") +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    # ---------------------------
    # Combine plots A and B using patchwork
    # Adds annotation tags (A, B, etc.)
    # ---------------------------
    plot_combined <- aa + ab + plot_annotation(tag_levels = "A")

    # ---------------------------
    # Export combined plots to JPEG and EPS formats
    # - Filenames include nfu and dataset index
    # ---------------------------
    ggsave(
      file.path(working_dir_output, paste0("plot_", nfu_value, "-", i, ".jpeg")),
      plot_combined,
      device = "jpeg",
      width = 30, height = 20, dpi = "retina"
    )

    ggsave(
      file.path(working_dir_output, paste0("plot_", nfu_value, "-", i, ".eps")),
      plot_combined,
      device = "eps",
      width = 30, height = 20
    )

  } # end nfu loop
} # end dataset loop
```

<figure>
<img src="./results/plot_qc-prot-1.jpeg" alt="Results per qc-prot" />
<figcaption aria-hidden="true">Results per qc-prot</figcaption>
</figure>

<figure>
<img src="./results/plot_NRprot7_2000kcal-1.jpeg"
alt="Results per NRprot7 (2000kcal-adjusted)" />
<figcaption aria-hidden="true">Results per NRprot7
(2000kcal-adjusted)</figcaption>
</figure>

### Summary of Metrics

The **NRF metric** is composed of two parts:

- **Favourable nutrients:** NRprot7  
- **Disqualifying nutrients:** LIM2

As seen in previous section ([nFU
comparison](###%20nFU%20(qc-Protein%20and%20NRF))), animal proteins rank
lower in the NRF metric compared to plant proteins. But wyh? We compare
the different submetrics for the NRF.

``` r
# -----------------------------------------------------------------------------
# 1. Prepare NRF dataset
#    - Select only relevant impacts ("NRprot7", "LIM2")
#    - Add empty placeholder rows for layout consistency
# -----------------------------------------------------------------------------
NRF <- tidy_d1_CH %>%
  filter(
    gender == "unisex",
    impact == "NRprot7" | impact == "LIM2"
  ) %>%
  add_row(item = " ", impact = "NRprot7") %>%  # empty row as separator
  add_row(item = " ", impact = "LIM2")         # empty row as separator

# -----------------------------------------------------------------------------
# 2. Define factor levels for items and impacts
#    - Ensures consistent order of items on the plot
# -----------------------------------------------------------------------------
NRF <- NRF %>%
  mutate(
    item = factor(
      item,
      levels = c(
        "Beef (CH), minced",
        "Chicken (CH) meat",
        "SBMA",
        "Tofu, plain",
        "Soybeans, cooked",
        " ",                      # blank separator
        "Cow milk (CH), UHT",
        "Soydrink, UHT"
      )
    ),
    impact = factor(impact, levels = c("NRprot7", "LIM2"))
  )

# -----------------------------------------------------------------------------
# 3. Define custom color palette
#    - Adds transparent white to create a visual gap (for empty row)
# -----------------------------------------------------------------------------
transparent_white <- adjustcolor("white", alpha.f = 0)  # fully transparent white

cbp1_new <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#999999",
  transparent_white,   # blank entry separator
  "#0072B2", "#F0E442"
)

# -----------------------------------------------------------------------------
# 4. Create bar plot for NRF and LIM2 comparison
# -----------------------------------------------------------------------------
abc <- NRF %>%
  ggplot(aes(impact, mean_1, fill = item)) +
  geom_col(position = dodge) +                            # main bars
  scale_fill_manual(values = cbp1_new) +                  # manual colors
  geom_hline(yintercept = 100) +                          # reference line
  labs(
    x = "",
    y = "Sub-scores relative to beef or cow milk [%]"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

# -----------------------------------------------------------------------------
# 5. Save plots in JPEG and EPS formats
# -----------------------------------------------------------------------------
ggsave(
  file.path(working_dir_output, "nrf_comparison.jpeg"),
  abc,
  device = "jpeg",
  width = 30, height = 20, dpi = "retina"
)

ggsave(
  file.path(working_dir_output, "nrf_comparison.eps"),
  abc,
  device = "eps",
  width = 30, height = 20
)
```

<figure>
<img src="./results/nrf_comparison.jpeg"
alt="Submetrics of NRF calculations visualisation" />
<figcaption aria-hidden="true">Submetrics of NRF calculations
visualisation</figcaption>
</figure>

## Graphical abstract and Poster

A subset of the full dataset is used for the graphical abstract. Only
results from **NRprot7 calculations**, using the **average DRI for
adults**, are included.

Focus is on three key impact indicators:

- **GWP100 (CO₂-eq)**  
- **Water scarcity (m³)**  
- **Land occupation per year (m²a)**

``` r
# -----------------------------------------------------------------------------
# 1. Define custom color palette for the poster
# -----------------------------------------------------------------------------
cbp1_new <- c("#000000", "#E69F00", "#0072B2", "#F0E442", "#D55E00", "#009E73")

# -----------------------------------------------------------------------------
# 2. Filter base dataset for poster (CH, adult, unisex, NRprot7_2000kcal)
#    - Select relevant impacts and round mean values
#    - Exclude unused impact categories
# -----------------------------------------------------------------------------
poster0 <- tidy_d1_CH %>%
  filter(
    gender == "unisex",
    nfu == "NRprot7_2000kcal",
    country == "CH",
    reference == "adult"
  ) %>%
  select(item, impact, mean_1) %>%
  mutate(mean_1 = round(mean_1, 0)) %>%
  filter(
    impact != "CED [MJ]",
    impact != "Eutr-fw [kg P-eq]",
    impact != "Acid-terr [kg SO2-eq]",
    impact != "NRprot7",
    impact != "LIM2",
    impact != "NRF"
  )

# -----------------------------------------------------------------------------
# 3. Prepare subset for key products (soy burger and chicken meat)
#    - Rename "SBMA" → "Soy burger"
# -----------------------------------------------------------------------------
poster1 <- poster0 %>%
  filter(item == "SBMA" | item == "Chicken (CH) meat") %>%
  mutate(item = ifelse(item == "SBMA", "Soy burger", "Chicken meat"))

# -----------------------------------------------------------------------------
# 4. Add beef reference and reshape to wide format
#    - Rename "Beef (CH), minced" → "Beef, minced"
#    - Each column = impact
# -----------------------------------------------------------------------------
poster2 <- poster1 %>%
  add_row(poster0 %>% filter(item == "Beef (CH), minced")) %>%
  mutate(item = ifelse(item == "Beef (CH), minced", "Beef, minced", item)) %>%
  pivot_wider(names_from = impact, values_from = mean_1)

# Remove square brackets from impact names for readability
names(poster2) <- gsub("\\[(.*?)\\]", "", names(poster2))

# -----------------------------------------------------------------------------
# 5. Reshape and add nutrient density indicators manually
#    - Adds NRprot7, LIM2, and NRF sub-scores for each product
#    - Capped at mean_1 ≤ 220 for visual scaling
# -----------------------------------------------------------------------------
poster3 <- poster2 %>%
  pivot_longer(cols = 2:4, names_to = "impact", values_to = "mean_1") %>%
  mutate(type = "Impact [%] of beef") %>%

  # Nutrient density sub-scores for Soy burger
  add_row(item = "Soy burger", impact = "NRprot7", mean_1 = 139.6, type = "nutrient density") %>%
  add_row(item = "Soy burger", impact = "LIM2", mean_1 = 33.3, type = "nutrient density") %>%
  add_row(item = "Soy burger", impact = "NRF", mean_1 = 139.7 - 33.3, type = "nutrient density") %>%

  # Nutrient density sub-scores for Chicken meat
  add_row(item = "Chicken meat", impact = "NRprot7", mean_1 = 81.8, type = "nutrient density") %>%
  add_row(item = "Chicken meat", impact = "LIM2", mean_1 = 2.7, type = "nutrient density") %>%
  add_row(item = "Chicken meat", impact = "NRF", mean_1 = 81.8 - 2.7, type = "nutrient density") %>%

  # Nutrient density sub-scores for Beef
  add_row(item = "Beef, minced", impact = "NRprot7", mean_1 = 211.7, type = "nutrient density") %>%
  add_row(item = "Beef, minced", impact = "LIM2", mean_1 = 19.5, type = "nutrient density") %>%
  add_row(item = "Beef, minced", impact = "NRF", mean_1 = 211.7 - 19.5, type = "nutrient density") %>%

  mutate(
    impact = factor(
      impact,
      levels = c("GWP100 ", "Land-occ ", "Water-scar ", "NRprot7", "LIM2", "NRF")
    ),
    mean_1 = ifelse(mean_1 > 220, 150, mean_1)  # cap values for readability
  )

# -----------------------------------------------------------------------------
# 6. Create poster plot
#    - Facet by "Impact [%] of beef" vs. "nutrient density"
#    - Use bold large text for poster visibility
# -----------------------------------------------------------------------------
final <- poster3 %>%
  ggplot(aes(item, mean_1, fill = impact)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cbp1_new) +
  facet_grid(type ~ .) +
  labs(y = "", x = "") +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    strip.text = element_text(size = 50, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 55, face = "bold"),
    axis.text.y = element_text(size = 50, face = "bold"),
    axis.text.x = element_text(size = 50, face = "bold")
  ) +
  scale_x_discrete(labels = label_wrap(width = 11))

# -----------------------------------------------------------------------------
# 7. Export poster figure in both JPEG and EPS formats
# -----------------------------------------------------------------------------
ggsave(
  file.path(working_dir_output, "poster.jpeg"),
  final,
  device = "jpeg",
  width = 30, height = 20
)

ggsave(
  file.path(working_dir_output, "poster.eps"),
  final,
  device = "eps",
  width = 30, height = 20
)
```

<figure>
<img src="./results/poster.jpeg" style="width:70.0%"
alt="Graphical abstract for the Paper." />
<figcaption aria-hidden="true">Graphical abstract for the
Paper.</figcaption>
</figure>
