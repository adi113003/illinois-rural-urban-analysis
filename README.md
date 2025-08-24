# Rural and Urban Disparities in Housing Affordability and Population Trends in Illinois

This project investigates how **population change** and **housing affordability** vary between metro and non-metro counties in Illinois from 2000 to 2020. Using U.S. Census data, ACS estimates, and RUCC (Rural-Urban Continuum Codes), the research explores demographic shifts, affordability pressures, and rural-urban disparities in Illinois counties.

---

## 📊 Project Overview
The study addresses:
- How has **population changed** in rural vs. urban counties (2000–2020)?
- How does **housing affordability** (home value / household income) relate to population change?
- Are **economic pressures** (like unemployment) driving these patterns?
- How do **age structures** differ between metro and rural counties?

---

## 📂 Repository Structure
- **`analysis.R`** → Core R script for data collection and visualization (population trends, affordability ratios, RUCC classification, plots, interactive maps).
- **`Ruralurbancontinuumcodes2023.xlsx`** → RUCC codes dataset for Illinois counties.
- **`Horizon.pptx`** → Project presentation summarizing findings with visuals.
- **Plots & Maps Generated**:
  - Population change (2000–2020) by county
  - Scatterplots of population change vs. county size
  - Population pyramids (metro vs. nonmetro)
  - Housing affordability choropleths
  - Interactive Leaflet maps

---

## 📈 Key Insights
- **Population Divergence**:
  - Metro counties (Chicago, suburbs) show growth or stability.
  - Most rural counties (RUCC 7–9) show steady population loss:contentReference[oaicite:0]{index=0}.
- **Housing Affordability**:
  - Metro counties (Cook, DuPage, Lake) have high affordability stress.
  - Rural counties *appear* affordable, but low incomes distort this picture.
- **Youth Outmigration**:
  - Rural counties lose young adults (18–24), accelerating aging and service decline:contentReference[oaicite:1]{index=1}.
- **Employment**:
  - Job scarcity is concentrated in Southern and Western Illinois, aligning with rural counties.
- **Conclusion**:
  - Metro counties are growing but less affordable.
  - Rural counties are affordable but shrinking and aging.
  - Affordability, opportunity, and demographics interact to shape Illinois’ rural-urban divide.

---

## 🛠️ Tools & Libraries
Analysis conducted in **R** using:
- `tidycensus` → Census & ACS data
- `dplyr`, `stringr`, `purrr`, `tidyr` → Data wrangling
- `ggplot2` → Static visualization
- `leaflet`, `plotly` → Interactive maps & plots
- `sf`, `tigris` → Spatial geometry

---

## 📊 Data Sources
- **U.S. Census Bureau** — Decennial Census (2000, 2010, 2020)  
- **American Community Survey (ACS)** — 2020 5-Year Estimates  
- **USDA Economic Research Service** — Rural-Urban Continuum Codes (RUCC)  
- **Illinois Department of Employment Security (IDES)** — Local Area Unemployment Statistics  
- **Housing Assistance Council (2023)** — Rural housing affordability  
- **Illinois Institute for Rural Affairs (IIRA, 2021)** — Youth migration & demographics  
- **City of Chicago Department of Housing (2020)** — Affordable housing gap:contentReference[oaicite:2]{index=2}

---

## 🚀 How to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/<your-username>/Illinois-Rural-Urban-Analysis.git
