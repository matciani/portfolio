# Portfolio

This repository contains examples of Shiny dashboards I developed for empirical economic research and a web-based automation tool.
They are not "end-goals" or outputs, but instruments that I have built for myself and my colleagues to have a better overview of the data. 


---

### Trade and Patents at HS6 Code level

This dashboard supports research on the evolution of technologies in international trade. It visualizes trends in HS6 product-level trade data from BACI and plots the number of patents published in each year for the most relevant IPC4 class.

Key features:
* Search and visualize trade values for any HS6 product  
* Reproduce the IGPC indicator (Fetzer et al., 2024) to show network-weighted product importance  
* Connect HS6 categories with patent IPC classes using the PATSTAT concordance

**App:** <a href="https://matciani.shinyapps.io/trade_and_patents_at_hs6_level/" target="_blank" rel="noopener noreferrer">Shiny Link</a>
**Code:** `Trade and Patents at HS6 level.Rmd`


#### Sources:
Trade data: <a href="https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37" target="_blank" rel="noopener noreferrer">BACI CEPII</a>  
Integrated Global Product Centrality: <a href="https://aipnet.io/paper/" target="_blank" rel="noopener noreferrer">AI-Generated Production Network</a>  
Patent Data: <a href="https://www.epo.org/en/searching-for-patents/business/patstat" target="_blank" rel="noopener noreferrer">PATSTAT</a>  


---

### Geography of Conflicts

This dashboard maps and visualizes conflict events over time using UCDP GED data. It allows users to explore the temporal and geographic distribution of violent events.

Key features:
* Map conflicts by type (state-based, non-state, one-sided)  
* Bubble size proportional to event severity (fatalities)  
* Interactive filters to explore conflict dynamics across time and space

**App:** <a href="https://matciani.shinyapps.io/Conflicts/" target="_blank" rel="noopener noreferrer">Shiny Link</a>
**Code:** `Geography of Conflicts.R`

#### Source: 
Conflict data: Conflict data: <a href="https://ucdp.uu.se/" target="_blank" rel="noopener noreferrer">UCDP Georeferenced Event Dataset (GED)</a> 


--- 

## Web Scraper

This tool automates the extraction of firm-level information from the Orbis database environment to support empirical research. It was designed to streamline data collection workflows and free time for analysis.

Key features:
* Logs in securely and retrieves a list of firms
* Searches each firm in the database interface and downloads its associated patent portfolios

**Code:** `Patent Data from Firm Profile.py`

