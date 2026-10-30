# FleetPulse: Vehicle Telematics, Fuel Economics & Anomaly Analytics Engine

An interactive, reactive telematics and operational fleet monitoring engine built using **R** and the **Shiny** web framework.

---

## Overview
FleetPulse provides real-time fleet surveillance, fuel burn anomaly detection, predictive route budgeting, and automated workshop maintenance scheduling across delivery networks.

### Key Highlights
- **Real-Time Fleet Surveillance:** Live tracking of operational fleet metrics, average mileage, and maintenance queues.
- **Dynamic Vehicle Ingestion (Action 1):** Ingest new vehicles into the active fleet with automated baseline calculations.
- **Predictive Trip Cost Modeling (Action 2):** Real-time fuel consumption and financial expense forecasts.
- **Service Queue & Workshop Dispatch (Action 3):** Automated rule-based alerts marking vehicles as `Service Due` upon crossing distance thresholds ($\ge 2,000\text{ KM}$), with automated odometer reset upon workshop dispatch.
- **Visual Analytics:** Interactive category distribution bar charts and odometer-to-fuel scatter plots identifying efficiency anomalies.
- **Export Engine:** One-click CSV extraction of live telematics logs.

---

## Tech Stack & Architecture
- **Language:** R (v4.0+)
- **Framework:** R Shiny (Reactive Web Architecture)
- **Data Manipulation & Visuals:** `dplyr`, `ggplot2`, Base R Graphics
- **Architecture:** Client-Server Reactive Model (`ui.R` / `server.R` in `vehicles.R`)

---

## Getting Started

### Prerequisites
Ensure R and RStudio are installed on your machine. Install required packages using the R console:

```R
install.packages(c("shiny", "dplyr", "ggplot2"))
