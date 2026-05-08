# dauPortalTools

[![R-Build-Check-and-Test](https://github.com/DFE-Digital/dauPortalTools/actions/workflows/r-build-check.yml/badge.svg)](https://github.com/DFE-Digital/dauPortalTools/actions)
`dauPortalTools` is the core utility library for the DfE Data Analysis Unit (DAU) portals. It provides the standardized UI components, backend logic, and data processing engines that power our suite of applications, including:

* **Significant Change Portal**
* **Warning Notice Portal**
* **RISE Universal Portal**
* **RISE Universal Hubs Portal**
* **SLIC (School Level Interventions and Changes)**

## 🚀 Core Functionality

* **Shared Routing Engine**: Robust URL parameter parsing and deep-linking logic for cross-app navigation.
* **Standardized UI Components**: GDS-themed wrappers, custom "pill" date pickers, and DAU-specific layouts built on `shinyGovstyle` and `bslib`.
* **Data Harmonization**: Centralized logic for sanitizing legacy database dates (e.g., handling `1900-01-01`) and standardizing odbc connections.
* **Environment Management**: Helper functions to automatically detect and adjust behavior between Local, Pre-Prod, and Production environments.

## 🛠 Installation

You can install the development version of `dauPortalTools` from GitHub:

```r
# install.packages("remotes")
remotes::install_github("DFE-Digital/dauPortalTools")
```

# Stability & Quality Assurance

As the foundation for multiple production apps, this package maintains high stability standards:

Automated Testing: 70+ internal stability tests run on every commit.

Continuous Integration: GitHub Actions verify builds on ubuntu-latest to ensure environment-agnostic reliability.

Weekly Health Checks: Automated "Monday Morning" builds (05:00 UTC) flag any issues with upstream dependency shifts before the work week begins.

# Deployment Note
Current stable version: 2026.5.0 ("The Sound of Silence"). This release marks the transition to fully automated testing and the migration away from legacy PowerApp logic across the DAU ecosystem.

# Authors
Ben Smith - Lead Developer
Ben Clewarth
Mark Horton

## Maintained by the Regions Group Data Analysis Unit (DAU).
