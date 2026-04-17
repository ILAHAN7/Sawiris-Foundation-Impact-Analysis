# Sawiris Foundation Impact Analysis

## Overview

This repository contains the replication code for an impact evaluation of the Sawiris Foundation for Social Development program in Egypt. The analysis compares the effectiveness of three intervention modalities — subsidized loans, in-kind grants, and cash grants — on labor market outcomes and business ownership, relative to a control group.

## Research Design

The study uses a randomized controlled trial (RCT) design with four experimental arms:

| Arm | Description |
|-----|-------------|
| Control | No intervention |
| Loan | Subsidized microloan |
| In-kind grant | Non-cash asset transfer |
| Cash grant | Unconditional cash transfer |

Primary outcomes are labor income and business ownership at endline. Heterogeneous treatment effects are estimated by baseline business ownership status. A cost-effectiveness comparison is conducted under a hypothetical $1M budget constraint.

## Analysis Pipeline

The full analysis is implemented in `data2.R` and proceeds as follows:

1. Data loading and merging of baseline, treatment assignment, and endline survey files.
2. Attrition diagnostics between baseline and endline samples.
3. Balance table construction across treatment arms with joint F-tests (Table 1).
4. OLS estimation of main treatment effects on labor income and business ownership with cohort fixed effects and heteroskedasticity-robust standard errors (Table 2).
5. Cost-adjusted budget allocation comparison across intervention types (Figure 1).
6. Heterogeneity analysis interacting treatment indicators with baseline business ownership (Table 3).

## Data

The raw data files (`baseline.csv`, `treatment.csv`, `endline.csv`) contain confidential survey microdata and are excluded from this repository. The analysis script expects these files in the working directory.

## Output

Generated tables and figures are stored in the `outputdata/` directory:

- `Table1.html` — Baseline balance across treatment arms
- `Table2.html` — Main treatment effects on labor income and business ownership
- `Table3.html` — Heterogeneous effects by baseline business ownership
- CSV files containing coefficient estimates and cost-effectiveness calculations

## Requirements

The following R packages are required:

- `tidyverse`
- `fixest`
- `modelsummary`
- `broom`
- `scales`
- `gt`

## Replication

To replicate the analysis:

1. Place `baseline.csv`, `treatment.csv`, and `endline.csv` in the project root directory.
2. Open `data2.Rproj` in RStudio.
3. Run `data2.R` in its entirety.

All tables and figures will be generated in the working directory.
