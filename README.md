# Claims Reserving Analysis with Bootstrapping

## 📋 Project Overview
This repository contains an R-based implementation of **bootstrapping techniques for claims reserving**, specifically designed for **low volatility insurance portfolios**. The analysis follows the classical chain-ladder methodology enhanced with Pearson residual bootstrapping to quantify reserve uncertainty and calculate risk adjustments compliant with IFRS 17 standards.

---

## 👥 Author Information
- **Team Member:** [M.H. Mahinsa Nethmina Perera](www.linkedin.com/in/mahinsa-nethmina-451284288)
- **Team Member:** G.H.G.M.G. Jayarathna
- **Team Member:** H.G.S.B. Harankahadeniya
- **Team Member:** L.D.D. Dewmini Perera
- **Team Member:** A.K.A.N. Vidyarathna
- **Team Member:** D.I.B. Samarasinghe
- **Team Member:** M.S.S. De Silva
- **Team Member:** C.W.U. Arachchi

- **Supervisor:**
  
- **Date:** December 2025
- **Institution:** University of Ruhuna
- **Programme:** Mathematical and Statistical Modeling Week 2025, Organized by Department of Mathematics, University of Ruhuna 

---

## 🔒 Data Confidentiality
**Important:** The actual claims data used in this analysis is **confidential** and not included in this repository. The code references a file path (`Low Volatility.xlsx`) that would contain proprietary insurance claims data. Users must replace this with their own data file or use the provided synthetic data generation functions for testing purposes.

This project is provided for educational and research purposes only. Users must ensure compliance with all applicable data privacy and intellectual property regulations when using their own data.

---

## 📊 Methodology

### Core Techniques Implemented

1. **Chain-Ladder Method**
   - Traditional deterministic reserving
   - Development factor calculation
   - Best estimate reserve projection

2. **Pearson Residual Bootstrapping**
   - Center-adjusted residual sampling
   - Non-parametric simulation of reserve uncertainty
   - 5,000 bootstrap iterations for stable estimates

3. **Risk Measurement Metrics**
   - Percentile-based risk measures (75%, 80%, 90%, 95%)
   - Conditional Tail Expectation (CTE/TVaR)
   - Risk adjustment calculations for IFRS 17

4. **Diagnostic Analysis**
   - Residual normality testing (Shapiro-Wilk)
   - Convergence checking for bootstrap stability
   - Visual diagnostics (QQ-plots, histograms, CDFs)

---




