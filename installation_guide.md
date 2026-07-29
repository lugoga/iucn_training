# IUCN Bahari Yetu: R & IDE Offline Setup Guide

This guide describes how to install R, an Integrated Development Environment (IDE like RStudio or Positron), and all required R packages completely offline using the training USB drive bundle.

---

## Step 1: Install R (Base Engine)

Before installing any interface, you must install the R programming language engine.

1. Open the USB drive and navigate to the `/installers/` folder.
2. Select the installer matching your operating system:
   * **Windows**: Run `R-4.6.1-win.exe` (or latest version provided). Follow the default prompts.
   * **macOS**: Run the `.pkg` file. If using Apple Silicon (M1/M2/M3), choose the `arm64` installer; otherwise, select the `x86_64` installer.
3. Keep all default configurations and complete the setup.

---

## Step 2: Install RStudio or Positron (IDE)

We recommend using **RStudio** or **Positron** for scientific scripting.

1. Navigate to the `/installers/` folder on the USB drive.
2. Open the directory corresponding to your choice:
   * **RStudio**: Run the setup executable (`RStudio-2024.xx.x-xxx.exe` for Windows or the `.dmg` for macOS).
   * **Positron**: Run the installer package for your OS (a modern, lightweight alternative by Posit).
3. Follow the installation wizard to completion.

---

## Step 3: Load R Packages Offline

Because internet connection in the field can be unstable, a pre-compiled local library cache of required R packages is provided on the USB drive under `/packages/`.

### Method A: Copying the Pre-compiled Library Cache (Recommended for Windows)

1. Locate the `library_cache.zip` on your USB drive.
2. Extract the contents of this zip file.
3. Copy all extracted folders.
4. Paste them directly into your local R library folder.
   * **Windows default path**: `C:\Users\<Your-Username>\AppData\Local\R\win-library\4.4\`
   * **macOS default path**: `/Library/Frameworks/R.framework/Versions/4.4/Resources/library/`

### Method B: Installing from Local source `.tar.gz` or `.zip` files

Alternatively, you can install the packages programmatically by pointing R to the USB drive directory:

1. Open RStudio/Positron.
2. Run the following command (replace `/path/to/usb/packages/` with the actual path of your USB drive, e.g. `D:/packages/`):

```r
# Define the path to the USB drive packages folder
usb_pkg_dir <- "D:/packages/"

# List of key packages to install
pkgs <- c("tidyverse", "sf", "terra", "tidyterra", "rstatix", "tidyplots", "here", "flextable", "knitr")

# Install packages from local zip/tar binaries
for (pkg in pkgs) {
  pkg_file <- list.files(usb_pkg_dir, pattern = paste0("^", pkg, "_"), full.names = TRUE)
  if (length(pkg_file) > 0) {
    install.packages(pkg_file[1], repos = NULL, type = "binary")
  } else {
    warning(paste("Package binary for", pkg, "not found on USB."))
  }
}
```

---

## Step 4: Verify Installation

Open your IDE and run the following lines in the Console to ensure everything is working:

```r
library(tidyverse)
library(sf)
library(terra)
library(rstatix)
library(here)

print("Installation successful! Your offline environment is ready.")
```
