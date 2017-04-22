# DataEntry

R package to make it easier to enter questionnaire data.

## Installation

The package is on CRAN, but if you want to install this development version,
you may follow these instructions:

In a terminal emulator:

```sh
# Clone DataEntry repository from github:
git clone https://github.com/jalvesaq/DataEntry.git
```

In R:

```r
# Start R and install DataEntry dependencies:
install.packages(c("gWidgets", "RGtk2", "gWidgetsRGtk2"))

# Compile the translations:
tools::update_pkg_po("DataEntry")

# Install the package:
install.packages("DataEntry", repos = NULL, type = "source",
                 INSTALL_opts = "--no-test-load")
```

**Notes:**

  - On Debian based systems (such as Ubuntu), you can install RGtk2 with the
    following command in a terminal emulator:

```sh
     sudo apt-get install r-cran-rgtk2
```

  - You only need to compile the translations if you want to run DataEntry
    whith its messages translated into Portuguese.

  - The argument `INSTALL_opts = "--no-test-load"` is necessary only on
    Windows.

## Screenshot

![DataEntry screenshot](https://raw.githubusercontent.com/jalvesaq/DataEntry/master/man/figures/Screenshot.png "DataEntry screenshot")
