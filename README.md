# DataEntry

R package to make it easier to enter questionnaire data.

## Screenshot

![DataEntry screenshot](https://raw.githubusercontent.com/jalvesaq/DataEntry/master/man/figures/Screenshot.png "DataEntry screenshot")

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
# Install DataEntry dependencies:
install.packages(c("gWidgets2", "RGtk2", "gWidgets2RGtk2"))

# Compile the translations:
tools::update_pkg_po("DataEntry")

# Install the package:
install.packages("DataEntry", repos = NULL, type = "source",
                 INSTALL_opts = "--no-test-load")
```

If you do not want to use `git`, you may do the following in R instead:

```r
# Install DataEntry dependencies:
install.packages(c("gWidgets2", "RGtk2", "gWidgets2RGtk2"))

# Donwload DataEntry from github:
download.file("https://github.com/jalvesaq/DataEntry/archive/master.zip", "DataEntry_master.zip")
unzip("DataEntry_master.zip")

# Compile the translations:
tools::update_pkg_po("DataEntry-master")

# Install the package:
install.packages("DataEntry-master", repos = NULL, type = "source",
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
