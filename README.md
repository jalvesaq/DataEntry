# DataEntry

R package to make it easier to enter questionnaire data.

## Installation

### 1. Start R and install DataEntry dependencies:

```r
   install.packages(c("gWidgets", "RGtk2", "gWidgetsRGtk2"))
```

### 2. Clone DataEntry repository from github:

```
   git clone https://github.com/jalvesaq/DataEntry.git
```

### 3. Start R and compile the translations:

```r
   library("tools")
   update_pkg_po("DataEntry")
```

### 4. Install the package:

```r
   install.packages("DataEntry", repos = NULL, type = "source",
                    INSTALL_opts = "--no-test-load")
```

### Notes:

  - On Debian based systems (such as Ubuntu), you can install RGtk2 with the
    following command in a terminal emulator:

```
    sudo apt-get install r-cran-rgtk2
```

  - You only need to compile the translations if you want to run DataEntry
    whith its messages translated into Portuguese.

  - The argument `INSTALL_opts = "--no-test-load"` is necessary only on
    Windows.

## Screenshot

![DataEntry screenshot](https://raw.githubusercontent.com/jalvesaq/DataEntry/master/man/figures/Screenshot.png "DataEntry screenshot")
