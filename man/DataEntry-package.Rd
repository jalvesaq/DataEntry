\name{DataEntry-package}
\alias{DataEntry-package}
\alias{DataEntry}
\docType{package}
\title{
\packageTitle{DataEntry}
}
\description{
\packageDescription{DataEntry}
}
\details{

  The package has only one function visible to users: \code{DataEntry()}. The
  function starts a graphical interface and is called when the package is
  loaded, but if you want to avoid \code{DataEntry()} being called while
  loading the package, put in your \samp{.Rprofile}:

  \code{
    options("DataEntry.autostart" = FALSE)
  }

  If the graphical interface is already running, \code{DataEntry()} just
  changes the focus to its window.


  There is no button to save the project because the data is saved whenever a
  new variable is added to the list of variables or a new row is added to the
  \code{data.frame}. The project is also saved when either a variable or a row
  is edited, and a new backup of the project is created each time the project
  is opened.

  Users have to define attributes of the variables before inserting data in a
  \code{data.frame}. Among the attributes, there is a label that will remind
  the user of the meaning of variable in the data entry form. There is also
  the possibility of setting valid values (either a list of values or the
  minimum and maximum valid values).

  DataEntry behavior is partially customizable. Users can:

  \itemize{
    \item Use a drop list instead of an edit box to insert \code{factor} data.
    In this case, you could use the Tab key to move from one field to another
    and the Up and Down arrow keys to choose the value.

    \item Allow blank cells in the data entry form.

    \item Choose a string to represent missing values while entering data. The
    default string is \samp{"NA"}, but users can change it to something easier
    to type, such as \samp{"-"}.

    \item Disable the automatic backup of the project.

    \item Set the maximum number of backup files to keep (that is,
    automatically delete the older backup files).
  }

  The project is saved with extension \samp{.dte}, but it is just a regular
  \samp{.RData} file. While DataEntry is running, data and options are stored
  in the environment object \code{DataEntry:::DEenv}. The application options
  are saved in the directory designated by the operating system for cached
  data (\samp{$XDG_CACHE_HOME} on Linux and \samp{$APPDATA} on Windows).

  The Export data button allows users to export the \code{data.frame} as a
  \samp{csv} file, save it in a \samp{RData} file or copy it to R Workspace.

  The list of variables can be locked with a password. This might be useful if
  you are setting a project that will later be used by other people to enter
  data and you want to prevent that the attributes of the variables are
  accidentally changed.

  Screenshot:

  \figure{Screenshot.png}{DataEntry screenshot}
}
\author{
\packageAuthor{DataEntry}

Maintainer: \packageMaintainer{DataEntry}
}

\examples{
\donttest{
# The command to open the gui application is:
DataEntry()
}
}
