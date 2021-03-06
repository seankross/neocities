% Generated by roxygen2 (4.0.1): do not edit by hand
\name{neocities_delete}
\alias{neocities_delete}
\title{Delete a file on your Neocities site.}
\usage{
neocities_delete(filename, ..., user = neocities_user(),
  pass = neocities_pass())
}
\arguments{
\item{filename}{The name of the file as it appears on neocities.org.}

\item{...}{Arguments that will be passed to the underlying httr POST request.}

\item{user}{The username for your neocities account. See ?neocities_user for more details.}

\item{pass}{The password for your neocities account. See ?neocities_pass for more details.}
}
\description{
Delete a file on your Neocities site.
}
\examples{
\dontrun{

## Username and password have not been set as environmental variables, so
## the user will be prompted to enter their username and password if in
## interactive mode. The file will be deleted from
## http://username.neocities.org/report.html

neocities_delete("report.html")

## Setting username and password as environmental variables is wise when not
## in interactive mode, but be sure not to distribute them with your code.

Sys.setenv(NEOCITIES_USERNAME = "username")
Sys.setenv(NEOCITIES_PASSWORD = "password")
neocities_delete("report.html")

## You can also pass your username and password as function arguments.

neocities_delete("report.html", user="username", pass="password")

}
}

