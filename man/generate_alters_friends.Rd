% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_alters_friends.R
\name{generate_alters_friends}
\alias{generate_alters_friends}
\title{Function to get friends of friends from a previously generated ego's list of friends}
\usage{
generate_alters_friends(ego_network, degree_n, n_alters = NULL,
  n_alters_alters = NULL)
}
\arguments{
\item{ego_network}{name of the ego network df (to minimize the number of API calls, you must pass in the ego network, not simply the ego user's name)}

\item{degree_n}{degree that these friends are connected to the original ego}

\item{n_alters}{number of ego's alters to retrieve friends for (set to NULL to retrieve friends for all alters. Value of NULL is likely to result in extremely long run time)}

\item{n_alters_alters}{max num of each alter's friends to retrieve (set to NULL to retrieve all friends for each alter. NB: Value of NULL is likely to result in extremely long run time)}
}
\description{
Get networks of each alter who is friends with the original ego
}
\examples{
generate_alters_friends(APSA_network)
}
