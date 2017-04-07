# twitter-network-graphR
R package to extract follower info from Twitter users. Output is ready for network graphing

This package is a work in progress with limited functionality. It is designed to extract data on n-degree follower networks from Twitter, i.e.:

- Followers of an ego user
- Followers of the followers of the ego user (can be repeated ad infinitum)

To install, you first need the `devtools` R library:
`install.packages("devtools")
library("devtools")`

Then install using

`devtools::install_github("leslie-huang/twitter-network-graphR")
library(twitter-network-graphR)`
