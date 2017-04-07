
#' Function to get followers of followers from a previously generated ego's list of followers
#'
#' Get networks of each alter following the original ego
#' @param ego_network name of the ego network df (to minimize the number of API calls, you must pass in the ego network, not simply the ego user's name)
#' @param n_alters number of ego's alters to retrieve followers for (set to NULL to retrieve followers for all alters. Value of NULL is likely to result in extremely long run time)
#' @param n_alters_alters max num of each alter's followers to retrieve (set to NULL to retrieve all followers for each alter. Value of NULL is likely to result in extremely long run time)
#' @export
#' @import twitteR
#'
#' @examples generate_alters_followers(APSA_network)

generate_alters_followers <- function(ego_network, n_alters = NULL, n_alters_alters = NULL) {

  # instantiate the dataframe
  df <- data.frame(user = character(), follower_name = character())

  # if not retrieving ALL of the ego's alters
  if (!is.null(n_alters)) {
    num_followers <- length(ego_network$follower_name)

    # check that ego has > 0 alters
    if (num_followers > 0) {

      # retrieve the smaller of (1) num ego's alters (2) n_alters parameter
      if (num_followers < n_alters) {
        n_alters <- num_followers
      }
    }
    # print warning and exit if the ego user has no alters
    else {
      print("Warning: the ego user you supplied has no followers.")
      return(df)
    }



  }

  # get character vector of n alters from the ego's df
  alters <- as.character(ego_network[1:n_alters, "follower_name"])

  # generate list of user objects for the alters
  alters_users <- lookupUsers(alters)

  # get followers of each alter in alters_users
  for (i in 1:length(alters_users)) {
    current_alter <- alters_users[[i]]

    # get current rate limit info
    current_calls_remaining <- as.numeric(getCurRateLimitInfo()[, 3])

    # Check no more API calls are left; wait 15 min
    if (0 %in% unique(current_calls_remaining)) {
      print("Sleeping...")

      Sys.sleep(15 * 60)
    }

    # proceed only if current user is not a private account AND user has > 0 followers
    if (current_alter$protected == FALSE && current_alter$followersCount > 0) {

      print(paste("Getting followers of", as.character(current_alter$screenName), sep = " "))

      # get the followers
      alter_followers <- current_alter$getFollowers(n = n_alters_alters)

      # append the followee-follower edges to the df
      for (i in 1:length(alter_followers)) {
        df <- rbind(df, data.frame(user = current_alter$screenName,
                                   follower_name = alter_followers[[i]]$screenName))

      }

    }

  }

  return(df)

}
