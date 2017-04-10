
#' Function to get friends of friends from a previously generated ego's list of friends
#'
#' Get networks of each alter who is friends with the original ego
#' @param ego_network name of the ego network df (to minimize the number of API calls, you must pass in the ego network, not simply the ego user's name)
#' @param degree_n degree that these friends are connected to the original ego
#' @param n_alters number of ego's alters to retrieve friends for (set to NULL to retrieve friends for all alters. Value of NULL is likely to result in extremely long run time)
#' @param n_alters_alters max num of each alter's friends to retrieve (set to NULL to retrieve all friends for each alter. NB: Value of NULL is likely to result in extremely long run time)
#' @export
#' @import twitteR
#'
#' @examples generate_alters_friends(APSA_network)

generate_alters_friends <- function(ego_network, degree_n, n_alters = NULL, n_alters_alters = NULL) {

  # instantiate the dataframe
  df <- data.frame(user = character(), friend_name = character(), degree_n = character())

  # if not retrieving ALL of the ego's alters
  if (!is.null(n_alters)) {
    num_friends <- length(ego_network$friend_name)

    # check that ego has > 0 alters
    if (num_friends > 0) {

      # retrieve the smaller of (1) num ego's alters (2) n_alters parameter
      if (num_friends < n_alters) {
        n_alters <- num_friends
      }
    }
    # print warning and exit if the ego user has no alters
    else {
      print("Warning: the ego user you supplied has no friends.")
      return(df)
    }



  }

  # get character vector of n alters from the ego's df
  alters <- unique(as.character(ego_network[1:n_alters, "friend_name"]))

  # generate list of user objects for the alters
  alters_users <- lookupUsers(alters)

  # get friends of each alter in alters_users
  for (i in 1:length(alters_users)) {
    current_alter <- alters_users[[i]]

    # get current rate limit info
    current_calls_remaining <- as.numeric(getCurRateLimitInfo()[, 3])

    # Check no more API calls are left; wait 15 min
    if (0 %in% unique(current_calls_remaining)) {
      print("Sleeping...")

      Sys.sleep(15 * 60)
    }

    # proceed only if current user is not a private account AND user has > 0 friends
    if (current_alter$protected == FALSE && current_alter$friendsCount > 0) {

      print(paste("Getting friends of", as.character(current_alter$screenName), sep = " "))

      # get the friends
      alter_friends <- current_alter$getFriends(n = n_alters_alters)

      # append the alter-friend edges to the df
      for (i in 1:length(alter_friends)) {
        df <- rbind(df, data.frame(user = current_alter$screenName,
                                   friend_name = alter_friends[[i]]$screenName, degree_n = as.character(degree_n)))

      }

    }

  }

  return(df)

}
