
#' Function to get all friends of an ego account and return a dataframe of all directed edges between ego and friends
#'
#' @param ego ego username
#' @param n number of friends to retrieve, default of NULL will retrieve all followers. If a user has fewer than n friends, all friends will be retrieved
#' @export
#' @import twitteR
#'
#' @examples generate_ego_friend_edgelist("APSAtweets")

generate_ego_friend_edgelist <- function(ego_name, n = NULL) {

  df <- data.frame(user = character(), friend_name = character(), degree_n = character()) # instantiate a df to populate with follower edges

  ego <- getUser(ego_name)

  # check the user's number of followers and terminate if the user has no followers
  friends_count <- ego$friendsCount

  if (friends_count == 0) {
    print("Warning: This user has no friends.")
    return(df)
  }

  # if the user has supplied n, retrieve n or the total number of followers the user has (whichever is smaller)
  if (!is.null(n)) {
    if (n > friends_count) {
      n <- friends_count
    }
  }

  friends <- ego$getFriends(n = n)

  num_edges <- length(friends)

  for (i in 1:num_edges) {
    df <- rbind(df, data.frame(user = ego$screenName,
                               friend_name = friends[[i]]$screenName, degree_n = "1"))
  }

  return(df)

}
