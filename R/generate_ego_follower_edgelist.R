
#' Function to get all followers of an ego account and return a dataframe of all directed edges between ego and followers
#'
#' @param ego ego username
#' @param n number of followers to retrieve, default of NULL will retrieve all followers. If a user has fewer than n followers, all followers will be retrieved
#' @export
#' @import twitteR
#'
#' @examples generate_ego_follower_edgelist("APSAtweets")

generate_ego_follower_edgelist <- function(ego_name, n = NULL) {

  df <- data.frame(user = character(), follower_name = character()) # instantiate a df to populate with follower edges

  ego <- getUser(ego_name)

  # check the user's number of followers and terminate if the user has no followers
  followers_count <- ego$followersCount

  if (followers_count == 0) {
    print("Warning: This user has no followers.")
    return(df)
  }

  # if the user has supplied n, retrieve n or the total number of followers the user has (whichever is smaller)
  if (!is.null(n)) {
    if (n > followers_count) {
      n <- followers_count
    }
  }

  followers <- ego$getFollowers(n = n)

  num_edges <- length(followers)

  for (i in 1:num_edges) {
    df <- rbind(df, data.frame(user = ego$screenName,
                               follower_name = followers[[i]]$screenName))
  }

  return(df)

}
