#' Post entry to user_author table
#'
#' Upload information to the \code{user_author} table in the data registry
#' 
#' @keywords internal
#'
#' @param user_url a \code{string} specifying the URL of an existing
#' \code{user}
#' @param author_url a \code{string} specifying the URL of an existing
#' \code{author}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_user_author <- function(user_url,
                            author_url,
                            endpoint = "http://127.0.0.1:8000/api/") {

  data <- list(user = user_url,
               author = author_url)

  post_data(table = "user_author",
            data =  data,
            endpoint = endpoint)
}
