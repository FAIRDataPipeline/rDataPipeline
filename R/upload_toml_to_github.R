#' Upload TOML file to GitHub
#'
#' Experimental
#'
#' @param path_to_toml path to the toml file you wish to add to the data repository
#' @param name (optional) the name you want to be associated with the git commit
#' @param email (optional) the email you want to be associated with the git commit
#'
#' @family upload functions
#'
#' @export
#'
upload_toml_to_github <- function(path_to_toml,
                                  name = "SCRC",
                                  email = "scrc@glasgow.ac.uk"){

  working_directory <- getwd()

  github_repo_path <- "github_repo"

  tryCatch({
    if(system("git --version"))
      stop("You must have git installed and in the system path for this function to work")

    if(!file.exists(path_to_toml))
      stop("toml does not exist")

    toml_name <- basename(path_to_toml)

    github_dir_path <- dirname(path_to_toml)

    github_toml_file_path <- file.path(github_dir_path, toml_name, fsep = "/")

    dir.create(github_repo_path)

    response <- system2("git", args = c(
      "clone", "--branch master",
      "https://github.com/ScottishCovidResponse/DataRepository.git",
      github_repo_path))

    if(response)
      stop("git could not clone the repo")

    if(file.exists(file.path(github_repo_path, github_toml_file_path,
                             fsep = "/")))
      stop(paste0("A File named ", toml_name, " already exists in the repo"))

    dir.create(file.path(github_repo_path, github_dir_path, fsep = "/"),
               recursive = TRUE)

    file.copy(path_to_toml, file.path(github_repo_path, github_dir_path,
                                      fsep = "/"), recursive = TRUE)

    setwd(github_repo_path)

    response <- system2("git", args = c("add", github_toml_file_path))
    if(response)
      stop("git could not add the file to the local repo")

    response <- system2("git", args = c("config user.email", email))
    response <- response + system2("git", args = c("config user.name", name))
    if(response)
      stop("git could not set username and name")

    response <- system2("git", args = c("commit",  paste0("-m \"add file ",
                                                          path_to_toml, "\"")))
    if(response)
      stop("git could not commit the file to the local repository")

    message("Github may now ask you for your credentials if you cancel this prompt you will need to stop R")

    response <- system2("git", args = c("push", "origin", "master"))
    if(response)
      stop("git could push to the repo")


  }, finally = {
    setwd(working_directory)
    unlink(github_repo_path, recursive = TRUE, force = TRUE)
  })

}
