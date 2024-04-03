testing_git_config <- function(repo) {
  gert::git_config_set(repo = repo, name = "user.name", value = "testthat")
  gert::git_config_set(
    repo = repo,
    name = "user.email",
    value = "PACTATesting@rmi.org"
  )
}
