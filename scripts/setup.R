# Restore this checkout only; package installation never happens while knitting.
renv::restore(prompt = FALSE)

# Keep hooks local to this worktree so other branches can run independently.
stopifnot(system2("git", c("config", "extensions.worktreeConfig", "true")) == 0)
stopifnot(system2("git", c(
    "config", "--worktree", "core.hooksPath", ".githooks"
)) == 0)
message("Packages restored and pre-commit hook enabled for this checkout.")
