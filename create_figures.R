library(ggplot2)
library(xtable)

richel_repo_names <- c(
  "richelbilderbeek/aureole",
  "ropensci/babette",
  paste0("richelbilderbeek/babette_example_", seq(1, 9)),
  "richelbilderbeek/babetter",
  "ropensci/beastier",
  "ropensci/beautier",
  "richelbilderbeek/becosys",
  "richelbilderbeek/daisieme",
  "richelbilderbeek/mcbette",
  "ropensci/mauricer",
  "richelbilderbeek/peregrine",
  "richelbilderbeek/pirouette",
  paste0("richelbilderbeek/pirouette_example_", seq(1, 30)),
  "richelbilderbeek/raztr",
  "richelbilderbeek/raztr",
  "richelbilderbeek/raket",
  "richelbilderbeek/razzo",
  "richelbilderbeek/ribir",
  "ropensci/tracerer"
)
# List the SLOC of all my projects
richel_project_names <- basename(richel_repo_names)

all_repo_names <- c(
  richel_repo_names,
  "CompEvol/beast2",
  "rsetienne/DAISIE",
  "rsetienne/DDD",
  "Giappo/mbd",
  "thijsjanzen/nLTT",
  "thijsjanzen/nodeSub",
  "rsetienne/PBD",
  "klausVigo/phangorn"
)

repo_name_to_title <- function(repo_name) {
  if (repo_name == "CompEvol/beast2") {
    return("BEAST2")
  }
  if (repo_name == "rsetienne/PBD") {
    d <- desc::description$new(file.path("../PBD/PBD/DESCRIPTION"))
    return(d$get_field("Title"))
  }
  d <- desc::description$new(file.path("..", basename(repo_name), "DESCRIPTION"))
  return(d$get_field("Title"))
}

repo_names_to_titles <- function(repo_names) {
  titles <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    titles[i] <- repo_name_to_title(repo_names[i])
  }
  titles
}

# Count non-whitespace lines
count_lines <- function(filenames) {
  sum <- 0
  for (filename in filenames) {
    text <- readLines(con = filename, warn = FALSE)
    text <- stringr::str_trim(text)
    text <- text[ text != ""]
    sum <- sum + length(text)
  }
  sum
}

collect_r_filenames <- function(project_name) {

  # Count the lines of code
  all_filenames <- list.files(
    path = file.path("~/GitHubs/", project_name),
    recursive = TRUE,
    full.names = TRUE
  )
  all_r_filenames <- stringr::str_match(
    string =  all_filenames,
    pattern = "^.*\\.(R|Rmd|java|cpp|h)$"
  )
  all_r_filenames <- as.character(na.omit(all_r_filenames[,1]))
  filenames <- all_r_filenames[
    stringr::str_detect(
      string = all_r_filenames,
      pattern = ".*/build/.*",
      negate = TRUE
    )
  ]
  filenames
}

get_sloccounts <- function(repo_names) {
  sloccounts <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    project_name <- basename(repo_names[i])
    filenames <- collect_r_filenames(project_name = project_name)
    sloccounts[i] <- count_lines(filenames)
  }
  as.integer(sloccounts)
}

# Count the number of stars for one repo
# Will return NA if there have been too many GitHub API requests
count_n_stargazers <- function(
  owner = "ropensci",
  repo = "beautier"
) {
  n <- NA
  tryCatch({
      n <- length(
        gh::gh(
          "GET /repos/:owner/:repo/stargazers",
          owner = owner,
          repo = repo,
          .limit = 1000
        )
      )
    },
    error = function(e) {} # nolint indeed ignore
  )
  n
}

# Many repos
count_ns_stargazers <- function(
  repos
) {
  n <- rep(x = 0, length(repos))
  for (i in seq_along(repos)) {
    n[i] <- count_n_stargazers(
        dirname(repos[i]),
        basename(repos[i])
    )
  }
  as.integer(n)
}

df <- data.frame(
  repo_name = all_repo_names,
  title = repo_names_to_titles(all_repo_names),
  sloccount = get_sloccounts(all_repo_names),
  n_stars = count_ns_stargazers(all_repo_names),
  stringsAsFactors = FALSE
)

total_sloccount <- sum(df$sloccount)
richels_sloccount <- sum(df[df$repo_name %in% richel_repo_names, ]$sloccount)

# aggregate pirouette examples
df <- rbind(
  df,
    data.frame(
    repo_name = "richelbilderbeek/pirouette_examples",
    title = "All pirouette examples",
    sloccount = sum(
      df$sloccount[
        stringr::str_detect(
          string = as.character(df$repo_name),
          pattern = "pirouette_example_.*"
        )
      ]
    ),
    n_stars = NA,
    stringsAsFactors = FALSE
  )
)
df <- rbind(
  df,
    data.frame(
    repo_name = "richelbilderbeek/babette_examples",
    title = "All babette examples",
    sloccount = sum(
      df$sloccount[
        stringr::str_detect(
          string = as.character(df$repo_name),
          pattern = "babette_example_.*"
        )
      ]
    ),
    n_stars = NA,
    stringsAsFactors = FALSE
  )
)

df_table <- df
df_table <- dplyr::rename(df_table, name = repo_name)
df_table$name <- basename(df_table$name)
# Remove the examples
df_table <- df_table[stringr::str_detect(df_table$name, "example_", negate = TRUE), ]

print(
  knitr::kable(
    df_table,
    row.names = FALSE
  )
)

print(
  xtable(
    df_table,
    type = "latex",
    caption = "Repository features",
    label = "tab:repos",
    align = c(
      "p{0.0\\textwidth}",
      "p{0.2\\textwidth}",
      "p{0.5\\textwidth}",
      "p{0.1\\textwidth}",
      "p{0.1\\textwidth}"
    )
  ),
  file = "repos.tex",
  include.rownames = FALSE
)


# Horizontal bar
ggplot(
  data = df[df$sloccount > 1000,],
  aes(x = basename(repo_name), y = sloccount / 1000)
) + geom_col(position = "identity", color = "black", fill = "white") + coord_flip() +
  theme(legend.position="none") +
  scale_y_continuous(name ="SLOCcount (x1000)", breaks = seq(0, 150, 5)) +
  scale_x_discrete(name ="Repository name") +
  ggsave("sloccount.png", width = 7, height = 7)



# Create the README table
df_readme <- data.frame(name = all_repo_names, stringsAsFactors = FALSE)
df_readme


create_readme_line <- function(
  owner = "richelbilderbeek",
  repo_name = "aureole"
) {
  travis_build_status <- paste0("[![Build Status](https://travis-ci.org/", owner, "/", repo_name, ".svg?branch=master)](https://travis-ci.org/", owner, "/", repo_name, ")")
  codecov <- paste0("[![codecov.io](https://codecov.io/github/", owner, "/", repo_name, "/coverage.svg?branch=master)](https://codecov.io/github/", owner, "/", repo_name, "/branch/master)")
  downloads <- paste0("[![](http://cranlogs.r-pkg.org/badges/babette)](https://CRAN.R-project.org/package=babette)")
  total_downloads <- paste0("[![](http://cranlogs.r-pkg.org/badges/grand-total/", repo_name, ")](https://CRAN.R-project.org/package=", repo_name, ")")
  paste(c(repo_name, travis_build_status, codecov, downloads, total_downloads), collapse = " | ")
}

create_readme_lines <- function(repo_names) {
  text <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    repo_name <- repo_names[i]
    owner = dirname(repo_name)
    text[i] <- create_readme_line(owner = owner, repo_name = basename(repo_name))
  }
  header <- c(
    "name|build status|codecov|downloads|total downloads",
    "----|------------|-------|---------|---------------"
  )
  c(
    header,
    text
  )
}
cat(create_readme_lines(all_repo_names), sep = "\n", file = "repos.md")

