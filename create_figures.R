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

repo_name_to_downloads_per_month <- function(repo_name) {
  # Numbers from screen grabs (in 'screen_grabs' folder),
  # taken at 2020-03-12
  if (repo_name == "ropensci/babette") return(452)
  if (repo_name == "ropensci/beastier") return(709)
  if (repo_name == "ropensci/beautier") return(975)
  if (repo_name == "ropensci/mauricer") return(742)
  if (repo_name == "ropensci/tracerer") return(849)
  if (repo_name == "rsetienne/DAISIE") return(736)
  if (repo_name == "rsetienne/DDD") return(1951)
  if (repo_name == "thijsjanzen/nLTT") return(702)
  if (repo_name == "rsetienne/PBD") return(649)
  if (repo_name == "klausVigo/phangorn") return(15e3)
  NA
}

repo_names_to_downloads_per_month <- function(repo_names) {
  n <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    n[i] <- repo_name_to_downloads_per_month(repo_names[i])
  }
  as.integer(n)
}


repo_name_to_total_downloads <- function(repo_name) {
  # Numbers from screen grabs (in 'screen_grabs' folder),
  # taken at 2020-03-12
  if (repo_name == "ropensci/babette") return(1173)
  if (repo_name == "ropensci/beastier") return(4579)
  if (repo_name == "ropensci/beautier") return(7135)
  if (repo_name == "ropensci/mauricer") return(2202)
  if (repo_name == "ropensci/tracerer") return(5359)
  if (repo_name == "rsetienne/DAISIE") return(18e3)
  if (repo_name == "rsetienne/DDD") return(70e3)
  if (repo_name == "thijsjanzen/nLTT") return(23e3)
  if (repo_name == "rsetienne/PBD") return(28e3)
  if (repo_name == "klausVigo/phangorn") return(420e3)
  NA
}

repo_names_to_total_downloads <- function(repo_names) {
  n <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    n[i] <- repo_name_to_total_downloads(repo_names[i])
  }
  as.integer(n)
}

repo_names_to_titles <- function(repo_names) {
  titles <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    titles[i] <- repo_name_to_title(repo_names[i])
  }
  titles
}

repo_name_to_codecov <- function(repo_name) {
  # Numbers from screen grabs (in 'screen_grabs' folder),
  # taken at 2020-03-12
  if (repo_name == "richelbilderbeek/aureole") return(100)
  if (repo_name == "ropensci/babette") return(100)
  if (repo_name == "ropensci/beastier") return(100)
  if (repo_name == "ropensci/beautier") return(100)
  if (repo_name == "richelbilderbeek/becosys") return(78)
  if (repo_name == "richelbilderbeek/daisieme") return(97)
  if (repo_name == "richelbilderbeek/mcbette") return(100)
  if (repo_name == "ropensci/mauricer") return(100)
  if (repo_name == "richelbilderbeek/peregrine") return(98)
  if (repo_name == "richelbilderbeek/pirouette") return(99)
  if (repo_name == "richelbilderbeek/raket") return(58)
  if (repo_name == "richelbilderbeek/razzo") return(76)
  if (repo_name == "richelbilderbeek/ribir") return(95)
  if (repo_name == "ropensci/tracerer") return(100)
  if (repo_name == "rsetienne/DAISIE") return(0)
  if (repo_name == "rsetienne/DDD") return(24)
  if (repo_name == "giappo/mbd") return(82)
  if (repo_name == "thijsjanzen/nLTT") return(99)
  if (repo_name == "thijsjanzen/nodeSub") return(53)
  if (repo_name == "rsetienne/PBD") return(57)
  if (repo_name == "klausVigo/phangorn") return(69)
  NA
}

repo_names_to_codecov <- function(repo_names) {
  codecovs <- rep(NA, length(repo_names))
  for (i in seq_along(repo_names)) {
    codecovs[i] <- repo_name_to_codecov(repo_names[i])
  }
  as.integer(codecovs)
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
  codecov = repo_names_to_codecov(all_repo_names),
  n_stars = count_ns_stargazers(all_repo_names),
  n_downloads_per_month = repo_names_to_downloads_per_month(all_repo_names),
  n_total_downloads = repo_names_to_total_downloads(all_repo_names),
  stringsAsFactors = FALSE
)

names(df)

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
    codecov = NA,
    n_stars = NA,
    n_downloads_per_month = NA,
    n_total_downloads = NA,
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
    codecov = NA,
    n_stars = NA,
    n_downloads_per_month = NA,
    n_total_downloads = NA,
    stringsAsFactors = FALSE
  )
)

# Sort on name
df <- df[ order(df$repo_name), ]

df_table <- df
df_table <- dplyr::rename(
  df_table,
  name = repo_name,
  cc = codecov,
  ns = n_stars,
  ndm = n_downloads_per_month,
  ndt = n_total_downloads
)
df_table$name <- basename(df_table$name)
# Remove the examples
df_table <- df_table[stringr::str_detect(df_table$name, "example_", negate = TRUE), ]

df_table$name[df_table$name == "babette_examples"] <- "babette examples"
df_table$name[df_table$name == "pirouette_examples"] <- "pirouette examples"

# Sort by name
df_table <- df_table[ order(df_table$name), ]

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
    caption = paste0(
      "Repository features. ",
      "name: the CRAN package name. ",
      "title: the R package title, as taken from the DESCRIPTION file",
      "sloccount: the number of (non-empty) lines of code.",
      "cc: code coverage, as a percentage, ",
      "where 100 percent denotes that all code is covered by tests. ",
      "ns: number of stars on GitHub. ",
      "ndm: number of CRAN downloads per month",
      "ndt: total number of CRAN downloads"
    ),
    label = "tab:repos",
    align = c(
      "p{0.0\\textwidth}",
      "p{0.11\\textwidth}",
      "p{0.4\\textwidth}",
      "p{0.1\\textwidth}",
      "p{0.05\\textwidth}",
      "p{0.05\\textwidth}",
      "p{0.1\\textwidth}",
      "p{0.1\\textwidth}"
    )
  ),
  file = "repos.tex",
  include.rownames = FALSE
)


# Horizontal bar
df_bar <- df
df_bar <- df_bar[df_bar$sloccount > 1000,]
df_bar$name <- basename(df_bar$repo_name)
df_bar$name[ df_bar$name == "pirouette_examples" ] <- "pirouette examples"

ggplot(
  data = df_bar,
  aes(x = name, y = sloccount / 1000)
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
  downloads <- paste0("[![](http://cranlogs.r-pkg.org/badges/", repo_name, ")](https://CRAN.R-project.org/package=", repo_name, ")")
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

