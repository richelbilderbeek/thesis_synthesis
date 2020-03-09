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

project_names <- basename(all_repo_names)
project_names <- sort(project_names)

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

df <- data.frame(name = project_names, sloccount = NA)

for (i in seq_along(project_names)) {
  project_name <- df$name[i]

  filenames <- collect_r_filenames(project_name = project_name)

  df$sloccount[i] <- count_lines(filenames)
}


total_sloccount <- sum(df$sloccount)

richels_sloccount <- sum(df[df$name %in% richel_project_names, ]$sloccount)


# aggregate pirouette examples
df <- rbind(
  df,
    data.frame(
    name = "pirouette_examples",
    sloccount = sum(
      df$sloccount[
        stringr::str_detect(
          string = as.character(df$name),
          pattern = "pirouette_example_.*"
        )
      ]
    )
  )
)
df <- rbind(
  df,
    data.frame(
    name = "babette_examples",
    sloccount = sum(
      df$sloccount[
        stringr::str_detect(
          string = as.character(df$name),
          pattern = "babette_example_.*"
        )
      ]
    )
  )
)

# Sort
df$name <- as.character(df$name)
df <- df[ order(df$name, decreasing = TRUE), ]

df <- df[df$sloccount > 1000,]
library(ggplot2)


# Pie
ggplot(
  data = df,
  aes(x = "", y = sloccount, fill = name)
) + geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = name))

# Horizontal bar
ggplot(
  data = df,
  aes(x = name, y = sloccount / 1000, fill = name)
) + geom_col(position = "identity", color = "black") + coord_flip() +
  theme(legend.position="none") +
  scale_y_continuous(name ="SLOCcount (x1000)", breaks = seq(0, 150, 10)) +
  ggsave("~/sloccount.png", width = 7, height = 7)


# Stacked bar
ggplot2::ggplot(
  data = df,
  aes(x = "", y = sloccount, fill = name)
) + geom_col(position = "stack", color = "black")

ggplot2::ggplot(
  data = df,
  aes(x = "", y = sloccount, fill = name)
) + facet_grid(. ~ name) +
  geom_bar(stat = "identity", color = "black") + geom_text(aes(label = name))






# GET /repos/:owner/:repo/stargazers
#x <- curl::curl_fetch_memory("https://github.com?GET=repos/:ropensci/:beautier/stargazers")
# curl::curl(
#   url = "https://github.com",
#   handle = "GET /repos/:ropensci/:beautier/stargazers"
# )

# One
count_n_stargazers <- function(
  owner = "ropensci",
  repo = "beautier"
) {
  length(gh::gh("GET /repos/:owner/:repo/stargazers", owner = owner, repo = repo, .limit = 1000))
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
  n
}

df <- data.frame(repo = all_repo_names, n_stars = count_ns_stargazers(all_repo_names))
df[ rev(order(df$n_stars)), ]
#                                     repo n_stars
# 57                       CompEvol/beast2     134
# 64                    klausVigo/phangorn     110
# 2                       ropensci/babette      20
# 14                     ropensci/beautier       6
# 56                     ropensci/tracerer       5
# 13                     ropensci/beastier       5
# 17              richelbilderbeek/mcbette       4
# 61                      thijsjanzen/nLTT       3
# 58                      rsetienne/DAISIE       3
# 20            richelbilderbeek/pirouette       3
# 54                richelbilderbeek/razzo       2
# 19            richelbilderbeek/peregrine       2
# 63                         rsetienne/PBD       1
# 62                   thijsjanzen/nodeSub       1
# 60                            Giappo/mbd       1
# 59                         rsetienne/DDD       1
# 18                     ropensci/mauricer       1
# 16             richelbilderbeek/daisieme       1
# 55                richelbilderbeek/ribir       0
# 53                richelbilderbeek/raket       0
# 52                richelbilderbeek/raztr       0
# 51                richelbilderbeek/raztr       0
# 50 richelbilderbeek/pirouette_example_30       0
# 49 richelbilderbeek/pirouette_example_29       0
# 48 richelbilderbeek/pirouette_example_28       0
# 47 richelbilderbeek/pirouette_example_27       0
# 46 richelbilderbeek/pirouette_example_26       0
# 45 richelbilderbeek/pirouette_example_25       0
# 44 richelbilderbeek/pirouette_example_24       0
# 43 richelbilderbeek/pirouette_example_23       0
# 42 richelbilderbeek/pirouette_example_22       0
# 41 richelbilderbeek/pirouette_example_21       0
# 40 richelbilderbeek/pirouette_example_20       0
# 39 richelbilderbeek/pirouette_example_19       0
# 38 richelbilderbeek/pirouette_example_18       0
# 37 richelbilderbeek/pirouette_example_17       0
# 36 richelbilderbeek/pirouette_example_16       0
# 35 richelbilderbeek/pirouette_example_15       0
# 34 richelbilderbeek/pirouette_example_14       0
# 33 richelbilderbeek/pirouette_example_13       0
# 32 richelbilderbeek/pirouette_example_12       0
# 31 richelbilderbeek/pirouette_example_11       0
# 30 richelbilderbeek/pirouette_example_10       0
# 29  richelbilderbeek/pirouette_example_9       0
# 28  richelbilderbeek/pirouette_example_8       0
# 27  richelbilderbeek/pirouette_example_7       0
# 26  richelbilderbeek/pirouette_example_6       0
# 25  richelbilderbeek/pirouette_example_5       0
# 24  richelbilderbeek/pirouette_example_4       0
# 23  richelbilderbeek/pirouette_example_3       0
# 22  richelbilderbeek/pirouette_example_2       0
# 21  richelbilderbeek/pirouette_example_1       0
# 15              richelbilderbeek/becosys       0
# 12             richelbilderbeek/babetter       0
# 11    richelbilderbeek/babette_example_9       0
# 10    richelbilderbeek/babette_example_8       0
# 9     richelbilderbeek/babette_example_7       0
# 8     richelbilderbeek/babette_example_6       0
# 7     richelbilderbeek/babette_example_5       0
# 6     richelbilderbeek/babette_example_4       0
# 5     richelbilderbeek/babette_example_3       0
# 4     richelbilderbeek/babette_example_2       0
# 3     richelbilderbeek/babette_example_1       0
# 1               richelbilderbeek/aureole       0


df
