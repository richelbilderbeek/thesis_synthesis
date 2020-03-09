# List the SLOC of all my projects
richel_project_names <- c(
  "aureole",
  "babette",
  paste0("babette_example_", seq(1, 9)),
  "babetter",
  "beastier",
  "beautier",
  "becosys",
  "daisieme",
  "mcbette",
  "mauricer",
  "peregrine",
  "pirouette",
  paste0("pirouette_example_", seq(1, 30)),
  "raztr",
  "raztr",
  "raket",
  "razzo",
  "ribir",
  "tracerer"
)


project_names <- c(
  richel_project_names,
  "beast2",
  "DAISIE",
  "DDD",
  "mbd",
  "nLTT",
  "nodeSub",
  "PBD",
  "phangorn"
)

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

