# List the SLOC of all my projects

project_names <- c(
  "aureole",
  "beautier",
  "daisieme",
  "tracerer",
  "beastier",
  "mauricer",
  "mbd",
  "raztr",
  "raztr",
  "raket",
  "babette",
  "babetter",
  "mcbette",
  "pirouette",
  "razzo",
  "nLTT",
  "nodeSub",
  "DAISIE",
  "becosys",
  "ribir",
  paste0("babette_example_", seq(1, 9)),
  paste0("pirouette_example_", seq(1, 30))
)

project_names <- sort(project_names)

count_lines <- function(filenames) {
  sum <- 0
  for (filename in filenames) {
    sum <- sum + length(readLines(con = filename, warn = FALSE))
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
    pattern = "^.*\\.(R|Rmd)$"
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
  aes(x = name, y = sloccount, fill = name)
) + geom_col(position = "identity", color = "black") + coord_flip() +
  theme(legend.position="none") +
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
