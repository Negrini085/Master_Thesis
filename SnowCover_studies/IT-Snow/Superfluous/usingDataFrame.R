# The goal of this script is to become proficient in using dataframes as
# a tool to be applied in different case studies. In particular, I want/need to
# be able to access columns with ease

# Defining data frame. As you can see, some variables are NULL because we want to
# access and change them later. Before doing other stuff, we will print a summary
# of the dataframe we just created, in order to check what we are dealing with
df <- data.frame(
  days = c(1:20),
  appo = NA,
  pippo = NA
)

summary(df)