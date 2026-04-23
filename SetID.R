setID4 <- function(idd)
{
  case_when (
    idd == 1 | idd == 21 ~ "TBX",
    idd == 2 | idd == 22 ~ "TBM",
    idd == 3 | idd == 23 ~ "BWB",
    idd == 4 | idd == 24 ~ "HHB",
    idd == 5 | idd == 25 ~ "MPB",
    idd == 6 | idd == 26 ~ "CBB",
    idd == 7 | idd == 27 ~ "QMT",
    idd == 8 | idd == 28 ~ "BBT",
    idd == 9 | idd == 29 ~ "TNB",
    idd == 11 | idd == 30 ~ "VNB",
    TRUE ~ "NAC")
}
