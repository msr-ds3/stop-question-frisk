library(webshot)
library(here)

webshot(url = here("figures", "white_stop_rates_by_precinct.html"),
        file = here("figures", "white_stop_rates_by_precinct.png"),
        cliprect = "viewport")

webshot(url = here("figures", "black_stop_rates_by_precinct.html"),
        file = here("figures", "black_stop_rates_by_precinct.png"),
        cliprect = "viewport")