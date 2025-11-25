install.packages(c("dplyr", "tidyr", "ggplot2"))
library(dplyr)
library(tidyr)
library(ggplot2)

Table2 <- data.frame(
     Year = c(2018, 2019, 2020, 2021, 2022),
     Registered.Users = c(22.6, 24.9, 26.4, 28.3, 30.5),
    Transactions = c(1512, 1840, 2201, 2846, 3165),
     Value = c(3982, 4500, 5210, 6870, 7890)
 )

Table4 <- data.frame(
    Year = c(2018, 2019, 2020, 2021, 2022),
     Reported.Fraud.Cases = c(12340, 14580, 17120, 19750, 21630),
     Estimated.Losses = c(190, 240, 300, 370, 410)
 )

 Table5 <- data.frame(
     Year = c(2015, 2017, 2019, 2021),
     MPesa = c(2100, 3100, 4500, 6800),
     Debit.Card = c(580, 700, 840, 1020),
     Credit.Card = c(230, 250, 290, 330),
     Online.Banking = c(190, 310, 470, 610)
 )

 Table6 <- data.frame(
     Year = c(2017, 2018, 2019, 2020, 2021),
     Adults.Mobile.Account = c(73, 76, 78, 80, 83),
     Using.for.Savings = c(32, 35, 37, 41, 44),
     Using.for.Loans = c(18, 20, 22, 24, 26)
 )
 View(Table2)
 View(Table4)
 View(Table5)
 View(Table6)
 Table2_Analysis <- Table2 %>%
     mutate(
         Transactions_YoY = ((Transactions / lag(Transactions)) - 1) * 100,
         Value_YoY = ((Value / lag(Value)) - 1) * 100
     )

 Table4_Analysis <- Table4 %>%
     mutate(
         Cases_YoY = ((Reported.Fraud.Cases / lag(Reported.Fraud.Cases)) - 1) * 100,
         Losses_YoY = ((Estimated.Losses / lag(Estimated.Losses)) - 1) * 100
     )

 print("--- Table 2: Transaction and Value Growth ---")
[1] "--- Table 2: Transaction and Value Growth ---"
> print(Table2_Analysis)
  Year Registered.Users Transactions Value Transactions_YoY Value_YoY
1 2018             22.6         1512  3982               NA        NA
2 2019             24.9         1840  4500         21.69312  13.00854
3 2020             26.4         2201  5210         19.61957  15.77778
4 2021             28.3         2846  6870         29.30486  31.86180
5 2022             30.5         3165  7890         11.20871  14.84716

 print("--- Table 4: Fraud Cases and Loss Growth ---")
[1] "--- Table 4: Fraud Cases and Loss Growth ---"
 print(Table4_Analysis)
  Year Reported.Fraud.Cases Estimated.Losses Cases_YoY Losses_YoY
1 2018                12340              190        NA         NA
2 2019                14580              240 18.152350   26.31579
3 2020                17120              300 17.421125   25.00000
4 2021                19750              370 15.362150   23.33333
5 2022                21630              410  9.518987   10.81081
 ggplot(Table2, aes(x = Year, y = Value)) +
     geom_line(color = "#0072B2", linewidth = 1.2) +
     geom_point(color = "#0072B2", size = 3) +
     labs(
         title = "Growth of Mobile Money Transaction Value (Ksh Millions)",
         x = "Year",
         y = "Transaction Value"
     ) +
    scale_x_continuous(breaks = Table2$Year) +
     theme_minimal()


 Table5_long <- Table5 %>%
     tidyr::pivot_longer(
         cols = starts_with("MPesa") | starts_with("Debit") | starts_with("Credit") | starts_with("Online"),
         names_to = "Payment_Method",
         values_to = "Value_Billion"
     )

 ggplot(Table5_long, aes(x = Year, y = Value_Billion, color = Payment_Method, group = Payment_Method)) +
     geom_line(linewidth = 1) +
     geom_point(size = 2) +
     labs(
         title = "Comparative Growth of Payment Methods (Ksh Billion)",
         x = "Year",
         y = "Value (Ksh Billion)",
         color = "Method"
     ) +
     scale_x_continuous(breaks = Table5$Year) +
     theme_bw()
