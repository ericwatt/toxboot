# Script to generate sample data to include with toxboot
# this data represents what would be output with toxbootQueryToxCast()
# and is included so that the package can be tested
# without requiring that that ToxCast database be configured.

library(tcpl)
library(toxboot)
tcplConf(db = "prod_external_invitrodb_v2")

# Get aeids corresponding to ER assays

assay_names <- c("NVS_NR_bER",
                 "OT_ER_ERaERa_1440",
                 "ATG_ERa_TRANS_up",
                 "TOX21_ERa_LUC_BG1_Agonist",
                 "ACEA_T47D_80hr_Positive")

aeid_table_full <- tcplLoadAeid()
aeid_table <- aeid_table_full[aenm %in% assay_names]
aeids <- aeid_table[,aeid]

dat <- toxbootQueryToxCast(aeids = aeids)

set.seed(12345)
m4ids <- sample(unique(dat[, m4id]), size = 200)
erl3data <- dat[m4id %in% m4ids]
devtools::use_data(erl3data, overwrite = TRUE)

erl5data <- tcplLoadData(5, fld = "m4id", val = m4ids, type = "mc")
devtools::use_data(erl5data, overwrite = TRUE)
