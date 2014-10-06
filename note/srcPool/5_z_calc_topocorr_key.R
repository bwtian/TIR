load('6_z_evaluate_DEMS.RData')

tc_key <- dem_freqs[1:3]
tc_key$do_tc <- (dem_freqs$pct95 > 10)

table(tc_key$do_tc)

write.csv(tc_key, file='Scene_topocorr_key.csv', row.names=FALSE)
