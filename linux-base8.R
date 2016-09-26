### WALSH

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Side_OPP")
system("./wal_sid_opp.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Toe_OPP")
system("./wal_toe_opp.sh", intern = F)

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Summit_Grass")
system("./wal_sum_grass.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Side_Grass")
system("./wal_sid_grass.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Toe_Grass")
system("./wal_toe_grass.sh", intern = F)