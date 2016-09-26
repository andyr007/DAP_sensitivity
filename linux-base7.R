### WALSH

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Toe_WCF")
system("./wal_toe_wcf.sh", intern = F)

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Summit_WCMF")
system("./wal_sum_wcmf.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Side_WCMF")
system("./wal_sid_wcmf.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Toe_WCMF")
system("./wal_toe_wcmf.sh", intern = F)

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Summit_OPP")
system("./wal_sum_opp.sh", intern = F)