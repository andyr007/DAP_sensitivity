### WALSH

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Summit_WF")
system("./wal_sum_wf.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Side_WF")
system("./wal_sid_wf.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Toe_WF")
system("./wal_toe_wf.sh", intern = F)

setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Summit_WCF")
system("./wal_sum_wcf.sh", intern = F)
setwd("/media/sf_M_DRIVE/WalshSims/Sensitivity Files/LHC_yao/Side_WCF")
system("./wal_sid_wcf.sh", intern = F)