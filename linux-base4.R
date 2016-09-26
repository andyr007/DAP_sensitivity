### STRATTON

setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCF")
system("./str_sum_wcf.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCF")
system("./str_sid_wcf.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Toe_WCF")
system("./str_toe_wcf.sh", intern = F)

setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WCMF")
system("./str_sum_wcmf.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WCMF")
system("./str_sid_wcmf.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Toe_WCMF")
system("./str_toe_wcmf.sh", intern = F)