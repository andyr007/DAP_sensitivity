### STERLING

setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Summit_WCMF")
system("./ste_sum_wcmf.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Side_WCMF")
system("./ste_sid_wcmf.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Toe_WCMF")
system("./ste_toe_wcmf.sh", intern = F)

setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Summit_OPP")
system("./ste_sum_opp.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Side_OPP")
system("./ste_sid_opp.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Toe_OPP")
system("./ste_toe_opp.sh", intern = F)