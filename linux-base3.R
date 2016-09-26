setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Summit_Grass")
system("./ste_sum_grass.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Side_Grass")
system("./ste_sid_grass.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Toe_Grass")
system("./ste_toe_grass.sh", intern = F)

### STRATTON

setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_WF")
system("./str_sum_wf.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_WF")
system("./str_sid_wf.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Toe_WF")
system("./str_toe_wf.sh", intern = F)