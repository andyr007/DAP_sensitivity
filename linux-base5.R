### STRATTON

setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_OPP")
system("./str_sum_opp.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_OPP")
system("./str_sid_opp.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Toe_OPP")
system("./str_toe_opp.sh", intern = F)

setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Summit_Grass")
system("./str_sum_grass.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Side_Grass")
system("./str_sid_grass.sh", intern = F)
setwd("/media/sf_M_DRIVE/StrattonSims/Sensitivity Files/LHC_yao/Toe_Grass")
system("./str_toe_grass.sh", intern = F)