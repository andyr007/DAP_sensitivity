### STERLING

setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Summit_WF")
system("./ste_sum_wf.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Side_WF")
system("./ste_sid_wf.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Toe_WF")
system("./ste_toe_wf.sh", intern = F)

setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Summit_WCF")
system("./ste_sum_wcf.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Side_WCF")
system("./ste_sid_wcf.sh", intern = F)
setwd("/media/sf_M_DRIVE/SterlingSims/Sensitivity Files/LHC_yao/Toe_WCF")
system("./ste_toe_wcf.sh", intern = F)
