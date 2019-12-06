clear
cd "B:\Workstation\Dropbox\DataCenter\GHTK\GHTK_Reports\Dataset
*-----------------------------------------------------------------------------------------

import excel "hoa_don_cod_20191130174920_5de24930-be3c-430e-972a-4a100a0a3422.xlsx", ///
			  sheet("Hóa đơn COD") firstrow

*-----------------------------------------------------------------------------------------

rename SLĐH volume
label variable volume "SLDH"

rename SLĐHđãđốisoát success
label variable success "SLDH doi soat"

rename SLĐHdelay delay
label variable delay "SL delay"

rename SLĐHđãtrả cancel
label variable cancel "SL tra"
*-----------------------------------------------------------------------------------------

gsort -volume +cancel
table Cod, contents(sum volume sum success sum delay sum cancel )




