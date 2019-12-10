clear
import excel "B:\Workstation\Dropbox\DataCenter\GHTK\GHTK_Reports\Dataset\ReportTruck500kg.xlsx", sheet("Truck.15003") firstrow

table DateStamp, c(sum SanLuong sum Success sum Success14h sum Delay sum Huy) 

table TimeShift, c(sum SanLuong sum Success sum Success14h sum Delay sum Huy) 
///
clear
import excel "B:\Workstation\Dropbox\DataCenter\GHTK\GHTK_Reports\Dataset\ReportTruck_2to72019.xlsx", sheet("TimeShifts") firstrow clear
twoway connected SanLuong Success Success14 Delay Huy TimeShift
 ///
 
 
 clear
 import excel "B:\Workstation\Dropbox\DataCenter\GHTK\GHTK_Reports\Dataset\ReportTruck_2to72019.xlsx", sheet("DateStamp") firstrow clear
twoway connected SanLuong Success Success14 Delay Huy DateStamp
