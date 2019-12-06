clear 
use auto

keep make rep78 foreign mpg price

tab rep78 foreign, missing
*-------------------- transform
recode mpg (min/10 = 1) ///
			(11/20 = 2) ///
			(20/30 = 3) ///
			(30/max = 4), gen(mpg_n) label(mpg_n)
			
*-------------- tabulate and crosstab			
tab mpg_n, plot
*------------------- select
list if rep78 >=4 & !missing(rep78)

list if rep78 >=2 & mpg_n == 3 & !missing(rep78)

*-------------------- Histogram
hist rep78, percent discrete
hist mpg_n, percent discrete width(0.4)
hist rep78, freq discrete by(foreign)
hist rep78, percent discrete by(foreign)

*--------------------- Box
graph box price, by(foreign)
graph box mpg, by(foreign)
graph box mpg, over(foreign)

graph box mpg, over(foreign) title("MPG theo Domestic va Foreign") noout // - Khong co outlier

*--------------- pie
graph pie, over(rep78) plabel(_all name) title("Repair Record 1978") // Hien ten
graph pie, over(rep78) plabel(_all percent) title("Repair Record 1978") // Hien phan tram

*---------------- scatter
graph twoway scatter price mpg, by(foreign)
graph twoway scatter price rep78, by(foreign)
//----------------------- scatter fit linear
graph twoway (scatter price mpg) (lfit price mpg), by(foreign)
graph twoway (scatter price mpg, mlabel(make) ) (lfit price mpg)
//------------------------- scatter fitci
graph twoway (scatter price mpg) (lfitci price mpg)
graph twoway (scatter price mpg) (lfitci price mpg), by(foreign)

*---------------- Tabulate

tab foreign, plot
tab foreign rep78, column row // hien thi phan tram
tab foreign rep78, all exact // hien thi cac test

tabstat rep78, by(foreign) stat(mean sum count) // Hien thi table theo ham thong ke
table foreign , contents(mean rep78 sum rep78 count rep78 min rep78 max rep78) row // tao table hien ham thong ke

sort foreign
bys foreign: asrol rep78, stat(gmean)

bys id: asrol X, window(months 5) stat(count) gen(count)


*----------------------------------- Tinh toan Mathematic tren STATA

scalar t = chi2(10,2)
display t // di t

scalar t = 10^2 - sin(20)
di t

*--------------------------- tao bien gia lap
egen mean_price=mean(price)
gen price_3 = mean_price - 10^2  


