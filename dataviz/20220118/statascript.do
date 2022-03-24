clear
// working directory
cd "C:\Users\hs17922\OneDrive - University of Bristol\Desktop"
// load data
import delimited "45f367cd-b1f9-4c71-8f3f-04fe33e1c254_Data"
// make wide
gen s="GDP" if seriescode=="NY.GDP.PCAP.PP.KD"
replace s="LE" if seriescode=="SP.DYN.LE00.IN"
drop if s==""
keep countrycode time s value
reshape wide value,i(countrycode time) j(s) string
drop if valueLE==. | valueGDP==.
replace valueGDP=ln(valueGDP)
tempfile tf
save `tf',replace
// load metadata
import delimited "45f367cd-b1f9-4c71-8f3f-04fe33e1c254_Country - Metadata", clear 
// keep what we need
rename ïcode countrycode
keep countrycode incomegroup
// merge back on
merge 1:m countrycode using `tf',nogen keep(3)
// at least 30 obs
bys countrycode: egen LEN=count(valueLE)
keep if LEN==30
// create chart
encode countrycode,gen(c)
encode incomegroup,gen(i)
local a=""
local b=""
levelsof(c),local(lab)
foreach v in `lab'{
	local a=`"`a' (line valueLE valueGDP if c==`v', lcolor(gs12))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==2019 & i==1,  msize(small) mcolor("235  94 52"))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==2019 & i==2, msize(small) mcolor("88 138 134"))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==2019 & i==3, msize(small)  mcolor("126 158 119"))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==2019 & i==4, msize(small) mcolor("196 116 155"))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==1990 & i==1, msize(tiny) msymbol(Oh) mcolor("235 94 52"30))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==1990 & i==2, msize(tiny) msymbol(Oh) mcolor("88 138 134"30))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==1990 & i==3, msize(tiny) msymbol(Oh) mcolor("126 158 119"30))"'
	local b=`"`b' (scatter valueLE valueGDP if c==`v' & time==1990 & i==4, msize(tiny) msymbol(Oh) mcolor("196 116 155"30))"'
}
sort time
tw  (scatter valueLE valueGDP if time==2030 ,  msize(large) mcolor("235 94 52") ) ///
	(scatter valueLE valueGDP if time==2030,  msize(large) mcolor("88 138 134") ) ///
	(scatter valueLE valueGDP if time==2030 ,  msize(large) mcolor("126 158 119") ) ///
	(scatter valueLE valueGDP if time==2030 ,  msize(large) mcolor("196 116 155") ) ///
	(scatter valueLE valueGDP if time==2030,  msize(large) mcolor(black)  ) ///
	(scatter valueLE valueGDP if time==2030,  msize(small) mcolor(black) msymbol(Oh) ) ///  
	(scatter valueLE valueGDP if time==2030,  msize(large) mcolor(white) ) `a' `b'   ///  
  , graphregion(lcolor(white) fcolor(white)) plotregion(lcolor(white) fcolor(white)) ///
   yscale(noline) xscale(noline) ///
   xlab(#5,notick grid glwidth(thin) nogmin nogmax) ylab(#10,noticks)  ///
   xmtick(, grid glwidth(thick) gmin gmax gextend) ///
   ylab(#5,notick grid glwidth(thin) nogmin nogmax) ylab(#10,noticks)  ///
   ymtick(, grid glwidth(thick) gmin gmax gextend) xtitle(Log GDP per capita) ytitle(Life expectancy at birth) note("Data source: World Bank") title("GDP per capita and Life Expectancy 1990-2019") /// 
   legend(order(7 "  " 6 "1990"   5 "2019" 7 "    " 2 "Low inc"  3 "Low middle inc" 4 "Up middle income" 1 "High inc" ) region(lcolor(white)) row(2) pos(12))
