// Time use 2022 and 2021, by HH Sievertsen 3-1-2023, 
// All credit to Asjad Naqvi  https://medium.com/the-stata-gallery/drawing-nyts-spiral-plot-in-stata-aee1c640f345 
 

// Load data
import delimited "Toggl_time_entries_2022-01-01_to_2022-12-31.csv", clear
tempfile tf
save `tf',replace
import delimited "Toggl_time_entries_2021-01-01_to_2022-01-01", clear
append using `tf',force

// Fix dates
gen year=substr(startdate,1,4)
gen month=substr(startdate,6,2)
gen day=substr(startdate,9,10)
destring year month day, replace
gen date=mdy(month,day,year)
sort date

// Fix duration
gen positioncolon=ustrpos(duration,":")
destring positioncolon,replace
gen durationmin=substr(duration,1,positioncolon-1)
replace positioncolon=positioncolon+1
gen durationsek=substr(duration,1,2)
destring durationmin durationsek,replace
gen durationsec=durationmin*60+durationsek

// clean and collapse
gen duration_teaching=durationsec if project=="teaching" | project=="admin"
gen duration_research=durationsec if project=="research"
collapse (sum) duration_teaching duration_research, by(date)
sort date
save `tf',replace

// Balanced panel
clear 
set obs 730
gen date=_n+mdy(12,31,2020)
merge 1:1 date using  `tf',nogen

// Convert seconds to hours
replace duration_research=(duration_research/60)/60
replace duration_teaching=(duration_teaching/60)/60

// Normalise to max of research
summ duration_research
gen duration_research_norm  = 10 * (duration_research - 0) / (r(max) - 0)  
summ duration_research
gen duration_teaching_norm = 10 * (duration_teaching - 0) / (r(max) - 0) 

//  Generate spiral basics (20 is level and 30 is curvature)
gen base_sp   = 15 + (_n / 30) 

// Normalise values relative to spiral
gen duration_research_sp  = base_sp  + duration_research_norm
gen duration_teaching_sp = base_sp - duration_teaching_norm

// Generate spiral  
gen angle = (_n * 2 * -_pi / 365) 
gen x = (base_sp * cos(angle)) 
gen y = (base_sp * sin(angle))
replace angle = angle + (90 * _pi / 180)
gen xrot = (base_sp * cos(angle)) 
gen yrot = (base_sp * sin(angle))
// Keep rotated 
drop x y 
ren xrot x
ren yrot y 

// Add data to spiral
gen x_duration_research = (duration_research_sp * cos(angle)) 
gen y_duration_research = (duration_research_sp * sin(angle))
gen x_duration_teaching = (duration_teaching_sp * cos(angle)) 
gen y_duration_teaching = (duration_teaching_sp * sin(angle))

 
 
// Monthly markers
cap drop theta
cap drop px1 px2 py1 py2
gen theta = _n * _pi / 6 in 1/6
gen px1 =  abs((45 + 2) * cos(theta))  
gen px2 = -abs((45 + 2) * cos(theta))
gen py1 = tan(theta)*px1
gen py2 = tan(theta)*px2
cap drop marker1 marker2
gen marker1 = ""  if theta!=.
gen marker2 = ""  if theta!=.
 
replace marker1 = "Mar" in 1
replace marker1 = "Feb" in 2
replace marker1 = "Jul" in 3
replace marker1 = "Jun" in 4
replace marker1 = "May" in 5
replace marker1 = "Apr" in 6
 
replace marker2 = "Sep" in 1
replace marker2 = "Aug" in 2
replace marker2 = "Jan" in 3
replace marker2 = "Dec" in 4
replace marker2 = "Nov" in 5
replace marker2 = "Oct" in 6
local spike
forval x = 1/6 {
 local theta = (`x') * _pi / 6   
 local liner = abs(45 * cos(`theta'))    
 
 local spike `spike' (function (tan(`theta'))*x, n(2) range(-`liner' `liner') lw(vthin) lc(gs13) lp(dash)) ||
 }

 
di "`spike'" 
 
// Figure

twoway ///
 `spike' ///
 (pcspike  y_duration_research  x_duration_research y x, lw(0.6) lcolor("109 176 127"))  ///
 (pcspike y_duration_teaching x_duration_teaching  y x, lw(0.6) lcolor("206 209 167")) ///
  (scatter py1 px1, mlabcolor(gs6) mc(none) ms(point) mlab(marker1) mlabpos(0) mlabsize(2.3)) ///
  (scatter py2 px2, mlabcolor(gs6)  mc(none) ms(point) mlab(marker2) mlabpos(0) mlabsize(2.3)) /// 
  (line y        x       , msize(vsmall) lpattern(solid) lc(gs6) lw(vthin)) ///
  , aspect(1) xsize(1) ysize(1) ///
  xlabel(-40 40, nogrid) ylabel(-40 40, nogrid) ///
  xscale(off) yscale(off)    ///
  text( 39.5 4 "Jan 1, 2023" ,size(tiny) color(gs6)) ///
  text( 15 -4 "Jan 1, 2021" ,size(tiny) color(gs6)) ///
  legend(order(7 "Research" 8 "Teaching & Admin") ring(0) pos(center) cols(1) symy(1) symx(3) textw(5) forces size(tiny)) ///
  title("{fontface Arial Bold:How I allocate my working hours}", size(3.5) color(gs4)) ///
  caption("Created by Hans Henrik Sievertsen, but everyting nice in this chart is by @AsjadNaqvi.", size(1.5) color(gs6))
