#!/usr/bin/env tclsh

proc erx { em } { if { [catch { winfo .e.em }] } then { puts stderr $em
  } else { .e.em configure -text $em;  pack .e.em;  tkwait window .e.em };  exit
}

array set stm { h ""  b ""  a ""  f 0  w "vwait stm"  c ""  t "" }

# proc pnl { c } { puts stderr $c;  flush stderr }

proc stmd_clr { fv } {  global stm;  if { $stm(a) != "" } then {
    if { $fv < 0 } then { append stm(t) t }
    after cancel $stm(a);  set stm(a) ""
    set stm(f) $fv;  if { $fv } then stmd_read
} }

proc stmd_read {} {  global stm;
  if { [set r [read $stm(h)]] == "" } then return
  append stm(b) $r; # append stm(t) r
  if { [string range $stm(b) end-1 end] != "> " } then return
  set c [string index $stm(b) end-2];
  if { $c != "\r" && $c != "\b" } then return
  stmd_clr 0;  incr stm(f);
  set stm(b) [string map { "\0" "" } [string range $stm(b) 0 end-3]] }

proc stmd_wait { ms } {  global stm
  flush $stm(h);  set stm(a) [after $ms stmd_clr -1]
  set stm(f) 0;  while { ! $stm(f) } $stm(w) }

proc stmd_put { cs } {  global stm;  puts $stm(h) [set stm(c) $cs]; }

proc stmd_cmd { cs ms ov } {  global stm;  set stm(b) "";  stmd_put $cs
  stmd_wait $ms;  if { $stm(f) == -1 } then { erx "timeout ($cs,$ms)" }
  upvar $ov o;  set o [split [string map { "\r\n" "\n" } $stm(b)] "\n"]
}

proc cqa { args } {  if { [llength [lappend ::cmdq $args]] > 1
  } then return;  while { $::cmdq != {}
  } { eval [lindex $::cmdq 0];  set ::cmdq [lrange $::cmdq 1 end] }
};  set cmdq {}

proc qc { args } { stmd_cmd [join $args] 500 ol;  foreach o $ol { puts $o } }

if { [catch { socket localhost 4444 } stm(h)] } then { erx $stm(h) }
fconfigure $stm(h) -translation binary -blocking false
fileevent $stm(h) readable [list stmd_read];  stmd_wait 200

package require Tk;  wm sizefrom . program;  wm resizable . 0 0
foreach f { e t l d m } { pack [frame .$f] -fill x -expand true -side top }
label .e.em -bg yellow -fg brown
pack [listbox .l.lbx -listvar lbx -width 0 -height 0 -font "courier 12" \
-state disabled -disabledforeground black] -fill x -expand true -side top
set lbx {}
pack [frame .t.r] -fill x -expand true -side top
for { set i 0 } { $i < 23 } { incr i
} { labelframe .t.r.$i -bg white -font "courier 12" -padx 0 -pady 0 -bd 0 \
    -labelanchor w -relief flat;  pack [button .t.r.$i.v -bg white	\
    -font "courier 12" -justify left -padx 0 -pady 0 -bd 0 -relief flat	\
    -textvariable ::reg($i,v) -command "regbut $i"] -expand true -fill x }
checkbutton .t.r.t  -overrelief raised	-bg #ffaaaa  -font "courier 10"	\
 -selectcolor lightgreen  -indicatoron false  -padx 0  -pady 0  -bd 2	\
 -textvariable runst  -variable tmode  -command { focus . }
#  -activebackground yellow  -bg lightgray
foreach { i1 i2 i3 i4 } { 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
 t 16 17 18 19 20 21 22 } { grid .t.r.$i1 .t.r.$i2 .t.r.$i3 .t.r.$i4 -sticky news }
label .d.al -font "Courier 12" -text "A=0x"
entry .d.ae -font "Courier 14" -textvar d(a) -width 9
label .d.sl -font "Courier 12" -text " S="
foreach q { B H W D Q } { radiobutton .d.s[string tolower $q]	\
  -font "Courier 14"  -indicatoron false  -selectcolor yellow	\
  -variable d(s)  -value $q  -text $q }
# -offrelief flat -overrelief raised
label .d.vl -font "Courier 12" -text " V="
entry .d.ve -font "Courier 14" -textvar d(v) -width 9
label .d.ol -font "Courier 12" -text " O="
foreach q { F S B W D R } { button .d.o[string tolower $q]	\
  -font "Courier 14"  -activebackground yellow  -text $q	\
  -command "cqa aop $q"  -padx 0  -pady 0 }
grid .d.al .d.ae .d.sl .d.sb .d.sh .d.sw .d.sd .d.sq	\
 .d.vl .d.ve .d.ol .d.of .d.os .d.ob .d.ow .d.od .d.or

proc mdc { bwi } { global mdl;  set wil [lsort -int [string map {
    ",a" "" } [array names mdl "*,a"]]];  set ci 1;  foreach wi $wil {
      set ci [expr { $ci^1 }];  if { $wi < $bwi } then continue
      .m.a$wi config -bg [lindex "#ddeeff #ffeedd" $ci]
      .m.d$wi config -bg [lindex "#ccffff #ffffcc" $ci] } }

# array set mdc { rx,a {^0x[0-9a-f]{8}: }  rx,b {(?:([0-9a-f]{2}) )}
# (?: ([0-9a-f]{2}))+ $} }

proc mdu1 { wi } { global mdl
  stmd_cmd "md$mdl($wi,t) $mdl($wi,a) $mdl($wi,s)" 3000 q
  for { set i [llength $q]; incr i -1 } { [regexp {^0x[0-9a-f]{8}: }	\
    [lindex $q [incr i -1]] a] } {} {# puts -nonewline " $i" };  set ds ""
  regexp {^0x[0-9a-f]{8}: } [set e [lindex $q [incr i]]] a
  if { ! [info exists a] } then { puts "No 'a'; e='$e', i=$i, q=$q"; continue }
  if { $a != [format "%#010x: " $mdl($wi,a)] } then {
    puts "'$a' != '[format "%#010x: " $mdl($wi,a)]'";  continue }
  foreach e [lrange $q $i end-1] { append ds [string range $e 12 end] }
  set epl [expr { 4*(1+($mdl($wi,t) != "w")) }]
  set lai [expr { 8*(1+($mdl($wi,t) != "b")) }]
  set dl [split [string trim $ds] " "];  set ec 0;  set ld {};
  set ll {};  set al {};  set a $mdl($wi,a);
  foreach d $dl { lappend ld $d;  if { [incr ec] % $epl } then continue
    set ls [join $ld];  if { $mdl($wi,t) == "b" } then {
      set ls [format "%-25s%s" $ls [binary format H* [join [
        regsub -all {[0189a-f][0-9a-f]|7f} $ld 2e] ""]]]
    };  lappend ll $ls;  set ld {};
    lappend al [format "%#010x: " $a];  incr a $lai
  }; if { $ld != {} } then { set ls [join $ld];  if { $mdl($wi,t) == "b"
    } then { set ls [format "%-25s%s" $ls [binary format H* [join [
        regsub -all {[0189a-f][0-9a-f]|7f} $ld 2e] ""]]]
    };  lappend ll $ls;  set ld {};  lappend al [format "%#010x: " $a] }
  set mdl($wi,d) [join $ll "\n"];  set mdl($wi,l) [join $al "\n"]
  #  puts $dl;  puts "$mdl($wi,l):\t[join $ll "\n\t\t"]";
}; # mdl($wi,a) mdl($wi,s) mdl($wi,t) mdl($wi,l) mdl($wi,d)
bind .m <1> "cqa mdua"

proc mdua {} { global mdl;  foreach wi [lsort -int [string map {
    ",a" "" } [array names mdl "*,a"]]] { mdu1 $wi }}

proc rmd { wi } { destroy .m.a$wi .m.d$wi .m.r$wi .m.x$wi
  global mdl;  array unset mdl "$wi,*";  mdc $wi }
# planned display formats:  W: 4/line;  H: 8/line;  B: 8/line + chars.
proc aop { op } { set t [string tolower $::d(s)]
  if { $op == "R" } then { mdua;  return }
  if { $t != "" && [set s [string first $t bhwdq]] >= 0
  && [scan $::d(a) %x%n a q] > 0 && [string index $::d(a) $q] == ""
  } then { set m [expr { 1<<$s }];  if { $s > 2 && $op != "W"
  } then { puts "Size $t too big";  return }
  switch $op {
    B {	if { $s == 0 } then { puts "Size $t too small";  return }
	stmd_cmd "rbp $a" 400 b;  set r ""
	if { [string match "no *" [lindex $b end-1]] } then {
	  set b "bp $a $m";  puts $b;  stmd_cmd $b 400 r;
	  set ::d(v) "BP added" } else { set ::d(v) "BP removed" }
	puts $b;  puts $r;  das 0x$::reg(15,v);  return }
    F {	stmd_cmd "md$t $a" 400 r
	if { [set c [scan [lindex $r end-1] "0x%x:%x" q v]] != 2
	|| $q != $a } then { puts $r;  puts "c=$c a=$a q=$q"
	} else { set ::d(v) [format %0[expr { $m*2 }]x $v] };  return }
    S {	if { [scan $::d(v) %x%n v q] > 0 && [string index $::d(v) $q] == ""
	} then { stmd_cmd "mw$t $a $v" 400 w;  puts "$w";  return }
	if { ! [info exists v] } then { set v -;  set q - }
	puts "$::d(v);  q=$q v=$v";  return }
    D {	if { ! [string is int -strict [set v $::d(v)]] } then { set v 8 }
	global mdl;  set wi $mdl(l);  set mdl($wi,a) $a;  set mdl($wi,t) $t
	set mdl($wi,s) $v;  set mdl($wi,l) [format "%#010x" $a]
	label .m.a$wi -textvar mdl($wi,l) -width 12	\
	 -bd 0 -padx 0 -pady 0 -font "courier 12" -anchor w
	label .m.d$wi -textvar mdl($wi,d) -width 39	\
	 -bd 0 -padx 0 -pady 0 -font "courier 12" -anchor w -justify left
	button .m.r$wi -text R -command "mdu1 $wi"	\
	 -bd 0 -padx 0 -pady 0 -font "courier 12"
	button .m.x$wi -text X -command "rmd $wi"	\
	 -bd 0 -padx 0 -pady 0 -font "courier 12"
	grid .m.a$wi .m.d$wi .m.r$wi .m.x$wi -sticky news
	mdc $wi;  mdu1 $wi;  incr mdl(l);  return }
    W {	# watchpoint; valid sizes are 1,2,4,8,16
	stmd_cmd "rwp $a" 400 b;  set r ""
	if { [string match "no *" [lindex $b end-1]] } then {
	  set b "wp $a $m";  puts $b;  stmd_cmd $b 400 r;
	  set ::d(v) "WP added" } else { set ::d(v) "WP removed" }
	puts $b;  puts $r;  return }
  } }
  puts "Addr=$::d(a) Size=$::d(s) Data=$::d(v) Op=$op"
};  set mdl(l) 0

binary scan $stm(b) H24a* h a;  puts $h;  puts $a.

stmd_cmd targets 120 tsl;  set tsi [lsearch $tsl "-- *----"]
if { [incr tsi] } then {
  set t "[set tsc [expr { [llength $tsl]-1-$tsi }]] target(s):"
  puts $t;  lappend lbx $t;  for { set i 0 } { $i < $tsc } { incr i
  } { set t [lindex $tsl [expr { $tsi+$i }]];  lappend lbx $t;  puts $t }
};  if { $tsc != 1 } then { erx "Need exactly 1 target" }
# if { $stm(f) == 1 && [regexp {([a-z]+)..$} $stm(b) - s] } then { puts "state=$s" }
# running / halted due to: debug-request / watchpoint / breakpoint
# target halted due to breakpoint, current mode: Thread
if 0 then {
stmd_cmd poll 200 tpl;  set thi [lsearch $tpl "target halted due to *"]
if { $thi < 0 } then { .t.r.t configure -text "running..."; # puts "running..."
} elseif { [regexp { to ([^,]*), current mode: (.)} [lindex $tpl $thi] - hr pm]
} then { .t.r.t configure -text "$hr/$pm"; # puts "halted/$hr, mode:$pm"
} else { puts "halted/[string range [lindex $tpl $thi] 21 end]" }
}

# r16 = PSR = Program Status Register:  31:27 = NZCVQiiT  24 = T
# 26:25,15:10 = ICI/IT (00....00 for ICI)  8:0 = ISR_NUMBER (0 in Thread mode)
# N=negative Z=zero C=carry/no_borrow V=overflow Q=saturation(sticky)

proc reg {} { global reg reg_i reg_f runst;  stmd_cmd poll 200 tpl
  set thi [lsearch $tpl "target halted due to *"]
  if { $thi < 0 } then { set runst "Running...";  return 0
  } elseif { [regexp { to ([^,]*), current mode: (..)} [lindex $tpl $thi
    ] - hr pm] } then { switch $hr {
      single-step { set hr "S_St" }  debug-request { set hr "D_Rq" }
      breakpoint { set hr "BPnt" }  watchpoint {  set hr "WPnt" }
      default { set hr [string range $hr 0 3] }
    }; # set runst "$hr/$pm  "
  } else { puts "halted/[string range [lindex $tpl $thi] 21 end]" }

  set rx {\(([0-9]+)\) ([^ ]+) \(/([0-9]+)\): 0x([^ ]+) *(.*)$}
  stmd_cmd reg 500 rgl;  if { [set rgi [lsearch $rgl "===== arm *"]] != -1
  } then { set qi 0;  array unset reg
    foreach rge [lrange $rgl [incr rgi] end] {
      if { [regexp $rx $rge - ri r(n) r(s) r(v) d] && $ri == $qi
      } then { incr qi;  foreach i { n s v } { set reg($ri,$i) $r($i) }
    } }
    if { ! [info exists reg_i] } then { set reg_i .
      for { set ri 0 } { $ri < 23 } { incr ri
      } { .t.r.$ri configure -text [set t [format "%4s=" $reg($ri,n)]]
        if { $reg($ri,n) == "xPSR" } then { set reg_f $ri }
    } };  set runst "$hr/$pm  "
    if { [info exists reg_f] } then { set f $reg($reg_f,v)
      binary scan [binary format H2 $f] B5 b; # puts "f=$f b=$b"
      foreach v [split $b ""] q { nN zZ cC vV qQ
      } { append runst [string index $q $v] }
    }
  } else { puts "$rgi/$rgl" };  return 1
}

proc das { pc } {
  if { [set pi [lsearch $::lbx [string tolower "???$pc *"]]] != -1 && $pi < 12
  } then { set dl $::lbx;  set ::lbx {};  foreach de $dl {
      if { ! [regexp {^(.)..([^ ]+)( .*)$} $de - df da dx] } then break
      if { $da == "0x$::reg(15,v)" } then { set dc "=>" } else { set dc "  " }
      lappend ::lbx "$df$dc$da$dx" };  return
  };  puts "no \"???$pc *\""
  set rx {^IVA breakpoint: (0x[0-9a-f]{8}), 0x2, 1, 0x0.$}
  stmd_cmd bp 300 bpl;  set bal {};  foreach bp $bpl {
    if { [regexp $rx $bp - ba] } then { lappend bal $ba } }
  foreach ba $bal { stmd_cmd "rbp $ba" 400 bps; # puts $bps }; # qc bp
  stmd_cmd "arm disassemble $pc 15" 900 dal;  set ::lbx {}
  foreach dae [lrange $dal 1 end-1] {
    foreach { da1 da2 da3 } [split $dae "\t"] break
    if { [string range $da1 0 9] == "0x$::reg(15,v)"
    } then { set da0 "=>" } else { set da0 "  " }
    if { [lsearch $bal [string range $da1 0 9]] == -1
    } then { set da0 " $da0" } else { set da0 "B$da0" }
    lappend ::lbx [format "%-25s  %-7s  %s" $da0$da1 $da2 $da3]
  };  # pack forget .l.lbx;  pack .l.lbx -fill x -expand true -side top
  foreach ba $bal { stmd_cmd "bp $ba 2" 200 bps }
}
# foreach rg $rgl { puts $rg }

# puts [join $tpl "\n"]
proc udi {} { if { [reg] } then { das 0x$::reg(15,v)
  } else { after 200 cqa udi } }

proc reg_ent { k } {
  switch $k { Return - KP_Enter { set a true }
    Escape { set a false }  default { return false }
  };  if { [catch { pack info .re } q]
  } then { puts "vcmd - no pack";  return false }
  set pp [lindex $q 1];  set bi [lindex [split $pp .] end]
  if { $a } then { global reg reg_ent
    if { [scan $reg_ent %x%n n q] > 0
    && [string index $reg_ent $q] == "" } then {
#      puts "$pp.v format %0[expr {[.re cget -width]-1}]X $n"
      set reg_ent [format %0[expr {[.re cget -width]-1}]X $n]
#      puts "vcmd: [$pp cget -text]$reg($bi,v)/$reg_ent"
      stmd_cmd "reg $reg($bi,n) 0x$reg_ent" 400 v;
      set reg($bi,v) $reg_ent;  puts $v
  } };  pack forget .re;  pack $pp.v;  return true
}

entry .re -bg yellow -width 9 -textvar reg_ent -font "Courier 10"	\
;  # -validate focusout -vcmd reg_ent
bind .re <Key> "cqa reg_ent %K";  

proc regbut { bi } { if { ![catch { pack info .re }] } then return
  global reg reg_ent;  set reg_ent $reg($bi,v)
  puts "r($bi): $reg($bi,n)=$reg($bi,v)"
  pack forget .t.r.$bi.v;  pack .re -in .t.r.$bi -expand true -fill x
  .re configure -width [expr { [string length $reg_ent]+1 }]
}

proc key { kn sh } { global tmode;  if { ! $tmode } then return
#  set w [focus];  puts "focus=$w tmode=$tmode sh=$sh kn=$kn"
#  set w [focus];  if { $w != "" && [winfo class $w] == "Entry"
#  } then { set tmode 0;  return }
# sh = shift (1=shift 2=CapsLock 4=ctrl 8=alt 16=NumLock 32=? 64=Super 128=?
# kn = key name (e.g. Return KP_Enter Escape space BackSpace asterisk a b c)
  set rh [expr { 76 & $sh }];  switch "$rh-$kn" {
    "0-space" { stmd_cmd step 200 o;  udi }
    "0-r" { stmd_cmd resume 200 o;  udi }
    "0-h" { stmd_cmd halt 200 o;  after 100;  udi }
    default { puts "Key $sh-$kn" }
} };  bind . <Key> "cqa key %K %s";  foreach w { .re .d.ae .d.ve
} { bind $w <FocusIn> { set tmode 0 }};  cqa udi
# SST:	 @ascii  Assem  Break  Compare  Display  Examine  Fill  Goto
#	 Hex Input  Klear  Load  Move  Name  Output  Protect  Quit
#	 Reg  Search  Trace  Unassem  Vector  Write  Xamine  Y286  Zamine
#  E - also Enter;  G - =addr [bp1 [bp2 ...]];  H - hex2bin;  K - also Key;
#  L - may load labels/varnames; N - also redirection and user strings;
#  P - set mem protectiow (wp in oocd); Q - a lot of extra settings;
#  RR - restore registers, Run - from start; Y - GDT;  Z - 8087 status.
# Trace: Break  # n  Go addr  Cont  Fast  Slow  Jmp  Nop  Re  Win
#	 Xam  Txam  ^Ascii -> <-  @scii  space  8087  Undo  View
#  Break   Break at current instruction (CI)
#  # n     Break at CI after n passes
#  Go adr  Break at adr
#  Cont    Trace Continuously (Q - same Quietly)
#  Fast    Break following CI (D - same if CI=CALL)
#  Slow    Trace CI even if INT n
#  Jmp     Jump unconditionally on conditional jmp's
#  Nop     Skip CI
#  Re      Redraw screen
#  Win     Toggle Window for program CRT output
#  Xam     Toggle memory eXamine window
#  Txam    Toggle Tracking eXamine window
#  ASCII   (Ctrl-A) Toggle ASCII vs HEX/ASCII displays
#  ←↑↓→    Move cursor in nonTracking Xam window
#  @scii   Display ASCII screen
#  SPACE   Single step, CR Return to COMMAND MODE
#  8087    Toggle 8087 window
#  Undo    Undo last instruction
#  View    Toggle full program display window

# button .b -relief flat .b config -command -padx 3p -pady 0
# -activebackground yellow -bg white -textvariable tv -anchor c/e
# -activeforeground black -fg black -bd 1 -font Courier
# -highlightcolor blue -highlightbackground lightgray -justify r
# -highlightthickness 1 -overrelief sunken
# single-step | debug-request | breakpoint | watchpoint
