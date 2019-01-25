# OpenOCD-GUI
GUI in Tcl/Tk for OpenOCD as debugger

Purpose: I wanted more comfortable debugging of a "Blue Pill" (an STM32F103C8 board, which I connected to a Raspberry Pi's GPIO) than putting every command like "step" and examining code/registers manually when I wanted to see it.

It is a preliminary version and till now the "Blue Pill" as a target and the Pi as an interface was the only H/W it was tested with - other interface will need other OpenOCD configuration, and with other target the GUI may behave badly or fail at all (e.g. it assumes the target has same register count as STM32F103C8 with Cortex-M3 core has).

I need comments to know what else features are the mostly needed, therefore I am putting this preliminary version here for others to try it.
