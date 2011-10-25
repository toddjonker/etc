http://web.mit.edu/answers/xwindows/xwindows_xmodmap.html

If you want to list the current keymap table, you can just type:
    xmodmap -pk | more
or
    xmodmap -pke | more

To auto-load the modmap, create a symlink from ~/.Xmodmap to the appropriate
configuration file here.  [That works on Amazon RHEL3 desktop 2008-04-22]


Kinesis PC over PS/2 Default Map
================================

keycap	 keycode     keysym

Ctr/Cmd  37          0xffe3 (Control_L)
Alt/Opt  64          0xffe9 (Alt_L)  0xffe7 (Meta_L)
Alt/Opt  113         0xffea (Alt_R)  0xffe8 (Meta_R)
Ctr/Cmd  109         0xffe4 (Control_R)

??       115         0xffeb (Super_L)
??       116         0xffec (Super_R)
??       117         0xff67 (Menu)


[jonker]$ xmodmap -pm
xmodmap:  up to 2 keys per modifier, (keycodes in parentheses):
 
shift       Shift_L (0x32),  Shift_R (0x3e)
lock        Caps_Lock (0x42)
control     Control_L (0x25),  Control_R (0x6d)
mod1        Alt_L (0x40),  Alt_R (0x71)
mod2        Num_Lock (0x4d)
mod3
mod4        Super_L (0x73),  Super_R (0x74)
mod5
