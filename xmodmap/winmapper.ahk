; ------------------------------------------------------
; Use the muhenkan key as Fn for HHKB and define shortcuts

; Fn + Numbers -> F1-F12
SC07B & SC002::send,{Blind}{F1}
SC07B & SC003::send,{Blind}{F2}
SC07B & SC004::send,{Blind}{F3}
SC07B & SC005::send,{Blind}{F4}
SC07B & SC006::send,{Blind}{F5}
SC07B & SC007::send,{Blind}{F6}
SC07B & SC008::send,{Blind}{F7}
SC07B & SC009::send,{Blind}{F8}
SC07B & SC00A::send,{Blind}{F9}
SC07B & SC00B::send,{Blind}{F10}
SC07B & SC00C::send,{Blind}{F11}
SC07B & SC00D::send,{Blind}{F12}

; HHKB Pro 2 arrow keys
SC07B & SC01A::send,{Blind}{Up}
SC07B & SC027::send,{Blind}{Left}
SC07B & SC028::send,{Blind}{Right}
SC07B & SC035::send,{Blind}{Down}
SC07B & SC026::send,{Blind}{PgUp}
SC07B & SC034::send,{Blind}{PgDn}

SC136::Return ; disable original Right Shift
; Right Shift (remapped to Num Lock with windows key remapper) -> Fn
SC145 & SC01A::send,{Blind}{Up}
SC145 & SC027::send,{Blind}{Left}
SC145 & SC028::send,{Blind}{Right}
SC145 & SC035::send,{Blind}{Down}
SC145 & SC026::send,{Blind}{PgUp}
SC145 & SC034::send,{Blind}{PgDn}

; WASD arrow keys
; SC07B & SC011::send,{Blind}{Up}
; SC07B & SC01E::send,{Blind}{Left}
; SC07B & SC020::send,{Blind}{Right}
; SC07B & SC01F::send,{Blind}{Down}

; Fn + s -> Backspace, Fn + d -> Backspace
SC07B & SC01F::send,{BS}
SC07B & SC020::send,{BS}

; Fn + p -> PrintScreen
SC07B & SC019::send,{PrintScreen}

; Fn + i -> Shift + Insert ; TODO make it rather Fn + Shift + i
SC07B & SC017::send,{ShiftDown}{Insert}{ShiftUp}

SC07B::send,{Enter} ; Sends Enter if Fn was used and not combined with anything else

; Sends Enter only when double pressing the Fn key
; SC07B::
;     If (A_TimeSincePriorHotkey < 300 and A_TimeSincePriorHotkey !=-1)
;         send,{Enter}
; Return

; SC038 UP::Return ; Do nothing if Alt was used and not combined with anything else

; Activate "\" and "|" (at the right of top row "=")
SC07D::Send {\}
+SC07D::Send {|}

; Activate "`" and "~" (at the right of top row "\")
SC00E::`
+SC00E::~

; KANA -> Backspace
SC070::send,{BS} ; "send" is needed in the case of SC070
+SC070::send,{BS}
^SC070::send,^{BS}

; henkan -> Backspace
SC079::send,{BS} ; "send" is needed in the case of SC079
+SC079::send,{BS}
^SC079::send,^{BS}

; Fn + ~ -> Delete
SC07B & SC00E::send,{Delete}

; Fn + Shift + \ -> Insert
; SC02A & SC07B & SC02B::send,{Insert} ; doesn't work

; extend Enter key bottom to the left (like on the HHKB)
SC02B::Enter

; E/J -> Escape
SC029::send,{Blind}{Escape}

; Right Alt (remapped to RWin) + ; -> RWin + l
#SC027::#l

; ----------------------------------
; Disable keys not available on HHKB

SC148::Return ; Up
SC14B::Return ; Left
SC14D::Return ; Right
SC150::Return ; Down

SC03B::Return ; F1
SC03C::Return ; F2
SC03D::Return ; F3
SC03E::Return ; F4
SC03F::Return ; F5
SC040::Return ; F6
SC041::Return ; F7
SC042::Return ; F8
SC043::Return ; F9
SC044::Return ; F10

SC152::Return ; Insert
SC147::Return ; Home
SC149::Return ; Page Up
SC153::Return ; Delete
SC14F::Return ; End
SC151::Return ; Page Down

SC137::Return ; Print Screen
SC046::Return ; Scroll Lock
SC045::Return ; Pause

; SC01D::Return ; Left Control (commented out because it seems to mess up the rest of the config)
; SC11D::Return ; Right Control (commented out because it seems to mess up the rest of the config)
SC138::Return ; Right Alt

SC01C::Return ; Enter

SC07B & SC018::Return ; Disable Fn + o
; SC07B & SC019::Return ; Disable Fn + p
SC07B & SC01B::Return ; disable Fn + ]
SC07B & SC02B::Return ; disable Fn + SC02B (the key used to simulate HHKB Prod 2 long Enter key)
