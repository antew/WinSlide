; WinSlide 1.0 by Henry Pate - www.antew.com

;   DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
;   Version 2, December 2004 
;
; Copyright (C) 2011 Henry Pate
;
; Everyone is permitted to copy and distribute verbatim or modified 
; copies of this license document, and changing it is allowed as long 
; as the name is changed. 
;
;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
;
;  0. You just DO WHAT THE FUCK YOU WANT TO. 


; Set up names for quadrants so the code isn't peppered with magic numbers
; Add quadrants together to take up larger areas. e.g. "5" refers to the left half of the screen.
; 1 | 2
; -----
; 4 | 8
TOP_LEFT     = 1
BOTTOM_LEFT  = 4
LEFT         = 5

TOP_RIGHT    = 2
BOTTOM_RIGHT = 8
RIGHT        = 10

TOP          = 3
BOTTOM       = 12
WHOLE_SCREEN = 15

WS_SIZEBOX = 0x40000

#InstallKeybdHook
; Create hotkeys for Numpad 1-9
Loop, 9
    Hotkey, #Numpad%A_Index%, HotkeyHandler

; Create a tray menu, remove standard menu items
;Menu, Tray, NoStandard
Menu, Tray, Add, Help, Help
Menu, Tray, Add, Suspend Hotkeys, SuspendHotkeys
Menu, Tray, Add
Menu, Tray, Add, Exit, Quit

return ; End Auto-execute section

Control & R::Reload

#F1::GoSub, Help

RestorePrevWindowPos(id, state)
{
    traytip, %id% %state%
    if (state = -1)
        WinMinimize, ahk_id %id%
    else if (state = 1)
        WinMaximize, ahk_id %id%
}

Help:
    IfWinExist, WinSlide Help
    {
        WinActivate, WinSlide Help
        return
    }

    Gui, 2:Default
    Gui, Add, Listview, w559 r17 +Grid +Owner vHotkeyListview, Hotkey|Description

    LV_Add("", "Win + F1"        , "This help dialog.")
    LV_Add("", "Win + Left"      , "Make active window take up the left half of the screen. Move to next monitor if already there.")
    LV_Add("", "Win + Right"     , "Make active window take up the right half of the screen. Move to next monitor if already there.")
    LV_Add("", "Win + Up"        , "Maximize active window.")
    LV_Add("", "Win + Down"      , "Restore active window.")
    LV_Add("", "Win + Numpad 1"  , "Make active window take up the bottom left quarter of the screen.")
    LV_Add("", "Win + Numpad 2"  , "Make active window take up the bottom half of the screeen.")
    LV_Add("", "Win + Numpad 3"  , "Make active window take up the bottom right quarter of the screen.")
    LV_Add("", "Win + Numpad 4"  , "Make active window take up the left half of the screen.")+
    LV_Add("", "Win + Numpad 5"  , "Toggle whether the active window is maximized.")
    LV_Add("", "Win + Numpad 6"  , "Make active window take up the right half of the screen.")
    LV_Add("", "Win + Numpad 7"  , "Make active window take up the upper left quarter of the screen.")
    LV_Add("", "Win + Numpad 8"  , "Make active window take up the top half of the screen.")
    LV_Add("", "Win + Numpad 9"  , "Make active window take up the top right quarter of the screen.")
    LV_Add("", "Win + Numpad Del", "Move active window to next screen.")
    LV_Add("", "Win + Numpad -"  , "Restore active window if maximized, otherwise minimize active window.")
    LV_Add("", "Win + Home"      , "Minimize all windows except the active window.")

    LV_ModifyCol()  ; Autosize headers

    Gui, 2:Show, AutoSize Center, WinSlide Help - Press Esc to Close
    Gui, 1:Default
return

2GuiEscape:
2GuiClose:
    Gui, 2:Destroy
return

Quit:
    ExitApp
return

; Toggle whether hotkeys are suspended, update tray menu with current state
SuspendHotkeys:
    Suspend, Toggle

    If A_IsSuspended
        Menu, Tray, % A_IsSuspended = 1 ? "Check" : "Uncheck", Suspend Hotkeys
return

; Handles the Numpad 1-9 hotkeys
HotkeyHandler:
    GetMonitorInformation()
    winInfo := GetActiveWindowInfo()
    ; Translate the numpad numbers 1-9 into the correct quadrants.
    ; Think of the layout of the numpad as the layout of windows on the monitor.
    ; We are translating from this representation on the NumPad
    ;
    ; 7 | 8 | 9
    ; ---------                  ; 1 | 2
    ; 4 | 5 | 6   to this form   ; -----
    ; ---------                  ; 4 | 8
    ; 1 | 2 | 3
    ;
    ; The corners of the diagram on the left translate to the quadrants on the right.
    ; 7 on the left = 1 on the right
    ; 9 on the left = 2 on the right
    ; 1 on the left = 4 on the right
    ; 3 on the left = 8 on the right
    ;
    ; The numbers in the middle on the left (2,4,6,8) represent half of the screen.
    ; 2 = bottom half of the screen
    ; 4 = left half of the screen
    ; 6 = right half of the screen
    ; 8 = top half of the screen
    ;
    ; Five (5) is a special case, it will maximize the active window
    keyNumber := SubStr(A_ThisHotkey, 8) ; Get which numpad key was pressed
    newQuadrant := (keyNumber = 1) ? BOTTOM_LEFT
                :  (keyNumber = 2) ? BOTTOM
                :  (keyNumber = 3) ? BOTTOM_RIGHT
                :  (keyNumber = 4) ? LEFT
                :  (keyNumber = 5) ? WHOLE_SCREEN
                :  (keyNumber = 6) ? RIGHT
                :  (keyNumber = 7) ? TOP_LEFT
                :  (keyNumber = 8) ? TOP
                :  (keyNumber = 9) ? TOP_RIGHT
                :   WHOLE_SCREEN

    currQuadrant := GetCurrentQuadrant(winInfo)
    If (newQuadrant = WHOLE_SCREEN) ; Special case, maximize window
    {
        If (winInfo.windowState)
            WinRestore, A
        else
            WinMaximize, A
    }
    else If (newQuadrant = currQuadrant)
    { ; If we're already where we are supposed to be then move to the next screen
        GetNextMonitor(winInfo, newQuadrant)
        MoveActiveWindowToQuadrant(winInfo, GetOppositeQuadrant(currQuadrant))
    }
    else
    {
        MoveActiveWindowToQuadrant(winInfo, newQuadrant)
    }

return

; Windows key + Home.
; Minimizes all windows but the active window
#Home::
    activeID := WinExist("A") ; Get HWND of active window
    WS_EX_APPWINDOW = 0x40000
    WS_EX_TOOLWINDOW = 0x80
    GW_OWNER = 4

    WinGet, id, list,,, Program Manager
    Loop, %id%
    { ; Loop through all windows

        this_id := id%A_Index%
        WinGet, style, ExStyle, ahk_id %this_id%

        ; GetWindow()   - returns HWND of parent window if it is a child window, otherwise returns null.
        ; Documentation - http://msdn.microsoft.com/en-us/library/ms633515(v=vs.85).aspx
        ; Doesn't seem to work correctly with HP Quality Center...
        if  ((!DllCall("GetWindow", "uint", this_id, "uint", GW_OWNER) and !(style & WS_EX_TOOLWINDOW))
        or  (style & WS_EX_APPWINDOW))
        and  activeID != this_id
            WinMinimize, ahk_id %this_id%
    }

return

; Windows key + Numpad Minus key.
; Restores the active window if maximized.
; Otherwise, it minimizes the active window.
#NumpadSub::
    WinMinimize, A
return

; Moves window to the next monitor preserving relative size and position.
#NumpadDot::
    GetMonitorInformation()
    winInfo := GetActiveWindowInfo()
    widthFactor = 1
    heightFactor = 1
    
    If (Monitors.Count = 1)
        return

    WinGet, maximized, MinMax, A
    WinGet, style, Style, A
    
    GetNextMonitor(winInfo, LEFT) ; Get next monitor moving left
    src  := "m" . winInfo.Monitor ; Make a shorter variable name...is that laziness?
    dest := "m" . winInfo.newMonitor
    
    if (style & WS_SIZEBOX)
    {
        widthFactor  := (Monitors[dest].Width  / Monitors[src].Width)
        heightFactor := (Monitors[dest].Height / Monitors[src].Height)
    }
    
    If (maximized)
        WinRestore, A

    ; Move the window keeping the relative size and position
    WinMove, A,, Monitors[dest].Left + (winInfo.X - Monitors[src].Left) * widthFactor  ; X
               , Monitors[dest].Top  + (winInfo.Y - Monitors[src].Top)  * heightFactor ; Y
               , winInfo.W * widthFactor                                               ; W
               , winInfo.H * heightFactor                                              ; H

    If (maximized)
        WinMaximize, A

return

; Gets the quadrant on the opposite side of the screen
; This is used when the window is already in the correct place and we need to
; wrap around to the next monitor.
GetOppositeQuadrant(quadrant)
{
    global
    ; 1 | 2
    ; -----
    ; 4 | 8
    return (quadrant = LEFT)         ? RIGHT
        :  (quadrant = RIGHT)        ? LEFT
        :  (quadrant = BOTTOM_LEFT)  ? BOTTOM_RIGHT
        :  (quadrant = BOTTOM_RIGHT) ? BOTTOM_LEFT
        :  (quadrant = TOP_LEFT)     ? TOP_RIGHT
        :  (quadrant = TOP_RIGHT)    ? TOP_LEFT
        :   WHOLE_SCREEN

}

; Windows key + Up
; If the window is minimized, restore it.
; Otherwise, maximize it.
#Up::
    window := GetActiveWindowInfo()

    If (window.windowState = -1) ; Minimized
        WinRestore, A
    else
        WinMaximize, A
return

; Windows key + Down
; If window is maximized, restore it.
#Down::
    window := GetActiveWindowInfo()

    If (window.windowState = 1) ; Maximized
        WinRestore, A

return


; Windows key + Left/Right
; Make window take up the left/right half of the screen.
; If the window is already there, move it to the next monitor.
; If the Up or Down keys are also pressed we move the make the window
; only take up the top half or bottom half of the screen, respectively.
#Left::
#Right::
    GetMonitorInformation()
    winInfo := GetActiveWindowInfo()

    ; If up or down is pressed we are going to make the window
    ; half the height of the screen rather than the full height
    base := A_ThisLabel = "#Right" ? "RIGHT" : "LEFT"
    modifier := GetKeyState("Up", "P") ? "TOP_" : GetKeyState("Down", "P") ? "BOTTOM_" : ""
    ; Use a quirk of how the quadrant variables are named to resolve the correct quadrant (BOTTOM/TOP)_(LEFT/RIGHT)
    newQuadrant := %modifier%%base%

    ; Moving maximized windows causes strange behavior so restore it first if maximized
    If (winInfo.windowState = 1)
        WinRestore, A

    currQuadrant := GetCurrentQuadrant(winInfo)

    If (currQuadrant = newQuadrant) ; Already at the correct spot, move to next monitor
    {
        GetNextMonitor(winInfo, currQuadrant)
        MoveActiveWindowToQuadrant(winInfo, GetOppositeQuadrant(currQuadrant))
    }
    else
    {
        MoveActiveWindowToQuadrant(winInfo, newQuadrant)
    }

return

; Returns an object containing information on the active window
; Specifically:
;   - X coordinate
;   - Y coordinate
;   - Window width
;   - Window height
;   - Window state (-1 = minimize, 0 = neither minimized nor maximized, 1 = maximized)
;   - Monitor (the monitor number the center of the window lies in)
GetActiveWindowInfo()
{
    WinGetPos, X, Y, W, H, A
    WinGet, windowState, MinMax, A
    Monitor := GetMonitorAt(X + W/2, Y + H/2)
    return Object("X", X
                 ,"Y", Y
                 ,"W", W
                 ,"H", H
                 ,"Monitor", Monitor
                 ,"windowState", windowState
                 ,"newMonitor", Monitor)
}

; Code credit goes to Lexikos of the AutoHotkey forums
; See http://www.autohotkey.com/forum/topic21703.html
; Find what monitor a coordinate (X, Y) lies on.
GetMonitorAt(x, y, default = 1)
{
    global Monitors

    ; Iterate through all monitors.
    Loop, % Monitors.count
    {   ; Check if the window is on this monitor.
        m := "m" A_Index
        if   (x >= Monitors[m].Left
           && x <= Monitors[m].Right
           && y >= Monitors[m].Top
           && y <= Monitors[m].Bottom)

            return A_Index
    }

    return default
}


; Determines if the window occupies the specified quadrant.
; To specify a combination add the quadrants together
; e.g. Pass in 5 for the quadrant parameter to determine if a window is
; occupying the left half of the screen
; 1 | 2
; -----
; 4 | 8
WindowByQuadrant(winInfo, quadrant)
{
    newCoords := GetQuadrantCoordinates(winInfo.newMonitor, quadrant)

    return winInfo.X = newCoords.X
        && winInfo.Y = newCoords.Y
        && winInfo.H = newCoords.H
        && winInfo.W = newCoords.W
}


; Moves window to the specified quadrant.
; To specify a combination add the quadrants together
; e.g. Pass quadrant = 5 to make a window occupy the left half of the screen
; 1 | 2
; -----
; 4 | 8
MoveActiveWindowToQuadrant(winInfo, quadrant)
{
    newCords := GetQuadrantCoordinates(winInfo.newMonitor, quadrant)

    If (winInfo.windowState = 1) ; Maxmized
        WinRestore, A

    WinMove, A,, newCords.X, newCords.Y, newCords.W, newCords.H
}

; Returns an object with the coordinates of the quadrant on the specified monitor
GetQuadrantCoordinates(monitor, quadrant)
{
    global
    m := "m" . monitor

    X := Monitors[m].Left
    Y := Monitors[m].Top
    W := Monitors[m].Width
    H := Monitors[m].Height

    ; Set X coord to mid-screen if we're moving to the right half of the screen
    ; WATCH OUT FOR SPACES BETWEEN THE NUMBERS IN THESE LISTS...AUTOHOTKEY
    ; WILL QUIT READING THE COMMA SEPARATED LIST WHEN IT FINDS A SPACE!!
    If quadrant in %TOP_RIGHT%,%BOTTOM_RIGHT%,%RIGHT%
        X += Monitors[m].Width // 2

    ; Set Y coord to mid-screen if we're moving to the lower half of the screen
    If quadrant in %BOTTOM_LEFT%,%BOTTOM_RIGHT%,%BOTTOM%
        Y += Monitors[m].Height // 2

    ; If we're moving to a quarter of the screen reduce the height by half
    If quadrant in %TOP_LEFT%,%TOP_RIGHT%,%TOP%,%BOTTOM_LEFT%,%BOTTOM_RIGHT%,%BOTTOM%
        H := H // 2

    ; Set width to half the screen if necessary
    If quadrant in %TOP_LEFT%,%TOP_RIGHT%,%BOTTOM_LEFT%,%LEFT%,%BOTTOM_RIGHT%,%RIGHT%
        W := W // 2

    return Object("X", X
                 ,"Y", Y
                 ,"H", H
                 ,"W", W)
}

; Returns the quadrant a window occupies.  If the window does not
; match a quadrant's coordinates we return zero (0).
GetCurrentQuadrant(winInfo)
{
    quadrants = 1,2,3,4,5,8,10,12
    StringSplit, s, quadrants, `,

    Loop, %s0%
    {
        quadrant := s%A_Index%
        If (WindowByQuadrant(winInfo, quadrant))
            return quadrant
    }

    return 0
}

; Returns the next monitor in a direction. Used for moving a window to the next screen.
; If we're moving left (quadrants 1, 4, 5) and we are at the last monitor then we wrap around to the rightmost monitor.
; If we're moving right (quadrants 2, 8, 10) and we are at the last monitor then we wrap around to the leftmost monitor.
GetNextMonitor(winInfo, quadrant)
{
    global

    If quadrant in %TOP_LEFT%,%BOTTOM_LEFT%,%LEFT%
        winInfo.newMonitor -= 1
    else if quadrant in %TOP_RIGHT%,%BOTTOM_RIGHT%,%RIGHT%
        winInfo.newMonitor += 1

   winInfo.newMonitor :=  (winInfo.newMonitor < 1)              ? Monitors.Count
                        : (winInfo.newMonitor > Monitors.Count) ? 1
                        :  winInfo.newMonitor

}
; Creates the Monitors object which conatins information about the monitor(s).
; VARIABLES
;   Count - The number of monitors
;   Primary - The primary monitor
; OBJECTS
;   m1 - Object containing information on the 1st monitor
;   m2 - Object containing information on the 2nd monitor
;   mN - Object containing information on the Nth monitor
;       mN VARIABLES
;           Left   - Left bounds of the work area
;           Right  - Right bounds of the work area
;           Top    - Top bounds of the work area
;           Bottom - Bottom bounds of the work area
;           Width  - Width of the work area
;           Height - Height of the work area
;       NOTE: All variables use the Monitor Work Area which excludes the taskbar, toolbars, etc.
GetMonitorInformation()
{
    global Monitors := Object()

    SysGet, monitorCount, MonitorCount
    SysGet, primaryMonitor, MonitorPrimary

    Monitors.Insert("Count", monitorCount)
    Monitors.Insert("Primary", primaryMonitor)

    ; Get work area for each monitor (excludes toolbars, taskbar, etc.)
    Loop, % Monitors.Count
    {
        SysGet, m%A_Index%, MonitorWorkArea, %A_Index%
        m_%A_Index% := Object("Left"  , m%A_Index%Left
                             ,"Right" , m%A_Index%Right
                             ,"Top"   , m%A_Index%Top
                             ,"Bottom", m%A_Index%Bottom
                             ,"Width" , m%A_Index%Right  - m%A_Index%Left
                             ,"Height", m%A_Index%Bottom - m%A_Index%Top)

        Monitors.Insert("m" . A_Index, m_%A_Index%)
    }
}