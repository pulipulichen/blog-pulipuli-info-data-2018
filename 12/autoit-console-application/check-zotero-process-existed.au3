Local $aProcessList = ProcessList("zotero.exe") ; 取得名為「zotero.exe」的程序列表

If $aProcessList[0][0] > 0 Then ; 如果有符合該名字的程序
   ConsoleWrite("true") ; 命令列顯示true
Else	; 否則
   ConsoleWrite("false") ; 命令列顯示 false
EndIf	; 結束偵測
