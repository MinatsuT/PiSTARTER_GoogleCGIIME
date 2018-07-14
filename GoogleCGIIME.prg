'
' Google CGI IME
'     by みなつ
'
' Powered by Google CGI API for Japanese Input
' http://www.google.co.jp/ime/cgiapi.html
'
option strict

var ver$="1.07"
var lf$=chr$(&h0A)
var q$=chr$(&h22)
var esc$=chr$(&h1b)
var ip$
var R$=chr$(&h1c)
var L$=chr$(&h1d)
var U$=chr$(&h1e)
var D$=chr$(&h1f)
var BS$=chr$(&h08)
var enter$=chr$(&h0d)

var cx,cy,cw,ch
var x,y
var k$
var edit$,editCsr,editLen
var yomi$,yomiCsr,yomiLen
var knji$,knjiCsr,knjiLen
var json$,knjiCand,knjiBunsetsu,knjiFirst$,knjiRest$
dim knjiAllCand$[0]
var lastLen
var blinkCounter
var lastBlink
var blinkX,blinkY
var kouho$[0,0]

'state
var idle=0
var edit=1
var yomi=2
var henkan=3
var state=idle

'result
var kanji_result
var result_finish

cls
?"Google CGI IME "+ver$+" by みなつ
?"
?"スペースキー:変換/次候補
?"エンター:確定
?"ESC:キャンセル
?"カーソルキーの←→:文節の長さを変更(変換中)
?"カーソルキーの:前候補/次候補(変換中)
?"
?"・ローマ字変換機能が付いていないので,全角/半角キーや変換キーを押してローマ字入力に切り替えて下さい.
?"・変換後の文字列はクリップボードにコピーされるので,Ctrl+Vでプログラムに貼り付けることができます.
?

repeat
 var clip$=kanji_input$()
 vsync
until state==idle
if clip$!="" then
 clipbOARD clip$
 ?"クリップボードにコピーしました.
endif
end

def kanji_init
 state=idle
 x=csrx
 y=csry
 editCsr=0
 yomiCsr=0
 knjiCsr=0
 edit$=""
 yomi$=""
 knji$=""
 json$=""
 knjiCand=0
 knjiBunsetsu=0
 knjiFirst$=""
 knjiRest$=""
 lastLen=0
 blinkCounter=maincnt
 lastBlink=0
 console out cx,cy,cw,ch
end

'MAIN routine
'==================================================
def kanji_input$()
 if state==idle then kanji_init:state=edit

 k$=inkey$()
 if k$=="" then kanji_csrBlink:return ""
 blinkCounter=maincnt

 if state==edit then kanji_edit
 if state==yomi then kanji_yomi
 if state==henkan then kanji_henkan

 kanji_csrBlinkClr

 locate x,y:?" "*lastLen;
 lastLen=editLen

 locate x,y
 if editCsr>0 then color #white,0:?left$(edit$,editCsr);
 if state==yomi then color #cyan,0:?yomi$;:inc lastLen,yomiLen
 if state==henkan then color 0,#yellow:?knjiFirst$;:color #yellow,0:?knjiRest$;:inc lastLen,knjiLen
 color #white,0:?right$(edit$,editLen-editCsr);

 kanji_csrBlink

 var ret$=""
 if state==idle then
  kanji_csrBlinkClr
  locate x,y:?edit$
  ret$=edit$
 endif

 return ret$
end


'STATE:edit
'--------------------------------------------------
def kanji_edit
 if isSpecial(k$) then
  if k$==R$ && editCsr<editLen then inc editCsr
  if k$==L$ && editCsr>0 then dec editCsr
  if k$==BS$ && editCsr>0 then dec editCsr:edit$[editCsr]=""
  if k$==" " then insStr edit$,editCsr,k$:inc editCsr
  if k$==esc$ then edit$="":editCsr=0:state=idle
  if k$==enter$ then state=idle
 else
  state=yomi
 endif
 editLen=len(edit$)
end

'STATE:yomi
'--------------------------------------------------
def kanji_yomi
 if isSpecial(k$) then
  if k$==BS$ && yomiCsr>0 then dec yomiCsr:yomi$[yomiCsr]=""
  if k$==" " && yomi$!="" then state=henkan
  if k$==enter$ then
   insStr edit$,editCsr,yomi$:editLen=len(edit$)
   inc editCsr,yomiLen
   yomi$="":yomiCsr=0
   state=edit
  endif
  if k$==esc$ then yomi$="":yomiCsr=0:state=edit
 else
  insStr yomi$,yomiCsr,k$:inc yomiCsr
 endif
 k$=""
 yomiLen=len(yomi$)
 if yomiLen==0 then state=edit
end

'STATE:henkan
'--------------------------------------------------
def kanji_henkan
 if k$==BS$ || k$==esc$ || len(yomi$)==0 then state=yomi:return

 if k$=="" then
  'Init henkan
  yomi$=replaceStr$(yomi$,","," ")
  yomi$=replaceStr$(yomi$,"-","ー")
  knjiGetCandidates yomi$
  return
 endif

 if !isSpecial(k$) then
  'all kakutei
  insStr edit$,editCsr,knji$:editLen=len(edit$)
  inc editCsr,len(knji$)
  yomi$=k$:yomiLen=1:yomiCsr=1
  inc knjiBunsetsu
  state=yomi
  return
 endif

 if k$==" " || k$==D$ then
  'next candidate
  knjiNextCand
 endif

 if k$==U$ then
  'previous candidate
  knjiPrevCand
 endif

 if k$==enter$ then
  'bunsetsu kakutei
  insStr edit$,editCsr,knjiFirst$:editLen=len(edit$)
  inc editCsr,len(knjiFirst$)
  var removeLen=len(knjiGetYomi$(knjiBunsetsu))
  yomi$=right$(yomi$,yomiLen-removeLen)
  dec yomiLen,removeLen:dec yomiCsr,removeLen
  if yomi$=="" then state=edit:return
  inc knjiBunsetsu
  knjiRest
  knjiCand=-1
  knjiNextCand
 endif

 if k$==L$ || k$==R$ then
  var l=len(knjiGetYomi$(knjiBunsetsu))
  inc l,(k$==R$)-(k$==L$)
  if l>=1 && l<=yomiLen then
   var y$=replaceStr$(yomi$,","," ")
   insStr y$,l,","
   knjiGetCandidates y$
  endif
 endif
end

'漢字変換用の変数を初期化する
def knjiRest
 knjiRest$=""
 var n=knjiBunsetsu+1
 while 1
  var cand$=knjiGetCand$(n,0)
  if cand$=="" then break
  inc knjiRest$,cand$
  inc n
 wend
end

'変換候補を取得する
def knjiGetCandidates yomi$
  knjiBunsetsu=0
  if ip$=="" then ip$=nslookup$("www.google.com")
  var url$="http://"+ip$+"/transliterate?langpair=ja-Hira|ja&text="+urlenCODE$(yomi$)
  var res$=httpgET$(url$)
  json$=json_decode$(replaceStr$(res$," ",","))
  if json$=="" then
   'エラーがあったので,wgetでリトライ
   var tmpFile$="/TEMP/.GoogleCGIIME.tmp"
   var outFile$="/boot/SMILEBOOM/SMILEBASIC-R/workspace"+tmpFile$
   var tmp$=system$(format$("sudo /usr/bin/wget -O %s %s%s%s",outFile$,q$,url$,q$))
   if chkfile(tmpFile$)!=1 then
    beep 2
    ?
    color #red:?"漢字変換サーバとの通信に失敗しました."
    color #white:?"何かキーを押してください."
    repeat:vsync:until inkey$()!=""
    stop
   endif
   
   dim d%[0]:load "raw:"+tmpFile$,d%:delete tmpFile$
   res$=utf8To16(d%)
   json$=json_decode$(replaceStr$(res$," ",","))
  endif
  knjiRest
  knjiCand=-1
  knjiNextCand
end

'次の候補に変更
def knjiNextCand
 inc knjiCand
 knjiFirst$=knjiGetCand$(knjiBunsetsu,knjiCand)
 if knjiFirst$=="" then
  'return to the first candidate
  knjiCand=0
  knjiFirst$=knjiGetCand$(knjiBunsetsu,knjiCand)
 endif
 knjiCsr=len(knjiFirst$)
 knji$=knjiFirst$+knjiRest$
 knjiLen=len(knji$)
end

'前の候補に変更
def knjiPrevCand
 var curCand=knjiCand
 var n
 while 1
  if knjiGetCand$(knjiBunsetsu,n)=="" then break
  inc n
 wend
 knjiCand=(knjiCand+n*2-2) mod n
 knjiFirst$=knjiGetCand$(knjiBunsetsu,knjiCand)
 knjiCsr=len(knjiFirst$)
 knji$=knjiFirst$+knjiRest$
 knjiLen=len(knji$)
end

'jsonから読みを抽出
def knjiGetYomi$(knjiBunsetsu)
 return json_get$(json$,chr$(knjiBunsetsu)+chr$(0))
end

'jsonから変換候補を抽出
def knjiGetCand$(knjiBunsetsu,knjiCand)
 return json_get$(json$,chr$(knjiBunsetsu)+chr$(1)+chr$(knjiCand))
end

'UTILITIES
'==================================================
'Cursor blinking
'--------------------------------------------------
def kanji_csrBlink
 var blink=!(((maincnt-blinkCounter) div 20) mod 2)
 if state==henkan then kanji_csrBlinkClr:return

 blinkX=x+editCsr+yomiCsr
 blinkY=y
 if blink then
  if yomiCsr>0 then color 0,#cyan else color 0,#white
 endif
 var x=blinkX mod cw
 var y=blinkY + (blinkX div cw)
 locate x,y:?chr$(chkchR(csrx,csry));
 locate x,y
 color #white,0

 lastBlink=Blink
end

def kanji_csrBlinkClr
 if !lastBlink then return
 color #white,0
 var x=blinkX mod cw
 var y=blinkY + (blinkX div cw)
 locate x,y:?chr$(chkchR(csrx,csry));
 locate x,y
 lastBlink=0
end

'String manipulations
'--------------------------------------------------
def insStr s$,pos,w$
 if pos==len(s$) then inc s$,w$ else s$[pos]=w$+s$[pos]
end

def replaceStr$(in$,from$,to$)
 var ret$=in$+""
 var pos=0
 while 1
  pos=instr(pos,ret$,from$)
  if pos==-1 then break
  ret$[pos]=to$
  inc pos
 wend
 return ret$
end

'Special keys
'--------------------------------------------------
def isSpecial(k$)
 return isArrow(k$) || k$==BS$ || k$==enter$ || k$==esc$ || k$==" "
end

def isArrow(k$)
 var c=asc(k$)
 return c>=&h1c && c<=&h1f
end

'Debug print
'--------------------------------------------------
def DP pos,s$
 var cw: console out ,,cw,
 var x=csrx,y=csry
 var dx=80/2,dy=45/2+pos
 color #RED
 locate dx,dy:?" "*(cw-dx);
 locate dx,dy:?s$;
 locate x,y
 color #white
end


'JSON Decoder
'==================================================
def json_get$(j$,key$)
 if j$=="" then return ""
 var keyStr$=json_key$(key$)
 var idx=instr(j$,keyStr$)
 if idx==-1 then return ""
 inc idx,len(keyStr$)
 var l=asc(j$[idx])
 return mid$(j$,idx+1,l)
end

def json_key$(key$)
 return lf$+key$+":"
end

def json_decode$(j$)
 var elem$,pos,err
 json_decodeList j$,0,"" out elem$,pos,err
 if err then return ""
 
 return elem$
end

def json_decodeList j$,pos,key$ out outElem$,outPos,outErr
 outElem$="":outPos=pos:outErr=0:
 var err$

 if j$[pos]!="[" then
  outErr=-1:return
  inc err$,"JSON decode error: '[' not found."+lf$
  inc err$,"Pre string:"+lf$+left$(j$,pos)+lf$
  inc err$,"Current string:"+lf$+right$(j$,len(j$)-pos)
  json_error err$
 endif

 var elem$,nextPos
 var n,c$
 while 1
  inc pos
  var myKey$=key$+chr$(n)

  c$=j$[pos]
  if c$=="[" then
   'list
   json_decodeList j$,pos,myKey$ out elem$,nextPos,outErr:if outErr then return
   'add the elemnt as it is
   inc outElem$,elem$
  elseif c$==q$ then
   'String
   json_decodeString j$,pos out elem$,nextPos,outErr:if outErr then return
   'insert a marker in front of the element
   inc outElem$,lf$+myKey$+":"+chr$(len(elem$))+elem$
  else
   outErr=-1:return
  endif

  pos=nextPos
  c$=j$[pos]
  if c$=="]" then
   break
  elseif c$=="," then
  else
   outErr=-1:return
  endif

  inc n
 wend

 outPos=pos+1
end

def json_decodeString j$,pos out outElem$,outPos,outErr
 outElem$="":outPos=pos:outErr=0:
 var err$

 if j$[pos]!=q$ then
  outErr=-1:return
 endif

 var elem$,nextPos
 var n,c$
 inc pos
 var st=pos
 while 1
  var idx=instR(pos,j$,q$)
  if idx==-1 then
   outErr=-1:return
  endif
  pos=idx
  if j$[pos-1]!="\" then break
  inc pos
 wend

 outElem$=mid$(j$,st,pos-st)
 outPos=pos+1
end

def json_error m$
 color #red
 ?m$
 color #white
 stop
end

'UTF8 to UTF16 Converter
'==================================================
def utf8To16(d%)
 dim ubuf[0]
 var utf16$
 
 var i,l=len(d%)
 for i=0 to l-1
  push ubuf,d%[i]
  var u0=ubuf[0],ulen=len(ubuf)
  if (u0 and &h80)==0 then
   utf8putchr utf16$,ubuf,&h7f
  elseif ulen==2 && (u0 and &he0)==&hc0 then utf8putchr utf16$,ubuf,&h1f
  elseif ulen==3 && (u0 and &hf0)==&he0 then utf8putchr utf16$,ubuf,&h0f
  elseif ulen==4 && (u0 and &hf8)==&hf0 then utf8putchr utf16$,ubuf,&h07
  elseif ulen==5 && (u0 and &hfa)==&hf8 then utf8putchr utf16$,ubuf,&h03
  elseif ulen==6 && (u0 and &hfe)==&hfc then utf8putchr utf16$,ubuf,&h01
  elseif ulen==7 then
   beep 2
   ?
   color #red:?"UTF8[";
   var j:for j=0 to ulen-1:?format$("%02X",ubuf[j]);","*(j!=(ulen-1));:next
   ?"]からUTF16への変換に失敗しました."
   color #white:?"何かキーを押してください."
   repeat:vsync:until inkey$()!=""
   stop
  endif
 next

 return utf16$
end

def utf8putchr utf16$,ubuf,mask
 var c=shift(ubuf) and mask
 while len(ubuf)
  c=(c<<6) or (shift(ubuf) and &h3f)
 wend
 if c<=&hffff then
  inc utf16$,chr$(c)
 endif
end

'名前解決
'==================================================
def nslookup$(h$)
 var res$=system$("/usr/bin/host -t A "+h$)
 var lfpos=instr(res$,lf$)
 if lfpos!=-1 then
  res$=left$(res$,lfpos)
  var l=len(h$)
  if left$(res$,l+13)==h$+" has address " then
   return right$(res$,len(res$)-l-13)
  endif
 endif
 return h$
end

