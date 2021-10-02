--                                      _,------------,_
module                             Main exposing(..)-------,___
import                        Browser.Events-------▄▄▄▄--------\____
import                   Html exposing(..)-------------▀▀▀██▄▄▄▄----\__
import                Array exposing(..)---------------------▀▀███▄▄---\__
import             Browser exposing(..)--------------------------▀▀██▄▄---\__
import          Html.Events exposing(..)-----------------------------▀▀█▄----\_
import        Html.Attributes exposing(..)------------------------------▀▀▄----\
d           ct sA st cA cB sB i = (round(40+30*(1/(sin i* (ct+2)*sA+st*cA+5))*(cos
           i*(ct+2)*cB-(sin i*(ct+2)*cA-st*sA)*sB)))+80*(round(12+15* (1/(sin i*(ct+
          2)*sA+st*cA+5))*(cos i*(ct+2)*sB+(sin i*(ct+2)*cA-st*sA)*cB)))------------\
o       (k,{b,z}) = if k<1760 then o (k+1,{b=push(if remainderBy 80 k==79 then"\n"else
       " ")b,z=push 0 z})else(k,{b=b,z=z})---------------------------------------------\
n     (j,jld,re) = if j<6.28 then n (j+0.07,jld,( \{sA,cA,cB,sB}j2 r2->(\(_,_,c)->c)(u (0
     ,{cA=cA,cB=cB,sA=sA,sB=sB,ct=cos j2,st=sin j2},r2)))jld j re)else(j,jld,re)---------\
u   (i,ild,re) = if i<6.28 then u (i+0.02,ild,(\{sA,cA,cB,sB,ct,st}i2{z,b} -> s (get (d ct
    sA st cA cB sB i2) z)(\zv->if(round(              12+15*(1/(sin i2*(ct+2)*sA+st*cA+5))*
   (cos i2* (ct+2)*sB+(sin i2*(ct+2)*                    cA-st*sA)*cB)))<22&&(round(12+15*(1
  /(sin i2*(ct+2)*sA+st*cA+5))*(cos                        i2*(ct+2)*sB+(sin i2*(ct+2)*cA-st
  *sA)*cB)))>=0&&(round(40+30*(1/(                           sin i2*(ct+2)*sA+st*cA+5))*(cos
 i2*(ct+2)*cB-(sin i2*(ct+2)*cA-                              st*sA)*sB)))>=0&&(round(40+30*(
 1/(sin i2* (ct+2)*sA+st*cA+5))*                              (cos i2*(ct+2)*cB-(sin i2*(ct+2
 )*cA-st*sA)*sB)))<79&&(1/ (sin                                i2*(ct+2)*sA+st*cA+5))>zv then
 {z=set(d ct sA st cA cB sB i2)                                (1/(sin i2*(ct+2)*sA+st*cA+5))
 z,b=set(d ct sA st cA cB sB i2                                )((\nn->if nn <= 0 then" "else
 Maybe.withDefault "▓"( get nn (                              fromList(String.split""(" ,-"++
 "~:;!*$▚▓" )))))(round (8*((st*                              sA- sin i2*ct*cA)*cB-sin i2*ct*
  sA-st*cA-cos i2*ct*sB))))b}else                            {z=z,b=b }){ z=z,b=b}) ild i re)
  else(i,ild,re)-------------▀▀██▄▄\                       /-------------------------------/'
t  =element{init=\_->({a=1,b=1,ax=True                   },Cmd.none),view= \m -> pre [ style
   "background" "black", style "line-height" "0.98", style "color"  "#ccc" , style "display"
    "inline-block",onClick"t",style"cursor""pointer"][text((\{a,b}-> String.join""<| toList
     ((.b)<|(\(_,_,c)->c)<|n (0,{cA=cos a,cB=cos b,sA=sin a, sB=sin b}, Tuple.second (o(0,
      {b=fromList[],z=fromList[]}))))){a=m.a,b=m.b}), text" ",a[href("https://lucamug" ++
       ".github.io/elm-donut/"),style"color""#ccc"] [text"Built with Elm"], div[ ] [text
        " "]],update=\msg m->if msg=="t" then ({m|ax=not m.ax},Cmd.none)else ({m|a=m.a+
         0.07,b=m.b+0.03},Cmd.none),subscriptions=\m->if m.ax then-------------------/
           Browser.Events.onAnimationFrameDelta(\_->"")else Sub.none}---------------/
s            m f z = case m of----------------------------------------------------/
                Just v->f v-----------------------------------------------------/
                Nothing->z---------------------------------------------------_/
main              :Program(){a:Float,b:Float,ax:Bool}String---------------__/
main                 =t---▀▀█▄▄▄▄-------------------------------▄▄-----__/
--                      \____--▀▀▀█████▄▄▄▄▄▄------▄▄▄▄▄▄▄▄███▀▀--____/
--                           \____----▀▀▀▀▀██████████▀▀▀▀----____/
--                                \_____ @luca_mug 2021 ____/
--                                      `--------------'
