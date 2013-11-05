function datatype,var, flag0, descriptor=desc, help=hlp, Tname = tname
 ;+
; NAME: 
;      DATATYPE()
;
; PURPOSE: 
;      Returns the data type of a variable.
;
; EXPLANATION: 
;      This routine returns the data type of a variable in a format specified
;      by the optional flag parameter.    Can also be used to emulate, in 
;      earlier versions of IDL, the SIZE(/TNAME) option introduced in V5.1.
;
;      This routine was originally derived from the JHUAPL library ***but has 
;      diverged from the JHUAPL library for the newer data types.***  For this
;      reason DATATYPE is no longer used in any other procedure in the IDL 
;      Astronomy Library.
; CALLING SEQUENCE         : 
;      Result = DATATYPE( VAR  [, FLAG , /TNAME, /DESC ] )
;
; INPUTS: 
;     VAR     = Variable to examine, no restrictions
;
; OPTIONAL INPUT PARAMETERS: 
;     FLAG  = Integer between 0 and 3 giving the output format flag as 
;             explained below.  The default is 0.
;     /DESC = If set, then return a descriptor for the given variable.  If the
;             variable is a scalar the value is returned as a string.  If it is
;             an array a description is returned just like the HELP command 
;             gives.  Ex:'
;             IDL> print, datatype(fltarr(2,3,5),/desc) gives the string
;                           'FLTARR(2,3,5)'
;     /TNAME - If set, then returns a identical result to the use of the /TNAME
;                keyword to the SIZE() function in IDL V5.2 and later.   
;               Overrides the value of FLAG.
;     /HELP    = If set, then a short explanation is printed out.
;
; OUTPUT PARAMETERS: 
;       The result of the function is the either a string or integer giving the
;       data type of VAR.  Depending on the value of FLAG or /TNAME, the result
;       will be one of the values from the following table:
;
;      FLAG = 0       FLAG = 1       FLAG = 2    FLAG = 3      /TNAME
;
;      UND            Undefined        0          UND          UNDEFINED  
;      BYT            Byte             1          BYT          BYTE
;      INT            Integer          2          INT          INT
;      LON            Long             3          LON          LONG
;      FLO            Float            4          FLT          FLOAT
;      DOU            Double           5          DBL          DOUBLE
;      COM            Complex          6          COMPLEX      COMPLEX
;      STR            String           7          STR          STRING
;      STC            Structure        8          STC          STRUCT
;      DCO            DComplex         9          DCOMPLEX     DCOMPLEX
;      PTR            Pointer         10          PTR          POINTER
;      OBJ            Object          11          OBJ          OBJREF
;      UIN            UInt            12          UINT         UINT
;      ULN            ULong           13          ULON         ULONG
;      L64            Long64          14          LON64        LONG64
;      U64            ULong64         15          ULON64       ULONG64
;
;
; REVISION HISTORY: 
;       Original Version: R. Sterner, JHU/APL, 24 October 1985.
;       Major rewrite, add /TNAME keyword, unsigned and 64 bit datatypes
;       W. Landsman   August 1999
;       Zarro (SM&A/GSFC) - November 2001, replace error stops by continues
;-
;-------------------------------------------------------------
 
 
        if (N_params() lt 1) or keyword_set(hlp) then begin
          print,' Datatype of variable as a string (3 char or spelled out).'
          print,' typ = datatype(var, [flag])'
          print,'   var = variable to examine.         in'
          print,'   flag = output format flag (def=0). in'
          print,'   typ = datatype string or number.   out'
          print,'      flag=0    flag=1      flag=2  flag=3      /TNAME'
          print,'      UND       Undefined   0       UND         UNDEFINE'
          print,'      BYT       Byte        1       BYT         BYTE'
          print,'      INT       Integer     2       INT         INT'
          print,'      LON       Long        3       LON         LONG'
          print,'      FLO       Float       4       FLT         FLOAT'
          print,'      DOU       Double      5       DBL         DOUBLE'
          print,'      COM       Complex     6       COMPLEX     COMPLEX'
          print,'      STR       String      7       STR         STRING'
          print,'      STC       Structure   8       STC         STRUCT'
          print,'      DCO       DComplex    9       DCOMPLEX    DCOMPLEX'
          print,'      PTR       Pointer    10       PTR         POINTER'
          print,'      OBJ       Object     11       OBJ         OBJREF'
          print,'      UIN       UInt       12       UINT        UINT'
          print,'      ULO       ULong      13       ULON        ULONG'
          print,'      L64       Long64     14       LON64       LONG64'
          print,'      U64       ULong64    15       ULON64      ULONG64'
          print,' Keywords:'                                     
          print,'  /TNAME - Identical output to SIZE(/TNAME) '
          print,'  /DESCRIPTOR returns a descriptor for the given variable.'
          print,'     If the variable is a scalar the value is returned as'
          print,'     a string.  If it is an array a description is return'
          print,'     just like the HELP command gives.  Ex:'
          print,'     datatype(fltarr(2,3,5),/desc) gives'
          print,'       FLTARR(2,3,5)  (flag always defaults to 3 for /DESC).'
          return, -1
        endif 
 
 s_tname = ['UNDEFINE', 'BYTE','INT','LONG','FLOAT','DOUBLE','COMPLEX',$
            'STRING','STRUCT','DCOMPLEX','POINTER','OBJREF','UINT','ULONG', $
            'LONG64','ULONG64']

 s_flag0 =  ['UND','BYT','INT','LON','FLO','DOU','COM','STR','STC','DCO','PTR',$
             'OBJ','UIN','ULO','L64','U64']

 s_flag1 = ['Undefined','Byte','Integer','Long','Float','Double','Complex', $
            'String','Structure','DComplex','Pointer','Object','UInt','ULong',$
            'Long64','ULong64']

 s_flag3 = [ 'UND','BYT','INT','LON','FLT','DBL','COMPLEX','STR','STC', $
            'DCOMPLEX','PTR','OBJ','UINT','ULON','LON64','ULON64']
 
       s = size(var)
       stype =  s[s[0]+1]
        if stype GT N_elements(s_tname) then begin
         message,'ERROR - Unrecognized IDL datatype',/cont
         stype=0
        endif

        if keyword_set(TNAME) then return, s_tname[stype]          

        if N_params() lt 2 then flag0 = 0      ; Default flag.
        if keyword_set(desc) then flag0 = 3
  
       case flag0 of 
 
  0: return, s_flag0[stype]
  1: return, s_flag1[stype]
  2: return, stype
  3: typ = s_flag3[stype]
  else: message,'ERROR - Flag parameter must be between 0 and 3'
  endcase 
  
 if keyword_set(desc) then begin
          if stype EQ 0 then begin
           message,'ERROR - Input variable is undefined',/cont
           return,'Undefined'
          endif
          if s[0] eq 0 then return,strtrim(var,2)       ; Return scalar desc.
          aa = typ+'ARR('
          for i = 1, s[0] do begin                      
            aa = aa + strtrim(s[i],2)                 
            if i lt s[0] then aa = aa + ','          
            endfor                                     
          aa = aa+')'                                   
          return, aa
        endif else return,typ
 
   end
