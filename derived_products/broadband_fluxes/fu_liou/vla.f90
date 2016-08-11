!===============================================================================
MODULE VLA_FU
USE FUINPUT,only :  fi,VI,VD,VO, mccx,mxovl,nv1x,mfix,mflo,maxin,maxrep,mri
implicit none

real,    parameter ::  REAL4DEFAULT = 3.402823E+38 
integer,parameter :: INT2DEFAULT = 32767
!---
real  pmax 
real, parameter    ::  pmin  = 1.0E-06 ! Minimum Pressure(Hpa) allowed

real, parameter    ::  tmax  = 400.  ! Maximum Temperature(K) allowed
real, parameter    ::  tmin  = 150.  ! Minimum Temperature(K) allowed

real, parameter    ::  qmax  = 0.50     ! Maximum H20 Mixing Ratio(g/g) allowed
real, parameter    ::  qmin  = 1.0E-10  ! Minimum H20 Mixing Ratio(g/g) allowed

real, parameter    ::  omax  = 1.0E-02  ! Maximum O3 Mixing Ratio(g/g) allowed
real, parameter    ::  omin  = 1.0E-10  ! Minimum O3 Mixing Ratio(g/g) allowed

real,parameter	   :: dpmin   = 1.0   ! Minimum pressure thickness of a layer allowed below Pres(pminedit)
real,parameter	   :: pminedit= 50.0  ! Profile not edited at pressures levles below pminedit
real,parameter	   :: nearsfc = 1.0  ! No Fixed levels within 'nearsfc (hpa)' of Surface 

integer ,dimension(nv1x) ::   idxi
real ytmp(4,nv1x)
real ptmp(nv1x)
 character*20 mycpu

 CONTAINS
!==============================================================================
subroutine vla_interface_fu
USE FUINPUT ,only : fi
integer icnd,iovl
fi%nv = FI%VO%nlev-1
fi%pp(1:FI%VO%nlev) = FI%VO%pp(1:FI%VO%nlev)
fi%pt(1:FI%VO%nlev) = FI%VO%pt(1:FI%VO%nlev)
fi%ph(1:FI%VO%nlev) = FI%VO%ph(1:FI%VO%nlev)
fi%po(1:FI%VO%nlev) = FI%VO%po(1:FI%VO%nlev)


do icnd=1,mccx
fi%fc(icnd)%icld_top(1:mxovl)=-10000
fi%fc(icnd)%icld_bot(1:mxovl)=-10000
if ( fi%fc(icnd)%cldfrac <= 0.0 ) cycle

do iovl=1,fi%fc(icnd)%novl
if (  FI%VO%cldindex(1,icnd,iovl) > 0 .and. &
      FI%VO%cldindex(2,icnd,iovl) > 0 ) then
fi%fc(icnd)%icld_top(iovl)  =  FI%VO%cldindex(1,icnd,iovl)	    ! Cloud Top LAYER
fi%fc(icnd)%icld_bot(iovl)  =  FI%VO%cldindex(2,icnd,iovl)	    ! Cloud Bottom LAYER
endif
enddo
enddo
end subroutine vla_interface_fu
!==============================================================================
subroutine sort_input_profile
integer , parameter :: isize = 16
real xtmp(4,vi%nlev)
!external ifunx4
!integer*2 ifunx4
integer ind(vi%nlev)

if ( vi%nlev > maxin ) vo%ierr = 10 !! EXCEEDE maxin # levels 
if ( vo%ierr > 0 ) return

xtmp(1,1:vi%nlev) = vi%pp(1:vi%nlev )
xtmp(2,1:vi%nlev) = vi%pt(1:vi%nlev )
xtmp(3,1:vi%nlev) = vi%ph(1:vi%nlev )
xtmp(4,1:vi%nlev) = vi%po(1:vi%nlev )

!   call qsort(xtmp,vi%nlev ,isize,ifunx4)

 call qsortd (vi%pp(1:vi%nlev ),ind,vi%nlev)

vi%pp(1:vi%nlev ) = xtmp(1,ind(1:vi%nlev))
vi%pt(1:vi%nlev ) = xtmp(2,ind(1:vi%nlev))
vi%ph(1:vi%nlev ) = xtmp(3,ind(1:vi%nlev))
vi%po(1:vi%nlev ) = xtmp(4,ind(1:vi%nlev))

end subroutine sort_input_profile
!==============================================================================
subroutine gen_vla_structure
integer :: i,j, n1,n2,n3
integer,parameter :: isize =16
!external ifunx4
!integer*2 ifunx4
!integer ind(vi%nlev)
integer ind(nv1x)
!---Generate Monotonically Increasing Pressure levels
if ( vd%nfix >  mfix )  vo%ierr = 11 !! Exceede mfix # of levels
if ( vd%nflo >  mflo )  vo%ierr = 12 !! Exceede mflo # of levels


if (  vo%ierr > 0 ) return

where( vd%pfix    <= pmin .or. vd%pfix    > pmax ) vd%pfix    = REAL4DEFAULT
where( vd%pflo    <= pmin .or. vd%pflo    > pmax ) vd%pflo    = REAL4DEFAULT
where( vd%cldpres <= pmin .or. vd%cldpres > pmax ) vd%cldpres = REAL4DEFAULT
where( vd%cldpres >  max (maxval(vd%pfix(1:vd%nfix)), &
			  maxval(vd%pflo(1:vd%nflo)))  ) vd%cldpres = REAL4DEFAULT ! NoClouds Below Surface

do i = 1, mccx
if ( fi%fc(i)%cldfrac <= 0.0 ) then
 vd%cldpres(1:2,i,1:mxovl) = REAL4DEFAULT
else
  do j=1,mxovl
   if ( j > fi%fc(i)%novl ) vd%cldpres(1:2,i,j) = REAL4DEFAULT
  enddo
endif
enddo


n1=  count(vd%pfix(1:vd%nfix) /= REAL4DEFAULT)
n2=  count(vd%pflo(1:vd%nflo) /= REAL4DEFAULT)
n3=  count(vd%cldpres         /= REAL4DEFAULT)


ytmp(1, 1:      n1      ) = pack( vd%pfix(1:vd%nfix),vd%pfix(1:vd%nfix) /= REAL4DEFAULT)
ytmp(1, n1+1:   n1+n2   ) = pack( vd%pflo(1:vd%nflo),vd%pflo(1:vd%nflo) /= REAL4DEFAULT)
ytmp(1, n1+n2+1:n1+n2+n3) = pack( vd%cldpres,                vd%cldpres /= REAL4DEFAULT)

ytmp(2, 1      :n1       ) =1 ! Fixed
ytmp(2, n1+1   :n1+n2    ) =2 ! Floating
ytmp(2, n1+n2+1:n1+n2+ n3) =3 ! Cloud

vo%nlev = n1+n2+n3

ptmp(1:vo%nlev) = ytmp(1,1:vo%nlev)

if ( vo%nlev >  nv1x )  vo%ierr = 20 !! Exceede nv1x # of levels
if ( vo%ierr > 0 ) return

!   call qsort(ytmp,vo%nlev ,isize,ifunx4)
 
 call qsortd (ptmp,ind,vo%nlev)
 
 vo%pp(1:vo%nlev) = ytmp(1,ind(1:vo%nlev))
 
 ytmp(1,1:vo%nlev) = ytmp(1,ind(1:vo%nlev)) 
 ytmp(2,1:vo%nlev) = ytmp(2,ind(1:vo%nlev)) 
 
 

end subroutine gen_vla_structure
!==============================================================================
subroutine edit_pressure_levels
real   dpb(nv1x)
integer i,nedited

logical iex(nv1x)
iex= .true.

EDIT_LAYER : do i=1,vo%nlev-1
!print'(i4,f8.2)',i,vo%pp(i)
dpb(i) = vo%pp(i+1) - vo%pp(i  )

if (  vo%pp(i) == vo%pp(i+1) .and. ytmp(2,i  )/= 3 ) iex(i)=.false. !! REMOVE DUPLICATE LAYERS
if (  vo%pp(i) == vo%pp(i+1) .and. ytmp(2,i  )== 3 .and. ytmp(2,i+1) == 3) iex(i)=.false. !! REMOVE DUPLICATE LAYERS
if (  vo%pp(i) == vo%pp(i+1) .and. ytmp(2,i  )== 3 .and. ytmp(2,i+1) /= 3) iex(i+1)=.false. !! REMOVE DUPLICATE LAYERS
!if (  vo%pp(i) == vo%pp(i+1) .and. ytmp(2,i  )== 3 .and. ytmp(2,i+1) == 1) iex(i+1)=.false. !! REMOVE DUPLICATE LAYERS
!THIN LAYER EDIT
if ( vo%pp(i)  > pminedit .and. ytmp(2,i  )== 1 .and. dpb(i) < dpmin) iex(i)=.false.
if ( vo%pp(i+1)> pminedit .and. ytmp(2,i+1)== 1 .and. dpb(i) < dpmin) iex(i+1)=.false.

!NEAR SURFACE LAYER EDIT
if (  vo%pp(i) > pminedit .and. ytmp(2,i) == 1 .and. &
      vo%pp(i) > vo%pp(vo%nlev)-nearsfc  ) iex(i)=.false.

if (  vo%pp(i) >  vd%pflo(1)   ) iex(i)=.false.

!if (  vo%pp(i) ==  vd%pflo(1)   ) iex(i)=.true. ! ASSUME vd%pflo(1) is surface and be sure to include

!!if( .not.iex(i) ) print*,i,vo%pp(i),ytmp(2,i  ),dpb(i),'dddd'

enddo EDIT_LAYER

!!print*,'dddd',vd%pflo(1)

!do i=1,vo%nlev-1 ; print'(i4,4f8.2,l1)',i ,vo%pp(i),vo%pp(i+1),dpb(i),ytmp(2,i),iex(i) ; enddo

nedited = count( iex(1:vo%nlev) )
vo%pp(1:nedited)  = pack( vo%pp(1:vo%nlev), iex(1:vo%nlev) ) 
ytmp(2,1:nedited) = pack( ytmp(2,1:vo%nlev), iex(1:vo%nlev) ) 
if ( nedited < vo%nlev ) vo%ierr= -10 !! PROFILE WAS EDITED to remove thin layers
if ( nedited > vo%nlev ) vo%ierr= 200 !! wacko edited # of layers > original # of layers
vo%nlev = nedited

end subroutine edit_pressure_levels
!==============================================================================
subroutine find_reporting_pressure_levels
real tmp(vo%nlev),perr(maxrep)
integer i ,iold(1),inew(1)
!------
 iold(1) = -9999
 vo%ireport = INT2DEFAULT !REAL4DEFAULT
do i=1,vd%nrep
tmp = abs(vo%pp(1:vo%nlev)-vd%report(i))
perr(i) = minval(tmp)

inew(1:1) = minloc(tmp)
if(  perr(i) < dpmin .and. inew(1) > iold(1) ) then
!if(  perr(i) < 10 .and. inew(1) > iold(1) ) then

 vo%ireport(i:i) = inew(1)
if ( vo%ireport(i) < 1 .or. vo%ireport(i) > vo%nlev  )  vo%ierr= 300 !!! BAD REPORTING LEVEL
endif

enddo


end subroutine find_reporting_pressure_levels
!==============================================================================
subroutine profile_interpolation
integer :: i,j
!----Find Indexes for Interpolation
idxi = -1
OUTER: do i = 1, vo%nlev

  if     (  vo%pp(i) < vi%pp(1) ) then
   idxi(i)=1
   vo%ierr = -1 ! Extrapolate Above Highest toa  level WARNING ONLY
  elseif (  vo%pp(i) >= vi%pp(vi%nlev ) ) then
   idxi(i)= vi%nlev -1 !!!!!! 20060111 (-1)
    
      if ( vo%pp(i) > vi%pp(vi%nlev ) )then 
	vo%ierr = -2 ! Extrapolate Below lowest sfc llevel WARNING ONLY
	idxi(i)= vi%nlev-1 !!FIX 2004_11_16
      endif
  else
   INNER: do j = 1, vi%nlev-1
    if ( vo%pp(i) >= vi%pp(j) .and. vo%pp(i) < vi%pp(j+1) )then
      idxi(i) = j
      exit
    endif
   enddo INNER
  endif
enddo OUTER

INTERPOLATE : do i = 1, vo%nlev


vo%pt(i)= rintlp(vi%pp(idxi(i)),vi%pp(idxi(i)+1) &
                ,vi%pt(idxi(i)),vi%pt(idxi(i)+1), vo%pp(i) ,itype=0 )

vo%ph(i)= rintlp(vi%pp(idxi(i)),vi%pp(idxi(i)+1) &
                ,vi%ph(idxi(i)),vi%ph(idxi(i)+1), vo%pp(i) ,itype=0) !: back to type0 3-14-11

vo%po(i)= rintlp(vi%pp(idxi(i)),vi%pp(idxi(i)+1) &
                ,vi%po(idxi(i)),vi%po(idxi(i)+1), vo%pp(i) ,itype=0)!: back to type0 3-14-11

!OPTION???
!      if(  vo%pp(i) > vi%pp(vi%nlev) ) then   ! 6.5K/Km lapse rate temperature below lowest level
!       vo%pt(i) = vi%pt(vi%nlev )+  (6.5*0.001*29.3*vi%pt(vi%nlev )) *log(  vo%pp(i)/ vi%pp(vi%nlev ))
!        print*,'vladebug',i,vi%nlev , vi%pp(vi%nlev ), vi%pt(vi%nlev ),   vo%pp(i) ,vo%pt(i)    
!      endif
!OPTION???



if ( vo%pt(i) < tmin .or. vo%pt(i) > tmax ) vo%ierr = 101 ! Interpolated temperature out of range
if ( vo%ph(i) < qmin .or. vo%ph(i) > qmax ) vo%ierr = 102 ! Interpolated Specific Humidity out of range
if ( vo%po(i) < omin .or. vo%po(i) > omax ) vo%ierr = 103 ! Interpolated O3 Mixing Ratio out of range

enddo INTERPOLATE
 
end subroutine profile_interpolation
!==============================================================================
subroutine assign_cloud_index
integer i,j,k
do i = 1,mccx 
if ( fi%fc(i)%cldfrac <= 0.0 ) cycle

 do j = 1,fi%fc(i)%novl

if (  vd%cldpres(1,i,j) /= REAL4DEFAULT .and. &
      vd%cldpres(2,i,j) /= REAL4DEFAULT) then

 if ( vd%cldpres(1,i,j) >= vd%cldpres(2,i,j) ) vo%ierr = 3 !!! Cloud Top Pressure  >= Cloud Bottom Pressure
 if ( vd%cldpres(1,i,j) >= VO%pp(VO%nlev)    ) vo%ierr = 4 !!! Cloud Top Pressure  >= SFC PRES
 if ( vd%cldpres(2,i,j) >  VO%pp(VO%nlev)    ) vo%ierr = 5 !!! Cloud Bot Pressure  > SFC PRES
 if ( vd%cldpres(1,i,j) > vd%cldpres(2,i,j)-dpmin ) vo%ierr = 6 !!! Cloud Pressure Thickness < dpmin

  if (  vo%ierr > 0  ) return

  do k=1,vo%nlev
   if ( vo%pp( k ) == vd%cldpres(1,i,j) ) VO%cldindex(1,i,j) = k   
   if ( vo%pp( k ) == vd%cldpres(2,i,j) ) VO%cldindex(2,i,j) = k-1
  enddo

endif

enddo ;enddo

end subroutine assign_cloud_index
!==============================================================================
subroutine create_reverse_pressure_index
integer k, ib ,ie

vo%ri_pp = -1

do  k = 1,vo%nlev-1
 ib= nint(vo%pp( k   ) )
 ie= nint(vo%pp( k+1 ) )
 if ( ib < 0 .or. ib > pmax .or. ie < 0 .or.  ie > pmax ) then
   vo%ierr = 21 ! Reverse Pressure index out of bounds
   return
 endif
 if ( ib == 0 ) ib=1
 if ( ie == 0 ) ie=1
!if ( vo%ierr == 21) print*,k,ib,ie
 vo%ri_pp(ib:ie)=k
enddo

end subroutine create_reverse_pressure_index
!==============================================================================
!==============================================================================
subroutine print_vla_in
integer :: i,j
print '(a80)', &
'<-----------------------------Begin print_vla_in Begin---------------------->'

! Print Inputs --------------------------------------------
print'(a20,i10)','FI%vi%nlev=',FI%vi%nlev
print*, 'Input P(z),   T(z),   Q(z),   O3(z) Profile'
do i=1,FI%vi%nlev
print'(2f10.3,2es10.3)',FI%vi%pp(i),FI%vi%pt(i),FI%vi%ph(i),FI%vi%po(i)
enddo
!----------------------------------------------------------
print'(a20,i10)','FI%vd%nfix=',FI%vd%nfix
print'(a20)',' FI%vd%pfix(1:FI%vd%nfix)'
do i=1,FI%vd%nfix
print'(I10,f10.3)',i,FI%vd%pfix(i)
enddo
print'(a25)',' FI%vd%pflo(1:FI%vd%nflo)'
do i=1,FI%vd%nflo
print'(I10,f10.3)',i,FI%vd%pflo(i)
enddo

print'(a33)','FI%vd%cldpres(1:2,i,j) CLDCND OVLCND'

do i = 1,mccx 
if ( fi%fc(i)%cldfrac <= 0.0 ) cycle

 do j = 1,mxovl
  if (  FI%vd%cldpres(1,i,j) > pmin .and. &
        FI%vd%cldpres(2,i,j) < pmax) & 
   print'(2f7.1,5x,2i6)',FI%vd%cldpres(1:2,i,j) ,i,j
enddo ;enddo
print '(a80)', &
'<-----------------------------END print_vla_in END--------------------------->'
end subroutine print_vla_in
!==============================================================================
subroutine print_vla_out
integer i,j,k
 character*2 cjj,ckk
 character*7 ipc( FI%vo%nlev ,mccx)
 ipc = ''
print '(a80)', &
'<-----------------------------Begin print_vla_out Begin---------------------->'

if( FI%vo%ierr < 0 ) print*,' Warning :FI%vo%ierr=', FI%vo%ierr
if( FI%vo%ierr > 0 ) print*,' ERROR   :FI%vo%ierr=', FI%vo%ierr

print*,'Number of interpolated levels (FI%vo%nlev)= ',FI%vo%nlev
print*,' VLA Interpolated Profile'
do i=1,FI%vo%nlev

do j=1,mccx 
if ( fi%fc(j)%cldfrac <= 0.0 ) cycle
do k=1,fi%fc(j)%novl
if (  i >= FI%vo%cldindex(1,j,k)    .and.  i <= FI%vo%cldindex(2,j,k)+1  )  then
 write(cjj,'(i2)') j
 write(ckk,'(i2)') k
ipc(i,j) = '('//cjj//','//ckk//')'
endif
enddo
enddo

print'(2i3,f3.0,2f10.3,2es10.3,30a7)' , &
 i,idxi(i),ytmp(2,i),FI%vo%pp(i),FI%vo%pt(i),FI%vo%ph(i),FI%vo%po(i),ipc(i,1:mccx)
enddo

print*,'PresTop  PresBot  TopIdx BotIdx  CLDCND OVLCND '
do i = 1,mccx 
if ( fi%fc(i)%cldfrac <= 0.0 ) cycle
 do j = 1,fi%fc(i)%novl
 if (  vd%cldpres(1,i,j) /= REAL4DEFAULT .and. &
        vd%cldpres(2,i,j) /= REAL4DEFAULT) &
       print'(2f8.1,2(2x,2I6))',vd%cldpres(1:2,i,j), FI%vo%cldindex(1:2,i,j),i,j

enddo;enddo
!------

print*,'----Reporting Level Index of Pressue level---'
do i=1,vd%nrep
print'(2I4,f10.3)',i, vo%ireport(i),vd%report(i) 
enddo

if( FI%vo%ierr < 0 ) print*,' Warning :FI%vo%ierr=', FI%vo%ierr
if( FI%vo%ierr > 0 ) print*,' ERROR   :FI%vo%ierr=', FI%vo%ierr

print '(a80)', &
'<-------------------------------END print_vla_out END ----------------------->'
end subroutine print_vla_out
!==============================================================================
subroutine prepare_model_profile_fu
! call print_vla_in

VD = FI%VD !!! ASSIGN  DEFINITION SUB-STRUCTURE
VI = FI%VI !!! ASSIGN INPUT       SUB-STRUCTURE

!Initalize Outputs--------------------------------------------------------
VO%pp= REAL4DEFAULT ;VO%pt= REAL4DEFAULT ;VO%ph= REAL4DEFAULT; VO%po= REAL4DEFAULT
VO%ierr    = 0; VO%cldindex= -10000 ; VO%nlev    = 0 ; pmax=float(mri)
!--------------------------------------------------------
 
 call sort_input_profile 		; if ( vo%ierr > 0 ) go to 1000
 call gen_vla_structure  		; if ( vo%ierr > 0 ) go to 1000
 call edit_pressure_levels		; if ( vo%ierr > 0 ) go to 1000
 call find_reporting_pressure_levels	; if ( vo%ierr > 0 ) go to 1000
 call profile_interpolation		; if ( vo%ierr > 0 ) go to 1000
 call assign_cloud_index		; if ( vo%ierr > 0 ) go to 1000
 call create_reverse_pressure_index

 
1000  FI%VO = VO  !!! ASSIGN OUTPUT STRUCTURE


! call print_vla_out
end subroutine prepare_model_profile_fu
!==============================================================================
!real function rintlp(p1,p2,v1,v2,px)
!real :: p1,p2,v1,v2,px
!rintlp = v1+((v1-v2)/log(p1/p2)) * log(px/p1) !ORG log pres~ln height
!end function rintlp
!======================================================================================
real function rintlp(p1,p2,v1,v2,px,ITYPE)
integer,optional :: ITYPE
real :: p1,p2,v1,v2,px
real r0


IF (PRESENT (ITYPE) ) then
 if ( itype == 0 ) r0 = v1+((v1-v2)/log(p1/p2)) * log(px/p1) !ORG Log pres ~ lin height
 if ( itype == 1 ) r0 = v1+ (v2-v1)/(p2-p1) * (px-p1) ! Linear in Pressure
 if ( itype == 2 ) r0 = exp( log(v1) + log(v2/v1)/(p2-p1) * (px-p1)) !Lin Pres & Log AMT
else
                  r0 = v1+((v1-v2)/log(p1/p2)) * log(px/p1) !ORG Log pres ~ lin height
endif

rintlp = r0

end function rintlp



END MODULE VLA_FU
!###############################################################################

!!-----------------------------------------------
!integer*2 function ifunx4(r1,r2)
!integer, parameter :: ndat = 4
! real*4 r1(ndat),r2(ndat)
! if(r1(1).gt.r2(1))ifunx4 =  1
! if(r1(1).eq.r2(1))ifunx4 =  0
! if(r1(1).lt.r2(1))ifunx4 = -1
!end function ifunx4
