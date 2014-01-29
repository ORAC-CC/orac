! Name: select_modis_albedo_file.F90
!
! Purpose:
! Populates the surface albedo part of the surface structure, using MODIS
! MCD43C spectral albedo over the land the cox_munk ocean surface reflectance
! model over the sea.
!
! Description and algorithm details
!
! Arguments:
! Name            Type     In/Out/Both Description
! doy    integer  in  
! mcd_date integer out
!
! Local variables:
! Name Type Description
!
! History:
! 2012/08/06 Caroline Poulsen original version
! 2012/08/16 Gareth Thomas Bug fix: removed unused variable "mcd_date"
! 2012/08/06 CP added in file name extension for 2009 changed how path is defined
!               i.e year no longer required in path name
! 2012/02/25 CP added in 2010 and 2007 data
! 2013/03/07 GT Reverted change made by CP on 2012/08/06, and added code to check
!               that the MODIS file exists and is readable.
!
! $Id$
!
! Bugs: there must be a way of searching for wildcard in fortran to make this co!de cleaner
! code should be modified so selects files from year either side
!
subroutine select_modis_albedo_file(cyear,doy,modis_surf_path,modis_surf_path_file)
use preproc_structures

implicit none


character(len=pathlength), intent(in)           :: modis_surf_path
character(len=pathlength), intent(out)          :: modis_surf_path_file
integer(kind=stint), intent(in)     :: doy
integer ::               nv,pos(1)
integer(kind=stint), allocatable, dimension(:) :: dates,newdates
character(len=datelength), allocatable, dimension(:) :: dates_s 
character(len=pathlength), allocatable, dimension(:) :: ext_s
character(len=datelength), intent(in)  :: cyear
character(len=3) ::mcd_date_s 
character(len=pathlength) :: ext_v_s     
logical :: modis_surf_file_exist
character(len=7) :: modis_surf_file_read

write(*,*) 'cyear doy ',cyear,doy

nv=46
allocate(dates(nv))
allocate(dates_s(nv))
allocate(ext_s(nv))
dates= long_int_fill_value


dates=(/1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153,&
     & 161,169,177,185,193,201,209,217,225,233,241,249,257,265,273,281,&
     & 289,297,305,313,321,329,337,345,353,361/)
dates_s=(/'001','009','017','025','033','041','049','057','065','073','081','089','097','105','113',&
     & '121','129','137','145','153','161','169','177','185','193','201','209','217','225','233','241',&
     & '249','257','265','273','281','289','297','305','313','321','329','337','345','353','361'/)

if (trim(adjustl(cyear)) .eq. '2008') then
ext_s=(/'2008025111631','2008033123214','2008040004943','2008047121240',&
     & '2008054151035','2008065051535','2008071162739','2008080232812',&
     & '2008086173808','2008097191941','2008102161344','2008114201334',&
     & '2008121040804','2008127174047','2008134041158','2008143081725',&
     & '2008152172444','2008159034505','2008165135311','2008177220047',&
     & '2008184035707','2008192032521','2008199215458','2008205151447',&
     & '2008215000941','2008224075819','2008238210148','2008246050040',&
     & '2008251160923','2008256090501','2008271144514','2008277153013',&
     & '2008285034751','2008291033412','2008304210016','2008309142820',&
     & '2008312174054','2008331165258','2008335204416','2008339113335',&
     & '2008354114332','2008359104853','2008359104537','2009012201920',&
     & '2009019041257','2009034120609'/)
endif

if (trim(adjustl(cyear)) .eq. '2009') then
ext_s=(/'2009035033047','2009035093419','2009037163503',&
     &'2009049045513','2009064153637','2009066185433','2009068173243',&
     &'2009086154612','2009094144004','2009095045748','2009102120958',&
     &'2009120060313','2009127003905','2009130202842','2009134174328',&
     &'2009142171615','2009153033721','2009164033657','2009171032902',&
     &'2009184041808','2009187003552','2009203041731','2009206041253',&
     &'2009213211007','2009225080242','2009227031838','2009230094129',&
     &'2009236062357','2009250080227','2009254115141','2009261030822',&
     &'2009269041609','2009277230851','2009286145535','2009308044615',&
     &'2009308095113','2009310073001','2009316045157','2009323225354',&
     &'2009345062739','2009356112107','2009357180357','2009359042705',&
     &'2009365230011','2010013042737','2010022205422'/)

endif
if (trim(adjustl(cyear)) .eq. '2007') then
ext_s=(/'2007046104338','2007053121538','2007105022224',&
     &'2007104152343','2007106062618','2007107034849','2007094051850',&
     &'2007100104315','2007102104743','2007103233946','2007107051030',&
     &'2007112101018','2007130154342','2007135095112','2007141035826',&
     &'2007144074928','2007164033906','2007167180146','2007188105839',&
     &'2007188085237','2007192114809','2007202132921','2007204051426',&
     &'2007211233709','2007214154431','2007231084532','2007235181022',&
     &'2007245082322','2007248163744','2007262093737','2007265185631',&
     &'2007276164428','2007283151217','2007287082301','2007312052123',&
     &'2007311190602','2007316190504','2007335193346',&
     &'2007338042306','2007341011248','2007358135904','2007360070224',&
     &'2007363144454','2008003081529','2008009025819','2008019130651'/)
endif

if (trim(adjustl(cyear)) .eq. '2010') then

ext_s=(/'2010027235522','2010034002012','2010040023228',&
     &'2010044091117','2010053034308','2010064145456','2010070115206',&
     &'2010100101037','2010097110209','2010112133134','2010107113858',&
     &'2010118064129','2010119005604','2010139073433','2010140032537',&
     &'2010141215238','2010155002933','2010160002804','2010167015805',&
     &'2010175035844','2010180023454','2010195052141','2010199221400',&
     &'2010204012137','2010244023636','2010249220111','2010252192730',&
     &'2010253011103','2010254133200','2010256224611','2010262033745',&
     &'2010274092421','2010279094051','2010288065023','2010293115945',&
     &'2010306052618','2010313024208','2010320043437','2010325063355',&
     &'2010332125021','2010343014933','2010350012725','2011042105315',&
     &'2011025133207','2011025054213','2011025043810'/)
endif

!
!find the closest data
!

allocate(newdates(nv))
newdates= long_int_fill_value
newdates=abs(dates-doy)

pos=minloc(newdates)


mcd_date_s=dates_s(pos(1))
ext_v_s=ext_s(pos(1))
!modis_surf_path_file=trim(adjustl(modis_surf_path))//'/'//trim(adjustl(cyear))//'/'//'MCD43C3.A'//&
modis_surf_path_file=trim(adjustl(modis_surf_path))//'/'//'MCD43C3.A'//&
     trim(adjustl(cyear))//trim(adjustl(mcd_date_s))//'.005.'//&
     trim(adjustl(ext_v_s))//'.hdf'
write(*,*) trim(modis_surf_path_file)

! Check that the defined file exists and is readable
inquire(file=trim(modis_surf_path_file), exist=modis_surf_file_exist, &
     & read=modis_surf_file_read)
if (.not.modis_surf_file_exist) then
   write(*,*) 'STOP: MODIS surface albedo file does not exist'
   stop
else if (trim(modis_surf_file_read).eq.'NO') then
   write(*,*) 'STOP: MODIS surface albedo file exists but is not readable'
   stop
end if

deallocate(dates)
deallocate(dates_s)
deallocate(ext_s)
deallocate(newdates)

end subroutine select_modis_albedo_file


