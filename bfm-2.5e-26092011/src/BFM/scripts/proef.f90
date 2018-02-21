
    function ppPhytoPlankton(n,constituent)

     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     ! Implicit typing is never allowed
     !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     IMPLICIT NONE

     integer ::ppPhytoPlankton
     integer, intent(IN) ::n
     integer, intent(IN) ::constituent

     integer,dimension(6) :: referto=[ppP1c,ppP2c,ppP3c,ppP4c,ppP5c,ppP6c]
     integer,dimension(6) :: const_max=[5,4,4,4,5,4]
     integer,dimension(9) :: constituent_add=[0,1,2,3,4,0,0,0,0]

     if ( constituent <=const_max(n) ) then
      ppPhytoPlankton=referto(n)+ constituent_add(constituent)
     else
      ppPhytoPlankton=0
     endif

END function



