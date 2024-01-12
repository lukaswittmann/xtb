! This file is part of xtb.
! SPDX-Identifier: LGPL-3.0-or-later
!
! xtb is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! xtb is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with xtb.  If not, see <https://www.gnu.org/licenses/>.

#ifndef WITH_DRACO
#define WITH_DRACO 0
#endif

!> This module implements the draco solvation library for xTB.

module xtb_solv_draco

#if WITH_DRACO
   use draco, only: TDraco
#endif
   
   use xtb_type_environment, only: TEnvironment
   use iso_fortran_env, only: input_unit, output_unit
   use mctc_env, only: error_type, fatal_error, wp

   implicit none

   private

   public :: TCdraco

   character(len=*), parameter :: source = 'xtb_solv_draco'

   !* DRACO calculation type
#if WITH_DRACO
   type, extends(TDraco) :: TCdraco
#else
   type :: TCdraco
#endif
      contains

      procedure :: setup => setup_draco
      ! procedure :: calc => calculate_draco
      ! procedure :: print => print_draco
      
   end type TCdraco

   contains

   subroutine setup_draco(self, env, solvent)
      implicit none

      !> Environments
      class(TCdraco), intent(inout) :: self
      type(TEnvironment), intent(inout) :: env

      !> Solvent name for internal CPCM-X database
      character(len=*), intent(in) :: solvent
      !> Error handling

      type(error_type), allocatable :: error

#if WITH_DRACO
      call env%warning('DRACO should be here...')
#else
      call no_draco_here(env)
#endif
   end subroutine setup_draco

!    !* Calculate draco
!    subroutine calculate_draco(self,env)

!       implicit none

!       !> Environments
!       class(TCdraco), intent(inout) :: self
!       type(TEnvironment), intent(inout) :: env
     
! #if WITH_DRACO
!       call env%warning(env, 'DRACO should be here...')
! #else
!       call no_draco_here(env)
! #endif
!    end subroutine calculate_draco

!    !* Print results
!    subroutine print_draco(self,verbose)
      
!       implicit none

!       !> Environments
!       class(TCdraco), intent(in) :: self

!       !> Output level
!       logical, intent(in), optional :: verbose

! #if WITH_DRACO
!       call env%warning(env, 'DRACO should be here...')
! #endif
!    end subroutine print_draco

#if ! WITH_DRACO
   subroutine no_draco_here(env)
      type(TEnvironment), intent(inout) :: env

      call env%error('DRACO is not available in this version of xtb.', source)
   end subroutine no_draco_here
#endif

end module xtb_solv_draco
