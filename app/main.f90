program main
  use tables_fortran
  use iso_c_binding
  implicit none
  
  type(FortranTable) :: table, tbl
  integer :: i, ierr
  real(kind=c_float) :: new_column_data(4)

  call add_f32_column(tbl, [10.0, 10.0, 10.0, 20.0])
  call add_f32_column(tbl, [09.1, 99.0, 88.0, 33.0])
  call add_f32_column(tbl, [19.1, 210.0, 311.0, 22.0])
  call remove_f32_column_idx(tbl, 2)

  print *, "EXAMPLE FILE"
  do i = 1, tbl%ncols
      print *, "Column", i, ":"
      print *, "  Values:", tbl%columns(i)%values
  end do

  print *, "=============================="

  do i = 1, tbl%ncols
      select type (d => tbl%columns(i)%data)
          type is (F32)
              print *, "Column", i, ":"
              print *, "  Values:", d%values
          class default
              print *, "Warning: Unknown type"
      end select
  end do

  call destroy_table(tbl)


end program main
