module tables_fortran
    use iso_c_binding
    ! use iso_fortran_env, only: uint64
    implicit none
    private
    
    ! validity bitmap, LSB
    type :: BitMap
        integer(kind=c_int64_t), allocatable :: bits(:)
    end type BitMap
    
    type :: FortranColumn
        type(BitMap) :: validity
        integer(kind=c_int32_t), allocatable :: offsets(:)
        real(kind=c_float), allocatable :: values(:)
    end type FortranColumn
    
    type :: FortranTable
        integer :: ncols = 0
        integer :: nrows = 0
        type(FortranColumn), allocatable :: columns(:)
    end type FortranTable

    public FortranTable
    public add_f32_column, remove_f32_column_idx, destroy_table
    
contains

  subroutine allocate_table(table, nrows, ncols)
    type(FortranTable), intent(inout) :: table
    integer, intent(in) :: nrows, ncols
    integer :: i

    table%nrows = nrows 
    table%ncols = ncols

    allocate(table%columns(ncols))
    do i = 1, ncols
      allocate(table%columns(i)%validity%bits(2))
      allocate(table%columns(i)%offsets(2))
      allocate(table%columns(i)%values(nrows))
    end do

  end subroutine allocate_table

  subroutine destroy_table(table)
    type(FortranTable), intent(inout) :: table
    integer :: i
    
    do i = 1, table%ncols
      deallocate(table%columns(i)%validity%bits)
      deallocate(table%columns(i)%offsets)
      deallocate(table%columns(i)%values)
    end do

    deallocate(table%columns)

  end subroutine destroy_table

  subroutine copy_table(f_table, t_table, keep_indices, err)
    type(FortranTable), intent(inout) :: f_table, t_table
    integer, intent(in), optional :: keep_indices(:)
    integer, intent(inout), optional:: err
    integer :: i, j, length

    if (present(keep_indices)) then 

      length = size(keep_indices)
      if (size(t_table%columns) /= length) then
        print *, "Column rows are not same length of current table rows."
        err = 1
        return 
      end if

      do i = 1, length
        print *, "C"
        j = keep_indices(i)
        t_table%columns(i)%validity%bits = f_table%columns(j)%validity%bits
        t_table%columns(i)%offsets = f_table%columns(j)%offsets
        t_table%columns(i)%values = f_table%columns(j)%values
      end do
      
    else 

      do i = 1, f_table%ncols
        t_table%columns(i)%validity%bits = f_table%columns(i)%validity%bits
        t_table%columns(i)%offsets = f_table%columns(i)%offsets
        t_table%columns(i)%values = f_table%columns(i)%values
      end do

    end if 

  end subroutine copy_table
    
  subroutine add_f32_column(table, values, err)
    type(FortranTable), intent(inout) :: table
    real(kind=c_float), intent(in) :: values(:)
    integer, intent(inout), optional :: err
    
    type(FortranTable) :: t_table
    integer :: nrows, ncols

    nrows = size(values)
    ncols = table%ncols + 1 

    if (.not. allocated(table%columns)) then 
      call allocate_table(table, nrows, ncols)
      table%columns(ncols)%values = values
      return
    end if 
    
    if (size(table%columns(1)%values) /= nrows) then
      print *, "Column rows are not same length of current table rows."
      err = 1
      return 
    end if
  
    call allocate_table(t_table, nrows, ncols)
    call copy_table(table, t_table)
    call destroy_table(table)
    t_table%columns(ncols)%values = values

    call allocate_table(table, nrows, ncols)
    call copy_table(t_table, table)
    call destroy_table(t_table)

  end subroutine add_f32_column

  subroutine remove_f32_column_idx(table, col_idx, err)
    type(FortranTable), intent(inout) :: table
    integer, intent(in) :: col_idx
    integer, intent(inout), optional :: err

    type(FortranTable) :: t_table
    integer, allocatable :: keep_indices(:)
    integer :: nrows, ncols, i, j

    if (col_idx < 1 .or. col_idx > table%ncols) then
      print *, "Error: Invalid column index"
      err = -133
      return
    end if

    allocate(keep_indices(table%ncols - 1))

    i = 1
    do j = 1, table%ncols
      if (j /= col_idx) then
        keep_indices(i) = j
        i = i + 1
      end if
    end do

    nrows = table%nrows
    ncols = table%ncols - 1
    call allocate_table(t_table, nrows, ncols)
    call copy_table(table, t_table, keep_indices=keep_indices)
    call destroy_table(table)
    call allocate_table(table, nrows, ncols)
    call copy_table(t_table, table)

  end subroutine remove_f32_column_idx
  
end module tables_fortran