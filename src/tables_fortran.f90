module tables_fortran
    use iso_c_binding

    implicit none
    private


    enum, bind(c)
      enumerator :: COL_TYPE_UNKNOWN = 0
      enumerator :: COL_TYPE_I32 = 1
      enumerator :: COL_TYPE_F32 = 2
    end enum

    type ColumnData
    end type

    type, extends(ColumnData) :: I32
        integer(kind=c_int32_t), allocatable :: values(:)
    end type I32

    type, extends(ColumnData) :: F32
        real(kind=c_float), allocatable :: values(:)
    end type F32
    
    ! validity bitmap, LSB
    type :: BitMap
        integer(kind=c_int64_t), allocatable :: bits(:)
    end type BitMap

    
    type :: FortranColumn
        type(BitMap) :: validity
        integer(kind=c_int32_t), allocatable :: offsets(:)
        real(kind=c_float), allocatable :: values(:)
        class(ColumnData), allocatable :: data
    end type FortranColumn
    
    type :: FortranTable
        integer :: ncols = 0
        integer :: nrows = 0
        logical :: has_headers = .false.
        type(FortranColumn), allocatable :: columns(:)
    end type FortranTable

    public FortranTable
    public add_f32_column, remove_f32_column_idx, destroy_table
    public F32, I32
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

      allocate(F32 :: table%columns(i)%data)
      select type (d => table%columns(i)%data)
        type is (F32)
            allocate(d%values(nrows))
        type is (I32)
            allocate(d%values(nrows))
        class default
            error stop "Unexpected type in allocate_table"
      end select

    end do

  end subroutine allocate_table

  subroutine destroy_table(table)
    type(FortranTable), intent(inout) :: table
    integer :: i
    
    do i = 1, table%ncols
      deallocate(table%columns(i)%validity%bits)
      deallocate(table%columns(i)%offsets)
      deallocate(table%columns(i)%values)

      select type (d => table%columns(i)%data)
        type is (I32)
          deallocate(d%values)
        type is (F32)
          deallocate(d%values)
        class default
            print *, "Warning: Unknown type during deallocation"
      end select
      deallocate(table%columns(i)%data)

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
        print *, "Tables are not the same shape."
        print *, "f_table: ", size(f_table%columns), " t_table: ", size(t_table%columns)
        err = 1
        return 
      end if

      do i = 1, length
        j = keep_indices(i)
        t_table%columns(i)%validity%bits = f_table%columns(j)%validity%bits
        t_table%columns(i)%offsets = f_table%columns(j)%offsets
        t_table%columns(i)%values = f_table%columns(j)%values
        select type (d => t_table%columns(i)%data)
          type is (F32)
            d%values = f_table%columns(j)%values
          class default
              error stop "Unexpected type in copy_table"
        end select
      end do
      
    else 

      do i = 1, f_table%ncols
        t_table%columns(i)%validity%bits = f_table%columns(i)%validity%bits
        t_table%columns(i)%offsets = f_table%columns(i)%offsets
        t_table%columns(i)%values = f_table%columns(i)%values

        select type (d => t_table%columns(i)%data)
          type is (F32)
            d%values = f_table%columns(i)%values
          class default
              error stop "Unexpected type in allocate_table"
        end select
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
      
      select type (d => table%columns(ncols)%data)
          type is (F32)
            d%values = values
          class default
              error stop "Unexpected type in allocate_table"
      end select
      return
    end if 
    
    if (size(table%columns(1)%values) /= nrows) then
      print *, "New column is not same length of current table."
      err = 1
      return 
    end if
  
    call allocate_table(t_table, nrows, ncols)
    call copy_table(table, t_table)
    call destroy_table(table)
    t_table%columns(ncols)%values = values
    select type (d => t_table%columns(ncols)%data)
      type is (F32)
        d%values = values
      class default
          error stop "Unexpected type in allocate_table"
    end select

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
