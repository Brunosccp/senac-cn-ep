!gfortran PageRank.f95 -o main

PROGRAM PageRank
    implicit none
    double precision, dimension(4, 4) :: A
    double precision, dimension(:), allocatable :: V

    A(1,1) = 0
    A(2,1) = 0
    A(3,1) = 1
    A(4,1) = 1/2.0

    A(1,2) = 1/3.0
    A(2,2) = 0
    A(3,2) = 0
    A(4,2) = 0

    A(1,3) = 1/3.0
    A(2,3) = 1/2.0
    A(3,3) = 0
    A(4,3) = 1/2.0

    A(1,4) = 1/3.0
    A(2,4) = 1/2.0
    A(3,4) = 0
    A(4,4) = 0

    V = calcPageRank(A)

    print *, V

    contains
    function calcPageRank(A)
        double precision, dimension(:, :) :: A
        real :: x
        double precision :: temp
        integer :: n, i, j, k
        double precision, dimension (:), allocatable :: calcPageRank

        !obtendo n da matriz n x n, e alocando no vetor de pesos
        x = size(A)
        n = sqrt(x)
        allocate(calcPageRank(n))

        !criando vetor de pesos
        do i = 1, n
            calcPageRank(i) = 1.0/n
        enddo

        do i = 1, 100
            do j = 1, n
                temp = 0
                do k = 1, n
                    temp = temp + A(k,j) * calcPageRank(k)
                enddo
                calcPageRank(j) = temp
            enddo
        enddo

        !print *, n

    end function

END PROGRAM PageRank