program check
    use honeytools !, only: say_hello
    use assert_m, only : assert
    implicit none
  
    type(hex)               :: a,b,c
    type(hex),dimension(6)  :: neighborhood
    integer                 :: i
  
    call say_hello()

    a = hex(1,2,-3)
    b = hex(q=1,r=-2,s=1)

    call hex_print([a,b])

    call assert(a == a,"a == a")
    call assert(b == b,"b == b")

    print*, a==a, "(a == a)"
    print*, a==b, "(a == b)"
    print*, a/=a, "(a != a)"
    print*, a/=b, "(a != b)"

    c = a + b
    c = a - b

    print*, hex_distance(a,b), " = dist(a-b)"
    print*, hex_norm(c), "= |a-b|"

    i = a * b

    print*, i, "= <a,b> = c"

    c = i * c

    print*, hex_norm(c), "= |i*c| = d"

    c = c * i

    print*, hex_norm(c), "= |i*d|"

    print*, "Neighbors of hex a are [q,r,s]:"
    do i = -2, 3
        c = hex_hop(a,i)
        open(unit=7,action='write')
        call hex_print(c,quiet=.true.,unit=7)
        call hex_print(c,quiet=.true.)
    end do

    print*, "Let's recompute them all at once:"
    neighborhood = hex_nearest(a)
    call hex_print(neighborhood,quiet=.true.)
    open(unit=8,action='write')
    call hex_print(neighborhood,quiet=.true.,unit=8)

end program check
