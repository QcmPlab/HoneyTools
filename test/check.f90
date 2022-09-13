program check
    use honeytools !, only: say_hello
    use assert_m, only : assert
    implicit none
  
    type(hex) :: a,b,c
    integer   :: i
  
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
    
end program check
