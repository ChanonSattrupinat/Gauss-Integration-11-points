function objective_function(o) result(obj)
    implicit none
    real,intent(in):: o
    real:: obj
    obj=SIN(o)
end function objective_function

program hello
    implicit none
    real,dimension(11) :: p,b
    real :: Upper_boundary,Lower_boundary,x,result,objective_function,I2,add_dx
    integer :: i
    print *,"****************************************"
    print *," GAUSS INTEGRATION 11 POINTS ALGORITHM "
    print *,"****************************************"
    print *,"Input intergral origin point :"
    read*,Lower_boundary
    print *,"****************************************"
    print *,"Input intergral destination point :"
    read*,Upper_boundary
    print *,"****************************************"
    p(1)=-0.9782286582
    p(2)=-0.8870625998
    p(3)=-0.7301520056
    p(4)=-0.5190961292
    p(5)=-0.269543156
    p(6)=0
    p(7)=0.269543156
    p(8)=0.5190961292
    p(9)=0.7301520056
    p(10)=0.8870625998
    p(11)=0.9782286582
    b(1)=0.0556685671
    b(2)=0.1255803695
    b(3)=0.186290211 
    b(4)=0.2331937646
    b(5)=0.2628045445
    b(6)=0.272925087
    b(7)=0.2628045445
    b(8)=0.233193765
    b(9)=0.1862902109
    b(10)=0.125580369
    b(11)=0.05566856712
    I2=0
    do i=1,11
        x=((Lower_boundary+Upper_boundary)/2)+(((Upper_boundary-Lower_boundary)/2)*p(i))
        result = objective_function(x)
        I2=I2+(b(i)*result)
    end do
    add_dx=((Upper_boundary-Lower_boundary)/2)
    I2=add_dx*I2
    print *,"********************************************"
    print *,"RESULT OF GAUSS INTEGRATION 11 POINTS : ",I2
    print *,"********************************************"
    pause '############END#############'
end program hello


