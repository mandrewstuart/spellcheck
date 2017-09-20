f = open('data','r')
l = f.read().split('\n')
l = list(set(['"' + x.replace('"','')[:100] + '"' for x in l][:-1]))
#l = [x for (y,x) in sorted(zip([len(z) for z in l],l))]

f = open('forpy.f90','a')
f.write("""function proximity(str1, str2) result(p)
  real :: reward = 0, penalty = 0, p
  integer(1) :: counter1, counter2
  character(len=*) :: str1, str2

  reward = 0
  penalty = 0
  do counter1 = 1, len(str1), 1
    do counter2 = counter1, len(str1), 1
      if(counter2-counter1 > 4)then
        exit
      end if
      if(INDEX(str2, str1(counter1:counter2)) /= 0)then
        reward = reward + 2*((counter2-counter1) + 1)
      else
        penalty = penalty + 1
      end if
    end do
    if(penalty > 32)then
        exit
    end if
  end do

  do counter1 = 1, len(str2), 1
    do counter2 = counter1, len(str2), 1
      if(counter2-counter1 > 4)then
        exit
      end if
      if(INDEX(str1, str2(counter1:counter2)) /= 0)then
        reward = reward + 2*((counter2-counter1) + 1)
      else
        penalty = penalty + 1
      end if
    end do
    if(penalty > 64)then
        exit
    end if
  end do

  p = reward/(reward + penalty)

end function


program main
  CHARACTER(len=50) :: arg
  CHARACTER(len=100) :: final
  real :: max_score = 0, temp_score
  character(100), dimension(""" + str(len(l)) + """) :: test
  integer :: i
""")

for i in range(len(l)):
    f.write("  test(" + str(i+1) + ") = " + l[i] + "\n")


f.write("""  CALL getarg(1, arg)
  do i = 1, size(test)
    temp_score = proximity(arg, test(i))
    if(temp_score > max_score)then
      max_score = temp_score
      final = test(i)
      write (*,*) final
    end if
  end do

  WRITE (*,*) "\\n"
  WRITE (*,*) arg
  WRITE (*,*) "Matches best to"
  WRITE (*,*) final
end program""")

f.close()
