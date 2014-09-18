program exec
implicit none
integer a(2),b(2);
equivalence (a(1),b(1))
a(1)=1;a(2)=2
write (*,*) b
end program
