> -- ***********************************************************
> -- �����: �������� �., ���檨� �.�. (26.09.2016, 16:06-16:14)
> --------------------------------------------------------------
> gamma x | x==1   = 1
>         | x==0.5 = sqrt pi
>         | True   = (x-1)*gamma (x-1)

> test1 = zip (map (\x -> gamma x) [3/2,2..11/2])
>             [1/2*sqrt pi, 1, 3/4*sqrt pi, 2, 15/8*sqrt pi,
>                           6, 105/16*sqrt pi, 24, 945/32*sqrt pi]

> -- ***********************************************************
> -- �����: �������� �., ���檨� �.�. (26.09.2016, 16:14-16:18)
> --------------------------------------------------------------
> s n = 2 * pi ** z / gamma z
>      where z = (n + 1)/2

> test2 = zip (map s [1..8])
>             [2*pi, 4*pi, 2*pi^^2, 8/3*pi^^2, pi^^3, 16/15*pi^^3,
>              1/3*pi^^4, 32/105*pi^^4]

> -- ***********************************************************
> -- �����: �������� �., ���檨� �.�. (26.09.2016, 16:18-16:23)
> --------------------------------------------------------------
> v n = pi ** z / gamma (1+z)
>      where z = n/2

> test3 = zip (map v [1..8])
>             [2,pi,4/3*pi,1/2*pi^^2,8/15*pi^^2,1/6*pi^^3,
>              16/105*pi^^3,1/24*pi^^4]

> test4 = map (\x -> (x,s x)) [1..100]
> test5 = map (\x -> (x,v x)) [1..100]
