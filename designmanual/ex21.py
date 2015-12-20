def mystery(n):
    r = 0
    for i in range(1, n):
        print("i: %s" % i)
        for j in range(i + 1, n + 1):
            print("\tj: %s" % j)
            for k in range(1, j + 1):
                print("\t\tk: %s" % k)
                r = r + 1
    return r
