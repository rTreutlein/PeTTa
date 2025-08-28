(= (fib $N)
          (if (lt $N 2)
              $N
              (plus (fib (minus $N 1))
                    (fib (minus $N 2)))))

!(eq (fib 30) 832040)
