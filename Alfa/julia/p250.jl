using Memoize
using Lazy

include("common.jl")

function wall (ls)
    x = ls [1]
    if x == 3
        return [vcat([0],ls)]
    elseif x == 2
        return [vcat([0],ls)]
    elseif x == 1
        return []
    elseif x == 0
        return ls
    else
        mpa = wall (vcat ([x-2],ls))
        mpb = wall (vcat ([x-3],ls))
        if isempty (mpa)
            return mpb
        elseif isempty (mpb)
            return mpa
        else
            return collect (mpa,mpb)
        end
    end
end

function sol249b (lim :: Int)
    prs = sieve (lim)
    rprs = reverse (prs)
    refs = sieve (sum (prs))
    modi :: BigInt = ^(BigInt (10),16)
    @memoize function count (n)
        if n == 1
            return 0
        elseif n < 0
            return 0
        elseif n == 0
            return 1
        else
            res :: BigInt = 0
            for p in rprs
                r = n-p
                if r <= p
                    res += count (r)
                    if res > modi
                        res -= modi
                    end
                elseif r > p
                    res += count (r)
                    if res > modi
                        res -= modi
                    end
                end
            end
            return res
        end
    end
    resti :: BigInt = 0
    for p in refs
        resti += count (p)
        if resti > modi
            resti -= modi
        end
    end
    return resti
end

function sol249 (lim :: Int)
    modi :: BigInt = ^(10,16)
    prs :: Array = sieve (lim)
    npr :: Dict = Dict ()
    nprs :: Dict = Dict ()
    nprss :: Dict = Dict ()

    function mergi (x,y)
        return rem (x+y, modi)
    end
    
    for p in prs
        tmp = keys (npr)
        tmp1 = map (x-> x+p, tmp)
        nprs = Dict ()
        for t in tmp1
            nprs [t] = npr [t-p]
        end
        nprs = mergewith (+, nprs,{p => 1})
        nprss = mergewith (mergi, npr,nprs)
        npr = nprss
        println (p)
    end
    res :: BigInt = 0
    for p in nprss
        if prime (p [1])
            res += p [2]
            if res > modi
                res %= modi
            end
        end
    end
    return rem (res,modi)
end


#=  (defn sol249d
   [^long lim]
     (loop [[x & xs] primes npr {}]
      (if x
       (let [nprs (->> (map #(+ x %) (keys npr))
                        (map #(vector % (npr (- % x))))
                         (into {})
                         (merge-with + {x 1}))
                        nprss (merge-with +' npr nprs)]
                   (do (println x)
                    (recur xs nprss)))
             (loop [[i & is] (keys npr) res (bigint 0)]
              (if i
               (if (aget refs i)
                (recur is (rem (+ res (npr i)) modi))
                (recur is res))
               res)))))))
               =#

function index (n::Int,a::Array)
    i :: Int = 1
    for p in a
        if p == n
            return i
        end
        i += 1
    end
end


function sol249c (lim :: Int)
    prs = sieve (lim)
    refs = zeros (Int,sum (prs))
    for i = 1:length (prs)
        p = prs [i]
        refs [p] += 1
        for r in prs [(1+i):end]
            refs [p+r] += 1
        end
    end
    res :: Int = 0
    modi :: Int = 10^16
    for i = 1:length (refs)
        if prime (i)
            res += refs [i]
            if res > modi
                res -= modi
            end
        end
    end
    return res
end

function sol249d (lim :: Int)
    prs = sieve (lim)
    refs = sieve (sum (prs))
    modi :: BigInt = ^(BigInt (10),16)
    @memoize function check (n :: Int)
        if n == 0
            return BigInt (1)
        elseif n == 1
            return BigInt (0)
        elseif n < 0
            return BigInt (0)
        elseif in (n,prs)
            return BigInt (1)
        else
            p = prevprime (n)
            r = n-p
            return check (r) + check (prevprime (r))
        end
    end

    return length (prs)+ rem (BigInt (sum (map (check, refs))), modi)
end

function sol204 (lim :: Int)
    bahan :: Array = sieve (100)
    res = list (1)
    for prime in bahan
        resi = takewhile (x -> x < lim, iterate (x-> x*prime, prime))
        println (prime)
        for r in res
            resj = Int []
            for ri in resi
                tmp = r * ri
                if tmp > lim
                    break
                end
                push! (resj, tmp)
            end
            res = concat (res, apply (list, resj))
        end
    end
    length (res)
end











