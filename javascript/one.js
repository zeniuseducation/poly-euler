function is_prime (p) {
    var stat = true;
    if (p < 2) {
        return stat = false;
    } else if (p == 2) {
        return stat = true;
    } else if (0 == (p % 2)) {
        return stat = false;
    } else {
        var i = 3;
        var lim = Math.sqrt(p);
        while (i <= lim) {
            if (0 == (p % i)) {
                return stat = false;
            }
            i += 2;
        }
        return stat;
    }
    return stat;
}

function odd_prime(p) {
    var i = 3;
    while ((i*i) <= p) {
        if (0 == (p % i)) {
            return false;
        }
        i += 2;
    }
    return true;
}

function sum_primes(lim) {
    var res = 2;
    for (var i = 3; i < lim; i += 2) {
        if (odd_prime(i)) {
            res += i;
        }
    }
    return res;
}

function sum_sieves (lim) {
    var refs = [];
    for (var i = 0; i <= lim; i++) {
        refs.push(false);
    }
    i = 3;
    res = 0, llim = Math.sqrt(lim)
    while (i < lim) {
        if ((i <= llim) && (!refs[i])) {
            for (var j = i*i; j < lim; j += (2*i)) {
                refs[j] = true;
            };
        };
        if (!refs[i]) {
            res += i;
        };
        i += 2;
    }
    return 2+res;
}

function main() {
    var start = new Date().getTime();
    var result = sum_primes(2000000);
    var duration = new Date().getTime() - start;
    alert("result " + result + "... executed in " + duration + "ms");
}

main();