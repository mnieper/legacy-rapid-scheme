function RapidModule(stdlib, foreign, heap) {
    'use asm';
    var a = 0;
    var i = 0;
    var f = 8;
    var e = 0;
    var p = 0;
    var s = 0;
    var g0 = 0;
    var imul = stdlib.Math.imul;
    var exit = foreign.exit;
    var writeString = foreign.writeString;
    var memoryError = foreign.memoryError;
    var callError = foreign.callError;
    var applicationError = foreign.applicationError;
    var h32 = new stdlib.Int32Array(heap);
    var hu8 = new stdlib.Uint8Array(heap);

    function alloc(s) {
        s = s | 0;
        var p = 0;
        p = f;
        f = (f + s) | 0;
        if ((f | 0) > 1000000) {
            memoryError();
        }
        return p | 0;
    }

    function procedure(l, f) {
        l = l | 0;
        f = f | 0;
        var p = 0;
        p = alloc(8) | 0;
        h32[p >> 2] = l;
        h32[(p + 4) >> 2] = f;
        p = p | 0x3;
        return p | 0;
    }

    function sum(i1, i2) { /*FIXME: sum gets a vector, list*/
        i1 = i1 | 0;
        i2 = i2 | 0;
        return (i1 + i2) | 0;
    }

    function difference(i1, i2) {
        i1 = i1 | 0;
        i2 = i2 | 0;
        return (i1 - i2) | 0;
    }

    function product(i1, i2) {
        i1 = i1 | 0;
        i2 = i2 | 0;
        return ((imul(i1 >> 1, i2 >> 1)|0) << 1) | 0;
    }

    function isIncreasing(i1, i2) {
        i1 = i1 | 0;
        i2 = i2 | 0;
        return ((i1 | 0) < (i2 | 0) ? 0x00010001 : 0x00000001)|0;
    }

    function truncateRemainder(i1, i2) {
        i1 = i1 | 0;
        i2 = i2 | 0;
        return ((i1 >> 1) % (i2 >> 1)) << 1 | 0;
    }

    function numberToString(i) {
        i = i | 0;
        var n = 0;
        var j = 0;
        var d = 0;
        var p = 0;
        var m = 0;
        i = i >> 1;
        if ((i | 0) < 0) {
            i = -i | 0;
            d = 1;
            n = n + 1 | 0;
        }
        if ((i | 0) == 0) {
            n = 1
        } else {
            for (j = i;
                (j | 0) != 0; j = (j | 0) / 10 | 0) {
                n = n + 1 | 0;
            }
        }
        s = alloc(n + 16 & 0xfffffff8) | 0;
        h32[s >> 2] = 0x0;
        h32[s + 4 >> 2] = n | 0;
        if ((d | 0) == 1) {
            hu8[s + 8 | 0] = 0x2d;
        }
        hu8[(s + 8 | 0) + n | 0] = 0x0;
        for (j = n - 1 | 0;
            (j | 0) >= 0; j = j - 1 | 0) {
            m = (i | 0) % 10 | 0;
            hu8[(s + 8 | 0) + j | 0] = m + 0x30 | 0;
            i = ((i - m) | 0) / 10 | 0;
        }
        return s | 0;
    }

    function frame(n) {
        n = n | 0;
        var p = 0;
        p = alloc(((n + 3) << 2) & 0xfffffff8) | 0;
        h32[p >> 2] = n;
        return p | 0;
    }

    function run() {
        while (1) {
            switch (i | 0) {
            case 0:
                p = frame(1) | 0;
                h32[(p + 8) >> 2] = procedure(1, e) | 0;
                e = p;
                p = 0;
                writeString((s = alloc(32) | 0, h32[s >> 2] = 0x0, h32[s + 4 >> 2] = 18, hu8[s + 8 | 0] = 0x41, hu8[s + 9 | 0] = 0x64, hu8[s + 10 | 0] = 0x64, hu8[s + 11 | 0] = 0x69, hu8[s + 12 | 0] = 0x74, hu8[s + 13 | 0] = 0x69, hu8[s + 14 | 0] = 0x6f, hu8[s + 15 | 0] = 0x6e, hu8[s + 16 | 0] = 0x20, hu8[s + 17 | 0] = 0x65, hu8[s + 18 | 0] = 0x78, hu8[s + 19 | 0] = 0x61, hu8[s + 20 | 0] = 0x6d, hu8[s + 21 | 0] = 0x70, hu8[s + 22 | 0] = 0x6c, hu8[s + 23 | 0] = 0x65, hu8[s + 24 | 0] = 0x3a, hu8[s + 25 | 0] = 0xa, hu8[s + 26 | 0] = 0x0, s) | 0) | 0;
                g0 = 40;
                g0 = 30;
                writeString(numberToString(sum(20, g0) | 0) | 0) | 0;
                writeString((s = alloc(16) | 0, h32[s >> 2] = 0x0, h32[s + 4 >> 2] = 1, hu8[s + 8 | 0] = 0xa, hu8[s + 9 | 0] = 0x0, s) | 0) | 0;
                p = frame(1) | 0;
                h32[(p + 8) >> 2] = 0x00010001;
                a = h32[(e + 8) >> 2] | 0;
                if ((a & 0x80000007) >>> 0 != 0x3) {
                    applicationError();
                }
                a = a & 0x7ffffff8;
                h32[(p + 4) >> 2] = h32[a + 4 >> 2] | 0;
                e = p;
                p = 0;
                i = h32[a >> 2] | 0;
                break;;
            case 1:
                a = h32[e >> 2] | 0;
                if (a | 0 == 1) {
                    exit(h32[(e + 8) >> 2] | 0) | 0;
                } else {
                    callError();
                }
            }
        }
    }
    return run;
}
