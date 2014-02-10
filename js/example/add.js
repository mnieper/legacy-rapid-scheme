function RapidModule(stdlib, foreign, heap) {
    'use asm';
    var a = 0;
    var i = 0;
    var f = 8;
    var e = 0;
    var p = 0;
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
        p = p | 0x02;
        return p | 0;
    }

    function sum(i1, i2) { /*FIXME: sum gets a vector, list*/
        i1 = i1 | 0;
        i2 = i2 | 0;
        return (i1 + i2) | 0;
    }

    function frame(n) {
        n = n | 0;
        var p = 0;
        p = alloc(((n + 3) << 4) & 0xfffffff8) | 0;
        h32[p >> 2] = n;
        return p|0;
    }

    function run() {
        while (1) {
            switch (i | 0) {
            case 0:
                p = frame(1) | 0;
                h32[(p + 8) >> 2] = procedure(1, (e + 4)|0) | 0;
                a = procedure(2, (e + 4)|0) | 0;
                if ((a & 0x80000007)>>>0 != 0x02) {
                    applicationError();
                }
                a = a & 0x7ffffff8;
                h32[(p + 4) >> 2] = h32[(a + 4) >> 2] | 0;
                e = p;
                p = 0;
                i = h32[a >> 2] | 0;
                break;
            case 2:
                a = h32[e >> 2] | 0;
                if (a | 0 == 1) {
                    writeString(sum(1, 2)|0);
                    p = frame(1) | 0;
                    h32[(p + 8) >> 2] = 0x00010001;
                    a = h32[(e + 8) >> 2] | 0;
                    if ((a & 0x80000007)>>>0 != 0x02) {
                        applicationError();
                    }
                    a = a & 0x7ffffff8;
                    h32[(p + 4) >> 2] = h32[(a + 4) >> 2] | 0;
                    e = p;
                    p = 0;
                    i = h32[a >> 2] | 0;
                    break;
                } else {
                    callError();
                }
            case 1:
                a = h32[e >> 2] | 0;
                if (a | 0 == 1) {
                    exit(h32[(e + 8) >> 2] | 0);
                } else {
                    callError();
                }
            }
        }
    }
    return run;
}
