module Extension where

struct S1 {
    x1: int32;
    assert(x1 == 1);
}

struct S2 extends S1 {
    x2: int32;
    assert(x1 <= x2);
}

struct S3 extends S2 {
    x3: int32;
    assert(x2 <= x3);
}

transform test(s3: S3) -> bit {
    // assert(s3.x1 <= s3.x2);
    // assert(s3.x2 <= s3.x3);
    // assert(s3.x1 <= s3.x3);

    let _ = S3 {x1 = 1; x2 = 2; x3 = 3};
    let _ = S3 {x1 = s3.x1; x2 = s3.x2; x3 = s3.x3};
    // let _ = S3 {x1 = 1; x2 = 3; x3 = 2}; // FAILURE

    true
}


