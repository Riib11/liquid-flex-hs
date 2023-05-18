module Temperature where

struct Temperature {
    celcius: float64;
    fahrenheit: float64;
    assert(((celcius * 9.0) / 5.0) + 32.0 == fahrenheit);
}

transform addTemperatures(t1: Temperature, t2: Temperature) -> Temperature {
    assert ((t1.celcius * 9.0) / 5.0) + 32.0 == t1.fahrenheit;
    assert ((t2.celcius * 9.0) / 5.0) + 32.0 == t2.fahrenheit;
    // assert (((t1.celcius + t2.celcius) * 9.0) / 5.0) + 32.0 == (t1.fahrenheit + t2.fahrenheit); // FAILS
    // Temperature{
    //     celcius = t1.celcius + t2.celcius;
    //     fahrenheit = t1.fahrenheit + t2.fahrenheit
    // }
    t1
}
