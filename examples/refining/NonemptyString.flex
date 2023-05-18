module NonemptyString where

message NonemptyString {
    str: string;
    assert(str != "");
}
