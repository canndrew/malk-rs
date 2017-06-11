## Strings

Malk has utf-8 string literals. There is a type family `Utf8: Um -> Type`
where `Utf8 n` is the type of strings of length `n`. Here "length" refers to
the length in bytes, not characters. The literal `"foo"` has type `&'static
Utf8 3`.

There are a number of helper types for handling strings. `StrRef: Region -> Type` holds
both the length of a string and a reference to a string of that length.

    let StrRef = (lt: Region) => #{
        length: Um,
        string: &'lt Utf8 length,
    };

Similarly there is `StrBox` which is an owned pointer to a string. `StrInline`
is an (unsized) type which holds a length and the string data inline.

    let StrInline = #{
        length: Um,
        string: Utf8 length,
    };






    Inline: Type -> Type
    impl (Ty: Type): Deref (Inline Ty) = ...;

    Box: Type -> Type
    impl (Ty: Type): Deref (Box Ty) = ...;


    Str: (Container: Type -> Type) -> ?(Deref Container) -> Type
    Str Box
    Str Inline
    Str Rc

    str: (?Container: Type -> Type)
      -> (?length: Um)
      -> (?_: Deref Container)
      -> Container (Utf8 length)
      -> Str Container

    // convert a `Ref (Utf8 5)` to a `Str Ref`
    let x: Ref (Utf8 5) = "hello";
    let x: Str Ref = str x;

    // convert a `Ref (Utf8 5)` to a `Str Box`
    let x: Ref (Utf8 5) = "hello";
    let x: Box (Utf8 5) = box *x;
    let x: Str Box = str x;

