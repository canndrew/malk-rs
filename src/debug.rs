use std::ops::Carrier;

#[cfg(debug_assertions)]
#[derive(Debug, Hash, Clone)]
pub struct Debug<T>(T);

#[cfg(not(debug_assertions))]
#[derive(Debug, Hash, Clone)]
pub struct Debug<T>(PhantomData<T>);

#[cfg(debug_assertions)]
impl<T> From<T> for Debug<T> {
    fn from(t: T) -> Self {
        Debug(t)
    }
}

#[cfg(not(debug_assertions))]
impl<T> From<T> for Debug<T> {
    fn from(_t: T) -> Self {
        Debug(PhantomData)
    }
}

#[cfg(debug_assertions)]
impl<T> Carrier for Debug<T> {
    type Success = T;
    type Error = !;

    fn from_success(t: T) -> Self {
        Debug(t)
    }

    fn from_error(n: !) -> Self {
        n
    }

    fn translate<U>(self) -> U
        where U: Carrier<Success=T, Error=!>
    {
        let Debug(t) = self;
        U::from_success(t)
    }
}

#[cfg(not(debug_assertions))]
impl<T> Carrier for Debug<T> {
    type Success = !;
    type Error = ();

    fn from_success(n: !) -> Self {
        n
    }

    fn from_error((): ()) -> Self {
        Debug(PhantomData)
    }

    fn translate<U>(self) -> U
        where U: Carrier<Success=!, Error=()>
    {
        U::from_error(())
    }
}

#[cfg(debug_assertions)]
pub fn catch<T, F, R>(f: F) -> Debug<T>
    where F: FnOnce() -> Debug<T>
{
    f()
}

#[cfg(not(debug_assertions))]
pub fn catch<T, F, R>(_f: F) -> Debug<T>
    where F: FnOnce() -> Debug<T>
{
    Debug(PhantomData)
}

