pub trait Parseable<T, E> {
    fn parse(self) -> Result<T, E>;
}

pub trait Pushable<T> {
    fn push(&mut self, elem: T);
}
