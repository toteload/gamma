pub enum Either<A, B> {
    Left(A),
    Right(B),
}

impl<A, B> Either<A, B> {
    fn is_left(&self) -> bool {
        matches!(self, Either::Left(_))
    }

    fn is_right(&self) -> bool {
        matches!(self, Either::Right(_))
    }
}
