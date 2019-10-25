extension Int {
    func bounded(to range: ClosedRange<Int>) -> Int {
        switch self {
        case ...range.lowerBound:
            return range.lowerBound
        case range.upperBound...:
            return range.upperBound
        default:
            return self
        }
    }
}
